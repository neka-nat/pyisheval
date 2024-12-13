use crate::ast::{BinOp, Expr};
use nom::error::Error;
use nom::error::ErrorKind;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{char, multispace0},
    combinator::opt,
    multi::separated_list0,
    sequence::{delimited, pair, preceded},
    IResult,
};

/// Grammar (informal):
/// expr := assignment | comparison | lambda_expr | additive
/// assignment := identifier "=" expr
/// lambda_expr := "lambda" identifier ":" expr
/// comparison := additive ((">" | "<" | ">=" | "<=" | "==" | "!=") additive)*
/// additive := multiplicative (("+" | "-") multiplicative)*
/// multiplicative := primary (("*" | "/" | "%") primary)*
/// primary := number | identifier | "(" ... ")" | "[" ... "]" | "{" ... "}" | call_expr
/// call_expr := (identifier | lambda_expr) "(" [expr ("," expr)*] ")"
///
/// list := "[" [expr ("," expr)*] "]"
/// tuple := "(" [expr ("," expr)* [","]] ")"  
///   - 複数要素または末尾カンマありでタプルと判定する処理は簡略化
/// set := "{" [expr ("," expr)*] "}" with no ':' inside => set
/// dict := "{" [pair ("," pair)*] "}"  (pair := key ":" expr) keyはidentifier限定
///
/// number := [0-9]+ ("." [0-9]+)?
/// identifier := [a-zA-Z_][a-zA-Z0-9_]*

pub fn parse_expr(input: &str) -> Result<Expr, String> {
    let input = input.trim();
    let (rest, expr) = top_level_expr(input).map_err(|e| format!("Parse error: {:?}", e))?;
    if rest.trim().is_empty() {
        Ok(expr)
    } else {
        Err(format!("Unexpected trailing input: {}", rest))
    }
}

fn top_level_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    assignment(input)
}

fn assignment(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, first) = alt((lambda_expr, comparison))(input)?;
    let (input, _) = multispace0(input)?;

    let res = opt(pair(
        preceded(multispace0, char('=')),
        preceded(multispace0, expr_wrapper),
    ))(input)?;

    if let (input, Some((_, right))) = res {
        if let Expr::Var(name) = first {
            Ok((
                input,
                Expr::Assign {
                    name,
                    expr: Box::new(right),
                },
            ))
        } else {
            Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag)))
        }
    } else {
        Ok((res.0, first))
    }
}

fn expr_wrapper(input: &str) -> IResult<&str, Expr> {
    assignment(input)
}

fn lambda_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("lambda")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, param) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, body) = expr_wrapper(input)?;
    Ok((
        input,
        Expr::Lambda {
            param,
            body: Box::new(body),
        },
    ))
}

fn comparison(input: &str) -> IResult<&str, Expr> {
    let (input, mut expr) = additive(input)?;
    let (mut input, _) = multispace0(input)?;

    loop {
        // 比較演算子をパース
        // 順番に注意: ">="を先に試し、それから">"、最後に"=="を試す
        // 実際には == と > を同じ並びでaltしてもよいですが、
        // >= や == のような2文字演算子は先に試す方が安全です
        let (next_input, opt_op) = opt(alt((
            tag(">="),
            tag(">"),
            tag("=="),
            tag("!="),
            tag("<="),
            tag("<"),
        )))(input)?;

        if let Some(op_str) = opt_op {
            let op = match op_str {
                ">=" => BinOp::Ge,
                ">" => BinOp::Gt,
                "==" => BinOp::Eq,
                "!=" => BinOp::Ne,
                "<=" => BinOp::Le,
                "<" => BinOp::Lt,
                _ => unreachable!(),
            };

            let (next_input2, _) = multispace0(next_input)?;
            let (next_input2, right) = additive(next_input2)?;
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
            input = next_input2;
        } else {
            break;
        }
    }

    Ok((input, expr))
}

fn additive(input: &str) -> IResult<&str, Expr> {
    let (input, mut expr) = multiplicative(input)?;

    let (mut input, _) = multispace0(input)?;
    loop {
        let (next_input, opt_op) = opt(alt((char('+'), char('-'))))(input)?;
        if let Some(op_char) = opt_op {
            let op = match op_char {
                '+' => BinOp::Add,
                '-' => BinOp::Sub,
                _ => unreachable!(),
            };
            let (next_input2, _) = multispace0(next_input)?;
            let (next_input2, right) = multiplicative(next_input2)?;
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
            input = next_input2;
        } else {
            break;
        }
    }

    Ok((input, expr))
}

fn exponentiation(input: &str) -> IResult<&str, Expr> {
    let (input, base_expr) = primary(input)?;
    let (input, _) = multispace0::<&str, nom::error::Error<&str>>(input)?;
    // 右結合: exponentiationは再帰的パースする
    if let Ok((input2, _)) = preceded::<&str, &str, &str, nom::error::Error<&str>, _, _>(
        multispace0::<&str, nom::error::Error<&str>>,
        tag::<&str, &str, nom::error::Error<&str>>("**"),
    )(input)
    {
        let (input3, exp_expr) = exponentiation(input2)?;
        Ok((
            input3,
            Expr::BinaryOp {
                op: BinOp::Exp,
                left: Box::new(base_expr),
                right: Box::new(exp_expr),
            },
        ))
    } else {
        Ok((input, base_expr))
    }
}

fn multiplicative(input: &str) -> IResult<&str, Expr> {
    // multiplicative := exponentiation (("*" | "/" | "%") exponentiation)*
    let (input, mut expr) = exponentiation(input)?;
    let (mut input, _) = multispace0(input)?;

    loop {
        let (next_input, opt_op) = opt(alt((char('*'), char('/'), char('%'))))(input)?;
        if let Some(op_char) = opt_op {
            let op = match op_char {
                '*' => BinOp::Mul,
                '/' => BinOp::Div,
                '%' => BinOp::Mod,
                _ => unreachable!(),
            };
            let (next_input2, _) = multispace0(next_input)?;
            let (next_input2, right) = exponentiation(next_input2)?;
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
            input = next_input2;
        } else {
            break;
        }
    }

    Ok((input, expr))
}

fn primary(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, expr) = alt((
        parenthesized_or_tuple,
        list_literal,
        dict_or_set,
        number,
        string_lit,
        call_or_var,
    ))(input)?;
    index_access(input, expr)
}

fn index_access(input: &str, expr: Expr) -> IResult<&str, Expr> {
    let mut current_expr = expr;
    let (mut input, _) = multispace0(input)?;
    loop {
        let (next_input, maybe_idx) = opt(delimited(char('['), expr_wrapper, char(']')))(input)?;

        if let Some(idx_expr) = maybe_idx {
            current_expr = Expr::Index {
                expr: Box::new(current_expr),
                index: Box::new(idx_expr),
            };
            input = next_input;
        } else {
            break;
        }
    }
    Ok((input, current_expr))
}

fn list_literal(input: &str) -> IResult<&str, Expr> {
    // list_literalは基本的に`[ expr (, expr)* ]`をパースしていましたが、
    // `[`を読んだ後、`expr`を読んで`for`が続く場合はlist comprehensionと判断します。

    let (input, _) = preceded(multispace0, char('['))(input)?;
    let (input, _) = multispace0(input)?;

    // まず最初のexprをパース
    let (input, first_expr) = opt(expr_wrapper)(input)?;
    let (input, _) = multispace0(input)?;

    // list comprehensionかどうかチェック
    if let Some(fe) = first_expr {
        // `for`が続いているか
        if let Ok((input2, _)) = preceded::<&str, &str, &str, nom::error::Error<&str>, _, _>(
            multispace0::<&str, nom::error::Error<&str>>,
            tag::<&str, &str, nom::error::Error<&str>>("for"),
        )(input)
        {
            // list comprehension
            let (input2, _) = multispace0(input2)?;
            let (input2, var_name) = identifier(input2)?;
            let (input2, _) = multispace0(input2)?;
            let (input2, _) = tag("in")(input2)?;
            let (input2, _) = multispace0(input2)?;
            let (input2, iter_expr) = expr_wrapper(input2)?;
            let (input2, _) = multispace0(input2)?;

            // ifがあるか？
            let (input2, cond_expr) = if let Ok((input3, _)) =
                preceded::<&str, &str, &str, nom::error::Error<&str>, _, _>(
                    multispace0::<&str, nom::error::Error<&str>>,
                    tag::<&str, &str, nom::error::Error<&str>>("if"),
                )(input2)
            {
                let (input3, _) = multispace0(input3)?;
                let (input3, cexpr) = expr_wrapper(input3)?;
                (input3, Some(cexpr))
            } else {
                (input2, None)
            };

            let (input2, _) = multispace0(input2)?;
            let (input2, _) = char(']')(input2)?;

            return Ok((
                input2,
                Expr::ListComp {
                    expr: Box::new(fe),
                    var: var_name,
                    iter: Box::new(iter_expr),
                    cond: cond_expr.map(Box::new),
                },
            ));
        } else {
            // 単なるリスト
            // first_exprがあり、続いてカンマがあれば複数要素
            let mut elems = vec![fe];
            let (mut input, _) = multispace0(input)?;
            loop {
                let (ni, opt_comma) = opt(char(','))(input)?;
                if opt_comma.is_some() {
                    let (ni, _) = multispace0(ni)?;
                    // 次のexprをパース
                    let (ni, next_expr) = opt(expr_wrapper)(ni)?;
                    if let Some(ne) = next_expr {
                        elems.push(ne);
                        input = ni;
                        continue;
                    } else {
                        // カンマ後にexprがなければリスト終わり？
                        input = ni;
                        break;
                    }
                } else {
                    // カンマがないので終わり
                    input = ni;
                    break;
                }
            }
            let (input, _) = multispace0(input)?;
            let (input, _) = char(']')(input)?;
            return Ok((input, Expr::List(elems)));
        }
    } else {
        // 空リスト
        let (input, _) = multispace0(input)?;
        let (input, _) = char(']')(input)?;
        return Ok((input, Expr::List(vec![])));
    }
}

/// parse_dict_item_or_expr:
/// 辞書要素か単なるセット/expr要素かを判定するための関数。
/// `expr`または `identifier ":" expr` をパースする。
///  - `identifier ":" expr` => (Var(identifier), Some(expr))で返す => dict item
///  - それ以外 => (expr, None) => set要素
fn parse_dict_item_or_expr(input: &str) -> IResult<&str, (Expr, Option<Expr>)> {
    let (input, first_expr) = expr_wrapper(input)?;
    let (input, _) = multispace0(input)?;
    let (input, maybe_colon) = opt(char(':'))(input)?;
    if let Some(_) = maybe_colon {
        let (input, _) = multispace0(input)?;
        let (input, val_expr) = expr_wrapper(input)?;
        Ok((input, (first_expr, Some(val_expr))))
    } else {
        Ok((input, (first_expr, None)))
    }
}

fn dict_or_set(input: &str) -> IResult<&str, Expr> {
    let (input, pairs) = delimited(
        preceded(multispace0, char('{')),
        opt(separated_list0(
            preceded(multispace0, char(',')),
            parse_dict_item_or_expr,
        )),
        preceded(multispace0, char('}')),
    )(input)?;

    let items = pairs.unwrap_or_else(|| vec![]);
    if items.is_empty() {
        // {}は空のdictとする
        return Ok((input, Expr::Dict(vec![])));
    }

    let is_dict = items.iter().any(|(_, v)| v.is_some());
    if is_dict {
        // dictモード: 全てSome(value)である必要がある
        let mut dict_pairs = vec![];
        for (k_expr, v_opt) in items {
            let v_expr =
                v_opt.ok_or_else(|| nom::Err::Failure(Error::new(input, ErrorKind::Tag)))?;
            // キーはidentifier(Var)でなければならない
            if let Expr::Var(k_str) = k_expr {
                dict_pairs.push((k_str, v_expr));
            } else if let Expr::StringLit(k_str) = k_expr {
                dict_pairs.push((k_str, v_expr));
            } else {
                return Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag)));
            }
        }
        Ok((input, Expr::Dict(dict_pairs)))
    } else {
        // setモード: 全てNone(value)
        let mut set_exprs = vec![];
        for (expr, none_val) in items {
            if none_val.is_some() {
                // あり得ないパターン
                return Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag)));
            }
            set_exprs.push(expr);
        }
        Ok((input, Expr::Set(set_exprs)))
    }
}

fn parenthesized_or_tuple(input: &str) -> IResult<&str, Expr> {
    let (input, exprs) = delimited(
        preceded(multispace0, char('(')),
        opt(separated_list0(
            preceded(multispace0, char(',')),
            preceded(multispace0, expr_wrapper),
        )),
        preceded(multispace0, char(')')),
    )(input)?;

    let exprs = exprs.unwrap_or_else(|| vec![]);

    match exprs.len() {
        0 => {
            // ()は空タプル
            Ok((input, Expr::Tuple(vec![])))
        }
        1 => {
            // (expr)は単一exprとして返す (タプルでなく1要素のみ)
            Ok((input, exprs.into_iter().next().unwrap()))
        }
        _ => {
            // 複数要素 => タプル
            Ok((input, Expr::Tuple(exprs)))
        }
    }
}

fn call_or_var(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((lambda_expr, var_expr))(input)?;
    let (input, _) = multispace0(input)?;

    let (input, maybe_call) = opt(delimited(
        char('('),
        separated_list0(
            preceded(multispace0, char(',')),
            preceded(multispace0, expr_wrapper),
        ),
        char(')'),
    ))(input)?;

    if let Some(args) = maybe_call {
        Ok((
            input,
            Expr::Call {
                func: Box::new(e),
                args,
            },
        ))
    } else {
        Ok((input, e))
    }
}

fn var_expr(input: &str) -> IResult<&str, Expr> {
    let (input, var) = identifier(input)?;
    Ok((input, Expr::Var(var)))
}

fn number(input: &str) -> IResult<&str, Expr> {
    let (input, num_str) = recognize_float(input)?;
    let val: f64 = num_str.parse().unwrap();
    Ok((input, Expr::Number(val)))
}

fn string_lit(input: &str) -> IResult<&str, Expr> {
    // シングルクォートで囲まれた文字列を単純にパース
    let (input, _) = char('\'')(input)?;
    let (input, s) = take_while1(|c: char| c != '\'')(input)?;
    let (input, _) = char('\'')(input)?;
    Ok((input, Expr::StringLit(s.to_string())))
}

fn recognize_float(input: &str) -> IResult<&str, String> {
    let (input, integer_part) = take_while1(|c: char| c.is_ascii_digit())(input)?;
    let (input, fractional_part) = opt(preceded(
        char('.'),
        take_while1(|c: char| c.is_ascii_digit()),
    ))(input)?;

    if let Some(frac) = fractional_part {
        let full_num = format!("{}.{}", integer_part, frac);
        Ok((input, full_num))
    } else {
        Ok((input, integer_part.to_string()))
    }
}

fn identifier(input: &str) -> IResult<&str, String> {
    let (input, first) =
        nom::character::complete::satisfy(|c: char| c.is_alphabetic() || c == '_')(input)?;
    let (input, rest) =
        take_while1::<_, &str, Error<&str>>(|c: char| c.is_alphanumeric() || c == '_')(input)
            .or(Ok((input, "")))?;
    Ok((input, format!("{}{}", first, rest)))
}
