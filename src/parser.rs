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
/// expr := assignment | lambda_expr | conditional_expr
///   - assignment := identifier "=" expr
///   - lambda_expr := "lambda" identifier ":" expr
///   - conditional_expr := comparison ( "if" comparison "else" comparison )?
///
/// comparison := additive ((">" | "<" | ">=" | "<=" | "==" | "!=") additive)*
/// additive := multiplicative (("+" | "-") multiplicative)*
/// multiplicative := exponentiation (("*" | "/" | "%") exponentiation)*
/// exponentiation := primary ("**" exponentiation)?
///
/// primary := number | string_lit | identifier | lambda_expr | parenthesized_or_tuple | list_literal | dict_or_set
///    (さらに後ろに [expr] → index_access や (args) → call_expr が続く可能性)
///
/// list_literal := "[" (  (expr ("," expr)*)?  or  (expr "for" identifier "in" expr (if expr)? )  )  "]"
/// dict_or_set := "{" ... "}"
///
/// ※ 簡略版のため、実際のPython文法とは異なる部分があります

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
    let (input, first) = alt((lambda_expr, conditional_expr))(input)?;
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

fn conditional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, if_true_expr) = comparison(input)?;

    // "if" ... "else" ... があれば三項演算子
    let (input, maybe_if) = opt(preceded(multispace0, tag("if")))(input)?;
    if maybe_if.is_none() {
        return Ok((input, if_true_expr));
    }
    let (input, _) = multispace0(input)?;
    let (input, condition_expr) = comparison(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("else")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, if_false_expr) = comparison(input)?;

    Ok((
        input,
        Expr::IfExpr {
            condition: Box::new(condition_expr),
            if_true: Box::new(if_true_expr),
            if_false: Box::new(if_false_expr),
        },
    ))
}

// comparison: additive ( (">=" | ">" | "==" | "!=" | "<=" | "<") additive )*
fn comparison(input: &str) -> IResult<&str, Expr> {
    let (input, mut expr) = additive(input)?;
    let (mut input, _) = multispace0(input)?;

    loop {
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

// additive: multiplicative (("+" | "-") multiplicative)*
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

// multiplicative: exponentiation (("*" | "/" | "%") exponentiation)*
fn multiplicative(input: &str) -> IResult<&str, Expr> {
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

// exponentiation: primary ("**" exponentiation)? (右結合)
fn exponentiation(input: &str) -> IResult<&str, Expr> {
    let (input, base_expr) = primary(input)?;
    let (input, _) = multispace0(input)?;

    if let Ok((input2, _)) =
        preceded(multispace0::<&str, nom::error::Error<&str>>, tag("**"))(input)
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

// primary: number | string_lit | paren/tuple | list_literal | dict_or_set | call_or_var
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
    // index / slice / etc...
    index_access(input, expr)
}

// index_access: expr[...] の連鎖
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

// list_literal: "[" ... "]"
//   - 空リスト []
//   - [ expr ("," expr)* ]
//   - [ <elem_expr> for <var> in <iter_expr> if <cond>? ]
fn list_literal(input: &str) -> IResult<&str, Expr> {
    // "["
    let (input, _) = preceded(multispace0, char('['))(input)?;
    let (input, _) = multispace0(input)?;

    if let Ok((input2, _)) =
        preceded(multispace0::<&str, nom::error::Error<&str>>, char(']'))(input)
    {
        return Ok((input2, Expr::List(vec![])));
    }

    // まず1つ目の要素(または要素式)をパース
    let (input_expr, first_expr) = expr_wrapper(input)?;
    let (input_expr, _) = multispace0(input_expr)?;

    // "for" があれば内包表記
    if let Ok((input_for, _)) =
        preceded(multispace0::<&str, nom::error::Error<&str>>, tag("for"))(input_expr)
    {
        parse_list_comprehension(input_for, first_expr)
    } else {
        parse_normal_list(input_expr, first_expr)
    }
}

//---------------------------------------------------------
// 内包表記: [ <elem_expr> for <var> in <iter_expr> (if <cond_expr>)? ]
//   ただし <iter_expr> が三項演算子とフィルタ if で衝突する場合があるため
//   バックトラックを使う
//---------------------------------------------------------
fn parse_list_comprehension(input: &str, elem_expr: Expr) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, var_name) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("in")(input)?;
    let (input, _) = multispace0(input)?;
    let (mut input, (iter_expr, filter_if_taken)) = parse_iter_expr_with_backtracking(input)?;

    // filter_if_taken == true の場合、すでに「if」が先に消費されている
    // filter_if_taken == false の場合は、これから if があるかチェック
    let (input, cond_expr) = if filter_if_taken {
        (input, None)
    } else {
        let mut cexpr = None;
        if let Ok((input_if, _)) =
            preceded(multispace0::<&str, nom::error::Error<&str>>, tag("if"))(input)
        {
            let (input_if, condition) = expr_wrapper(input_if)?;
            cexpr = Some(Box::new(condition));
            input = input_if;
        }
        (input, cexpr)
    };

    // "]"
    let (input, _) = multispace0(input)?;
    let (input, _) = char(']')(input)?;

    Ok((
        input,
        Expr::ListComp {
            expr: Box::new(elem_expr),
            var: var_name,
            iter: Box::new(iter_expr),
            cond: cond_expr,
        },
    ))
}

fn parse_iter_expr_with_backtracking(input: &str) -> IResult<&str, (Expr, bool)> {
    let parse_result = expr_wrapper(input);
    if parse_result.is_ok() {
        // そのまま成功
        let (rest, expr) = parse_result.unwrap();
        return Ok((rest, (expr, false)));
    }

    let (rest, fallback_expr) = alt((number, string_lit, var_expr))(input)?;

    Ok((rest, (fallback_expr, false)))
}

fn parse_normal_list(mut input: &str, first_expr: Expr) -> IResult<&str, Expr> {
    let mut elems = vec![first_expr];

    loop {
        // カンマがあるか
        let (input2, opt_comma) = opt(preceded(multispace0, char(',')))(input)?;
        if opt_comma.is_some() {
            let (input2, _) = multispace0(input2)?;
            // 次の要素をパース
            if let Ok((input2, next_expr)) = expr_wrapper(input2) {
                elems.push(next_expr);
                input = input2;
                continue;
            } else {
                // カンマ後に式が無ければ末尾カンマ扱いで終了
                input = input2;
                break;
            }
        } else {
            input = input2;
            break;
        }
    }

    let (input, _) = multispace0(input)?;
    let (input, _) = char(']')(input)?;
    Ok((input, Expr::List(elems)))
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
        // {} は空dictとする
        return Ok((input, Expr::Dict(vec![])));
    }

    let is_dict = items.iter().any(|(_, v)| v.is_some());
    if is_dict {
        // dict
        let mut dict_pairs = vec![];
        for (k_expr, v_opt) in items {
            let v_expr =
                v_opt.ok_or_else(|| nom::Err::Failure(Error::new(input, ErrorKind::Tag)))?;
            match k_expr {
                Expr::Var(k_str) => dict_pairs.push((k_str, v_expr)),
                Expr::StringLit(k_str) => dict_pairs.push((k_str, v_expr)),
                _ => {
                    return Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag)));
                }
            }
        }
        Ok((input, Expr::Dict(dict_pairs)))
    } else {
        // set
        let mut set_exprs = vec![];
        for (expr, none_val) in items {
            if none_val.is_some() {
                return Err(nom::Err::Failure(Error::new(input, ErrorKind::Tag)));
            }
            set_exprs.push(expr);
        }
        Ok((input, Expr::Set(set_exprs)))
    }
}

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
            // () は空タプル
            Ok((input, Expr::Tuple(vec![])))
        }
        1 => {
            // (expr) は単一expr
            Ok((input, exprs.into_iter().next().unwrap()))
        }
        _ => {
            // 複数 => タプル
            Ok((input, Expr::Tuple(exprs)))
        }
    }
}

fn call_or_var(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((lambda_expr, var_expr))(input)?;
    let (input, _) = multispace0(input)?;

    // 関数呼び出し?
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
