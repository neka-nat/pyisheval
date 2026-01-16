use crate::ast::{BinOp, Expr, UnOp};
use nom::error::Error;
use nom::error::ErrorKind;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, digit0, digit1, multispace0, one_of},
    combinator::{opt, recognize},
    multi::separated_list0,
    sequence::{delimited, pair, preceded, tuple},
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
/// exponentiation := unary_expr ("**" exponentiation)?  (右結合)
/// unary_expr := ("+" | "-")? primary
///
/// primary := number | string_lit | identifier | lambda_expr | parenthesized_or_tuple
///             | list_literal | dict_or_set
///    (さらに後ろに [expr] → index_access や (args) → call_expr が続く可能性)
///
/// list_literal := "[" ... "]" (list or list comprehension)
/// dict_or_set := "{" ... "}" (normal dict/set or dict comprehension)
///
/// dict_comprehension := "{" key_expr ":" value_expr "for" identifier "in" expr ( "if" expr )? "}"
/// list_comprehension := "[" elem_expr "for" identifier "in" expr ( "if" expr )? "]"
///
/// ※ 簡略版のため、実際のPython文法とは異なる部分があります

//---------------------------------------------------------
// パブリックなエントリポイント: parse_expr
//---------------------------------------------------------
pub fn parse_expr(input: &str) -> Result<Expr, String> {
    let input = input.trim();
    let (rest, expr) = top_level_expr(input).map_err(|e| format!("Parse error: {:?}", e))?;
    if rest.trim().is_empty() {
        Ok(expr)
    } else {
        Err(format!("Unexpected trailing input: {}", rest))
    }
}

//---------------------------------------------------------
// トップレベル: assignment / lambda / conditional_expr
//---------------------------------------------------------
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

//---------------------------------------------------------
// lambda_expr: "lambda <param> : <body>"
//---------------------------------------------------------
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

//---------------------------------------------------------
// 三項演算子 (cond_expr if condition else cond_expr)
//---------------------------------------------------------
fn conditional_expr(input: &str) -> IResult<&str, Expr> {
    let (input, if_true_expr) = logical_or(input)?;

    let (input, maybe_if) = opt(preceded(multispace0, tag("if")))(input)?;
    if maybe_if.is_none() {
        return Ok((input, if_true_expr));
    }
    let (input, _) = multispace0(input)?;
    let (input, condition_expr) = logical_or(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("else")(input)?;
    let (input, _) = multispace0(input)?;
    // Recursively parse else branch to allow chained conditionals
    let (input, if_false_expr) = conditional_expr(input)?;

    Ok((
        input,
        Expr::IfExpr {
            condition: Box::new(condition_expr),
            if_true: Box::new(if_true_expr),
            if_false: Box::new(if_false_expr),
        },
    ))
}

//---------------------------------------------------------
// Helper: Parse left-associative chain of logical operators
//---------------------------------------------------------
fn logical_chain<'a, F>(
    input: &'a str,
    inner: F,
    op: BinOp,
    keyword: &'static str,
) -> IResult<&'a str, Expr>
where
    F: Fn(&'a str) -> IResult<&'a str, Expr>,
{
    let (input, mut expr) = inner(input)?;
    let (mut input, _) = multispace0(input)?;

    loop {
        if let Ok((input_after_kw, _)) = tag::<_, _, nom::error::Error<_>>(keyword)(input) {
            // Boundary check: ensure keyword is not part of an identifier
            let is_identifier_part = input_after_kw
                .chars()
                .next()
                .map_or(false, |c| c.is_alphanumeric() || c == '_');

            if is_identifier_part {
                // Keyword is part of an identifier, so we're done
                break;
            }

            // This is a standalone operator
            let (next_input, _) = multispace0(input_after_kw)?;
            let (next_input, right) = inner(next_input)?;
            expr = Expr::BinaryOp {
                op,
                left: Box::new(expr),
                right: Box::new(right),
            };
            let (new_input, _) = multispace0(next_input)?;
            input = new_input;
        } else {
            // No more operators found
            break;
        }
    }

    Ok((input, expr))
}

//---------------------------------------------------------
// logical_or: logical_and ( "or" logical_and )*
// Python's "or" operator has lowest precedence among logical operators
//---------------------------------------------------------
fn logical_or(input: &str) -> IResult<&str, Expr> {
    logical_chain(input, logical_and, BinOp::Or, "or")
}

//---------------------------------------------------------
// logical_and: logical_not ( "and" logical_not )*
// Python's "and" operator has higher precedence than "or"
//---------------------------------------------------------
fn logical_and(input: &str) -> IResult<&str, Expr> {
    logical_chain(input, logical_not, BinOp::And, "and")
}

//---------------------------------------------------------
// logical_not: "not"? comparison
// Python's "not" operator has lower precedence than comparisons
//---------------------------------------------------------
fn logical_not(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;

    // Check for "not" keyword
    if let Ok((input_after_not, _)) = tag::<_, _, nom::error::Error<_>>("not")(input) {
        // Boundary check: ensure "not" is not part of an identifier like "notfoo"
        let is_identifier_part = input_after_not
            .chars()
            .next()
            .map_or(false, |c| c.is_alphanumeric() || c == '_');

        if !is_identifier_part {
            // This is a standalone "not" operator
            // It must be followed by an operand, which `preceded` will handle
            let (input, operand) = preceded(multispace0, logical_not)(input_after_not)?;
            return Ok((
                input,
                Expr::UnaryOp {
                    op: UnOp::Not,
                    operand: Box::new(operand),
                },
            ));
        }
        // Otherwise, it's part of an identifier (e.g., "notable"), so fall through
    }

    // No "not" operator, parse comparison
    comparison(input)
}

//---------------------------------------------------------
// comparison: additive ( (">=" | ">" | "==" | "!=" | "<=" | "<") additive )*
//---------------------------------------------------------
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

//---------------------------------------------------------
// additive: multiplicative ( ("+"|"-") multiplicative )*
//---------------------------------------------------------
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

//---------------------------------------------------------
// multiplicative: exponentiation ( ("*"|"/"|"%") exponentiation )*
//---------------------------------------------------------
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

//---------------------------------------------------------
// exponentiation: unary_expr ("**" exponentiation)? (右結合)
//---------------------------------------------------------
fn exponentiation(input: &str) -> IResult<&str, Expr> {
    let (input, base_expr) = unary_expr(input)?;
    let (input, _) = multispace0(input)?;

    // " ** " が続いていれば右結合で再帰
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

//---------------------------------------------------------
// unary_expr: ("+"|"-")? primary
//   例: -10 や +10, -foo などを "0 - foo" 的なASTに変換
//---------------------------------------------------------
fn unary_expr(input: &str) -> IResult<&str, Expr> {
    // まずオプショナルな符号を取る
    let (input, sign_opt) = opt(alt((char('+'), char('-'))))(input)?;
    let (input, _) = multispace0(input)?;

    // 次に primary をパース
    let (input, mut node) = primary(input)?;

    // 符号が '-' なら「(0 - node)」という BinaryOp に変換
    if let Some(sign) = sign_opt {
        match sign {
            '-' => {
                let zero = Expr::Number(0.0);
                node = Expr::BinaryOp {
                    op: BinOp::Sub,
                    left: Box::new(zero),
                    right: Box::new(node),
                };
            }
            '+' => {
                // '+' は何もしない（node をそのまま）
            }
            _ => unreachable!(),
        }
    }

    Ok((input, node))
}

//---------------------------------------------------------
// primary: number | string_lit | paren/tuple | list_literal
//           | dict_or_set | call_or_var
//---------------------------------------------------------
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

    // 次に配列アクセス [...]* を繰り返し
    let (input, expr) = index_access(input, expr)?;

    // さらにメソッド呼び出し .method(...) を繰り返しパース
    let (input, expr) = method_chain(input, expr)?;

    Ok((input, expr))
}

//---------------------------------------------------------
// index_access: expr[...] の連鎖
//---------------------------------------------------------
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

//---------------------------------------------------------
// method_chain: .method(...) の連鎖
//---------------------------------------------------------
fn method_chain(mut input: &str, mut current: Expr) -> IResult<&str, Expr> {
    loop {
        let (input2, _) = multispace0(input)?;
        // ドットがあるかどうか
        let (input2, dot_opt) = opt(char('.'))(input2)?;
        if dot_opt.is_none() {
            // ドットが無ければループ終了
            break;
        }
        // メソッド名をパース
        let (input2, _) = multispace0(input2)?;
        let (input2, method_name) = identifier(input2)?; // 例: replace, upper, etc.
        let (input2, _) = multispace0(input2)?;

        // "(" args ")" があるかどうか
        let (input2, maybe_call) = opt(delimited(
            char('('),
            separated_list0(
                preceded(multispace0, char(',')),
                preceded(multispace0, expr_wrapper),
            ),
            char(')'),
        ))(input2)?;

        if let Some(args) = maybe_call {
            // メソッド呼び出し
            current = Expr::MethodCall {
                object: Box::new(current),
                method: method_name,
                args,
            };
        } else {
            // obj.property のような形を許すなら別バリアントを返す実装など
            // ここでは Pythonっぽく常にメソッド呼び出し想定なのでエラー
            return Err(nom::Err::Failure(Error::new(input2, ErrorKind::Tag)));
        }

        input = input2;
    }

    Ok((input, current))
}

//---------------------------------------------------------
// list_literal: "[" (list or list comprehension) "]"
//---------------------------------------------------------
fn list_literal(input: &str) -> IResult<&str, Expr> {
    let (input, _) = preceded(multispace0, char('['))(input)?;
    let (input, _) = multispace0(input)?;

    // 空リスト
    if let Ok((input2, _)) =
        preceded(multispace0::<&str, nom::error::Error<&str>>, char(']'))(input)
    {
        return Ok((input2, Expr::List(vec![])));
    }

    // 1つ目の要素式
    let (input_expr, first_expr) = expr_wrapper(input)?;
    let (input_expr, _) = multispace0(input_expr)?;

    // "for" なら内包表記
    if let Ok((input_for, _)) =
        preceded(multispace0::<&str, nom::error::Error<&str>>, tag("for"))(input_expr)
    {
        parse_list_comprehension(input_for, first_expr)
    } else {
        parse_normal_list(input_expr, first_expr)
    }
}

//---------------------------------------------------------
// parse_list_comprehension: [ elem_expr for var in expr (if cond)? ]
//---------------------------------------------------------
fn parse_list_comprehension(input: &str, elem_expr: Expr) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, var_name) = identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("in")(input)?;
    let (input, _) = multispace0(input)?;

    let (input, (iter_expr, _filter_if_taken)) = parse_iter_expr_with_backtracking(input)?;
    let (input, cond_expr) = {
        let mut cexpr = None;
        if let Ok((input_if, _)) =
            preceded(multispace0::<&str, nom::error::Error<&str>>, tag("if"))(input)
        {
            let (input_if, condition) = expr_wrapper(input_if)?;
            cexpr = Some(Box::new(condition));
            (input_if, cexpr)
        } else {
            (input, None)
        }
    };

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

//---------------------------------------------------------
// parse_normal_list: [ expr, expr, ... ]
//---------------------------------------------------------
fn parse_normal_list(mut input: &str, first_expr: Expr) -> IResult<&str, Expr> {
    let mut elems = vec![first_expr];

    loop {
        let (input2, opt_comma) = opt(preceded(
            multispace0::<&str, nom::error::Error<&str>>,
            char(','),
        ))(input)?;
        if opt_comma.is_some() {
            let (input2, _) = multispace0(input2)?;
            if let Ok((input2, next_expr)) = expr_wrapper(input2) {
                elems.push(next_expr);
                input = input2;
                continue;
            } else {
                // 末尾カンマ等
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

//---------------------------------------------------------
// dict_or_set: "{" ... "}"
//   1) 空なら => Dict(vec![])
//   2) 最初が "key : value" ならさらに "for" があれば dict comprehension
//   3) なければ通常のdict or set
//---------------------------------------------------------
fn dict_or_set(input: &str) -> IResult<&str, Expr> {
    let (input, _) = preceded(multispace0, char('{'))(input)?;
    let (input, _) = multispace0(input)?;

    // 空
    if let Ok((rest, _)) = preceded(multispace0::<&str, nom::error::Error<&str>>, char('}'))(input)
    {
        return Ok((rest, Expr::Dict(vec![])));
    }

    // まず key_expr : value_expr を試す
    let (input_key, key_expr) = expr_wrapper(input)?;
    let (input_key, _) = multispace0(input_key)?;
    let (input_key, colon_char) = opt(char(':'))(input_key)?;

    if colon_char.is_some() {
        // value_expr
        let (input_val, _) = multispace0(input_key)?;
        let (input_val, value_expr) = expr_wrapper(input_val)?;
        let (input_val, _) = multispace0(input_val)?;

        // "for" => dict comprehension
        if let Ok((input_for, _)) =
            preceded(multispace0::<&str, nom::error::Error<&str>>, tag("for"))(input_val)
        {
            return parse_dict_comprehension(input_for, key_expr, value_expr);
        } else {
            // 通常の dict
            return parse_normal_dict(input_val, key_expr, value_expr);
        }
    } else {
        // key_expr のみ => set
        return parse_normal_set(input_key, key_expr);
    }
}

//---------------------------------------------------------
// parse_dict_comprehension: { key_expr : value_expr for var in expr if cond? }
//---------------------------------------------------------
fn parse_dict_comprehension(input: &str, key_expr: Expr, value_expr: Expr) -> IResult<&str, Expr> {
    let (input, _) = multispace0(input)?;
    let (input, var_name) = identifier(input)?; // for var
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("in")(input)?;
    let (input, _) = multispace0(input)?;

    let (input, (iter_expr, _)) = parse_iter_expr_with_backtracking(input)?;

    // if cond?
    let (input, cond_expr) = {
        let mut cexpr = None;
        if let Ok((input_if, _)) =
            preceded(multispace0::<&str, nom::error::Error<&str>>, tag("if"))(input)
        {
            let (input_if, condition) = expr_wrapper(input_if)?;
            cexpr = Some(Box::new(condition));
            (input_if, cexpr)
        } else {
            (input, None)
        }
    };

    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;

    Ok((
        input,
        Expr::DictComp {
            key_expr: Box::new(key_expr),
            value_expr: Box::new(value_expr),
            var: var_name,
            iter: Box::new(iter_expr),
            cond: cond_expr,
        },
    ))
}

//---------------------------------------------------------
// parse_normal_dict: "key_expr : value_expr" を最初に1つ受け取った後、
//   カンマ区切りで後続の (k_expr, v_expr) を収集
//---------------------------------------------------------
fn parse_normal_dict(mut input: &str, first_key: Expr, first_val: Expr) -> IResult<&str, Expr> {
    let mut pairs = vec![(expr_to_dict_key(first_key)?, first_val)];

    loop {
        // カンマチェック
        let (input2, opt_comma) = opt(preceded(multispace0, char(',')))(input)?;
        if opt_comma.is_some() {
            let (input2, _) = multispace0(input2)?;
            // 次が '}' なら末尾カンマ
            if let Ok((rest, _)) =
                preceded(multispace0::<&str, nom::error::Error<&str>>, char('}'))(input2)
            {
                return Ok((rest, Expr::Dict(pairs)));
            }
            // それ以外は parse_dict_item_or_expr
            let (input2, (k_expr, v_opt)) = parse_dict_item_or_expr(input2)?;
            let v_expr =
                v_opt.ok_or_else(|| nom::Err::Failure(Error::new(input2, ErrorKind::Tag)))?;
            pairs.push((expr_to_dict_key(k_expr)?, v_expr));
            input = input2;
        } else {
            // カンマが無い => 次は '}' のはず
            input = input2;
            break;
        }
    }

    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, Expr::Dict(pairs)))
}

//---------------------------------------------------------
// parse_normal_set: set は { expr, expr, ... }
//   既に1つ目の expr を受け取っているので、カンマ区切りで要素を収集
//---------------------------------------------------------
fn parse_normal_set(mut input: &str, first_expr: Expr) -> IResult<&str, Expr> {
    let mut set_elems = vec![first_expr];

    loop {
        let (input2, opt_comma) = opt(preceded(multispace0, char(',')))(input)?;
        if opt_comma.is_some() {
            let (input2, _) = multispace0(input2)?;
            if let Ok((input2, (expr, v_opt))) = parse_dict_item_or_expr(input2) {
                // set要素なので v_opt があればエラー
                if v_opt.is_some() {
                    return Err(nom::Err::Failure(Error::new(input2, ErrorKind::Tag)));
                }
                set_elems.push(expr);
                input = input2;
                continue;
            } else {
                // 末尾カンマ等
                input = input2;
                break;
            }
        } else {
            input = input2;
            break;
        }
    }

    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;
    Ok((input, Expr::Set(set_elems)))
}

//---------------------------------------------------------
// parse_dict_item_or_expr:
//   - dict item: expr ":" expr => (expr, Some(expr))
//   - otherwise => (expr, None)
//---------------------------------------------------------
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

//---------------------------------------------------------
// parenthesized_or_tuple: "(" [ expr ("," expr)* [","] ] ")"
//---------------------------------------------------------
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
        0 => Ok((input, Expr::Tuple(vec![]))),               // ()
        1 => Ok((input, exprs.into_iter().next().unwrap())), // (expr)
        _ => Ok((input, Expr::Tuple(exprs))),                // (a, b, c,...)
    }
}

//---------------------------------------------------------
// call_or_var: var_expr / lambda_expr + optional (args)
//---------------------------------------------------------
fn call_or_var(input: &str) -> IResult<&str, Expr> {
    let (input, e) = alt((lambda_expr, var_expr))(input)?;
    let (input, _) = multispace0(input)?;

    // "(" args ")" ?
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

//---------------------------------------------------------
// var_expr: identifier => Expr::Var
//---------------------------------------------------------
fn var_expr(input: &str) -> IResult<&str, Expr> {
    let (input, var) = identifier(input)?;
    Ok((input, Expr::Var(var)))
}

//---------------------------------------------------------
// number: 整数 or 小数 (ただし先頭に符号は含まない)
//   ただし unary_expr で + / - を扱うのでここでは非負を想定
//---------------------------------------------------------
fn number(input: &str) -> IResult<&str, Expr> {
    let (remaining_input, num_str) = recognize_float(input)?;
    let val: f64 = num_str
        .parse()
        .map_err(|_| nom::Err::Failure(Error::new(input, ErrorKind::Float)))?;
    Ok((remaining_input, Expr::Number(val)))
}

//---------------------------------------------------------
// string_lit: シングルクォート囲み
//---------------------------------------------------------
fn string_lit(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('\'')(input)?;
    let (input, s) = take_while(|c: char| c != '\'')(input)?;
    let (input, _) = char('\'')(input)?;
    Ok((input, Expr::StringLit(s.to_string())))
}

//---------------------------------------------------------
// recognize_float: 整数 or 小数 (先頭符号は含まない想定)
// Supports:
// - Leading dot: .5, .02
// - Trailing dot: 4., 123.
// - Scientific notation: 1e-3, 2.5e2, .5e-2
//
// Grammar: mantissa [exponent]?
// - mantissa: digit+ [. digit*]? | . digit+
// - exponent: [eE] [+-]? digit+
//---------------------------------------------------------
fn recognize_float(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        // Mantissa: standard (123.456, 123.) or leading dot (.456)
        alt((
            // Standard: 123.456 or 123.
            recognize(tuple((digit1, opt(tuple((char('.'), digit0)))))),
            // Leading dot: .456
            recognize(tuple((char('.'), digit1))),
        )),
        // Optional exponent: e/E, optional +/-, digits
        opt(tuple((
            one_of("eE"),
            opt(one_of("+-")),
            digit1,
        ))),
    ))(input)
}

//---------------------------------------------------------
// identifier: [a-zA-Z_][a-zA-Z0-9_]*
//---------------------------------------------------------
fn identifier(input: &str) -> IResult<&str, String> {
    let (input, first) =
        nom::character::complete::satisfy(|c: char| c.is_alphabetic() || c == '_')(input)?;
    let (input, rest) =
        take_while1::<_, &str, Error<&str>>(|c: char| c.is_alphanumeric() || c == '_')(input)
            .or(Ok((input, "")))?;
    Ok((input, format!("{}{}", first, rest)))
}

//---------------------------------------------------------
// バックトラック: イテレーション対象をパース (三項演算子 vs if フィルタ衝突対策)
//---------------------------------------------------------
fn parse_iter_expr_with_backtracking(input: &str) -> IResult<&str, (Expr, bool)> {
    // まず expr_wrapper を試す
    if let Ok((rest, expr)) = expr_wrapper(input) {
        return Ok((rest, (expr, false)));
    }

    // 失敗 => フォールバック
    let (rest, fallback_expr) = alt((number, string_lit, var_expr))(input)?;
    Ok((rest, (fallback_expr, false)))
}

//---------------------------------------------------------
// ユーティリティ: Expr -> dictキー文字列の取り出し
//---------------------------------------------------------
fn expr_to_dict_key(e: Expr) -> Result<String, nom::Err<Error<&'static str>>> {
    match e {
        Expr::Var(s) => Ok(s),
        Expr::StringLit(s) => Ok(s),
        _ => Err(nom::Err::Failure(Error::new("", ErrorKind::Tag))),
    }
}
