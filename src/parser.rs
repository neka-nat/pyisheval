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
/// expr := assignment | lambda_expr | additive
/// assignment := identifier "=" expr
/// lambda_expr := "lambda" identifier ":" expr
/// additive := multiplicative (("+" | "-") multiplicative)*
/// multiplicative := primary (("*" | "/") primary)*
/// primary := number | identifier | "(" expr ")" | call_expr
/// call_expr := (identifier | lambda_expr) "(" [expr ("," expr)*] ")"
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
    let (input, first) = alt((lambda_expr, additive))(input)?;
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

fn multiplicative(input: &str) -> IResult<&str, Expr> {
    let (input, mut expr) = primary(input)?;
    let (mut input, _) = multispace0(input)?;

    loop {
        let (next_input, opt_op) = opt(alt((char('*'), char('/'))))(input)?;
        if let Some(op_char) = opt_op {
            let op = match op_char {
                '*' => BinOp::Mul,
                '/' => BinOp::Div,
                _ => unreachable!(),
            };
            let (next_input2, _) = multispace0(next_input)?;
            let (next_input2, right) = primary(next_input2)?;
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
    alt((parenthesized, number, call_or_var))(input)
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

fn parenthesized(input: &str) -> IResult<&str, Expr> {
    let (input, _) = char('(')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, e) = expr_wrapper(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char(')')(input)?;
    Ok((input, e))
}

fn number(input: &str) -> IResult<&str, Expr> {
    let (input, num_str) = recognize_float(input)?;
    let val: f64 = num_str.parse().unwrap();
    Ok((input, Expr::Number(val)))
}

fn recognize_float(input: &str) -> IResult<&str, String> {
    let (input, integer_part) = take_while1(|c: char| c.is_ascii_digit())(input)?;
    let (input, fractional_part) = opt(preceded(
        char('.'),
        take_while1(|c: char| c.is_ascii_digit()),
    ))(input)?;

    if let Some(frac) = fractional_part {
        // construct full number string
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
