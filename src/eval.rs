use crate::ast::{BinOp, Expr};
use crate::parser::parse_expr;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        self.vars == other.vars && self.parent == other.parent
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Lambda {
        param: String,
        body: Expr,
        env: Rc<Env>,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Lambda { param, .. } => write!(f, "<lambda {}>", param),
        }
    }
}

#[derive(Debug, Error)]
pub enum EvalError {
    #[error("Undefined variable: {0}")]
    UndefinedVar(String),
    #[error("Type error: expected number")]
    TypeError,
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Lambda call error")]
    LambdaCallError,
    #[error("Parse error: {0}")]
    ParseError(String),
}

#[derive(Clone, Debug)]
pub struct Env {
    vars: HashMap<String, Value>,
    parent: Option<Rc<Env>>,
}

impl Env {
    fn new() -> Self {
        Env {
            vars: HashMap::new(),
            parent: None,
        }
    }

    fn with_parent(parent: Rc<Env>) -> Self {
        Env {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.vars.get(name) {
            Some(v.clone())
        } else if let Some(p) = &self.parent {
            p.get(name)
        } else {
            None
        }
    }

    fn set(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }
}

pub struct Interpreter {
    env: Rc<Env>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: Rc::new(Env::new()),
        }
    }

    pub fn eval(&mut self, code: &str) -> Result<Value, EvalError> {
        let expr = parse_expr(code).map_err(EvalError::ParseError)?;
        let (val, new_env) = eval_expr(expr, Rc::clone(&self.env))?;
        self.env = new_env;
        Ok(val)
    }
}

fn eval_expr(expr: Expr, env: Rc<Env>) -> Result<(Value, Rc<Env>), EvalError> {
    match expr {
        Expr::Number(n) => Ok((Value::Number(n), env)),
        Expr::Var(name) => {
            let val = env
                .get(&name)
                .ok_or_else(|| EvalError::UndefinedVar(name))?;
            Ok((val, env))
        }
        Expr::BinaryOp { op, left, right } => {
            let (lval, env) = eval_expr(*left, env)?;
            let (rval, env) = eval_expr(*right, env)?;
            let ln = match lval {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError),
            };
            let rn = match rval {
                Value::Number(n) => n,
                _ => return Err(EvalError::TypeError),
            };
            let val = match op {
                BinOp::Add => Value::Number(ln + rn),
                BinOp::Sub => Value::Number(ln - rn),
                BinOp::Mul => Value::Number(ln * rn),
                BinOp::Div => {
                    if rn == 0.0 {
                        return Err(EvalError::DivisionByZero);
                    }
                    Value::Number(ln / rn)
                }
            };
            Ok((val, env))
        }
        Expr::Assign { name, expr } => {
            let (val, mut_env) = eval_expr(*expr, env)?;
            // create a new env if needed (but we can mutate current top-level env)
            let mut new_env = (*mut_env).clone();
            new_env.set(&name, val.clone());
            let new_env = Rc::new(new_env);
            Ok((val, new_env))
        }
        Expr::Lambda { param, body } => {
            let val = Value::Lambda {
                param,
                body: *body,
                env: env.clone(),
            };
            Ok((val, env))
        }
        Expr::Call { func, arg } => {
            let (fval, env) = eval_expr(*func, env)?;
            let (arg_val, env) = eval_expr(*arg, env)?;
            match fval {
                Value::Lambda {
                    param,
                    body,
                    env: closure_env,
                } => {
                    let mut call_env = Env::with_parent(closure_env.clone());
                    call_env.set(&param, arg_val);
                    let call_env = Rc::new(call_env);
                    let (res, _) = eval_expr(body, call_env)?;
                    Ok((res, env))
                }
                _ => Err(EvalError::LambdaCallError),
            }
        }
    }
}
