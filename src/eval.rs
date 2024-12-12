use crate::ast::{BinOp, Expr};
use crate::parser::parse_expr;
use std::collections::HashMap;
use std::rc::Rc;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Env {
    vars: HashMap<String, Value>,
    parent: Option<Rc<Env>>,
}

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
    Builtin {
        name: String,
        func: BuiltinFn,
    },
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Lambda { param, .. } => write!(f, "<lambda {}>", param),
            Value::Builtin { name, .. } => write!(f, "<builtin {}>", name),
        }
    }
}

type BuiltinFn = fn(&[f64]) -> Result<f64, EvalError>;

fn builtin_abs(args: &[f64]) -> Result<f64, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("abs".to_string()));
    }
    Ok(args[0].abs())
}

fn builtin_max(args: &[f64]) -> Result<f64, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArgError("max".to_string()));
    }
    Ok(args.iter().cloned().fold(f64::NEG_INFINITY, f64::max))
}

fn builtin_min(args: &[f64]) -> Result<f64, EvalError> {
    if args.is_empty() {
        return Err(EvalError::ArgError("min".to_string()));
    }
    Ok(args.iter().cloned().fold(f64::INFINITY, f64::min))
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
    #[error("Argument error in builtin function: {0}")]
    ArgError(String),
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
        let mut base_env = Env::new();
        // Add builtins
        base_env.set(
            "abs",
            Value::Builtin {
                name: "abs".to_string(),
                func: builtin_abs,
            },
        );
        base_env.set(
            "max",
            Value::Builtin {
                name: "max".to_string(),
                func: builtin_max,
            },
        );
        base_env.set(
            "min",
            Value::Builtin {
                name: "min".to_string(),
                func: builtin_min,
            },
        );

        Interpreter {
            env: Rc::new(base_env),
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
                BinOp::Mod => Value::Number(ln % rn),
            };
            Ok((val, env))
        }
        Expr::Assign { name, expr } => {
            let (val, mut_env) = eval_expr(*expr, env)?;
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
        Expr::Call { func, args } => {
            let (fval, env) = eval_expr(*func, env)?;
            let mut arg_vals = Vec::new();
            let mut new_env = env.clone();
            for a in args {
                let (av, e) = eval_expr(a, new_env)?;
                new_env = e;
                arg_vals.push(av);
            }

            match fval {
                Value::Lambda {
                    param,
                    body,
                    env: closure_env,
                } => {
                    // Lambdaは1引数想定（元コードが1引数のみ扱い）
                    if arg_vals.len() != 1 {
                        return Err(EvalError::LambdaCallError);
                    }
                    let arg_val = arg_vals.into_iter().next().unwrap();
                    let mut call_env = Env::with_parent(closure_env.clone());
                    call_env.set(&param, arg_val);
                    let call_env = Rc::new(call_env);
                    let (res, _) = eval_expr(body, call_env)?;
                    Ok((res, new_env))
                }
                Value::Builtin { name: _, func } => {
                    // 全ての引数はNumberである必要がある
                    let mut nums = Vec::new();
                    for v in arg_vals {
                        match v {
                            Value::Number(n) => nums.push(n),
                            _ => return Err(EvalError::TypeError),
                        }
                    }
                    let res_num = func(&nums)?;
                    Ok((Value::Number(res_num), new_env))
                }
                _ => Err(EvalError::LambdaCallError),
            }
        }
    }
}
