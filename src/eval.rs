use crate::ast::{BinOp, Expr};
use crate::parser::parse_expr;
use indexmap::IndexMap;
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
    BuiltinValue {
        name: String,
        func: BuiltinValueFn,
    },
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Set(Vec<Value>),
    Dict(IndexMap<String, Value>),
    Var(String),
    StringLit(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Lambda { param, .. } => write!(f, "<lambda {}>", param),
            Value::Builtin { name, .. } => write!(f, "<builtin {}>", name),
            Value::BuiltinValue { name, .. } => write!(f, "<builtin {}>", name),
            Value::List(v) => {
                let strs: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                write!(f, "[{}]", strs.join(", "))
            }
            Value::Tuple(v) => {
                let strs: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                if v.len() == 1 {
                    write!(f, "({},)", strs[0])
                } else {
                    write!(f, "({})", strs.join(", "))
                }
            }
            Value::Set(v) => {
                let strs: Vec<String> = v.iter().map(|x| x.to_string()).collect();
                write!(f, "{{{}}}", strs.join(", "))
            }
            Value::Dict(m) => {
                let mut pairs = vec![];
                for (k, val) in m.iter() {
                    pairs.push(format!("{}: {}", k, val));
                }
                write!(f, "{{{}}}", pairs.join(", "))
            }
            Value::Var(v) => write!(f, "{}", v),
            Value::StringLit(s) => write!(f, "{}", s),
        }
    }
}

type BuiltinFn = fn(&[f64]) -> Result<f64, EvalError>;
type BuiltinValueFn = fn(&[Value]) -> Result<Value, EvalError>;

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

fn builtin_int(args: &[f64]) -> Result<f64, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("int".to_string()));
    }
    Ok(args[0].floor())
}

fn builtin_round(args: &[f64]) -> Result<f64, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("round".to_string()));
    }
    Ok(args[0].round())
}

fn builtin_float(args: &[f64]) -> Result<f64, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("float".to_string()));
    }
    Ok(args[0])
}

fn builtin_len_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("len".to_string()));
    }

    match &args[0] {
        Value::StringLit(s) => Ok(Value::Number(s.len() as f64)),
        Value::List(v) => Ok(Value::Number(v.len() as f64)),
        Value::Tuple(v) => Ok(Value::Number(v.len() as f64)),
        Value::Set(v) => Ok(Value::Number(v.len() as f64)),
        Value::Dict(m) => Ok(Value::Number(m.len() as f64)),
        _ => Err(EvalError::TypeError),
    }
}

fn builtin_sum_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("sum".to_string()));
    }

    match &args[0] {
        Value::List(_v) | Value::Tuple(_v) | Value::Set(_v) => {
            let items = match &args[0] {
                Value::List(list) => list,
                Value::Tuple(tup) => tup,
                Value::Set(st) => st,
                _ => unreachable!(),
            };
            let mut total = 0.0;
            for elem in items {
                match elem {
                    Value::Number(n) => {
                        total += n;
                    }
                    _ => {
                        return Err(EvalError::TypeError);
                    }
                }
            }
            Ok(Value::Number(total))
        }
        _ => Err(EvalError::TypeError),
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
    #[error("Argument error in builtin function: {0}")]
    ArgError(String),
    #[error("Dict key error: keys must be identifiers")]
    DictKeyError,
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
        base_env.set(
            "int",
            Value::Builtin {
                name: "int".to_string(),
                func: builtin_int,
            },
        );
        base_env.set(
            "round",
            Value::Builtin {
                name: "round".to_string(),
                func: builtin_round,
            },
        );
        base_env.set(
            "float",
            Value::Builtin {
                name: "float".to_string(),
                func: builtin_float,
            },
        );
        base_env.set(
            "len",
            Value::BuiltinValue {
                name: "len".to_string(),
                func: builtin_len_value,
            },
        );
        base_env.set(
            "sum",
            Value::BuiltinValue {
                name: "sum".to_string(),
                func: builtin_sum_value,
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

pub fn eval_expr(expr: Expr, env: Rc<Env>) -> Result<(Value, Rc<Env>), EvalError> {
    match expr {
        Expr::Number(n) => Ok((Value::Number(n), env)),
        Expr::Var(name) => {
            let val = env
                .get(&name)
                .ok_or_else(|| EvalError::UndefinedVar(name))?;
            Ok((val, env))
        }
        Expr::StringLit(s) => Ok((Value::StringLit(s), env)),
        Expr::BinaryOp { op, left, right } => {
            let (lval, env) = eval_expr(*left, env)?;
            let (rval, env) = eval_expr(*right, env)?;
            let val = match op {
                BinOp::Add => {
                    match (lval, rval) {
                        // 1) 数値 + 数値
                        (Value::Number(ln), Value::Number(rn)) => Value::Number(ln + rn),
                        // 2) リスト + リスト
                        (Value::List(mut lv), Value::List(rv)) => {
                            lv.extend(rv);
                            Value::List(lv)
                        }
                        // そのほかは TypeError
                        _ => return Err(EvalError::TypeError),
                    }
                }
                BinOp::Sub => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => Value::Number(a - b),
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Mul => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => Value::Number(a * b),
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Div => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        if b == 0.0 {
                            return Err(EvalError::DivisionByZero);
                        }
                        Value::Number(a / b)
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::FloorDiv => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        if b == 0.0 {
                            return Err(EvalError::DivisionByZero);
                        }
                        Value::Number(a.floor() / b.floor())
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Mod => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => Value::Number(a % b),
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Exp => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => Value::Number(a.powf(b)),
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Gt => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        Value::Number(if a > b { 1.0 } else { 0.0 })
                    }
                    (Value::StringLit(a), Value::StringLit(b)) => {
                        Value::Number(if a > b { 1.0 } else { 0.0 })
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Lt => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        Value::Number(if a < b { 1.0 } else { 0.0 })
                    }
                    (Value::StringLit(a), Value::StringLit(b)) => {
                        Value::Number(if a < b { 1.0 } else { 0.0 })
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Ge => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        Value::Number(if a >= b { 1.0 } else { 0.0 })
                    }
                    (Value::StringLit(a), Value::StringLit(b)) => {
                        Value::Number(if a >= b { 1.0 } else { 0.0 })
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Le => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        Value::Number(if a <= b { 1.0 } else { 0.0 })
                    }
                    (Value::StringLit(a), Value::StringLit(b)) => {
                        Value::Number(if a <= b { 1.0 } else { 0.0 })
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Eq => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        Value::Number(if a == b { 1.0 } else { 0.0 })
                    }
                    (Value::StringLit(a), Value::StringLit(b)) => {
                        Value::Number(if a == b { 1.0 } else { 0.0 })
                    }
                    _ => return Err(EvalError::TypeError),
                },
                BinOp::Ne => match (lval, rval) {
                    (Value::Number(a), Value::Number(b)) => {
                        Value::Number(if a != b { 1.0 } else { 0.0 })
                    }
                    (Value::StringLit(a), Value::StringLit(b)) => {
                        Value::Number(if a != b { 1.0 } else { 0.0 })
                    }
                    _ => return Err(EvalError::TypeError),
                },
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
                    // Lambdaは1引数想定
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
                // lenなどValueベースの関数
                Value::BuiltinValue { name: _, func } => {
                    let res_val = func(&arg_vals)?;
                    Ok((res_val, new_env))
                }
                _ => Err(EvalError::LambdaCallError),
            }
        }
        Expr::List(exprs) => {
            let mut vals = Vec::new();
            let mut cur_env = env;
            for e in exprs {
                let (val, e2) = eval_expr(e, cur_env)?;
                cur_env = e2;
                vals.push(val);
            }
            Ok((Value::List(vals), cur_env))
        }
        Expr::Tuple(exprs) => {
            let mut vals = Vec::new();
            let mut cur_env = env;
            for e in exprs {
                let (val, e2) = eval_expr(e, cur_env)?;
                cur_env = e2;
                vals.push(val);
            }
            Ok((Value::Tuple(vals), cur_env))
        }
        Expr::Set(exprs) => {
            let mut vals = Vec::new();
            let mut cur_env = env;
            for e in exprs {
                let (val, e2) = eval_expr(e, cur_env)?;
                cur_env = e2;
                vals.push(val);
            }
            // 本来はsetなら重複排除など必要だが、ここでは簡易実装
            Ok((Value::Set(vals), cur_env))
        }
        Expr::Dict(pairs) => {
            let mut map = IndexMap::new();
            let mut cur_env = env;
            for (k, vexpr) in pairs {
                let (val, env2) = eval_expr(vexpr, cur_env)?;
                cur_env = env2;
                map.insert(k, val);
            }
            Ok((Value::Dict(map), cur_env))
        }
        Expr::ListComp {
            expr,
            var,
            iter,
            cond,
        } => {
            // iterを評価
            let (iter_val, env) = eval_expr(*iter, env)?;

            // iter_valがList, Tuple, Set, Dictのいずれか
            let items: Vec<Value> = match iter_val {
                Value::List(v) => v,
                Value::Tuple(v) => v,
                Value::Set(v) => v,
                Value::Dict(m) => {
                    // Dictの場合はキーをイテレートすることにします(簡略)
                    m.into_iter().map(|(k, _)| Value::Var(k)).collect()
                }
                _ => return Err(EvalError::TypeError),
            };

            let mut result = Vec::new();
            let current_env = env.clone();
            for item in items {
                // varにitemをバインドした環境でexprを評価
                let mut new_env_data = (*current_env).clone();
                new_env_data.set(&var, item);
                let new_env = Rc::new(new_env_data);

                // 条件式チェック
                if let Some(cond_expr) = &cond {
                    let (cond_val, _) = eval_expr((**cond_expr).clone(), Rc::clone(&new_env))?;
                    if !is_truthy(&cond_val) {
                        continue; // 条件を満たさないのでスキップ
                    }
                }

                let (val, _) = eval_expr((*expr).clone(), new_env)?;
                result.push(val);
            }

            Ok((Value::List(result), env))
        }
        Expr::DictComp {
            key_expr,
            value_expr,
            var,
            iter,
            cond,
        } => {
            let (iter_val, env) = eval_expr(*iter, env)?;
            let items: Vec<Value> = match iter_val {
                Value::List(v) => v,
                Value::Tuple(v) => v,
                Value::Set(v) => v,
                Value::Dict(m) => {
                    // IndexMap だがValue::Dict(m)ならキーをVarにして
                    m.into_iter()
                        .map(|(k, _)| Value::Var(k)) // or StringLit(k)でもよい
                        .collect()
                }
                _ => return Err(EvalError::TypeError),
            };

            let mut result_map = IndexMap::new();
            let current_env = env.clone();
            for item in items {
                let mut new_env_data = (*current_env).clone();
                new_env_data.set(&var, item);
                let new_env = Rc::new(new_env_data);

                if let Some(cond_expr) = &cond {
                    let (cond_val, _) = eval_expr((**cond_expr).clone(), new_env.clone())?;
                    if !is_truthy(&cond_val) {
                        continue;
                    }
                }

                let (kval, new_env2) = eval_expr((*key_expr).clone(), new_env.clone())?;
                let (vval, _) = eval_expr((*value_expr).clone(), new_env2)?;

                let kstr = match kval {
                    Value::Var(s) => s,
                    Value::StringLit(s) => s,
                    Value::Number(n) => n.to_string(),
                    _ => return Err(EvalError::TypeError),
                };

                result_map.insert(kstr, vval);
            }
            Ok((Value::Dict(result_map), env))
        }
        Expr::Index { expr, index } => {
            let (container_val, env) = eval_expr(*expr, env)?;
            let (index_val, env) = eval_expr(*index, env)?;
            let val = match container_val {
                Value::List(v) => {
                    let i = extract_index(&index_val)?;
                    v.get(i).cloned().ok_or_else(|| EvalError::TypeError)?
                }
                Value::Tuple(v) => {
                    let i = extract_index(&index_val)?;
                    v.get(i).cloned().ok_or_else(|| EvalError::TypeError)?
                }
                Value::Set(v) => {
                    let i = extract_index(&index_val)?;
                    v.get(i).cloned().ok_or_else(|| EvalError::TypeError)?
                }
                Value::Dict(m) => {
                    let k = extract_key(&index_val)?;
                    m.get(&k)
                        .cloned()
                        .ok_or_else(|| EvalError::UndefinedVar(k))?
                }
                _ => return Err(EvalError::TypeError),
            };
            Ok((val, env))
        }
        Expr::IfExpr {
            condition,
            if_true,
            if_false,
        } => {
            let (cond_val, env) = eval_expr(*condition, env)?;
            if is_truthy(&cond_val) {
                eval_expr(*if_true, env)
            } else {
                eval_expr(*if_false, env)
            }
        }
    }
}

fn is_truthy(val: &Value) -> bool {
    match val {
        Value::Number(n) => *n != 0.0,
        Value::List(v) => !v.is_empty(),
        Value::Tuple(v) => !v.is_empty(),
        Value::Set(v) => !v.is_empty(),
        Value::Dict(m) => !m.is_empty(),
        _ => false, // 簡易的に、それ以外はfalse扱い
    }
}

fn extract_index(val: &Value) -> Result<usize, EvalError> {
    match val {
        Value::Number(n) => {
            if *n < 0.0 {
                return Err(EvalError::TypeError);
            }
            let i = *n as usize;
            if (i as f64 - n).abs() > f64::EPSILON {
                // 小数部分がある場合はエラー
                return Err(EvalError::TypeError);
            }
            Ok(i)
        }
        _ => Err(EvalError::TypeError),
    }
}

fn extract_key(val: &Value) -> Result<String, EvalError> {
    match val {
        Value::Var(s) => Ok(s.clone()),
        Value::StringLit(s) => Ok(s.clone()),
        _ => Err(EvalError::TypeError),
    }
}
