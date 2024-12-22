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
    BoundMethod {
        receiver: Box<Value>,
        method: String,
    },
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
            Value::BoundMethod {
                receiver: _,
                method,
            } => {
                write!(f, "<bound method {}>", method)
            }
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
        Expr::MethodCall {
            object,
            method,
            args,
        } => {
            // 1) オブジェクトを評価
            let (obj_val, env) = eval_expr(*object, env)?;

            // 2) 引数を評価
            let mut arg_vals = Vec::new();
            let mut cur_env = env.clone();
            for a in args {
                let (av, e2) = eval_expr(a, cur_env)?;
                cur_env = e2;
                arg_vals.push(av);
            }

            // 3) オブジェクトが文字列かどうか
            let result = match obj_val {
                Value::StringLit(s) => {
                    // メソッド名に応じて処理を分岐
                    do_string_method(&s, &method, &arg_vals)?
                }
                // もし他の型に対してもメソッドを定義したければここで分岐
                _ => {
                    return Err(EvalError::TypeError);
                }
            };

            Ok((result, cur_env))
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

fn do_string_method(s: &str, method: &str, args: &[Value]) -> Result<Value, EvalError> {
    match method {
        // 1) capitalize
        "capitalize" => {
            // 最初の文字だけ大文字、それ以外小文字
            let mut c = s.chars();
            let capitalized = match c.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().collect::<String>() + c.as_str(),
            }
            .to_lowercase();
            Ok(Value::StringLit(capitalized))
        }

        // 2) casefold (ここでは単純に lower() と同じにする)
        "casefold" => Ok(Value::StringLit(s.to_lowercase())),

        // 3) center
        //    center(width, fillchar=' ')
        "center" => {
            let width = match args.get(0) {
                Some(Value::Number(n)) => *n as usize,
                _ => return Err(EvalError::ArgError("center".to_string())),
            };
            let fillchar = match args.get(1) {
                Some(Value::StringLit(ch)) if ch.len() == 1 => ch.chars().next().unwrap(),
                None => ' ',
                _ => return Err(EvalError::ArgError("center".to_string())),
            };
            if s.len() >= width {
                Ok(Value::StringLit(s.to_string()))
            } else {
                let diff = width - s.len();
                let left = diff / 2;
                let right = diff - left;
                let res = format!(
                    "{}{}{}",
                    fillchar.to_string().repeat(left),
                    s,
                    fillchar.to_string().repeat(right)
                );
                Ok(Value::StringLit(res))
            }
        }

        // 4) count(substring)
        "count" => {
            let sub = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("count".to_string())),
            };
            let cnt = s.matches(sub).count() as f64;
            Ok(Value::Number(cnt))
        }

        // 5) encode
        //   Python だと bytes になるが、ここでは単純に同じ文字列を返すか、
        //   あるいはバイナリ列を表す別の Value を用意してもよい。
        "encode" => {
            // 簡易実装としてはエンコードせず、そのまま返す
            Ok(Value::StringLit(s.to_string()))
        }

        // 6) endswith(suffix)
        "endswith" => {
            let suffix = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("endswith".to_string())),
            };
            let yes = s.ends_with(suffix);
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 7) expandtabs
        //    Python ではタブ文字を指定幅でスペース展開するが、ここではデフォルト8とする。
        "expandtabs" => {
            let tabsize = match args.get(0) {
                Some(Value::Number(n)) => *n as usize,
                None => 8,
                _ => return Err(EvalError::ArgError("expandtabs".to_string())),
            };
            let expanded = s.replace('\t', &" ".repeat(tabsize));
            Ok(Value::StringLit(expanded))
        }

        // 8) find(substring)
        //    見つかったらその開始インデックス、なければ -1 を返す (Python 互換風)
        "find" => {
            let sub = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("find".to_string())),
            };
            if let Some(pos) = s.find(sub) {
                Ok(Value::Number(pos as f64))
            } else {
                Ok(Value::Number(-1.0))
            }
        }

        // 9) format
        //    本来は `"{0} ... {1} ...".format(x,y)` のような高度な機能があるが、
        //    ここでは簡易実装で、"{}" を引数で置き換える程度にする
        "format" => {
            let mut result = s.to_string();
            // "{}" を順番に args の文字列 (to_string()) に置き換え
            for val in args {
                let placeholder = "{}";
                if let Some(idx) = result.find(placeholder) {
                    result.replace_range(idx..idx + 2, &val.to_string());
                }
            }
            Ok(Value::StringLit(result))
        }

        // 10) format_map
        //     今回は簡易的に同じ扱いにするか、未実装エラーでもOK
        "format_map" => {
            // 今回は単純に format と同じ扱いにする
            do_string_method(s, "format", args)
        }

        // 11) index(substring)
        //     find と同じだが、見つからなければエラーにするのが Python 流
        "index" => {
            let sub = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("index".to_string())),
            };
            if let Some(pos) = s.find(sub) {
                Ok(Value::Number(pos as f64))
            } else {
                // Python だと ValueError だが、ここでは EvalError::UndefinedVar などを使うか
                return Err(EvalError::ArgError("substring not found".to_string()));
            }
        }

        // 12) isalnum
        "isalnum" => {
            let yes = s.chars().all(|c| c.is_alphanumeric());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 13) isalpha
        "isalpha" => {
            let yes = s.chars().all(|c| c.is_alphabetic());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 14) isascii
        "isascii" => {
            let yes = s.is_ascii();
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 15) isdecimal
        "isdecimal" => {
            // Rust の is_ascii_digit() などで近似
            let yes = s.chars().all(|c| c.is_ascii_digit());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 16) isdigit
        "isdigit" => {
            // 上と同じ程度の簡易判定
            let yes = s.chars().all(|c| c.is_ascii_digit());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 17) isidentifier
        //     Python の厳密なルールは複雑だが、ここでは [a-zA-Z_][a-zA-Z0-9_]* で簡易判定
        "isidentifier" => {
            // 空なら false
            if s.is_empty() {
                return Ok(Value::Number(0.0));
            }
            let mut chars = s.chars();
            let first = chars.next().unwrap();
            if !(first.is_alphabetic() || first == '_') {
                return Ok(Value::Number(0.0));
            }
            let yes = chars.all(|c| c.is_alphanumeric() || c == '_');
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 18) islower
        "islower" => {
            let yes = !s.is_empty() && s == s.to_lowercase() && s != s.to_uppercase();
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 19) isnumeric
        //     ここでは isdigit と同じくらいにする
        "isnumeric" => {
            let yes = s.chars().all(|c| c.is_ascii_digit());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 20) isprintable
        "isprintable" => {
            let yes = s.chars().all(|c| !c.is_control());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 21) isspace
        "isspace" => {
            let yes = !s.is_empty() && s.chars().all(|c| c.is_whitespace());
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 22) istitle
        //     Rust には直接の is_titlecase() などがなく、簡易チェックにする
        "istitle" => {
            // "Hello World" など一応1単語ごとに先頭大文字か判定する簡易版
            let yes = s
                .split_whitespace()
                .all(|w| w.chars().next().map_or(false, |c| c.is_uppercase()));
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 23) isupper
        "isupper" => {
            let yes = !s.is_empty() && s == s.to_uppercase() && s != s.to_lowercase();
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 24) join
        //     `'sep'.join(list)`
        "join" => {
            let list_val = match args.get(0) {
                Some(v) => v,
                None => return Err(EvalError::ArgError("join".to_string())),
            };
            let joined = match list_val {
                Value::List(vec) => {
                    let mut strs = Vec::new();
                    for item in vec {
                        strs.push(item.to_string());
                    }
                    strs.join(s)
                }
                _ => return Err(EvalError::TypeError),
            };
            Ok(Value::StringLit(joined))
        }

        // 25) ljust(width, fillchar=' ')
        "ljust" => {
            let width = match args.get(0) {
                Some(Value::Number(n)) => *n as usize,
                _ => return Err(EvalError::ArgError("ljust".to_string())),
            };
            let fillchar = match args.get(1) {
                Some(Value::StringLit(ch)) if ch.len() == 1 => ch.chars().next().unwrap(),
                None => ' ',
                _ => return Err(EvalError::ArgError("ljust".to_string())),
            };
            if s.len() >= width {
                Ok(Value::StringLit(s.to_string()))
            } else {
                let diff = width - s.len();
                let res = format!("{}{}", s, fillchar.to_string().repeat(diff));
                Ok(Value::StringLit(res))
            }
        }

        // 26) lower
        "lower" => Ok(Value::StringLit(s.to_lowercase())),

        // 27) lstrip(chars=' ')
        "lstrip" => {
            // Python では chars が与えられたらその文字集合を strip するが、
            // ここではデフォルト空白のみ・あるいは与えられた文字列全部を除去くらいにする
            let strip_chars = match args.get(0) {
                Some(Value::StringLit(sub)) => sub.as_str(),
                None => "\t\r\n ",
                _ => return Err(EvalError::TypeError),
            };
            let trimmed = s.trim_start_matches(|c| strip_chars.contains(c));
            Ok(Value::StringLit(trimmed.to_string()))
        }

        // 28) maketrans
        //     Python では文字 → 文字、または削除文字など複雑。
        //     ここでは簡易的に dict を返すだけにする or 未実装エラー
        "maketrans" => {
            // 本家 Python だと {ord(c1): c2, ...} のようなマッピングを作る
            // ここでは単に TypeError にするか、空dictを返す例など
            Ok(Value::Dict(IndexMap::new()))
        }

        // 29) partition(sep)
        //     (head, sep, tail) の3要素タプル
        "partition" => {
            let sep = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("partition".to_string())),
            };
            if let Some(idx) = s.find(sep) {
                let head = &s[..idx];
                let tail = &s[idx + sep.len()..];
                Ok(Value::Tuple(vec![
                    Value::StringLit(head.to_string()),
                    Value::StringLit(sep.to_string()),
                    Value::StringLit(tail.to_string()),
                ]))
            } else {
                Ok(Value::Tuple(vec![
                    Value::StringLit(s.to_string()),
                    Value::StringLit("".to_string()),
                    Value::StringLit("".to_string()),
                ]))
            }
        }

        // 30) removeprefix(prefix)
        "removeprefix" => {
            let prefix = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("removeprefix".to_string())),
            };
            let res = if s.starts_with(prefix) {
                &s[prefix.len()..]
            } else {
                s
            };
            Ok(Value::StringLit(res.to_string()))
        }

        // 31) removesuffix(suffix)
        "removesuffix" => {
            let suffix = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("removesuffix".to_string())),
            };
            let res = if s.ends_with(suffix) {
                &s[..(s.len() - suffix.len())]
            } else {
                s
            };
            Ok(Value::StringLit(res.to_string()))
        }

        // 32) replace(old, new, count=∞)
        "replace" => {
            let old = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("replace".to_string())),
            };
            let new = match args.get(1) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("replace".to_string())),
            };
            let count = match args.get(2) {
                Some(Value::Number(n)) => *n as usize,
                None => usize::MAX,
                _ => return Err(EvalError::ArgError("replace".to_string())),
            };
            if count == 0 {
                Ok(Value::StringLit(s.to_string()))
            } else {
                // 簡易実装: replace_n 回だけ置換
                let mut replaced = String::new();
                let mut remain = s;
                let mut done = 0;
                while let Some(pos) = remain.find(old) {
                    replaced.push_str(&remain[..pos]);
                    replaced.push_str(new);
                    remain = &remain[pos + old.len()..];
                    done += 1;
                    if done >= count {
                        break;
                    }
                }
                replaced.push_str(remain);
                Ok(Value::StringLit(replaced))
            }
        }

        // 33) rfind(sub)
        //     右から検索、見つからなければ -1
        "rfind" => {
            let sub = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("rfind".to_string())),
            };
            if let Some(pos) = s.rfind(sub) {
                Ok(Value::Number(pos as f64))
            } else {
                Ok(Value::Number(-1.0))
            }
        }

        // 34) rindex(sub)
        //     右から検索、見つからなければエラー
        "rindex" => {
            let sub = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("rindex".to_string())),
            };
            if let Some(pos) = s.rfind(sub) {
                Ok(Value::Number(pos as f64))
            } else {
                return Err(EvalError::ArgError("substring not found".to_string()));
            }
        }

        // 35) rjust(width, fillchar=' ')
        "rjust" => {
            let width = match args.get(0) {
                Some(Value::Number(n)) => *n as usize,
                _ => return Err(EvalError::ArgError("rjust".to_string())),
            };
            let fillchar = match args.get(1) {
                Some(Value::StringLit(ch)) if ch.len() == 1 => ch.chars().next().unwrap(),
                None => ' ',
                _ => return Err(EvalError::ArgError("rjust".to_string())),
            };
            if s.len() >= width {
                Ok(Value::StringLit(s.to_string()))
            } else {
                let diff = width - s.len();
                let res = format!("{}{}", fillchar.to_string().repeat(diff), s);
                Ok(Value::StringLit(res))
            }
        }

        // 36) rpartition(sep)
        //     (head, sep, tail) の3要素タプル（右から分割）
        "rpartition" => {
            let sep = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("rpartition".to_string())),
            };
            if let Some(idx) = s.rfind(sep) {
                let head = &s[..idx];
                let tail = &s[idx + sep.len()..];
                Ok(Value::Tuple(vec![
                    Value::StringLit(head.to_string()),
                    Value::StringLit(sep.to_string()),
                    Value::StringLit(tail.to_string()),
                ]))
            } else {
                Ok(Value::Tuple(vec![
                    Value::StringLit("".to_string()),
                    Value::StringLit("".to_string()),
                    Value::StringLit(s.to_string()),
                ]))
            }
        }

        // 37) rsplit(sep=None, maxsplit=-1)
        //     簡易実装: sep が None なら空白区切り、maxsplit は未考慮
        "rsplit" => {
            let sep = match args.get(0) {
                Some(Value::StringLit(sub)) => {
                    if sub.is_empty() {
                        None
                    } else {
                        Some(sub.as_str())
                    }
                }
                None => None, // デフォルトは空白区切り
                _ => return Err(EvalError::ArgError("rsplit".to_string())),
            };
            // Python の rsplit と同じように右から分割は面倒なので、ここでは普通の split に
            let splitted: Vec<&str> = match sep {
                Some(sepstr) => s.split(sepstr).collect(),
                None => s.split_whitespace().collect(),
            };
            Ok(Value::List(
                splitted
                    .into_iter()
                    .map(|item| Value::StringLit(item.to_string()))
                    .collect(),
            ))
        }

        // 38) rstrip(chars=' ')
        "rstrip" => {
            let strip_chars = match args.get(0) {
                Some(Value::StringLit(sub)) => sub.as_str(),
                None => "\t\r\n ",
                _ => return Err(EvalError::TypeError),
            };
            let trimmed = s.trim_end_matches(|c| strip_chars.contains(c));
            Ok(Value::StringLit(trimmed.to_string()))
        }

        // 39) split(sep=None, maxsplit=-1)
        //     簡易実装: sep が None なら空白区切り、maxsplit は考慮しない
        "split" => {
            let sep = match args.get(0) {
                Some(Value::StringLit(sub)) => {
                    if sub.is_empty() {
                        None
                    } else {
                        Some(sub.as_str())
                    }
                }
                None => None, // デフォルトは空白区切り
                _ => return Err(EvalError::ArgError("split".to_string())),
            };
            let splitted: Vec<&str> = match sep {
                Some(sepstr) => s.split(sepstr).collect(),
                None => s.split_whitespace().collect(),
            };
            Ok(Value::List(
                splitted
                    .into_iter()
                    .map(|item| Value::StringLit(item.to_string()))
                    .collect(),
            ))
        }

        // 40) splitlines()
        "splitlines" => {
            // 改行で区切る
            let lines: Vec<&str> = s.split('\n').collect();
            Ok(Value::List(
                lines
                    .into_iter()
                    .map(|line| Value::StringLit(line.to_string()))
                    .collect(),
            ))
        }

        // 41) startswith(prefix)
        "startswith" => {
            let prefix = match args.get(0) {
                Some(Value::StringLit(sub)) => sub,
                _ => return Err(EvalError::ArgError("startswith".to_string())),
            };
            let yes = s.starts_with(prefix);
            Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
        }

        // 42) strip(chars=' ')
        "strip" => {
            let strip_chars = match args.get(0) {
                Some(Value::StringLit(sub)) => sub.as_str(),
                None => "\t\r\n ",
                _ => return Err(EvalError::TypeError),
            };
            let trimmed = s.trim_matches(|c| strip_chars.contains(c));
            Ok(Value::StringLit(trimmed.to_string()))
        }

        // 43) swapcase
        //     大文字↔小文字 反転
        "swapcase" => {
            let swapped: String = s
                .chars()
                .map(|c| {
                    if c.is_ascii_lowercase() {
                        c.to_ascii_uppercase()
                    } else if c.is_ascii_uppercase() {
                        c.to_ascii_lowercase()
                    } else {
                        c
                    }
                })
                .collect();
            Ok(Value::StringLit(swapped))
        }

        // 44) title
        //     本格的には単語境界で先頭大文字、他小文字など。ここでは簡易版で。
        "title" => {
            let titled = s
                .split_whitespace()
                .map(|w| {
                    let mut c = w.chars();
                    match c.next() {
                        None => String::new(),
                        Some(first) => first.to_uppercase().collect::<String>() + c.as_str(),
                    }
                })
                .collect::<Vec<_>>()
                .join(" ");
            Ok(Value::StringLit(titled))
        }

        // 45) translate(table)
        //     Python では maketrans で作った dict を使うが、ここでは未実装/省略でもOK
        "translate" => {
            // 省略実装: そのまま返す
            Ok(Value::StringLit(s.to_string()))
        }

        // 46) upper
        "upper" => Ok(Value::StringLit(s.to_uppercase())),

        // 47) zfill(width)
        "zfill" => {
            let width = match args.get(0) {
                Some(Value::Number(n)) => *n as usize,
                _ => return Err(EvalError::ArgError("zfill".to_string())),
            };
            if s.len() >= width {
                Ok(Value::StringLit(s.to_string()))
            } else {
                let diff = width - s.len();
                let res = format!("{}{}", "0".repeat(diff), s);
                Ok(Value::StringLit(res))
            }
        }

        // それ以外のメソッド名
        other => Err(EvalError::UndefinedVar(format!(
            "Unknown string method: {}",
            other
        ))),
    }
}
