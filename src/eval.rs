use crate::ast::{BinOp, Expr, UnOp};
use crate::parser::parse_expr;
use indexmap::IndexMap;
use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct Env {
    vars: HashMap<String, Value>,
    parent: Option<Rc<RefCell<Env>>>,
}

impl PartialEq for Env {
    fn eq(&self, other: &Self) -> bool {
        self.vars == other.vars && self.parent == other.parent
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    Lambda {
        param: String,
        body: Expr,
        env: Rc<RefCell<Env>>,
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

/// Compare two sets as multisets (order-independent, counts matter).
///
/// Sets are backed by Vec which can contain duplicates due to imperfect
/// deduplication (e.g., set([1, 1.0]) may have duplicates). This handles
/// both true sets and multisets correctly.
///
/// Time complexity: O(n²), but avoids expensive clones and memory shifts.
fn multiset_equal(a: &[Value], b: &[Value]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    // Use boolean flags to track matched elements (avoids cloning Values)
    let mut b_matched = vec![false; b.len()];
    for item_a in a {
        if let Some(pos) = b.iter().enumerate().position(|(i, item_b)| !b_matched[i] && item_a == item_b) {
            b_matched[pos] = true;
        } else {
            return false;
        }
    }
    true
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Cross-type equality: Var and StringLit are semantically equivalent
            (Value::Var(a), Value::StringLit(b)) | (Value::StringLit(a), Value::Var(b)) => a == b,

            // Lambda comparisons always return false (no identity tracking)
            // Direct lambda == lambda throws TypeError in eval_expr, so this only
            // affects lambdas inside collections (e.g., [lambda x: 1] == [lambda x: 2])
            (Value::Lambda { .. }, _) | (_, Value::Lambda { .. }) => false,

            // Same-type comparisons
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Var(a), Value::Var(b)) => a == b,
            (Value::StringLit(a), Value::StringLit(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Set(a), Value::Set(b)) => multiset_equal(a, b),
            (Value::Dict(a), Value::Dict(b)) => a == b,
            (Value::Builtin { name: a, .. }, Value::Builtin { name: b, .. }) => a == b,
            (Value::BuiltinValue { name: a, .. }, Value::BuiltinValue { name: b, .. }) => a == b,
            (Value::BoundMethod { receiver: a_rec, method: a_meth },
             Value::BoundMethod { receiver: b_rec, method: b_meth }) => {
                a_meth == b_meth && a_rec == b_rec
            }

            // Different types are never equal
            (_, _) => false,
        }
    }
}

/// Write an escaped string directly to a formatter for Python repr-style output
/// Handles all control characters using char::is_control() for robust round-trip serialization
fn write_escaped_string(f: &mut std::fmt::Formatter<'_>, s: &str) -> std::fmt::Result {
    for c in s.chars() {
        match c {
            '\\' => write!(f, "\\\\")?,
            '\'' => write!(f, "\\'")?,
            c if c.is_control() => {
                for escaped_char in c.escape_default() {
                    write!(f, "{}", escaped_char)?;
                }
            }
            _ => write!(f, "{}", c)?,
        }
    }
    Ok(())
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
                write!(f, "{{")?;
                let mut first = true;
                for (k, val) in m.iter() {
                    // IMPORTANT: Always quote dict keys (even numeric-looking ones like "1", "2.5")
                    // This ensures round-trip serialization works: parse → to_string() → parse
                    // pyisheval only supports string keys, so unquoted numeric keys would fail to reparse
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "'")?;
                    write_escaped_string(f, k)?;
                    write!(f, "': {}", val)?;
                }
                write!(f, "}}")
            }
            Value::Var(v) => write!(f, "{}", v),
            // StringLit outputs quoted, escaped strings for valid Python syntax
            Value::StringLit(s) => {
                write!(f, "'")?;
                write_escaped_string(f, s)?;
                write!(f, "'")
            }
            Value::BoundMethod {
                receiver: _,
                method,
            } => {
                write!(f, "<bound method {}>", method)
            }
        }
    }
}

impl Value {
    pub fn to_bool(&self) -> bool {
        match self {
            Value::Number(n) => *n != 0.0,
            Value::StringLit(s) => !s.is_empty(),
            Value::List(v) => !v.is_empty(),
            Value::Tuple(v) => !v.is_empty(),
            Value::Set(v) => !v.is_empty(),
            Value::Dict(m) => !m.is_empty(),
            _ => true, // Lambdas, builtins, etc. are truthy
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

/// list(obj) -> 新しいリストを作る
/// - 0引数なら空リスト [] を返す
/// - 1引数が文字列なら、各文字を取り出したリストを返す ["h", "e", "l", ...] (簡易)
/// - 1引数がListなら複製
/// - 1引数がDictならキーをリストに
/// - etc...
fn builtin_list_value(args: &[Value]) -> Result<Value, EvalError> {
    match args.len() {
        0 => {
            // list() -> []
            Ok(Value::List(vec![]))
        }
        1 => {
            match &args[0] {
                Value::StringLit(s) => {
                    // 1文字ずつに分解
                    let chars = s.chars().map(|c| Value::StringLit(c.to_string())).collect();
                    Ok(Value::List(chars))
                }
                Value::List(lst) => {
                    // 複製して返す
                    Ok(Value::List(lst.clone()))
                }
                Value::Tuple(tup) => Ok(Value::List(tup.clone())),
                Value::Set(setv) => Ok(Value::List(setv.clone())),
                Value::Dict(d) => {
                    // キー一覧をリストに
                    let keys = d.keys().map(|k| Value::StringLit(k.clone())).collect();
                    Ok(Value::List(keys))
                }
                // 他の型は適当にエラー or 空リストにするなど
                _ => Err(EvalError::TypeError),
            }
        }
        _ => Err(EvalError::ArgError("list".to_string())),
    }
}

/// dict(obj) -> 新しい辞書を作る
/// - 0引数なら空のdict {}
/// - 1引数が既にDictならコピー
/// - 1引数が[(k,v), (k,v)] のようなリスト/タプルならキーと値を取り出してdict化
fn builtin_dict_value(args: &[Value]) -> Result<Value, EvalError> {
    match args.len() {
        0 => {
            // dict() -> {}
            Ok(Value::Dict(IndexMap::new()))
        }
        1 => {
            match &args[0] {
                Value::Dict(d) => {
                    // 複製
                    Ok(Value::Dict(d.clone()))
                }
                Value::List(lst) | Value::Tuple(lst) => {
                    // 各要素がタプル (k, v) であることを期待
                    let mut new_map = IndexMap::new();
                    for item in lst {
                        match item {
                            Value::Tuple(tv) if tv.len() == 2 => {
                                let k = tv[0].to_string(); // 簡易: to_string()
                                let v = tv[1].clone();
                                new_map.insert(k, v);
                            }
                            _ => return Err(EvalError::TypeError),
                        }
                    }
                    Ok(Value::Dict(new_map))
                }
                // 他の型をどう解釈するかはアレンジ可能
                _ => Err(EvalError::TypeError),
            }
        }
        _ => Err(EvalError::ArgError("dict".to_string())),
    }
}

/// set(obj) -> 新しいセット(Value::Set)を作る
/// - 0引数なら空セット
/// - 1引数がリスト/タプル/文字列なら要素を列挙してセット化
fn builtin_set_value(args: &[Value]) -> Result<Value, EvalError> {
    match args.len() {
        0 => Ok(Value::Set(vec![])),
        1 => {
            match &args[0] {
                Value::List(lst) => {
                    // 重複排除するか、ここでは単にVecに詰める程度
                    let mut v = lst.clone();
                    // 簡易的にユニーク化
                    v.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                    v.dedup_by(|a, b| a.to_string() == b.to_string());
                    Ok(Value::Set(v))
                }
                Value::Tuple(tup) => {
                    let mut v = tup.clone();
                    v.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                    v.dedup_by(|a, b| a.to_string() == b.to_string());
                    Ok(Value::Set(v))
                }
                Value::StringLit(s) => {
                    // 文字ごとに
                    let mut chars: Vec<Value> =
                        s.chars().map(|c| Value::StringLit(c.to_string())).collect();
                    chars.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                    chars.dedup_by(|a, b| a.to_string() == b.to_string());
                    Ok(Value::Set(chars))
                }
                // Dict ならキーをセット化 etc... 必要に応じて
                Value::Dict(d) => {
                    let mut v: Vec<Value> = d.keys().map(|k| Value::StringLit(k.clone())).collect();
                    v.sort_by(|a, b| a.to_string().cmp(&b.to_string()));
                    v.dedup_by(|a, b| a.to_string() == b.to_string());
                    Ok(Value::Set(v))
                }
                _ => Err(EvalError::TypeError),
            }
        }
        _ => Err(EvalError::ArgError("set".to_string())),
    }
}

/// zip(*iterables) -> list of tuples
/// - 最短の iterable の長さで切り詰め
/// - 例: zip([1,2],[3,4]) => [(1,3), (2,4)]
fn builtin_zip_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.is_empty() {
        // zip() => []
        return Ok(Value::List(vec![]));
    }
    // まず各引数を「リスト(要素列)」に変換
    let mut lists = Vec::new();
    for arg in args {
        match arg {
            Value::List(lst) => lists.push(lst.clone()),
            Value::Tuple(tup) => lists.push(tup.clone()),
            Value::Set(st) => lists.push(st.clone()),
            Value::StringLit(s) => {
                // 文字列を char 単位でリスト化
                let charvals = s.chars().map(|c| Value::StringLit(c.to_string())).collect();
                lists.push(charvals);
            }
            // Dictの場合はキー一覧にする等、適宜実装
            Value::Dict(d) => {
                let keys = d.keys().map(|k| Value::StringLit(k.clone())).collect();
                lists.push(keys);
            }
            _ => return Err(EvalError::TypeError),
        }
    }
    // 各リストの長さの min を取る
    let min_len = lists.iter().map(|v| v.len()).min().unwrap_or(0);
    // i=0..min_len までループしてタプルを作る
    let mut result = Vec::new();
    for i in 0..min_len {
        let mut elem_tuple = Vec::new();
        for lst in &lists {
            elem_tuple.push(lst[i].clone());
        }
        result.push(Value::Tuple(elem_tuple));
    }
    Ok(Value::List(result))
}

/// str(obj) -> Convert object to string
/// Python's str() extracts the underlying value without repr-style quotes
/// - 0 args: empty string
/// - 1 arg: convert to string (StringLit/Var → raw value, others → formatted)
fn builtin_str_value(args: &[Value]) -> Result<Value, EvalError> {
    match args.len() {
        0 => Ok(Value::StringLit("".to_string())),
        1 => {
            // For StringLit and Var, extract the underlying string without quotes
            // For other types, use to_string() for formatted representation
            let result = match &args[0] {
                Value::StringLit(s) => s.clone(),
                Value::Var(v) => v.clone(),
                // For other types, to_string() gives appropriate representation
                // (e.g., lists → "[1, 2, 3]", dicts → "{'a': 1}", etc.)
                other => other.to_string(),
            };
            Ok(Value::StringLit(result))
        }
        _ => Err(EvalError::ArgError("str".to_string())),
    }
}

/// byte(obj) -> Python の bytes に近いもの？
/// - ここでは簡易的に、文字列を ASCII コードのリストに変換するとか、
///   あるいは "バイナリ" を表す文字列にして返す等、好きに決めてOK。
fn builtin_byte_value(args: &[Value]) -> Result<Value, EvalError> {
    // 例として、引数1つの文字列を ASCII コード(=Number)のリストにする簡易実装
    if args.len() != 1 {
        return Err(EvalError::ArgError("byte".to_string()));
    }
    match &args[0] {
        Value::StringLit(s) => {
            let codes = s.bytes().map(|b| Value::Number(b as f64)).collect();
            Ok(Value::List(codes))
        }
        _ => Err(EvalError::TypeError),
    }
}

/// any(iterable) -> 1.0 if any element is truthy, else 0.0
fn builtin_any_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("any".to_string()));
    }
    // iterableを取り出す
    let list = match &args[0] {
        Value::List(v) => v.clone(),
        Value::Tuple(v) => v.clone(),
        Value::Set(v) => v.clone(),
        // string を1文字ずつ見たい、dictのキーを見たい等、必要なら追加
        _ => return Err(EvalError::TypeError),
    };
    let yes = list.iter().any(|val| is_truthy(val));
    Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
}

/// all(iterable) -> 1.0 if all elements are truthy, else 0.0
fn builtin_all_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 1 {
        return Err(EvalError::ArgError("all".to_string()));
    }
    // iterableを取り出す
    let list = match &args[0] {
        Value::List(v) => v.clone(),
        Value::Tuple(v) => v.clone(),
        Value::Set(v) => v.clone(),
        _ => return Err(EvalError::TypeError),
    };
    let yes = list.iter().all(|val| is_truthy(val));
    Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
}

/// bool(obj) -> 1.0 if truthy, else 0.0
/// - 0引数なら false(=0.0) とするかは好みで
fn builtin_bool_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() == 0 {
        return Ok(Value::Number(0.0));
    } else if args.len() == 1 {
        let yes = is_truthy(&args[0]);
        Ok(Value::Number(if yes { 1.0 } else { 0.0 }))
    } else {
        Err(EvalError::ArgError("bool".to_string()))
    }
}

/// divmod(a, b) -> (a // b, a % b)
/// - 両方数値でないとエラー
/// - Python のように整数同士なら整数除算を想定 (小数が来るとどうするかはお好み)
fn builtin_divmod_value(args: &[Value]) -> Result<Value, EvalError> {
    if args.len() != 2 {
        return Err(EvalError::ArgError("divmod".to_string()));
    }
    let a = match &args[0] {
        Value::Number(n) => *n,
        _ => return Err(EvalError::TypeError),
    };
    let b = match &args[1] {
        Value::Number(n) => *n,
        _ => return Err(EvalError::TypeError),
    };
    if b == 0.0 {
        return Err(EvalError::DivisionByZero);
    }

    // Python 的には int( floor(a / b) ) と remainder
    // ここでは小数を使っても強引にfloorする例
    let quot = (a / b).floor();
    let rem = a - quot * b;
    let tuple_val = Value::Tuple(vec![Value::Number(quot), Value::Number(rem)]);
    Ok(tuple_val)
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

    fn with_parent(parent: Rc<RefCell<Env>>) -> Self {
        Env {
            vars: HashMap::new(),
            parent: Some(parent),
        }
    }

    fn get(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.vars.get(name) {
            Some(v.clone())
        } else if let Some(p) = &self.parent {
            p.borrow().get(name)
        } else {
            None
        }
    }

    fn set(&mut self, name: &str, val: Value) {
        self.vars.insert(name.to_string(), val);
    }
}

pub struct Interpreter {
    env: Rc<RefCell<Env>>,
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
        base_env.set(
            "list",
            Value::BuiltinValue {
                name: "list".to_string(),
                func: builtin_list_value,
            },
        );
        base_env.set(
            "dict",
            Value::BuiltinValue {
                name: "dict".to_string(),
                func: builtin_dict_value,
            },
        );
        base_env.set(
            "set",
            Value::BuiltinValue {
                name: "set".to_string(),
                func: builtin_set_value,
            },
        );
        base_env.set(
            "zip",
            Value::BuiltinValue {
                name: "zip".to_string(),
                func: builtin_zip_value,
            },
        );
        base_env.set(
            "str",
            Value::BuiltinValue {
                name: "str".to_string(),
                func: builtin_str_value,
            },
        );
        base_env.set(
            "byte",
            Value::BuiltinValue {
                name: "byte".to_string(),
                func: builtin_byte_value,
            },
        );
        base_env.set(
            "any",
            Value::BuiltinValue {
                name: "any".to_string(),
                func: builtin_any_value,
            },
        );
        base_env.set(
            "all",
            Value::BuiltinValue {
                name: "all".to_string(),
                func: builtin_all_value,
            },
        );
        base_env.set(
            "bool",
            Value::BuiltinValue {
                name: "bool".to_string(),
                func: builtin_bool_value,
            },
        );
        base_env.set(
            "divmod",
            Value::BuiltinValue {
                name: "divmod".to_string(),
                func: builtin_divmod_value,
            },
        );

        // Python boolean constants
        base_env.set("True", Value::Number(1.0));
        base_env.set("False", Value::Number(0.0));

        Interpreter {
            env: Rc::new(RefCell::new(base_env)),
        }
    }

    pub fn eval(&mut self, code: &str) -> Result<Value, EvalError> {
        let expr = parse_expr(code).map_err(EvalError::ParseError)?;
        let (val, new_env) = eval_expr(expr, Rc::clone(&self.env))?;
        self.env = new_env;
        Ok(val)
    }

    pub fn eval_with_context(
        &self,
        code: &str,
        context: &HashMap<String, Value>,
    ) -> Result<Value, EvalError> {
        let env = Rc::new(RefCell::new(self.env.borrow().clone()));
        for (name, value) in context.iter() {
            env.borrow_mut().set(name, value.clone());
        }
        let expr = parse_expr(code).map_err(EvalError::ParseError)?;
        let (val, _) = eval_expr(expr, Rc::clone(&env))?;
        Ok(val)
    }

    pub fn eval_boolean(&self, code: &str) -> Result<bool, EvalError> {
        let expr = parse_expr(code).map_err(EvalError::ParseError)?;
        let (val, _) = eval_expr(expr, Rc::clone(&self.env))?;
        Ok(val.to_bool())
    }
}

pub fn eval_expr(expr: Expr, env: Rc<RefCell<Env>>) -> Result<(Value, Rc<RefCell<Env>>), EvalError> {
    match expr {
        Expr::Number(n) => Ok((Value::Number(n), env)),
        Expr::Var(name) => {
            let val = env
                .borrow()
                .get(&name)
                .ok_or_else(|| EvalError::UndefinedVar(name))?;
            Ok((val, env))
        }
        Expr::StringLit(s) => Ok((Value::StringLit(s), env)),
        Expr::UnaryOp { op, operand } => {
            let (val, env) = eval_expr(*operand, env)?;
            let result = match op {
                UnOp::Not => Value::Number(if val.to_bool() { 0.0 } else { 1.0 }),
            };
            Ok((result, env))
        }
        Expr::BinaryOp { op, left, right } => {
            // Handle And with short-circuit evaluation
            if op == BinOp::And {
                let (lval, env) = eval_expr(*left, env)?;
                if !lval.to_bool() {
                    return Ok((lval, env)); // Short-circuit on false
                }
                return eval_expr(*right, env);
            }
            // Handle Or with short-circuit evaluation
            if op == BinOp::Or {
                let (lval, env) = eval_expr(*left, env)?;
                if lval.to_bool() {
                    return Ok((lval, env)); // Short-circuit on true
                }
                return eval_expr(*right, env);
            }

            // For other operators, evaluate both sides
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
                BinOp::Eq | BinOp::Ne => {
                    // Direct lambda comparisons throw TypeError
                    if matches!(lval, Value::Lambda { .. }) || matches!(rval, Value::Lambda { .. }) {
                        return Err(EvalError::TypeError);
                    }
                    // Use PartialEq for all other comparisons (including mixed types)
                    let are_equal = lval == rval;
                    let result = if op == BinOp::Eq { are_equal } else { !are_equal };
                    Value::Number(if result { 1.0 } else { 0.0 })
                }
                BinOp::And | BinOp::Or => {
                    unreachable!("And/Or handled above with short-circuit evaluation")
                }
            };
            Ok((val, env))
        }
        Expr::Assign { name, expr } => {
            let (val, env) = eval_expr(*expr, env)?;
            env.borrow_mut().set(&name, val.clone());
            Ok((val, env))
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
                    let call_env = Rc::new(RefCell::new(call_env));
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
            let mut cur_env = env.clone();
            for e in exprs {
                let (val, e2) = eval_expr(e, cur_env)?;
                cur_env = e2;
                vals.push(val);
            }
            Ok((Value::List(vals), cur_env))
        }
        Expr::Tuple(exprs) => {
            let mut vals = Vec::new();
            let mut cur_env = env.clone();
            for e in exprs {
                let (val, e2) = eval_expr(e, cur_env)?;
                cur_env = e2;
                vals.push(val);
            }
            Ok((Value::Tuple(vals), cur_env))
        }
        Expr::Set(exprs) => {
            let mut vals = Vec::new();
            let mut cur_env = env.clone();
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
            let mut cur_env = env.clone();
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
                let mut new_env_data = (*current_env).borrow().clone();
                new_env_data.set(&var, item);
                let new_env = Rc::new(RefCell::new(new_env_data));

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
                let mut new_env_data = (*current_env).borrow().clone();
                new_env_data.set(&var, item);
                let new_env = Rc::new(RefCell::new(new_env_data));

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

            // 3) オブジェクト型ごとにメソッドを呼び出す
            let result = match obj_val {
                Value::StringLit(s) => {
                    // 文字列用メソッド (前回実装済み想定)
                    do_string_method(&s, &method, &arg_vals)?
                }
                Value::List(lst) => {
                    // リスト用メソッド (前回実装済み想定)
                    do_list_method(&lst, &method, &arg_vals)?
                }
                Value::Dict(m) => {
                    // 今回新たに追加: Dict 用メソッド
                    do_dict_method(&m, &method, &arg_vals)?
                }
                // その他の型に対してメソッド呼び出しはエラー
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
        Value::StringLit(s) => !s.is_empty(),
        Value::List(v) => !v.is_empty(),
        Value::Tuple(v) => !v.is_empty(),
        Value::Set(v) => !v.is_empty(),
        Value::Dict(m) => !m.is_empty(),
        // Lambda や Builtin などの場合の真偽は実装次第
        _ => true,
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

/// 文字列/Var からキー文字列を取り出す補助関数
fn extract_key(val: &Value) -> Result<String, EvalError> {
    match val {
        Value::Var(s) => Ok(s.clone()),
        Value::StringLit(s) => Ok(s.clone()),
        Value::Number(n) => Ok(n.to_string()), // 数値キーを文字列化して使うなど(簡易実装)
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
        //     ここでは簡易的に dict を返すだけにす��� or 未実装エラー
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

fn do_list_method(list: &Vec<Value>, method: &str, args: &[Value]) -> Result<Value, EvalError> {
    match method {
        //--------------------------------------------------------------------------------
        // 1) append(x)
        //    - 末尾に x を追加して返す
        "append" => {
            if args.len() != 1 {
                return Err(EvalError::ArgError("append".to_string()));
            }
            let mut new_list = list.clone(); // 複製してから push
            new_list.push(args[0].clone());
            Ok(Value::List(new_list))
        }

        //--------------------------------------------------------------------------------
        // 2) clear()
        //    - 空リストを返す
        "clear" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("clear".to_string()));
            }
            Ok(Value::List(vec![]))
        }

        //--------------------------------------------------------------------------------
        // 3) copy()
        //    - 同じ要素を持つ新たなリストを返す
        "copy" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("copy".to_string()));
            }
            let new_list = list.clone();
            Ok(Value::List(new_list))
        }

        //--------------------------------------------------------------------------------
        // 4) count(x)
        //    - x と等しい要素の個数を返す (数値で)
        "count" => {
            if args.len() != 1 {
                return Err(EvalError::ArgError("count".to_string()));
            }
            let target = &args[0];
            let c = list.iter().filter(|v| *v == target).count();
            Ok(Value::Number(c as f64))
        }

        //--------------------------------------------------------------------------------
        // 5) extend(iterable)
        //    - 引数がリストならその要素を末尾に追加して返す
        "extend" => {
            if args.len() != 1 {
                return Err(EvalError::ArgError("extend".to_string()));
            }
            // extend する対象をリスト(あるいはタプル/セット等)とみなす
            let mut new_list = list.clone();
            match &args[0] {
                Value::List(v) => {
                    new_list.extend(v.clone());
                }
                Value::Tuple(v) => {
                    new_list.extend(v.clone());
                }
                Value::Set(v) => {
                    new_list.extend(v.clone());
                }
                _ => {
                    // 簡易的にエラー
                    return Err(EvalError::TypeError);
                }
            }
            Ok(Value::List(new_list))
        }

        //--------------------------------------------------------------------------------
        // 6) index(x)
        //    - x と等しい最初の要素のインデックスを返す (無ければエラーにする)
        "index" => {
            if args.len() != 1 {
                return Err(EvalError::ArgError("index".to_string()));
            }
            let target = &args[0];
            if let Some(pos) = list.iter().position(|v| v == target) {
                Ok(Value::Number(pos as f64))
            } else {
                // Python なら ValueError 的なものを投げる
                Err(EvalError::ArgError("value not in list".to_string()))
            }
        }

        //--------------------------------------------------------------------------------
        // 7) insert(i, x)
        //    - i番目に x を挿入して返す（ i > list.len() なら末尾扱い ）
        "insert" => {
            if args.len() != 2 {
                return Err(EvalError::ArgError("insert".to_string()));
            }
            let i = match &args[0] {
                Value::Number(n) => *n as isize,
                _ => return Err(EvalError::TypeError),
            };
            let x = &args[1];
            let mut new_list = list.clone();

            // Python では負数指定もあるが、ここでは簡易的に 0 未満なら 0、上回れば末尾にする
            let mut idx = if i < 0 { 0 } else { i as usize };
            if idx > new_list.len() {
                idx = new_list.len();
            }

            new_list.insert(idx, x.clone());
            Ok(Value::List(new_list))
        }

        //--------------------------------------------------------------------------------
        // 8) pop([i])
        //    - i を指定しなければ末尾を pop して、その popped item を返す
        //    - i が範囲外ならエラー
        "pop" => {
            let mut new_list = list.clone();
            if new_list.is_empty() {
                return Err(EvalError::ArgError("pop from empty list".to_string()));
            }

            // 引数があれば i、無ければ -1 (末尾)
            let idx = if args.is_empty() {
                (new_list.len() - 1) as isize
            } else if args.len() == 1 {
                match &args[0] {
                    Value::Number(n) => *n as isize,
                    _ => return Err(EvalError::TypeError),
                }
            } else {
                return Err(EvalError::ArgError("pop".to_string()));
            };

            // 負数に対応するなら Python 的に末尾から数えるが、ここでは簡易実装
            if idx < 0 {
                // -1 以外の負数は未対応 (必要なら対応する)
                if idx != -1 {
                    return Err(EvalError::TypeError);
                }
                // -1 => 末尾
                let popped = new_list.pop().unwrap();
                Ok(popped)
            } else {
                let i = idx as usize;
                if i >= new_list.len() {
                    return Err(EvalError::ArgError("pop index out of range".to_string()));
                }
                let popped = new_list.remove(i);
                Ok(popped)
            }
        }

        //--------------------------------------------------------------------------------
        // 9) remove(x)
        //    - x と等しい最初の要素を削除して返す（見つからなければエラー）
        "remove" => {
            if args.len() != 1 {
                return Err(EvalError::ArgError("remove".to_string()));
            }
            let target = &args[0];
            let mut new_list = list.clone();
            if let Some(pos) = new_list.iter().position(|v| v == target) {
                new_list.remove(pos);
                Ok(Value::List(new_list))
            } else {
                Err(EvalError::ArgError("value not in list".to_string()))
            }
        }

        //--------------------------------------------------------------------------------
        // 10) reverse()
        //     - リストを逆順にして返す
        "reverse" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("reverse".to_string()));
            }
            let mut new_list = list.clone();
            new_list.reverse();
            Ok(Value::List(new_list))
        }

        //--------------------------------------------------------------------------------
        // 11) sort()
        //     - リストをソートして返す
        //       (要素が全て数値 or 全て文字列のときだけ対応。それ以外はエラー)
        "sort" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("sort".to_string()));
            }
            // まず型を確認
            if list.is_empty() {
                // 空ならそのまま返す
                return Ok(Value::List(vec![]));
            }

            let all_numbers = list.iter().all(|v| matches!(v, Value::Number(_)));
            let all_strings = list.iter().all(|v| matches!(v, Value::StringLit(_)));

            if !all_numbers && !all_strings {
                return Err(EvalError::TypeError);
            }

            let mut new_list = list.clone();
            if all_numbers {
                // 数値としてソート
                new_list.sort_by(|a, b| {
                    let na = match a {
                        Value::Number(x) => x,
                        _ => &0.0, // ここには来ない想定
                    };
                    let nb = match b {
                        Value::Number(x) => x,
                        _ => &0.0,
                    };
                    na.partial_cmp(nb).unwrap_or(std::cmp::Ordering::Equal)
                });
            } else {
                // 文字列としてソート
                new_list.sort_by(|a, b| {
                    let sa = match a {
                        Value::StringLit(x) => x,
                        _ => "",
                    };
                    let sb = match b {
                        Value::StringLit(x) => x,
                        _ => "",
                    };
                    sa.cmp(sb)
                });
            }
            Ok(Value::List(new_list))
        }
        // その他
        other => Err(EvalError::UndefinedVar(format!(
            "Unknown list method: {}",
            other
        ))),
    }
}

fn do_dict_method(
    map: &IndexMap<String, Value>,
    method: &str,
    args: &[Value],
) -> Result<Value, EvalError> {
    match method {
        //--------------------------------------------------------------------------------
        // 1) clear()
        //    - すべての要素を削除した空の Dict を返す
        "clear" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("clear".to_string()));
            }
            let new_map = IndexMap::new();
            Ok(Value::Dict(new_map))
        }

        //--------------------------------------------------------------------------------
        // 2) copy()
        //    - 同じ要素を持つ新たな Dict を返す
        "copy" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("copy".to_string()));
            }
            let new_map = map.clone();
            Ok(Value::Dict(new_map))
        }

        //--------------------------------------------------------------------------------
        // 3) fromkeys(iterable, value=None)
        //    - クラスメソッド的な位置付けだが、ここでは "some_dict.fromkeys(...)" 形式で呼ばれたら
        //      新規にキーだけをセットした Dict を返す
        "fromkeys" => {
            // 引数: iterable, value(省略可)
            let iterable = args
                .get(0)
                .ok_or_else(|| EvalError::ArgError("fromkeys".to_string()))?;
            let default_val = args.get(1).unwrap_or(&Value::Number(0.0)).clone();
            // ここでは省略時に Number(0.0) など、適当に
            let keys_list = match iterable {
                Value::List(v) => v.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
                Value::Tuple(v) => v.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
                Value::Set(v) => v.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
                Value::Dict(m) => m.keys().cloned().collect::<Vec<_>>(),
                // Python だと文字列を1文字ずつイテレートするなどあるが、ここでは簡易実装
                _ => return Err(EvalError::TypeError),
            };

            let mut new_map = IndexMap::new();
            for k in keys_list {
                new_map.insert(k, default_val.clone());
            }
            Ok(Value::Dict(new_map))
        }

        //--------------------------------------------------------------------------------
        // 4) get(key, default=None)
        //    - キーがあればその値、無ければデフォルトを返す
        "get" => {
            let key = args
                .get(0)
                .ok_or_else(|| EvalError::ArgError("get".to_string()))?;
            let default_val = args.get(1).unwrap_or(&Value::Number(0.0));
            let k = extract_key(key)?;
            if let Some(v) = map.get(&k) {
                Ok(v.clone())
            } else {
                Ok(default_val.clone())
            }
        }

        //--------------------------------------------------------------------------------
        // 5) items()
        //    - [(k, v), (k, v), ...] というリストを返す
        "items" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("items".to_string()));
            }
            let mut lst = Vec::new();
            for (k, v) in map.iter() {
                // タプル (key, value)
                lst.push(Value::Tuple(vec![Value::StringLit(k.clone()), v.clone()]));
            }
            Ok(Value::List(lst))
        }

        //--------------------------------------------------------------------------------
        // 6) keys()
        //    - [k1, k2, k3, ...] というリストを返す
        "keys" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("keys".to_string()));
            }
            let mut lst = Vec::new();
            for k in map.keys() {
                lst.push(Value::StringLit(k.clone()));
            }
            Ok(Value::List(lst))
        }

        //--------------------------------------------------------------------------------
        // 7) pop(key, default=エラー)
        //    - key があればその値を返す（返した結果、辞書から消える…はずだが、ここは純粋関数型なので消えない）
        //    - 無ければ default があればそれを返す、無ければエラー
        "pop" => {
            let key_val = args
                .get(0)
                .ok_or_else(|| EvalError::ArgError("pop".to_string()))?;
            let default_val = args.get(1);
            let k = extract_key(key_val)?;
            if let Some(v) = map.get(&k) {
                // key が存在
                Ok(v.clone())
            } else {
                // key が無い
                if let Some(dv) = default_val {
                    Ok(dv.clone())
                } else {
                    return Err(EvalError::ArgError("key not found".to_string()));
                }
            }
        }

        //--------------------------------------------------------------------------------
        // 8) popitem()
        //    - 最後に挿入された (key, value) を削除して (key, value) タプルを返す
        //    - Python 3.6+ は OrderedDict 同等なので、IndexMap の挿入順序と同じ
        //    - ここも実際には消えないが、値だけ返す例にする
        "popitem" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("popitem".to_string()));
            }
            if map.is_empty() {
                return Err(EvalError::ArgError("popitem: dict is empty".to_string()));
            }
            let (last_k, last_v) = map.last().unwrap(); // IndexMap は最後の要素をこう取れる
                                                        // 返すのは (key, value) タプル
            Ok(Value::Tuple(vec![
                Value::StringLit(last_k.clone()),
                last_v.clone(),
            ]))
        }

        //--------------------------------------------------------------------------------
        // 9) setdefault(key, default=None)
        //    - key があればその値を返す。無ければ default を登録して default を返す…が、
        //      ここでは純粋関数型なので dict は更新されず、「最終的な値だけ返す」にする
        "setdefault" => {
            let key_val = args
                .get(0)
                .ok_or_else(|| EvalError::ArgError("setdefault".to_string()))?;
            let default_val = args.get(1).unwrap_or(&Value::Number(0.0));
            let k = extract_key(key_val)?;

            if let Some(v) = map.get(&k) {
                // すでにキーがある場合 => その値を返す
                Ok(v.clone())
            } else {
                // ない場合 => default_val
                Ok(default_val.clone())
            }
        }

        //--------------------------------------------------------------------------------
        // 10) update(other_dict)
        //     - 引数を Dict や (key, value) リスト・タプルなどとみなし、キーを上書き or 追加して、
        //       「新しい dict」を返す
        "update" => {
            if args.len() != 1 {
                return Err(EvalError::ArgError("update".to_string()));
            }
            let mut new_map = map.clone();

            match &args[0] {
                // 1) Dict => すべての (k, v) をコピー
                Value::Dict(m2) => {
                    for (k, v) in m2.iter() {
                        new_map.insert(k.clone(), v.clone());
                    }
                }
                // 2) リスト or タプル => [(k, v), ...] な構造かを期待
                Value::List(lst) | Value::Tuple(lst) => {
                    for item in lst {
                        match item {
                            Value::Tuple(tv) if tv.len() == 2 => {
                                let k = tv[0].to_string();
                                let v = tv[1].clone();
                                new_map.insert(k, v);
                            }
                            _ => return Err(EvalError::TypeError),
                        }
                    }
                }
                _ => return Err(EvalError::TypeError),
            }

            Ok(Value::Dict(new_map))
        }

        //--------------------------------------------------------------------------------
        // 11) values()
        //     - [v1, v2, v3, ...] というリストを返す
        "values" => {
            if !args.is_empty() {
                return Err(EvalError::ArgError("values".to_string()));
            }
            let mut lst = Vec::new();
            for v in map.values() {
                lst.push(v.clone());
            }
            Ok(Value::List(lst))
        }

        //--------------------------------------------------------------------------------
        // 未知のメソッド
        other => Err(EvalError::UndefinedVar(format!(
            "Unknown dict method: {}",
            other
        ))),
    }
}
