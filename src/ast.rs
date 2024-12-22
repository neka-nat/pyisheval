use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(f64),
    Var(String),
    StringLit(String),
    BinaryOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Assign {
        name: String,
        expr: Box<Expr>,
    },
    Lambda {
        param: String,
        body: Box<Expr>,
    },
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    Set(Vec<Expr>),
    Dict(Vec<(String, Expr)>),
    ListComp {
        expr: Box<Expr>,
        var: String,
        iter: Box<Expr>,
        cond: Option<Box<Expr>>,
    },
    DictComp {
        key_expr: Box<Expr>,
        value_expr: Box<Expr>,
        var: String,
        iter: Box<Expr>,
        cond: Option<Box<Expr>>,
    },
    Index {
        expr: Box<Expr>,
        index: Box<Expr>,
    },
    IfExpr {
        condition: Box<Expr>,
        if_true: Box<Expr>,
        if_false: Box<Expr>,
    },
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    FloorDiv,
    Mod,
    Exp,
    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::FloorDiv => write!(f, "//"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Exp => write!(f, "**"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lt => write!(f, "<"),
            BinOp::Ge => write!(f, ">="),
            BinOp::Le => write!(f, "<="),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
        }
    }
}
