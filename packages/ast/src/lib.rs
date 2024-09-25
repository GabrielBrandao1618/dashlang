use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy, Eq)]
pub struct Location {
    pub start: usize,
    pub end: usize,
}
impl From<(usize, usize)> for Location {
    fn from(value: (usize, usize)) -> Self {
        let (start, end) = value;
        Location::new(start, end)
    }
}
impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
impl Default for Location {
    fn default() -> Self {
        Location::new(0, 0)
    }
}
#[derive(Clone, PartialEq, Debug)]
pub struct Located<T> {
    pub location: Location,
    pub value: T,
}
impl<T> Located<T> {
    pub fn new(value: T, location: Location) -> Self {
        Self { location, value }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct AssignmentExpr {
    pub symbol: String,
    pub value: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Gt, // Greater than
    Eq, // Equal
    Ge, // Greater or equal than
    Lt, // Less than
    Le, // Less of equal than
    And,
    Or,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseShiftLeft,
    BitwiseShiftRight,
}
#[derive(Debug, PartialEq, Clone)]
pub struct BinaryExpr {
    pub left: Located<Expr>,
    pub right: Located<Expr>,
    pub operator: BinaryOperator,
}
impl BinaryExpr {
    pub fn new(left: Located<Expr>, right: Located<Expr>, op: BinaryOperator) -> Self {
        Self {
            left,
            right,
            operator: op,
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Not,
    BitwiseNot,
}
#[derive(Debug, PartialEq, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: Located<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Call {
    pub symbol: String,
    pub args: Vec<Located<Expr>>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Closure {
    pub params: Vec<String>,
    pub body: Program,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Symbol {
    pub value: String,
}
#[derive(Debug, PartialEq, Clone)]
pub struct SubExpr {
    pub value: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct DestructuringAsignment {
    pub symbols: Vec<Symbol>,
    pub value: Box<Expr>,
}
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    BinaryExpr(Box<Located<BinaryExpr>>),
    UnaryExpr(Box<Located<UnaryExpr>>),
    Assignment(Located<AssignmentExpr>),
    Call(Located<Call>),
    Symbol(Located<Symbol>),
    Literal(Located<Literal>),
    SubExpr(Located<SubExpr>),
    DestructuringAsignment(Located<DestructuringAsignment>),
}
#[derive(Debug, PartialEq, Clone)]
pub struct Int {
    pub value: i64,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Float {
    pub value: f64,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Str {
    pub value: String,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Vector {
    pub value: Vec<Located<Expr>>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Tuple {
    pub value: Vec<Located<Expr>>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct Map {
    pub value: HashMap<String, Located<Expr>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Atom {
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Closure(Located<Closure>),
    Int(Located<Int>),
    Float(Located<Float>),
    String(Located<Str>),
    Bool(Located<Boolean>),
    Vector(Located<Vector>),
    Null(Located<()>),
    Void(Located<()>),
    Tuple(Located<Tuple>),
    Map(Located<Map>),
    Atom(Located<Atom>),
}
impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Literal::Closure(_) => false,
            Literal::Int(val) => {
                if let Literal::Int(other_val) = other {
                    if other_val.value == val.value {
                        return true;
                    }
                }
                return false;
            }
            Literal::Float(val) => {
                if let Literal::Float(other_val) = other {
                    if other_val.value == val.value {
                        return true;
                    }
                }
                return false;
            }
            Literal::String(_) => todo!(),
            Literal::Bool(_) => todo!(),
            Literal::Vector(_) => todo!(),
            Literal::Null(_) => {
                if let Literal::Null(_) = other {
                    return true;
                }
                return false;
            }
            Literal::Void(_) => todo!(),
            Literal::Tuple(_) => todo!(),
            Literal::Map(_) => todo!(),
            Literal::Atom(val) => {
                if let Literal::Atom(other_val) = other {
                    if other_val.value == val.value {
                        return true;
                    }
                }
                return false;
            }
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub cond: Expr,
    pub body: Program,
    pub else_block: Option<Program>,
}
#[derive(Debug, PartialEq, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: Program,
}
#[derive(Debug, PartialEq, Clone)]
pub struct For {
    pub cond: Expr,
    pub body: Program,
    pub init: Stmt,
    pub iteration: Stmt,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
    pub value: Expr,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Return(Located<Return>),
    If(Located<If>),
    While(Located<While>),
    For(Box<Located<For>>),
    Expr(Located<Expr>),
}

pub type Program = Vec<Stmt>;
