#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Asignment {
    pub symbol: String,
    pub value: Box<Expr>,
}
#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
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
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct BinaryExpr {
    pub left: Expr,
    pub right: Expr,
    pub op_type: BinaryOperator,
}
impl BinaryExpr {
    pub fn new(left: Expr, right: Expr, op_type: BinaryOperator) -> Self {
        Self {
            left,
            right,
            op_type,
        }
    }
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum UnaryOperator {
    Not,
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct UnaryExpr {
    pub op_type: UnaryOperator,
    pub operand: Expr,
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Call {
    pub symbol: String,
    pub args: Vec<Expr>,
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct Closure {
    pub params: Vec<String>,
    pub body: Program,
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr {
    BinaryExpr(Box<BinaryExpr>),
    UnaryExpr(Box<UnaryExpr>),
    Asignment(Asignment),
    Call(Call),
    Symbol(String),
    Literal(Literal),
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Literal {
    Closure(Closure),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
    Void,
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct If {
    pub cond: Expr,
    pub body: Program,
    pub else_block: Option<Program>,
}
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct While {
    pub cond: Expr,
    pub body: Program,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Stmt {
    Return(Expr),
    If(If),
    While(While),
    Print(Expr),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Instruction {
    Stmt(Stmt),
    Expr(Expr),
}
pub type Program = Vec<Instruction>;
