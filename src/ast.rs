use logos::Span;

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Function {
        ident: String,
        body: Expression,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add, Sub, Mul, Assign
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
pub enum Precedence {
    Assign,
    Add,
    Mul,
}

impl BinaryOp {
    pub fn precedence(&self) -> Precedence {
        match self {
            BinaryOp::Assign => Precedence::Assign,
            BinaryOp::Add | BinaryOp::Sub => Precedence::Add,
            BinaryOp::Mul => Precedence::Mul,
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Neg, Pos
}

#[derive(Debug)]
pub enum Expression {
    Block(Vec<Expression>, Option<Box<Expression>>),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Literal(Literal),
    Ident(String),
    Binary {
        op: BinaryOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Definition {
        ident: String,
        expr: Box<Expression>,
    },
}

#[derive(Debug)]
pub enum Literal {
    Number(i64),
    String(String),
}

#[derive(Debug)]
pub enum ParsingError {
    UnknownToken(String, Span),
    UnexpectedToken(String, Span),
    IncorrectPattern(String, Expression),
    UnexpectedEof,
}
