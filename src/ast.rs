use crate::span::Span;

#[derive(Debug)]
pub struct Ast<T>(pub T, pub Span);

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Ast<Item>>,
}

#[derive(Debug)]
pub enum Item {
    Function {
        ident: String,
        body: Ast<Expression>,
    },
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Assign,
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
    Neg,
    Pos,
}

#[derive(Debug)]
pub enum Expression {
    Block(Vec<Ast<Expression>>, Option<Box<Ast<Expression>>>),
    Call {
        callee: Box<Ast<Expression>>,
        args: Vec<Ast<Expression>>,
    },
    Literal(Literal),
    Ident(String),
    Binary {
        op: Ast<BinaryOp>,
        lhs: Box<Ast<Expression>>,
        rhs: Box<Ast<Expression>>,
    },
    Unary {
        op: Ast<UnaryOp>,
        expr: Box<Ast<Expression>>,
    },
    Definition {
        ident: Ast<String>,
        expr: Box<Ast<Expression>>,
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
