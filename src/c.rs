pub use crate::tir::{LocalId};

#[derive(Debug, Clone, Copy)]
pub struct StructId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct TempId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct GlobalId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct ExternalId(pub usize);

#[derive(Debug, Clone, Copy)]
pub struct ParamId(pub usize);

#[derive(Debug)]
pub struct Struct {
    pub fields: Vec<CType>,
}

#[derive(Debug)]
pub struct Module {
    pub includes: Vec<String>,
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
    pub externals: Vec<ExternalFunction>,
    pub main: Option<GlobalId>,
}

#[derive(Debug)]
pub struct ExternalFunction {
    pub name: String,
    pub parameters: Vec<CType>,
    pub return_type: CType,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: Vec<CType>,
    pub body: Vec<Statement>,
    pub locals: Vec<CType>,
    pub temps: Vec<CType>,
    pub return_type: CType,
}

#[derive(Debug, Clone)]
pub enum CType {
    Int,
    Char,
    Void,
    Struct(StructId),
    Pointer(Box<CType>),
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expression),
    Return(Expression),
}

#[derive(Debug)]
pub enum Expression {
    Int(i64),
    String(String),
    Local(LocalId),
    Param(ParamId),
    Temp(TempId),
    Global(GlobalId),
    Call(GlobalId, Vec<Expression>),
    ExternalCall(ExternalId, Vec<Expression>),
    DynamicCall(Box<Expression>, Vec<Expression>),
    Assign(LocalId, Box<Expression>),
    AssignTemp(TempId, Box<Expression>),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    UnaryPlus(Box<Expression>),
    UnaryMinus(Box<Expression>),
    StructAccess(Box<Expression>, usize)
}
