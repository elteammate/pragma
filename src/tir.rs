pub use crate::hir::{ConstId, GlobalId, LocalId};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    String,
    Function(Vec<Type>, Box<Type>),
}

#[derive(Debug)]
pub struct Module {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub struct Function {
    pub locals: Vec<Type>,
    pub ident: String,
    pub body: Typed,
}

#[derive(Debug)]
pub struct Typed {
    pub ty: Type,
    pub expr: Box<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Constant(ConstId),
    Local(LocalId),
    Global(GlobalId),
    Block(Vec<Typed>, Typed),
    Method {
        object: Typed,
        name: String,
        args: Vec<Typed>,
    },
    Assign {
        var: LocalId,
        expr: Typed,
    },
}

#[derive(Debug)]
pub enum Constant {
    Unit,
    Int(i64),
    String(String),
}

impl Constant {
    pub fn ty(&self) -> Type {
        match self {
            Constant::Unit => Type::Unit,
            Constant::Int(_) => Type::Int,
            Constant::String(_) => Type::String,
        }
    }
}
