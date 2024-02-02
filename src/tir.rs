use crate::c::ParamId;
pub use crate::hir::{ConstId, FunctionId, IntrinsicId, LocalId, Const};
use crate::ivec::IVec;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Int,
    String,
    Function(FunctionId, IVec<Type, ParamId>, Box<Type>),
    Intrinsic(IntrinsicId, IVec<Type, ParamId>, Box<Type>),
}

impl Type {
    pub fn is_zero_sized(&self) -> bool {
        match self {
            Type::Unit => true,
            Type::Int => false,
            Type::String => false,
            Type::Function(_, _, _) => true,
            Type::Intrinsic(_, _, _) => true,
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub constants: IVec<Const, ConstId>,
    pub functions: IVec<Function, FunctionId>,
}

#[derive(Debug)]
pub struct Function {
    pub locals: IVec<Type, LocalId>,
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
    Function(FunctionId),
    Intrinsic(IntrinsicId),
    Block(Vec<Typed>, Typed),
    Method {
        object: Typed,
        name: String,
        // TODO: maybe reconsider the type: IVec<Typed, ParamId>
        args: Vec<Typed>,
    },
    Assign {
        var: LocalId,
        expr: Typed,
    },
    Return {
        value: Typed,
    },
}
