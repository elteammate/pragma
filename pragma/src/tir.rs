pub use crate::hir::{ConstId, FunctionId, IntrinsicId, LocalId, ParamId, Const};
use crate::ivec::IVec;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Unit,
    Int,
    String,
    Function(FunctionId, IVec<ParamId, Type>, Box<Type>),
    Intrinsic(IntrinsicId, IVec<ParamId, Type>, Box<Type>),
    Never,
}

impl Type {
    pub fn is_zero_sized(&self) -> bool {
        match self {
            Type::Unit => true,
            Type::Int => false,
            Type::String => false,
            Type::Function(_, _, _) => true,
            Type::Intrinsic(_, _, _) => true,
            Type::Never => true,
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub constants: IVec<ConstId, Const>,
    pub functions: IVec<FunctionId, Function>,
}

#[derive(Debug)]
pub struct Function {
    pub ident: String,
    pub return_ty: Type,
    pub args: IVec<ParamId, LocalId>,
    pub locals: IVec<LocalId, Type>,
    pub body: Typed,
}

#[derive(Debug)]
pub struct Typed {
    pub ty: Type,
    pub expr: Box<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Uninit,
    Constant(ConstId),
    Local(LocalId),
    Function(FunctionId),
    Intrinsic(IntrinsicId),
    Block(Vec<Typed>, Typed),
    Method {
        object: Typed,
        name: String,
        // TODO: maybe reconsider the type: IVec<ParamId, Typed>
        args: Vec<Typed>,
    },
    Assign {
        var: LocalId,
        expr: Typed,
    },
    // TODO: make into tuple variant
    Return {
        value: Typed,
    },
    Trap,
}
