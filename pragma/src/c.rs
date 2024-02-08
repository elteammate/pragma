use crate::create_index;
use crate::ivec::IVec;

create_index!(pub StructId);
create_index!(pub TempId);
create_index!(pub FunctionId);
create_index!(pub ExternalId);
create_index!(pub ParamId);
create_index!(pub LocalId);
create_index!(pub StructFieldId);

macro_rules! typed_id {
    ($name:ident) => {
        paste! {
            #[derive(Debug, Clone)]
            pub struct [<Typed $name>] {
                id: $name,
                ty: CType,
            }

            impl [<Typed $name>] {
                pub fn new(id: $name, ty: CType) -> Self {
                    Self { id, ty }
                }

                pub fn ty(&self) -> CType {
                    self.ty.clone()
                }
            }

            impl From<[<Typed $name>]> for $name {
                fn from(typed: [<Typed $name>]) -> Self {
                    typed.id
                }
            }
        }
    }
}

typed_id!(LocalId);
typed_id!(TempId);
typed_id!(FunctionId);
typed_id!(ExternalId);
typed_id!(ParamId);

#[derive(Debug)]
pub struct Struct {
    pub fields: IVec<CType, StructFieldId>,
}

#[derive(Debug)]
pub struct Module {
    pub includes: Vec<String>,
    pub structs: IVec<Struct, StructId>,
    pub functions: IVec<Function, FunctionId>,
    pub externals: IVec<ExternalFunction, ExternalId>,
    pub main: Option<FunctionId>,
}

#[derive(Debug)]
pub struct ExternalFunction {
    pub name: String,
    pub parameters: IVec<CType, ParamId>,
    pub return_type: CType,
}

#[derive(Debug)]
pub struct Function {
    pub parameters: IVec<CType, ParamId>,
    pub body: Vec<Statement>,
    pub locals: IVec<CType, LocalId>,
    pub temps: IVec<CType, TempId>,
    pub return_type: CType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CType {
    Int,
    Char,
    Void,
    Struct(StructId),
    Pointer(Box<CType>),
    Function(IVec<CType, ParamId>, Box<CType>),
}

impl CType {
    pub fn is_sized(&self) -> bool {
        match self {
            CType::Int => true,
            CType::Char => true,
            CType::Void => false,
            CType::Struct(_) => true,
            CType::Pointer(_) => true,
            CType::Function(_, _) => false,
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    Expression(Expr),
    Return(Expr),
    ReturnVoid,
}

#[derive(Debug)]
pub struct Expr {
    pub expr: Expression,
    pub ty: CType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Precedence {
    Lowest,
    SuffixUnary,
    PrefixUnary,
    Multiply,
    Add,
    Shift,
    Compare,
    Equality,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
    Ternary,
    Assign,
    Comma,
    Highest,
}

impl Expr {
    pub fn prec(&self) -> Precedence {
        self.expr.prec()
    }
}

macro_rules! create_expressions {
    {
        $(
            $name:ident
            ($($param_name:ident : $param_ty:ty $(as $modifier:ty)?),+)
            [$prec:expr]
            : $($ctype:expr)?;
        )*
    } => {
        #[derive(Debug)]
        pub enum Expression {
            $($name {
                $(
                    $param_name : create_expressions!(@modifier $param_ty $(as $modifier)?),
                )+
            },)*
        }

        impl Expression {
            pub fn prec(&self) -> Precedence {
                match self {
                    $(Expression::$name { .. } => $prec,)*
                }
            }
        }

        $(
            create_expressions!(
                @impl
                $name
                ($($param_name : $param_ty),+)
                : $($ctype)?
            );
        )*
    };

    (@impl $name:ident ($($param_name:ident : $param_ty:ty),+) : ) => {
        paste! {
            impl Expr {
                pub fn [<new_ $name:snake>] (
                    $($param_name : $param_ty,)+
                    ty: CType,
                ) -> Self {
                    Self { expr: Expression::$name {
                        $(
                            $param_name: $param_name.into(),
                        )+
                    }, ty }
                }
            }
        }
    };

    (@impl $name:ident ($($param_name:ident : $param_ty:ty),+) : $ctype:expr) => {
        paste! {
            impl Expr {
                #[allow(dead_code)]
                pub fn [<new_ $name:snake>] (
                    $($param_name : $param_ty,)+
                ) -> Self {
                    let ty = $ctype;
                    Self {
                        expr: Expression::$name {
                            $(
                                $param_name: $param_name.into(),
                            )+
                        },
                        ty
                    }
                }
            }
        }
    };

    (@modifier $ty:ty) => {
        $ty
    };

    (@modifier $ty:ty as $modifier:ty) => {
        $modifier
    };
}

create_expressions! {
    // TODO: figure out proper integer formats
    Int(x: i64)[Precedence::Lowest] : CType::Int;
    String(s: String)[Precedence::Lowest] : CType::Pointer(Box::new(CType::Char));
    Local(id: TypedLocalId as LocalId)[Precedence::Lowest] : id.ty();
    Param(id: TypedParamId as ParamId)[Precedence::Lowest] : id.ty();
    Temp(id: TypedTempId as TempId)[Precedence::Lowest] : id.ty();
    Global(id: TypedFunctionId as FunctionId)[Precedence::Lowest] : id.ty();
    External(id: TypedExternalId as ExternalId)[Precedence::Lowest] : id.ty();
    Call(f: Expr as Box<Expr>, args: IVec<Expr, ParamId>)[Precedence::SuffixUnary] : {
        match &f.ty {
            CType::Function(params, ret) => {
                assert_eq!(params.len(), args.len());
                for (param, arg) in params.iter().zip(args.iter()) {
                    assert_eq!(param, &arg.ty);
                }
                (**ret).clone()
            },
            _ => panic!("Expected pointer type, got {:?}", &f.ty),
        }
    };
    Assign(lhs: TypedLocalId as LocalId, rhs: Expr as Box<Expr>)[Precedence::Assign] : {
        assert_eq!(&lhs.ty, &rhs.ty);
        lhs.ty()
    };
    AssignTemp(lhs: TypedTempId as TempId, rhs: Expr as Box<Expr>)[Precedence::Assign] : {
        assert_eq!(&lhs.ty, &rhs.ty);
        lhs.ty()
    };
    Plus(lhs: Expr as Box<Expr>, rhs: Expr as Box<Expr>)[Precedence::Add] : {
        match &lhs.ty {
            CType::Int => assert_eq!(&rhs.ty, &CType::Int),
            CType::Pointer(pointee) => {
                assert!(pointee.is_sized());
                assert_eq!(&rhs.ty, &CType::Int);
            },
            _ => panic!("Expected int or pointer type, got {:?}", &lhs.ty),
        }
        lhs.ty.clone()
    };
    Minus(lhs: Expr as Box<Expr>, rhs: Expr as Box<Expr>)[Precedence::Add] : {
        match &lhs.ty {
            CType::Int => assert_eq!(&rhs.ty, &CType::Int),
            CType::Pointer(pointee) => {
                match &rhs.ty {
                    CType::Int => assert!(pointee.is_sized()),
                    CType::Pointer(pointee2) => assert_eq!(pointee, pointee2),
                    _ => panic!("Expected int or pointer type, got {:?}", &rhs.ty),
                }
            },
            _ => panic!("Expected int or pointer type, got {:?}", &lhs.ty),
        }
        lhs.ty.clone()
    };
    Multiply(lhs: Expr as Box<Expr>, rhs: Expr as Box<Expr>)[Precedence::Multiply] : {
        assert_eq!(&lhs.ty, &CType::Int);
        assert_eq!(&rhs.ty, &CType::Int);
        lhs.ty.clone()
    };
    UnaryPlus(x: Expr as Box<Expr>)[Precedence::PrefixUnary] : {
        assert_eq!(&x.ty, &CType::Int);
        x.ty.clone()
    };
    UnaryMinus(x: Expr as Box<Expr>)[Precedence::PrefixUnary] : {
        assert_eq!(&x.ty, &CType::Int);
        x.ty.clone()
    };
    // Type is left blank because it's determined by the struct
    StructAccess(x: Expr as Box<Expr>, field: StructFieldId)[Precedence::SuffixUnary] : ;
    // Type checking is left for the caller
    StructBuild(id: StructId, fields: IVec<Expr, StructFieldId>)[Precedence::Lowest] : CType::Struct(id);
}
