use crate::{hir, ivec};
use crate::c::ParamId;
use crate::ivec::{IIndex, IVec};
use crate::tir::{self, ConstId, FunctionId, IntrinsicId, LocalId, Type};

#[derive(Debug)]
pub enum TypeError {
    MismatchedTypes(Type, Type),
    UnknownType,
    NoMethod(Type, String),
}

type TypeResult<T> = Result<T, TypeError>;

fn const_ty(c: &hir::Const) -> Type {
    match c {
        hir::Const::Unit => Type::Unit,
        hir::Const::Int(_) => Type::Int,
        hir::Const::String(_) => Type::String,
    }
}

fn eval_type_expr(expr: &hir::TypeExpr) -> Type {
    match expr {
        hir::TypeExpr::Unit => Type::Unit,
        hir::TypeExpr::Int => Type::Int,
        hir::TypeExpr::String => Type::String,
    }
}

pub fn solve_types(module: hir::Module) -> TypeResult<tir::Module> {
    let function_types: IVec<Type, FunctionId> = module
        .functions
        .indexed_iter()
        .map(|(id, f)| Type::Function(
            id,
            ivec![],
            Box::new(eval_type_expr(&f.return_ty)),
        ))
        .collect();

    let functions: IVec<tir::Function, FunctionId> = module
        .functions
        .iter()
        .map(|f| solve_function(&module, &function_types, f))
        .collect::<TypeResult<_>>()?;

    Ok(tir::Module {
        constants: module.constants,
        functions,
    })
}

#[derive(Debug, Clone)]
struct TyVar<'hir>(LocalId, Option<&'hir hir::Expression>);

#[derive(Debug)]
struct TyContext<'hir> {
    module: &'hir hir::Module,
    function_types: &'hir IVec<Type, FunctionId>,
    vars: IVec<Option<Type>, LocalId>,
    return_ty: Type,
    _marker: std::marker::PhantomData<&'hir hir::Expression>,
}

impl<'hir> TyContext<'hir> {
    fn new(
        module: &'hir hir::Module,
        function_types: &'hir IVec<Type, FunctionId>,
        function: &'hir hir::Function,
    ) -> Self {
        Self {
            module,
            function_types,
            vars: function.locals.iter().map(|_| None).collect(),
            return_ty: eval_type_expr(&function.return_ty),
            _marker: std::marker::PhantomData,
        }
    }

    fn local(&self, id: LocalId) -> MaybeType<'hir> {
        match &self.vars[id] {
            Some(ty) => MaybeType::Type(ty.clone()),
            None => MaybeType::TyVar(TyVar(id, None)),
        }
    }

    fn fresh_var(&mut self, expr: &'hir hir::Expression) -> TyVar<'hir> {
        TyVar(self.vars.push(None), Some(expr))
    }

    fn constant(&self, id: ConstId) -> Type {
        const_ty(&self.module.constants[id])
    }

    fn intrinsic(&self, id: IntrinsicId) -> Type {
        // TODO: This function is temporary and will be rewritten with real globals
        //       once they are implemented.
        match id.index() {
            0 => Type::Intrinsic(id, ivec![Type::String], Box::new(Type::Unit)),
            1 => Type::Intrinsic(id, ivec![Type::String], Box::new(Type::Unit)),
            _ => panic!("Unknown intrinsic: {:?}", id),
        }
    }

    fn set(&mut self, var: TyVar<'hir>, ty: Type) -> TypeResult<()> {
        if let Some(old) = &self.vars[var.0] {
            if old != &ty {
                return Err(TypeError::MismatchedTypes(old.clone(), ty));
            }
        } else {
            self.vars[var.0] = Some(ty);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
enum MaybeType<'hir> {
    Type(Type),
    TyVar(TyVar<'hir>),
}

#[derive(Debug, Clone)]
enum TypeEquation<'hir> {
    HasMethod {
        ty: MaybeType<'hir>,
        method: String,
        args: Vec<MaybeType<'hir>>,
        ret: MaybeType<'hir>,
    },
    Equal(MaybeType<'hir>, MaybeType<'hir>),
}

fn solve_function(
    module: &hir::Module,
    function_types: &IVec<Type, FunctionId>,
    function: &hir::Function,
) -> TypeResult<tir::Function> {
    let mut ctx = TyContext::new(module, function_types, function);
    let mut equations = Vec::new();

    let return_type = form_equations(&mut equations, &mut ctx, &function.body)?;
    enforce_type(&mut equations, return_type, ctx.return_ty.clone());

    solve_equations(&mut ctx, equations)?;

    Ok(tir::Function {
        locals: ctx.vars.iter().map(|ty| ty.clone().unwrap()).collect(),
        ident: function.ident.clone(),
        body: assign_types(&ctx, &function.body),
    })
}

fn enforce_type<'hir>(
    equations: &mut Vec<TypeEquation<'hir>>,
    ty: MaybeType<'hir>,
    expected: Type,
) {
    equations.push(TypeEquation::Equal(ty.clone(), MaybeType::Type(expected)));
}

fn form_equations<'hir>(
    equations: &mut Vec<TypeEquation<'hir>>,
    ctx: &mut TyContext<'hir>,
    expr: &'hir hir::Expression,
) -> TypeResult<MaybeType<'hir>> {
    match expr {
        hir::Expression::Const(c) => Ok(MaybeType::Type(ctx.constant(*c))),
        hir::Expression::Local(l) => Ok(ctx.local(*l)),
        hir::Expression::Function(f) => Ok(MaybeType::Type(ctx.function_types[*f].clone())),
        hir::Expression::Intrinsic(f) => Ok(MaybeType::Type(ctx.intrinsic(*f))),
        hir::Expression::Block(exprs, expr) => {
            for expr in exprs {
                form_equations(equations, ctx, expr)?;
            }
            form_equations(equations, ctx, expr)
        }
        hir::Expression::Method { object, args, name } => {
            let object = form_equations(equations, ctx, object)?;
            match object {
                MaybeType::Type(ty) => {
                    let (arg_tys, ret_ty) = get_method(ty, name)?;
                    for (arg, arg_ty) in args.iter().zip(arg_tys) {
                        let arg = form_equations(equations, ctx, arg)?;
                        equations.push(TypeEquation::Equal(arg, MaybeType::Type(arg_ty)));
                    }
                    Ok(MaybeType::Type(ret_ty))
                }
                MaybeType::TyVar(ty) => {
                    let args = args
                        .iter()
                        .map(|arg| form_equations(equations, ctx, arg))
                        .collect::<TypeResult<Vec<_>>>()?;
                    let ret = MaybeType::TyVar(ctx.fresh_var(expr));
                    equations.push(TypeEquation::HasMethod {
                        ty: MaybeType::TyVar(ty),
                        method: name.clone(),
                        args,
                        ret: ret.clone(),
                    });
                    Ok(ret)
                }
            }
        }
        hir::Expression::Assign { var, expr } => {
            let ty = form_equations(equations, ctx, expr)?;
            equations.push(TypeEquation::Equal(ctx.local(*var), ty.clone()));
            Ok(ty)
        }
        hir::Expression::Return(expr) => {
            let ty = form_equations(equations, ctx, expr)?;
            enforce_type(equations, ty, ctx.return_ty.clone());
            Ok(MaybeType::Type(Type::Unit))
        }
    }
}

fn solve_equations<'hir>(
    ctx: &mut TyContext<'hir>,
    mut equations: Vec<TypeEquation<'hir>>,
) -> TypeResult<()> {
    let mut updated = true;
    while updated {
        updated = false;
        let mut new_equations = vec![];
        for eq in equations {
            match &eq {
                old @ TypeEquation::HasMethod {
                    ty,
                    method,
                    ret,
                    args,
                } => match ty {
                    MaybeType::Type(concrete) => {
                        let method = get_method(concrete.clone(), &method)?;
                        let (arg_tys, ret_ty) = method;
                        for (arg, arg_ty) in args.iter().zip(arg_tys) {
                            new_equations
                                .push(TypeEquation::Equal(arg.clone(), MaybeType::Type(arg_ty)));
                        }
                        new_equations
                            .push(TypeEquation::Equal(ret.clone(), MaybeType::Type(ret_ty)));
                    }
                    MaybeType::TyVar(_) => {
                        new_equations.push(old.clone());
                    }
                },
                TypeEquation::Equal(a, b) => match (a, b) {
                    (MaybeType::Type(a), MaybeType::Type(b)) => {
                        if a != b {
                            return Err(TypeError::MismatchedTypes(a.clone(), b.clone()));
                        }
                    }
                    (MaybeType::Type(a), MaybeType::TyVar(b)) => {
                        ctx.set(b.clone(), a.clone())?;
                        updated = true;
                    }
                    (MaybeType::TyVar(a), MaybeType::Type(b)) => {
                        ctx.set(a.clone(), b.clone())?;
                        updated = true;
                    }
                    (MaybeType::TyVar(a), MaybeType::TyVar(b)) => {
                        new_equations.push(TypeEquation::Equal(
                            MaybeType::TyVar(a.clone()),
                            MaybeType::TyVar(b.clone()),
                        ));
                    }
                },
            }
        }

        equations = new_equations;
    }

    if equations.len() > 0 {
        return Err(TypeError::UnknownType);
    }

    Ok(())
}

fn assign_types(ctx: &TyContext, expression: &hir::Expression) -> tir::Typed {
    match expression {
        hir::Expression::Const(c) => tir::Typed {
            ty: ctx.constant(*c),
            expr: Box::new(tir::Expression::Constant((*c).into())),
        },
        hir::Expression::Local(l) => tir::Typed {
            ty: match ctx.local(*l) {
                MaybeType::Type(ty) => ty,
                MaybeType::TyVar(_) => panic!("Unsolved type variable"),
            },
            expr: Box::new(tir::Expression::Local(*l)),
        },
        hir::Expression::Function(f) => tir::Typed {
            ty: ctx.function_types[*f].clone(),
            expr: Box::new(tir::Expression::Function(*f)),
        },
        hir::Expression::Intrinsic(f) => tir::Typed {
            ty: ctx.intrinsic(*f),
            expr: Box::new(tir::Expression::Intrinsic(*f)),
        },
        hir::Expression::Block(exprs, expr) => {
            let mut typed_exprs = vec![];
            for expr in exprs {
                typed_exprs.push(assign_types(ctx, expr));
            }
            let typed_expr = assign_types(ctx, expr);
            tir::Typed {
                ty: typed_expr.ty.clone(),
                expr: Box::new(tir::Expression::Block(typed_exprs, typed_expr)),
            }
        }
        hir::Expression::Method { object, args, name } => {
            let typed_object = assign_types(ctx, object);
            let typed_args = args
                .into_iter()
                .map(|arg| assign_types(ctx, arg))
                .collect::<Vec<_>>();

            let (_arg_tys, ret_ty) =
                get_method(typed_object.ty.clone(), name).expect("Method type should be known");

            tir::Typed {
                ty: ret_ty,
                expr: Box::new(tir::Expression::Method {
                    object: typed_object,
                    name: name.clone(),
                    args: typed_args,
                }),
            }
        }
        hir::Expression::Assign { var, expr } => {
            let typed_expr = assign_types(ctx, expr);
            tir::Typed {
                ty: typed_expr.ty.clone(),
                expr: Box::new(tir::Expression::Assign {
                    var: *var,
                    expr: typed_expr,
                }),
            }
        }
        hir::Expression::Return(expr) => {
            let typed_expr = assign_types(ctx, expr);
            tir::Typed {
                ty: typed_expr.ty.clone(),
                expr: Box::new(tir::Expression::Return {
                    value: typed_expr,
                }),
            }
        }
    }
}

fn get_method(ty: Type, method: &str) -> TypeResult<(IVec<Type, ParamId>, Type)> {
    match (&ty, method) {
        (Type::Function(_id, args, ret), "()") => Ok((args.clone(), *ret.clone())),
        (Type::Intrinsic(_id, args, ret), "()") => Ok((args.clone(), *ret.clone())),
        (Type::Int, "+2") => Ok((ivec![Type::Int], Type::Int)),
        (Type::Int, "-2") => Ok((ivec![Type::Int], Type::Int)),
        (Type::Int, "*2") => Ok((ivec![Type::Int], Type::Int)),
        (Type::Int, "+1") => Ok((ivec![], Type::Int)),
        (Type::Int, "-1") => Ok((ivec![], Type::Int)),
        (Type::String, "+2") => Ok((ivec![Type::String], Type::String)),
        (Type::String, "*2") => Ok((ivec![Type::Int], Type::String)),
        _ => Err(TypeError::NoMethod(ty, method.to_string())),
    }
}