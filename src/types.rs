use crate::hir;
use crate::tir;
use crate::tir::Type;

#[derive(Debug)]
pub enum TypeError {
    MismatchedTypes(Type, Type),
    UnknownType,
    NoMethod(Type, String),
}

type TypeResult<T> = Result<T, TypeError>;

pub fn solve_types(module: hir::Module) -> TypeResult<tir::Module> {
    let constants = module.constants
        .into_iter()
        .map(|c| match c {
            hir::Constant::Unit => tir::Constant::Unit,
            hir::Constant::Int(i) => tir::Constant::Int(i),
            hir::Constant::String(s) => tir::Constant::String(s),
        })
        .collect();

    let functions: Vec<tir::Function> = module.functions
        .into_iter()
        .map(|f| solve_function(&constants, &f))
        .collect::<TypeResult<_>>()?;

    Ok(tir::Module { constants, functions })
}

#[derive(Debug, Clone)]
struct TyVar<'hir>(usize, Option<&'hir hir::Expression>);

#[derive(Debug)]
struct TyContext<'hir> {
    constants: &'hir Vec<tir::Constant>,
    known: Vec<Option<Type>>,
    _marker: std::marker::PhantomData<&'hir hir::Expression>,
}

impl<'hir> TyContext<'hir> {
    fn new(constants: &'hir Vec<tir::Constant>, function: &'hir hir::Function) -> Self {
        Self {
            constants,
            known: vec![None; function.locals],
            _marker: std::marker::PhantomData,
        }
    }

    fn local(&self, id: hir::LocalId) -> MaybeType<'hir> {
        match &self.known[id.0] {
            Some(ty) => MaybeType::Type(ty.clone()),
            None => MaybeType::TyVar(TyVar(id.0, None)),
        }
    }

    fn fresh_var(&mut self, expr: &'hir hir::Expression) -> TyVar<'hir> {
        let id = self.known.len();
        self.known.push(None);
        TyVar(id, Some(expr))
    }

    fn constant(&self, id: hir::ConstId) -> Type {
        self.constants[id.0].ty().clone()
    }

    fn global(&self, id: hir::GlobalId) -> Type {
        // TODO: This function is temporary and will be rewritten with real globals
        //       once they are implemented.
        match id {
            hir::GlobalId::Intrinsic(i) => match i {
                0 => Type::Function(id, vec![Type::String], Box::new(Type::Unit)),
                1 => Type::Function(id, vec![Type::String], Box::new(Type::Unit)),
                _ => panic!("Unknown intrinsic: {}", i),
            },
            hir::GlobalId::Function(_) => Type::Function(
                id,
                vec![],
                Box::new(Type::Unit),
            )
        }
    }

    fn set(&mut self, var: TyVar<'hir>, ty: Type) -> TypeResult<()> {
        if let Some(old) = &self.known[var.0] {
            if old != &ty {
                return Err(TypeError::MismatchedTypes(old.clone(), ty));
            }
        } else {
            self.known[var.0] = Some(ty);
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
    constants: &Vec<tir::Constant>,
    function: &hir::Function,
) -> TypeResult<tir::Function> {
    let mut ctx = TyContext::new(constants, function);
    let mut equations = Vec::new();

    let return_type = form_equations(&mut equations, &mut ctx, &function.body)?;
    equations.push(TypeEquation::Equal(
        return_type,
        MaybeType::Type(Type::Unit),
    ));

    solve_equations(&mut ctx, equations)?;

    Ok(tir::Function {
        locals: ctx.known.iter().map(|ty| ty.clone().unwrap()).collect(),
        ident: function.ident.clone(),
        body: assign_types(&ctx, &function.body),
    })
}

fn form_equations<'hir>(
    equations: &mut Vec<TypeEquation<'hir>>,
    ctx: &mut TyContext<'hir>,
    expr: &'hir hir::Expression,
) -> TypeResult<MaybeType<'hir>> {
    match expr {
        hir::Expression::Const(c) => {
            Ok(MaybeType::Type(ctx.constant(*c)))
        }
        hir::Expression::Local(l) => {
            Ok(ctx.local(*l))
        }
        hir::Expression::Global(g) => {
            Ok(MaybeType::Type(ctx.global(*g)))
        }
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
            equations.push(TypeEquation::Equal(
                ctx.local(*var),
                ty.clone(),
            ));
            Ok(ty)
        }
    }
}

fn solve_equations<'hir>(ctx: &mut TyContext<'hir>, mut equations: Vec<TypeEquation<'hir>>) -> TypeResult<()> {
    let mut updated = true;
    while updated {
        updated = false;
        let mut new_equations = vec![];
        for eq in equations {
            match &eq {
                old@TypeEquation::HasMethod {
                    ty,
                    method,
                    ret,
                    args,
                } => {
                    match ty {
                        MaybeType::Type(concrete) => {
                            let method = get_method(concrete.clone(), &method)?;
                            let (arg_tys, ret_ty) = method;
                            for (arg, arg_ty) in args.iter().zip(arg_tys) {
                                new_equations.push(TypeEquation::Equal(
                                    arg.clone(),
                                    MaybeType::Type(arg_ty),
                                ));
                            }
                            new_equations.push(TypeEquation::Equal(
                                ret.clone(),
                                MaybeType::Type(ret_ty),
                            ));
                        }
                        MaybeType::TyVar(_) => {
                            new_equations.push(old.clone());
                        }
                    }
                }
                TypeEquation::Equal(a, b) => {
                    match (a, b) {
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
                    }
                }
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
        hir::Expression::Const(c) => {
            tir::Typed {
                ty: ctx.constant(*c),
                expr: Box::new(tir::Expression::Constant((*c).into())),
            }
        }
        hir::Expression::Local(l) => {
            tir::Typed {
                ty: match ctx.local(*l) {
                    MaybeType::Type(ty) => ty,
                    MaybeType::TyVar(_) => panic!("Unsolved type variable"),
                },
                expr: Box::new(tir::Expression::Local(*l)),
            }
        }
        hir::Expression::Global(g) => {
            tir::Typed {
                ty: ctx.global(*g),
                expr: Box::new(tir::Expression::Global(*g)),
            }
        }
        hir::Expression::Block(exprs, expr) => {
            let mut typed_exprs = vec![];
            for expr in exprs {
                typed_exprs.push(assign_types(ctx, expr));
            }
            let typed_expr = assign_types(ctx, expr);
            tir::Typed {
                ty: typed_expr.ty.clone(),
                expr: Box::new(tir::Expression::Block(
                    typed_exprs,
                    typed_expr,
                )),
            }
        }
        hir::Expression::Method { object, args, name } => {
            let typed_object = assign_types(ctx, object);
            let typed_args = args
                .into_iter()
                .map(|arg| assign_types(ctx, arg))
                .collect::<Vec<_>>();

            let (_arg_tys, ret_ty) = get_method(typed_object.ty.clone(), name)
                .expect("Method type should be known");

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
    }
}

fn get_method(ty: Type, method: &str) -> TypeResult<(Vec<Type>, Type)> {
    match (&ty, method) {
        (Type::Function(_id, args, ret), "()") => Ok((args.clone(), *ret.clone())),
        (Type::Int, "+2") => Ok((vec![Type::Int], Type::Int)),
        (Type::Int, "-2") => Ok((vec![Type::Int], Type::Int)),
        (Type::Int, "*2") => Ok((vec![Type::Int], Type::Int)),
        (Type::Int, "+1") => Ok((Vec::new(), Type::Int)),
        (Type::Int, "-1") => Ok((Vec::new(), Type::Int)),
        (Type::String, "+2") => Ok((vec![Type::String], Type::String)),
        (Type::String, "*2") => Ok((vec![Type::Int], Type::String)),
        _ => Err(TypeError::NoMethod(ty, method.to_string())),
    }
}
