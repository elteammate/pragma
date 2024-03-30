use crate::c::ParamId;
use crate::hir::ExprId;
use crate::ivec::{IIndex, IVec};
use crate::tir::{self, FunctionId, IntrinsicId, LocalId, Type};
use crate::{hir, ivec};

// TODO: the amount of cloning here is inadequate

#[derive(Debug)]
pub enum TypeError {
    MismatchedTypes,
    UnknownType,
    NoMethod,
    WrongNumberOfArguments,
    RecursiveType,
}

type TypeResult<T> = Result<T, TypeError>;

pub fn solve_types(module: hir::Module) -> TypeResult<tir::Module> {
    let function_types: IVec<FunctionId, Type> = module
        .functions
        .indexed_iter()
        .map(|(id, f)| {
            let arg_tys = 
                f.args
                    .iter()
                    .map(|id| eval_type_expr(&f.locals[*id].clone().expect("type parameters must have type annotations")))
                    .collect();
            Type::Function(id, arg_tys, Box::new(eval_type_expr(&f.return_ty)))
        })
        .collect();

    let functions: IVec<FunctionId, tir::Function> = module
        .functions
        .iter()
        .map(|f| solve_function(&module, &function_types, f))
        .collect::<TypeResult<_>>()?;

    Ok(tir::Module {
        constants: module.constants,
        functions,
    })
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum TyVar {
    Expr(ExprId),
    Local(LocalId),
}

#[derive(Debug, Clone)]
enum UnsolvedType {
    Unit,
    Int,
    String,
    Var(TyVar),
    Function(FunctionId, IVec<ParamId, UnsolvedType>, Box<UnsolvedType>),
    Intrinsic(IntrinsicId, IVec<ParamId, UnsolvedType>, Box<UnsolvedType>),
    Never,
}

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

impl From<Type> for UnsolvedType {
    fn from(value: Type) -> Self {
        match value {
            Type::Unit => UnsolvedType::Unit,
            Type::Int => UnsolvedType::Int,
            Type::String => UnsolvedType::String,
            Type::Function(id, args, ret) => UnsolvedType::Function(
                id,
                args.into_iter().map(|ty| ty.into()).collect(),
                Box::new((*ret).into()),
            ),
            Type::Intrinsic(id, args, ret) => UnsolvedType::Intrinsic(
                id,
                args.into_iter().map(|ty| ty.into()).collect(),
                Box::new((*ret).into()),
            ),
            Type::Never => UnsolvedType::Never,
        }
    }
}

impl TryFrom<UnsolvedType> for Type {
    type Error = ();

    fn try_from(value: UnsolvedType) -> Result<Self, Self::Error> {
        match value {
            UnsolvedType::Unit => Ok(Type::Unit),
            UnsolvedType::Int => Ok(Type::Int),
            UnsolvedType::String => Ok(Type::String),
            UnsolvedType::Var(_) => Err(()),
            UnsolvedType::Function(id, args, ret) => Ok(Type::Function(
                id,
                args.into_iter().map(|ty| ty.try_into().unwrap()).collect(),
                Box::new((*ret).try_into().unwrap()),
            )),
            UnsolvedType::Intrinsic(id, args, ret) => Ok(Type::Intrinsic(
                id,
                args.into_iter().map(|ty| ty.try_into().unwrap()).collect(),
                Box::new((*ret).try_into().unwrap()),
            )),
            UnsolvedType::Never => Ok(Type::Never),
        }
    }
}

#[derive(Debug, Clone)]
struct MethodConstraint {
    ty: TyVar,
    name: String,
    args: Vec<UnsolvedType>,
    ret: UnsolvedType,
}

#[derive(Debug)]
struct ControlFlow {
    reachable: bool,
}

impl ControlFlow {
    fn merge_with(&mut self, other: ControlFlow) {
        self.reachable &= other.reachable;
    }
}

#[derive(Debug)]
struct TyContext<'hir> {
    module: &'hir hir::Module,
    function_types: &'hir IVec<FunctionId, Type>,
    local_bounds: IVec<LocalId, Option<UnsolvedType>>,
    expr_bounds: IVec<ExprId, Option<UnsolvedType>>,
    method_bounds: Vec<MethodConstraint>,
    reachability: IVec<ExprId, bool>,
    return_ty: Type,
    occurrence_stack: Vec<TyVar>,
}

impl<'hir> TyContext<'hir> {
    fn new(
        module: &'hir hir::Module,
        function_types: &'hir IVec<FunctionId, Type>,
        function: &'hir hir::Function,
    ) -> Self {
        Self {
            module,
            function_types,
            local_bounds: function
                .locals
                .iter()
                .map(|_| None)
                .collect(),
            expr_bounds: function
                .expr_ids
                .iter()
                .map(|_| None)
                .collect(),
            return_ty: eval_type_expr(&function.return_ty),
            occurrence_stack: vec![],
            method_bounds: vec![],
            reachability: function.expr_ids.iter().map(|_| true).collect(),
        }
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
    
    fn get_bound(&self, alpha: TyVar) -> &Option<UnsolvedType> {
        match alpha {
            TyVar::Expr(id) => &self.expr_bounds[id],
            TyVar::Local(id) => &self.local_bounds[id],
        }
    }
    
    fn unify_impl(&mut self, sigma: UnsolvedType, tau: UnsolvedType) -> TypeResult<UnsolvedType> {
        Ok(match (sigma, tau) {
            (UnsolvedType::Never, UnsolvedType::Never) => UnsolvedType::Never,
            (UnsolvedType::Unit, UnsolvedType::Unit) => UnsolvedType::Unit,
            (UnsolvedType::Int, UnsolvedType::Int) => UnsolvedType::Int,
            (UnsolvedType::String, UnsolvedType::String) => UnsolvedType::String,
            (UnsolvedType::Var(a), UnsolvedType::Var(b)) if a == b => UnsolvedType::Var(a),
            (UnsolvedType::Var(a), UnsolvedType::Var(b)) => {
                self.assign_impl(a, UnsolvedType::Var(b))?;
                UnsolvedType::Var(b)
            },
            (UnsolvedType::Var(a), b) => {
                self.assign_impl(a, b.clone())?;
                b
            },
            (a, UnsolvedType::Var(b)) => {
                self.assign_impl(b, a.clone())?;
                a
            },
            (UnsolvedType::Function(id1, args1, ret1), UnsolvedType::Function(id2, args2, ret2)) => {
                if id1 != id2 {
                    return Ok(UnsolvedType::Never);
                }
                let args = args1
                    .into_iter()
                    .zip(args2)
                    .map(|(a, b)| self.unify_impl(a, b))
                    .collect::<TypeResult<_>>()?;
                let ret = self.unify_impl(*ret1, *ret2)?;
                UnsolvedType::Function(id1, args, Box::new(ret))
            },
            (UnsolvedType::Intrinsic(id1, args1, ret1), UnsolvedType::Intrinsic(id2, args2, ret2)) => {
                if id1 != id2 {
                    return Ok(UnsolvedType::Never);
                }
                let args = args1
                    .into_iter()
                    .zip(args2)
                    .map(|(a, b)| self.unify_impl(a, b))
                    .collect::<TypeResult<_>>()?;
                let ret = self.unify_impl(*ret1, *ret2)?;
                UnsolvedType::Intrinsic(id1, args, Box::new(ret))
            },
            _ => return Err(TypeError::MismatchedTypes),
        })
    }
    
    fn assign_impl(&mut self, alpha: TyVar, sigma: UnsolvedType) -> TypeResult<()> {
        if self.occurrence_stack.contains(&alpha) {
            return Err(TypeError::RecursiveType);
        }
        self.occurrence_stack.push(alpha);
        let ty = match alpha {
            TyVar::Expr(id) => self.expr_bounds[id].clone(),
            TyVar::Local(id) => self.local_bounds[id].clone(),
        };
        
        let ty = match ty {
            Some(ty) => self.unify_impl(ty, sigma)?,
            None => sigma,
        };
        
        match alpha {
            TyVar::Expr(id) => self.expr_bounds[id] = Some(ty),
            TyVar::Local(id) => self.local_bounds[id] = Some(ty),
        }
        
        self.occurrence_stack.pop();
        Ok(())
    }

    fn unify(&mut self, sigma: UnsolvedType, tau: UnsolvedType) -> TypeResult<UnsolvedType> {
        let res = self.unify_impl(sigma, tau);
        self.occurrence_stack.clear();
        res
    }
    
    fn assign(&mut self, alpha: TyVar, sigma: UnsolvedType) -> TypeResult<()> {
        let res = self.assign_impl(alpha, sigma);
        self.occurrence_stack.clear();
        res
    }
    
    // TODO: fix clippy: parameter is only used in recursion
    // TODO: fix subsequent clones occurring in solve_equations
    fn substitute(&mut self, alpha: TyVar, sigma: &UnsolvedType, tau: &mut UnsolvedType) -> TypeResult<()> {
        match tau {
            UnsolvedType::Var(beta) if *beta == alpha => {
                *tau = sigma.clone();
                Ok(())
            },
            UnsolvedType::Function(_id, args, ret) => {
                for arg in args {
                    self.substitute(alpha, sigma, arg)?;
                }
                self.substitute(alpha, sigma, ret)
            },
            UnsolvedType::Intrinsic(_id, args, ret) => {
                for arg in args {
                    self.substitute(alpha, sigma, arg)?;
                }
                self.substitute(alpha, sigma, ret)
            },
            _ => Ok(()),
        }
    }
}

fn solve_function(
    module: &hir::Module,
    function_types: &IVec<FunctionId, Type>,
    function: &hir::Function,
) -> TypeResult<tir::Function> {
    let mut ctx = TyContext::new(module, function_types, function);
    
    for (local, ty) in function.locals.indexed_iter() {
        let Some(ty) = ty else { continue };
        ctx.assign(TyVar::Local(local), eval_type_expr(ty).into())?;
    }

    let (ret_ty, flow) = form_equations(&mut ctx, &function.body)?;

    if flow.reachable {
        ctx.assign(ret_ty, ctx.return_ty.clone().into())?;
    }

    solve_equations(&mut ctx)?;

    Ok(tir::Function {
        ident: function.ident.clone(),
        locals: ctx
            .local_bounds
            .iter()
            .map(|ty| match ty {
                None => Err(TypeError::UnknownType),
                Some(UnsolvedType::Var(_)) => panic!("Unsolved type variable"),
                Some(ty) => Ok(ty.clone().try_into().unwrap()),
            })
            .collect::<TypeResult<IVec<LocalId, Type>>>()?,
        args: function.args.clone(),
        body: assign_types(&ctx, &function.body),
        return_ty: ctx.return_ty.clone(),
    })
}

fn form_equations<'hir>(
    ctx: &mut TyContext<'hir>,
    expr: &'hir hir::Expr,
) -> TypeResult<(TyVar, ControlFlow)> {
    let ty_var = TyVar::Expr(expr.id);
    let mut flow = ControlFlow { reachable: true };

    let ty = match &expr.expr {
        hir::Expression::Uninit => return Ok((ty_var, flow)),
        hir::Expression::Const(c) => const_ty(&ctx.module.constants[*c]).into(),
        hir::Expression::Local(l) => UnsolvedType::Var(TyVar::Local(*l)),
        hir::Expression::Function(f) => ctx.function_types[*f].clone().into(),
        hir::Expression::Intrinsic(f) => ctx.intrinsic(*f).into(),
        hir::Expression::Block(stmts, expr) => {
            for stmt in stmts {
                if !flow.reachable { ctx.reachability[stmt.id] = false; }
                let (_stmt_ty, stmt_flow) = form_equations(ctx, stmt)?;
                flow.merge_with(stmt_flow);
            }
            if !flow.reachable { ctx.reachability[expr.id] = false; }
            let (expr_ty, expr_flow) = form_equations(ctx, expr)?;
            flow.merge_with(expr_flow);
            UnsolvedType::Var(expr_ty)
        }
        hir::Expression::Method { object, args, name } => {
            let (object, obj_flow) = form_equations(ctx, object)?;
            flow.merge_with(obj_flow);
            
            let mut arg_tys = vec![];
            for arg in args {
                if !flow.reachable { ctx.reachability[expr.id] = false; }
                let (arg, arg_flow) = form_equations(ctx, arg)?;
                flow.merge_with(arg_flow);
                arg_tys.push(UnsolvedType::Var(arg));
            }
            
            ctx.method_bounds.push(MethodConstraint {
                ty: object,
                name: name.clone(),
                args: arg_tys,
                ret: UnsolvedType::Var(TyVar::Expr(expr.id)),
            });
            
            return Ok((ty_var, flow));
        }
        hir::Expression::Assign { var, expr } => {
            let (ty, expr_flow) = form_equations(ctx, expr)?;
            if expr_flow.reachable {
                ctx.assign(TyVar::Local(*var), UnsolvedType::Var(ty))?;
            }
            UnsolvedType::Unit
        }
        hir::Expression::Return(expr) => {
            let (ty, _flow) = form_equations(ctx, expr)?;
            ctx.assign(ty, ctx.return_ty.clone().into())?;
            flow.reachable = false;
            UnsolvedType::Never
        }
    };
    
    ctx.assign(ty_var, ty)?;
    Ok((ty_var, flow))
}

fn solve_equations(
    ctx: &mut TyContext,
) -> TypeResult<()> {
    let mut updated = true;
    // TODO: error recovery possibilities SUCK here (!)
    //       please rewrite without this pushing every exit hackery
    
    while updated {
        updated = false;

        let method_bounds = std::mem::take(&mut ctx.method_bounds);
        for method in method_bounds {
            let MethodConstraint {
                ty: method_ty,
                name: method_name,
                args: method_args,
                ret: method_ret,
            } = &method;
            
            let Some(bound) = ctx.get_bound(*method_ty) else {
                ctx.method_bounds.push(method);
                continue;
            };

            let maybe_method = get_method(bound, method_name)?;
            if maybe_method.is_none() {
                ctx.method_bounds.push(method);
                continue;
            }
            
            let (args, ret) = maybe_method.unwrap();
            
            if args.len() != method_args.len() {
                ctx.method_bounds.push(method);
                return Err(TypeError::WrongNumberOfArguments);
            }
            
            for (arg, method_arg) in method_args.iter().cloned().zip(args) {
                ctx.unify(arg, method_arg)?;
            }
            
            ctx.unify(method_ret.clone(), ret)?;
            
            updated = true;
        }
        
        let mut substituted = vec![];
        
        let local_ids = ctx.local_bounds.indices();
        let expr_ids = ctx.expr_bounds.indices();
        
        for ty_var in 
            local_ids.iter().map(TyVar::Local)
                .chain(expr_ids.iter().map(TyVar::Expr))
        {
            let Some(bound) = ctx.get_bound(ty_var).clone() else {
                continue;
            };
            
            substituted.push(ty_var);
            
            let mut methods_after_substitution = vec![];
            
            for mut method in ctx.method_bounds.clone() {
                ctx.substitute(ty_var, &bound, &mut method.ret)?;
                for arg in &mut method.args {
                    ctx.substitute(ty_var, &bound, arg)?;
                }
                methods_after_substitution.push(method);
            }
            
            ctx.method_bounds = methods_after_substitution;
            
            let mut locals_after_substitution = ivec![];
            for mut local in ctx.local_bounds.clone() {
                if let Some(local) = &mut local {
                    ctx.substitute(ty_var, &bound, local)?;
                }
                locals_after_substitution.push(local.clone());
            }
            
            ctx.local_bounds = locals_after_substitution;
            
            let mut exprs_after_substitution = ivec![];
            for mut expr in ctx.expr_bounds.clone() {
                if let Some(expr) = &mut expr {
                    ctx.substitute(ty_var, &bound, expr)?;
                }
                exprs_after_substitution.push(expr.clone());
            }
            
            ctx.expr_bounds = exprs_after_substitution;
        }
    }

    Ok(())
}

fn assign_types(ctx: &TyContext, expr: &hir::Expr) -> tir::Typed {
    if !ctx.reachability[expr.id] {
        return tir::Typed {
            ty: Type::Never,
            expr: Box::new(tir::Expression::Trap),
        };
    }
    
    let ty: Type = match ctx.expr_bounds[expr.id].clone() {
        None => panic!("Unknown type"),
        Some(ty) => ty.try_into().unwrap(),
    };
    
    let assign = |expr: tir::Expression| {
        tir::Typed {
            ty: ty.clone(),
            expr: Box::new(expr),
        }
    };
    
    match &expr.expr {
        hir::Expression::Uninit => assign(tir::Expression::Uninit),
        hir::Expression::Const(c) => assign(tir::Expression::Constant(*c)),
        hir::Expression::Local(l) => assign(tir::Expression::Local(*l)),
        hir::Expression::Function(f) => assign(tir::Expression::Function(*f)),
        hir::Expression::Intrinsic(f) => assign(tir::Expression::Intrinsic(*f)),
        hir::Expression::Block(exprs, expr) => {
            let mut typed_exprs = vec![];
            for expr in exprs {
                typed_exprs.push(assign_types(ctx, expr));
            }
            let typed_expr = assign_types(ctx, expr);
            assign(tir::Expression::Block(typed_exprs, typed_expr))
        }
        hir::Expression::Method { object, args, name } => {
            assign(tir::Expression::Method {
                object: assign_types(ctx, object),
                name: name.clone(),
                args: args.iter().map(|arg| assign_types(ctx, arg)).collect(),
            })
        }
        hir::Expression::Assign { var, expr } => {
            assign(tir::Expression::Assign {
                var: *var,
                expr: assign_types(ctx, expr),
            })
        }
        hir::Expression::Return(expr) => {
            assign(tir::Expression::Return { value: assign_types(ctx, expr) })
        }
    }
}

// TODO: return normal Type instead of UnsolvedType
fn get_method(ty: &UnsolvedType, method: &str) -> TypeResult<Option<(IVec<ParamId, UnsolvedType>, UnsolvedType)>> {
    let method = match (&ty, method) {
        (UnsolvedType::Function(_id, args, ret), "()") => Some((args.clone(), *ret.clone())),
        (UnsolvedType::Intrinsic(_id, args, ret), "()") => Some((args.clone(), *ret.clone())),
        (UnsolvedType::Int, "+2") => Some((ivec![UnsolvedType::Int], UnsolvedType::Int)),
        (UnsolvedType::Int, "-2") => Some((ivec![UnsolvedType::Int], UnsolvedType::Int)),
        (UnsolvedType::Int, "*2") => Some((ivec![UnsolvedType::Int], UnsolvedType::Int)),
        (UnsolvedType::Int, "+1") => Some((ivec![], UnsolvedType::Int)),
        (UnsolvedType::Int, "-1") => Some((ivec![], UnsolvedType::Int)),
        (UnsolvedType::String, "+2") => Some((ivec![UnsolvedType::String], UnsolvedType::String)),
        (UnsolvedType::String, "*2") => Some((ivec![UnsolvedType::Int], UnsolvedType::String)),
        (UnsolvedType::Var(_), _) => None,
        _ => return Err(TypeError::NoMethod),
    };
    
    Ok(method)
}
