use crate::{ast, ivec};
use crate::ast::Ast;
use crate::hir;
use crate::ivec::{ISource, IVec};
use std::collections::HashMap;

pub enum HirError {
    UnknownName(String),
    UnknownTypeName(String),
    ExpectedIdentifier(ast::Expression),
}

type HirResult<T> = Result<T, HirError>;

struct GlobalResolver {
    function_ids: HashMap<String, hir::FunctionId>,
    intrinsic_ids: HashMap<String, hir::IntrinsicId>,
    constants: IVec<hir::ConstId, hir::Const>,
    intrinsics: IVec<hir::IntrinsicId, String>,
}

impl GlobalResolver {
    fn new(intrinsics: Vec<String>) -> Self {
        let intrinsics: IVec<hir::IntrinsicId, String> = intrinsics.into_iter().collect();

        let intrinsic_ids = intrinsics
            .indexed_iter()
            .map(|(index, name)| (name.clone(), index))
            .collect();

        Self {
            function_ids: HashMap::new(),
            intrinsic_ids,
            constants: IVec::new(),
            intrinsics,
        }
    }

    fn find_function(&self, name: &str) -> Option<hir::FunctionId> {
        self.function_ids.get(name).copied()
    }

    fn find_intrinsic(&self, name: &str) -> Option<hir::IntrinsicId> {
        self.intrinsic_ids.get(name).copied()
    }
}

fn ast_to_hir_type_expr(
    _resolver: &mut GlobalResolver,
    expr: ast::TypeExpr,
) -> HirResult<hir::TypeExpr> {
    match expr {
        ast::TypeExpr::Pointer(inner) => Ok(
            hir::TypeExpr::Pointer(Box::new(ast_to_hir_type_expr(_resolver, inner.0)?)
        )),
        ast::TypeExpr::Unit => Ok(hir::TypeExpr::Unit),
        ast::TypeExpr::Ident(ident) => match &ident[..] {
            "Int" => Ok(hir::TypeExpr::Int),
            "String" => Ok(hir::TypeExpr::String),
            _ => Err(HirError::UnknownName(ident.to_string())),
        }
    }
}

struct Resolver {
    locals: IVec<hir::LocalId, Option<hir::TypeExpr>>,
    local_scopes: Vec<HashMap<String, hir::LocalId>>,
    globals: GlobalResolver,
    ids: ISource<hir::ExprId>,
}

impl Resolver {
    fn new(globals: GlobalResolver) -> Self {
        Self {
            locals: IVec::new(),
            local_scopes: vec![HashMap::new()],
            globals,
            ids: ISource::new(),
        }
    }

    fn find_local(&mut self, name: &str) -> Option<hir::LocalId> {
        self.local_scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get(name).copied())
    }

    fn get(&mut self, name: &str) -> HirResult<hir::Expression> {
        if let Some(id) = self.find_local(name) {
            Ok(hir::Expression::Local(id))
        } else if let Some(id) = self.globals.find_function(name) {
            Ok(hir::Expression::Function(id))
        } else if let Some(id) = self.globals.find_intrinsic(name) {
            Ok(hir::Expression::Intrinsic(id))
        } else {
            Err(HirError::UnknownName(name.to_string()))
        }
    }

    fn get_local(&mut self, name: &str) -> HirResult<hir::LocalId> {
        if let Some(id) = self.find_local(name) {
            Ok(id)
        } else {
            Err(HirError::UnknownName(name.to_string()))
        }
    }

    fn with_scope<F, T>(&mut self, f: F) -> HirResult<T>
    where
        F: FnOnce(&mut Self) -> HirResult<T>,
    {
        self.local_scopes.push(HashMap::new());
        let res = f(self)?;
        self.local_scopes.pop();
        Ok(res)
    }

    fn add_local(&mut self, name: String, ty: Option<hir::TypeExpr>) -> hir::LocalId {
        let id = self.locals.push(ty);
        self.local_scopes
            .last_mut()
            .expect("No local scope")
            .insert(name, id);
        id
    }
    
    fn with_id(&mut self, expr: hir::Expression) -> hir::Expr {
        let id = self.ids.next();
        hir::Expr { id, expr }
    }
}

pub fn ast_to_hir(intrinsics: Vec<String>, module: ast::Module) -> HirResult<hir::Module> {
    let mut globals = GlobalResolver::new(intrinsics);

    let ast_functions: IVec<hir::FunctionId, ast::Function> =
        IVec::from_iter(module.items.into_iter().map(|x| x.0));

    globals.function_ids = ast_functions
        .indexed_iter()
        .map(|(id, f)| (f.ident.0.clone(), id))
        .collect();

    let mut functions: IVec<hir::FunctionId, hir::Function> = ivec![];

    for ast::Function {
        body,
        ident,
        return_ty,
        args,
    } in ast_functions {
        let mut resolver = Resolver::new(globals);

        let args = args
            .into_iter()
            .map(|arg| {
                let ty = ast_to_hir_type_expr(&mut resolver.globals, arg.0.ty.0)?;
                Ok(resolver.add_local(arg.0.ident.0, Some(ty)))
            })
            .collect::<HirResult<_>>()?;

        let body = resolver.with_scope(
            |resolver|
                ast_to_hir_expression(resolver, body.0)
        )?;

        let return_type = match return_ty {
            None => hir::TypeExpr::Unit,
            Some(return_type) =>
                ast_to_hir_type_expr(&mut resolver.globals, return_type.0)?,
        };

        let function = hir::Function {
            locals: resolver.locals,
            ident: ident.0,
            args,
            return_ty: return_type,
            body,
            expr_ids: resolver.ids,
        };

        globals = resolver.globals;
        functions.push(function);
    }

    let constants = globals.constants;

    Ok(hir::Module {
        constants,
        functions,
    })
}

fn ast_to_hir_expression(
    resolver: &mut Resolver,
    expr: ast::Expression,
) -> HirResult<hir::Expr> {
    let expr: hir::Expression = match expr {
        ast::Expression::Block(statements, last) => resolver.with_scope(|resolver| {
            let statements = statements
                .into_iter()
                .map(|expr| ast_to_hir_expression(resolver, expr.0))
                .collect::<HirResult<Vec<_>>>()?;
            let unit = resolver.globals.constants.push(hir::Const::Unit);
            let last = last
                .map(|expr| ast_to_hir_expression(resolver, expr.0))
                .transpose()?
                .unwrap_or(resolver.with_id(hir::Expression::Const(unit)));
            Ok(hir::Expression::Block(statements, Box::new(last)))
        })?,
        ast::Expression::Call { callee, args } => {
            let callee = ast_to_hir_expression(resolver, callee.0)?;
            let args = args
                .into_iter()
                .map(|expr| ast_to_hir_expression(resolver, expr.0))
                .collect::<HirResult<Vec<_>>>()?;
            hir::Expression::Method {
                object: Box::new(callee),
                name: "()".to_string(),
                args,
            }
        }
        ast::Expression::Literal(literal) => {
            let constant = match literal {
                ast::Literal::Unit => hir::Const::Unit,
                ast::Literal::Uninit => return Ok(resolver.with_id(hir::Expression::Uninit)),
                ast::Literal::String(s) => hir::Const::String(s),
                ast::Literal::Number(n) => hir::Const::Int(n),
            };
            hir::Expression::Const(resolver.globals.constants.push(constant))
        }
        ast::Expression::Ident(ident) => resolver.get(&ident)?,
        ast::Expression::Binary {
            op: Ast(ast::BinaryOp::Assign, _),
            lhs,
            rhs,
        } => match lhs.0 {
            ast::Expression::Ident(ident) => {
                let rhs = ast_to_hir_expression(resolver, rhs.0)?;
                let id = resolver.get_local(&ident)?;
                hir::Expression::Assign {
                    var: id,
                    expr: Box::new(rhs),
                }
            }
            not_ident => return Err(HirError::ExpectedIdentifier(not_ident)),
        },
        ast::Expression::Binary { op, lhs, rhs } => {
            let lhs = ast_to_hir_expression(resolver, lhs.0)?;
            let rhs = ast_to_hir_expression(resolver, rhs.0)?;
            let op = match op.0 {
                ast::BinaryOp::Add => "+2".to_string(),
                ast::BinaryOp::Sub => "-2".to_string(),
                ast::BinaryOp::Mul => "*2".to_string(),
                ast::BinaryOp::Assign => unreachable!(),
            };
            hir::Expression::Method {
                object: Box::new(lhs),
                name: op,
                args: vec![rhs],
            }
        }
        ast::Expression::Unary { op, expr } => {
            let expr = ast_to_hir_expression(resolver, expr.0)?;
            let op = match op.0 {
                ast::UnaryOp::Neg => "-1".to_string(),
                ast::UnaryOp::Pos => "+1".to_string(),
            };
            hir::Expression::Method {
                object: Box::new(expr),
                name: op,
                args: Vec::new(),
            }
        }
        ast::Expression::Definition { ident, ty, expr } => {
            let expr = ast_to_hir_expression(resolver, expr.0)?;
            let ty = ty
                .map(|ty| ast_to_hir_type_expr(&mut resolver.globals, ty.0))
                .transpose()?;
            let id = resolver.add_local(ident.0, ty);

            hir::Expression::Assign {
                var: id,
                expr: Box::new(expr),
            }
        }
        ast::Expression::Return(expr) => {
            let expr = ast_to_hir_expression(resolver, expr.0)?;
            hir::Expression::Return(Box::new(expr))
        }
        ast::Expression::ReturnUnit => {
            let unit = resolver.globals.constants.push(hir::Const::Unit);
            hir::Expression::Return(Box::new(resolver.with_id(hir::Expression::Const(unit))))
        },
    };
    
    Ok(resolver.with_id(expr))
}
