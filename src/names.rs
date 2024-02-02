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
    constants: IVec<hir::Const, hir::ConstId>,
    intrinsics: IVec<String, hir::IntrinsicId>,
}

impl GlobalResolver {
    fn new(intrinsics: Vec<String>) -> Self {
        let intrinsics: IVec<String, hir::IntrinsicId> = intrinsics.into_iter().collect();

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
        ast::TypeExpr::Unit => Ok(hir::TypeExpr::Unit),
        ast::TypeExpr::Ident(ident) => match &ident[..] {
            "Int" => Ok(hir::TypeExpr::Int),
            "String" => Ok(hir::TypeExpr::String),
            _ => Err(HirError::UnknownName(ident.to_string())),
        }
    }
}

struct Resolver {
    locals: ISource<hir::LocalId>,
    local_scopes: Vec<HashMap<String, hir::LocalId>>,
    globals: GlobalResolver,
}

impl Resolver {
    fn new(globals: GlobalResolver) -> Self {
        Self {
            locals: ISource::new(),
            local_scopes: Vec::new(),
            globals,
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

    fn add_local(&mut self, name: String) -> hir::LocalId {
        let id = self.locals.next();
        self.local_scopes
            .last_mut()
            .expect("No local scope")
            .insert(name, id);
        id
    }
}

pub fn ast_to_hir(intrinsics: Vec<String>, module: ast::Module) -> HirResult<hir::Module> {
    let mut globals = GlobalResolver::new(intrinsics);

    let ast_functions: IVec<ast::Function, hir::FunctionId> =
        IVec::from_iter(module.items.into_iter().map(|x| x.0));

    globals.function_ids = ast_functions
        .indexed_iter()
        .map(|(id, f)| (f.ident.0.clone(), id))
        .collect();

    let mut functions: IVec<hir::Function, hir::FunctionId> = ivec![];

    for ast::Function { body, ident, return_ty: return_type } in ast_functions {
        let mut resolver = Resolver::new(globals);

        let body = resolver.with_scope(
            |resolver|
                ast_to_hir_expression(resolver, body.0)
        )?;

        let return_type = match return_type {
            None => hir::TypeExpr::Unit,
            Some(return_type) =>
                ast_to_hir_type_expr(&mut resolver.globals, return_type.0)?,
        };

        let function = hir::Function {
            locals: resolver.locals,
            ident: ident.0,
            return_ty: return_type,
            body,
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
) -> HirResult<hir::Expression> {
    match expr {
        ast::Expression::Block(statements, last) => resolver.with_scope(|resolver| {
            let statements = statements
                .into_iter()
                .map(|expr| ast_to_hir_expression(resolver, expr.0))
                .collect::<HirResult<Vec<_>>>()?;
            let last = last
                .map(|expr| ast_to_hir_expression(resolver, expr.0))
                .transpose()?
                .unwrap_or(hir::Expression::Const(
                    resolver.globals.constants.push(hir::Const::Unit)
                ));
            Ok(hir::Expression::Block(statements, Box::new(last)))
        }),
        ast::Expression::Call { callee, args } => {
            let callee = ast_to_hir_expression(resolver, callee.0)?;
            let args = args
                .into_iter()
                .map(|expr| ast_to_hir_expression(resolver, expr.0))
                .collect::<HirResult<Vec<_>>>()?;
            Ok(hir::Expression::Method {
                object: Box::new(callee),
                name: "()".to_string(),
                args,
            })
        }
        ast::Expression::Literal(literal) => {
            let constant = match literal {
                ast::Literal::String(s) => hir::Const::String(s),
                ast::Literal::Number(n) => hir::Const::Int(n),
            };
            Ok(hir::Expression::Const(resolver.globals.constants.push(constant)))
        }
        ast::Expression::Ident(ident) => resolver.get(&ident),
        ast::Expression::Binary {
            op: Ast(ast::BinaryOp::Assign, _),
            lhs,
            rhs,
        } => match lhs.0 {
            ast::Expression::Ident(ident) => {
                let rhs = ast_to_hir_expression(resolver, rhs.0)?;
                let id = resolver.get_local(&ident)?;
                Ok(hir::Expression::Assign {
                    var: id,
                    expr: Box::new(rhs),
                })
            }
            not_ident => Err(HirError::ExpectedIdentifier(not_ident)),
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
            Ok(hir::Expression::Method {
                object: Box::new(lhs),
                name: op,
                args: vec![rhs],
            })
        }
        ast::Expression::Unary { op, expr } => {
            let expr = ast_to_hir_expression(resolver, expr.0)?;
            let op = match op.0 {
                ast::UnaryOp::Neg => "-1".to_string(),
                ast::UnaryOp::Pos => "+1".to_string(),
            };
            Ok(hir::Expression::Method {
                object: Box::new(expr),
                name: op,
                args: Vec::new(),
            })
        }
        ast::Expression::Definition { ident, expr } => {
            let expr = ast_to_hir_expression(resolver, expr.0)?;
            let id = resolver.add_local(ident.0);

            Ok(hir::Expression::Assign {
                var: id,
                expr: Box::new(expr),
            })
        }
        ast::Expression::Return(expr) => {
            let expr = ast_to_hir_expression(resolver, expr.0)?;
            Ok(hir::Expression::Return(Box::new(expr)))
        }
    }
}
