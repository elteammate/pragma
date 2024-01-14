use std::collections::HashMap;
use crate::ast;
use crate::hir;

pub enum HirError {
    UnknownName(String),
    ExpectedIdentifier(ast::Expression),
}

type HirResult<T> = Result<T, HirError>;

struct GlobalResolver {
    global_counter: usize,
    globals: HashMap<String, hir::GlobalId>,
    constants: Vec<hir::Constant>,
    intrinsics: Vec<String>,
}

impl GlobalResolver {
    fn new(intrinsics: Vec<String>) -> Self {
        let globals = intrinsics.iter()
            .enumerate()
            .map(|(index, name)| (name.clone(), hir::GlobalId::Intrinsic(index)))
            .collect::<HashMap<_, _>>();

        Self {
            globals,
            constants: Vec::new(),
            global_counter: 0,
            intrinsics,
        }
    }

    fn get(&mut self, name: &str) -> HirResult<hir::GlobalId> {
        if let Some(&id) = self.globals.get(name) {
            Ok(id)
        } else {
            Err(HirError::UnknownName(name.to_string()))
        }
    }

    fn add_constant(&mut self, constant: hir::Constant) -> hir::ConstId {
        let id = hir::ConstId(self.constants.len());
        self.constants.push(constant);
        id
    }

    fn add_function(&mut self, name: String) -> hir::GlobalId {
        let id = hir::GlobalId::Function(self.global_counter);
        self.global_counter += 1;
        self.globals.insert(name, id);
        id
    }
}

struct Resolver {
    local_counter: usize,
    locals: Vec<HashMap<String, hir::LocalId>>,
    globals: GlobalResolver,
}

impl Resolver {
    fn new(globals: GlobalResolver) -> Self {
        Self {
            local_counter: 0,
            locals: Vec::new(),
            globals,
        }
    }

    fn get_local(&mut self, name: &str) -> HirResult<hir::LocalId> {
        if let Some(&id) = self.locals.last()
            .and_then(|locals| locals.get(name))
        {
            Ok(id)
        } else {
            Err(HirError::UnknownName(name.to_string()))
        }
    }

    fn get(&mut self, name: &str) -> HirResult<hir::Expression> {
        if let Ok(id) = self.get_local(name) {
            Ok(hir::Expression::Local(id))
        } else {
            let global = self.globals.get(name)?;
            Ok(hir::Expression::Global(global))
        }
    }

    fn add_local(&mut self, name: String) -> hir::LocalId {
        let id = hir::LocalId(self.local_counter);
        self.local_counter += 1;
        self.locals.last_mut().unwrap().insert(name.to_string(), id);
        id
    }

    fn with_scope<F, T>(&mut self, f: F) -> HirResult<T>
        where F: FnOnce(&mut Self) -> HirResult<T>
    {
        self.locals.push(HashMap::new());
        let res = f(self)?;
        self.locals.pop();
        Ok(res)
    }

    fn add_constant(&mut self, constant: hir::Constant) -> hir::ConstId {
        self.globals.add_constant(constant)
    }
}

pub fn ast_to_hir(intrinsics: Vec<String>, module: ast::Module) -> HirResult<hir::Module> {
    let mut globals = GlobalResolver::new(intrinsics);

    for item in &module.items {
        match item {
            ast::Item::Function { ident, .. } => {
                globals.add_function(ident.clone());
            }
        }
    }

    let mut functions = Vec::new();

    for item in module.items {
        match item {
            ast::Item::Function { body, ident } => {
                let mut resolver = Resolver::new(globals);
                let body = resolver.with_scope(|resolver| {
                    ast_to_hir_expression(resolver, body)
                })?;

                let (global_resolver, function) = (
                    resolver.globals,
                    hir::Function {
                        locals: resolver.local_counter,
                        ident,
                        body,
                    }
                );
                functions.push(function);
                globals = global_resolver;
            }
        }
    }

    let constants = globals.constants;

    Ok(hir::Module {
        constants,
        functions,
    })
}

fn ast_to_hir_expression(resolver: &mut Resolver, expr: ast::Expression) -> HirResult<hir::Expression> {
    match expr {
        ast::Expression::Block(statements, last) => resolver.with_scope(|resolver| {
            let statements = statements.into_iter()
                .map(|expr| ast_to_hir_expression(resolver, expr))
                .collect::<HirResult<Vec<_>>>()?;
            let last = last
                .map(|expr| ast_to_hir_expression(resolver, *expr))
                .transpose()?
                .unwrap_or(hir::Expression::Const(resolver.add_constant(hir::Constant::Unit)));
            Ok(hir::Expression::Block(statements, Box::new(last)))
        }),
        ast::Expression::Call { callee, args } => {
            let callee = ast_to_hir_expression(resolver, *callee)?;
            let args = args.into_iter()
                .map(|expr| ast_to_hir_expression(resolver, expr))
                .collect::<HirResult<Vec<_>>>()?;
            Ok(hir::Expression::Method {
                object: Box::new(callee),
                name: "()".to_string(),
                args,
            })
        },
        ast::Expression::Literal(literal) => {
            let constant = match literal {
                ast::Literal::String(s) => hir::Constant::String(s),
                ast::Literal::Number(n) => hir::Constant::Int(n),
            };
            Ok(hir::Expression::Const(resolver.add_constant(constant)))
        },
        ast::Expression::Ident(ident) => resolver.get(&ident),
        ast::Expression::Binary { op: ast::BinaryOp::Assign, lhs, rhs } => {
            match *lhs {
                ast::Expression::Ident(ident) => {
                    let rhs = ast_to_hir_expression(resolver, *rhs)?;
                    let id = resolver.get_local(&ident)?;
                    Ok(hir::Expression::Assign {
                        var: id,
                        expr: Box::new(rhs),
                    })
                },
                not_ident => Err(HirError::ExpectedIdentifier(not_ident)),
            }
        },
        ast::Expression::Binary { op, lhs, rhs } => {
            let lhs = ast_to_hir_expression(resolver, *lhs)?;
            let rhs = ast_to_hir_expression(resolver, *rhs)?;
            let op = match op {
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
        },
        ast::Expression::Unary { op, expr } => {
            let expr = ast_to_hir_expression(resolver, *expr)?;
            let op = match op {
                ast::UnaryOp::Neg => "-1".to_string(),
                ast::UnaryOp::Pos => "+1".to_string(),
            };
            Ok(hir::Expression::Method {
                object: Box::new(expr),
                name: op,
                args: Vec::new(),
            })
        },
        ast::Expression::Definition { ident, expr } => {
            let expr = ast_to_hir_expression(resolver, *expr)?;
            let id = resolver.add_local(ident);
            Ok(hir::Expression::Assign {
                var: id,
                expr: Box::new(expr),
            })
        },
    }
}
