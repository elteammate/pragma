use std::collections::HashMap;
use crate::{c, tir};
use crate::c::{CType, Expr, ExternalId, GlobalId, ParamId, StructId, TypedExternalId, TypedGlobalId, TypedLocalId};
use crate::tir::{Type};

pub fn transpile_to_c(module: tir::Module) -> c::Module {
    let mut cbuilder = CBuilder::new(&module);
    let main = cbuilder.get_method(
        Type::Function(
            tir::GlobalId::Function(0),
            vec![],
            Box::new(Type::Unit),
        ), "()",
    );

    assert_eq!(main.ty(), c::CType::Function(vec![], Box::new(c::CType::Void)));

    c::Module {
        includes: cbuilder.includes,
        structs: cbuilder.structs,
        functions: cbuilder.functions,
        externals: cbuilder.externals,
        main: Some(main.into()),
    }
}

struct CBuilder<'tir> {
    module: &'tir tir::Module,
    structs: Vec<c::Struct>,
    functions: Vec<c::Function>,
    externals: Vec<c::ExternalFunction>,
    includes: Vec<String>,
    int_wrap: Option<StructId>,
    string_wrap: Option<StructId>,
    generated_methods: HashMap<(Type, String), TypedGlobalId>,
}

impl<'tir> CBuilder<'tir> {
    fn new(module: &'tir tir::Module) -> Self {
        Self {
            module,
            structs: Vec::new(),
            functions: Vec::new(),
            externals: Vec::new(),
            includes: Vec::new(),
            int_wrap: None,
            string_wrap: None,
            generated_methods: HashMap::new(),
        }
    }

    fn add_struct(&mut self, fields: Vec<c::CType>) -> StructId {
        let id = StructId(self.structs.len());
        self.structs.push(c::Struct { fields });
        id
    }

    fn get_int_struct(&mut self) -> StructId {
        match self.int_wrap {
            Some(id) => id,
            None => {
                let id = self.add_struct(vec![c::CType::Int]);
                self.int_wrap = Some(id);
                id
            }
        }
    }

    fn get_string_struct(&mut self) -> StructId {
        match self.string_wrap {
            Some(id) => id,
            None => {
                let id = self.add_struct(vec![c::CType::Pointer(Box::new(c::CType::Char))]);
                self.string_wrap = Some(id);
                id
            }
        }
    }

    fn function(&mut self, function: c::Function) -> TypedGlobalId {
        let id = GlobalId(self.functions.len());
        self.functions.push(function);
        let ty = c::CType::Function(
            self.functions[id.0].parameters.clone(),
            Box::new(self.functions[id.0].return_type.clone()),
        );
        TypedGlobalId::new(id, ty)
    }

    fn ty2c(&mut self, ty: Type) -> c::CType {
        match ty {
            Type::Int => c::CType::Struct(self.get_int_struct()),
            Type::String => c::CType::Struct(self.get_string_struct()),
            ty if ty.is_zero_sized() => panic!("Zero-sized type"),
            _ => todo!("Arbitrary types are not implemented yet")
        }
    }

    fn get_method(&mut self, object: Type, name: &str) -> TypedGlobalId {
        if let Some(id) = self.generated_methods.get(&(object.clone(), name.to_string())) {
            return id.clone();
        } else {
            let method = self.generate_method(object.clone(), name);
            let id = self.function(method);
            self.generated_methods.insert((object, name.to_string()), id.clone());
            id
        }
    }

    fn include_if_needed(&mut self, name: &str) {
        if !self.includes.contains(&name.to_string()) {
            self.includes.push(name.to_string());
        }
    }

    fn get_external(&mut self, name: &str) -> ExternalId {
        if let Some((id, _fn)) = self.externals.iter().enumerate().find(|(_, e)| e.name == name) {
            ExternalId(id)
        } else {
            let id = ExternalId(self.externals.len());
            let char_ptr = c::CType::Pointer(Box::new(c::CType::Char));
            let (args_ty, ret_ty) = match name {
                "printf" => {
                    self.include_if_needed("stdio.h");
                    (vec![char_ptr.clone(), char_ptr], c::CType::Int)
                }
                _ => panic!("Unknown external function"),
            };
            self.externals.push(c::ExternalFunction {
                name: name.to_string(),
                parameters: args_ty,
                return_type: ret_ty,
            });
            id
        }
    }

    fn generate_method(&mut self, object: Type, name: &str) -> c::Function {
        match (object, name) {
            (Type::Int, "+2") => {
                let int = self.ty2c(Type::Int);
                let param0 = c::TypedParamId::new(ParamId(0), int.clone());
                let param1 = c::TypedParamId::new(ParamId(1), int.clone());
                c::Function {
                    parameters: vec![int.clone(), int.clone()],
                    body: vec![
                        c::Statement::Return(
                            Expr::new_struct_build(
                                self.get_int_struct(),
                                vec![Expr::new_plus(
                                    Expr::new_struct_access(Expr::new_param(param0), 0, CType::Int),
                                    Expr::new_struct_access(Expr::new_param(param1), 0, CType::Int),
                                )],
                            )
                        ),
                    ],
                    locals: vec![],
                    temps: vec![],
                    return_type: int,
                }
            }
            (Type::Int, "*2") => {
                let int = self.ty2c(Type::Int);
                let param0 = c::TypedParamId::new(ParamId(0), int.clone());
                let param1 = c::TypedParamId::new(ParamId(1), int.clone());
                c::Function {
                    parameters: vec![int.clone(), int.clone()],
                    body: vec![
                        c::Statement::Return(
                            Expr::new_struct_build(
                                self.get_int_struct(),
                                vec![Expr::new_multiply(
                                    Expr::new_struct_access(Expr::new_param(param0), 0, c::CType::Int),
                                    Expr::new_struct_access(Expr::new_param(param1), 0, c::CType::Int),
                                )],
                            )
                        ),
                    ],
                    locals: vec![],
                    temps: vec![],
                    return_type: int,
                }
            }
            (Type::Int, "-2") => {
                let int = self.ty2c(Type::Int);
                let param0 = c::TypedParamId::new(ParamId(0), int.clone());
                let param1 = c::TypedParamId::new(ParamId(1), int.clone());
                c::Function {
                    parameters: vec![int.clone(), int.clone()],
                    body: vec![
                        c::Statement::Return(
                            Expr::new_struct_build(
                                self.get_int_struct(),
                                vec![Expr::new_minus(
                                    Expr::new_struct_access(Expr::new_param(param0), 0, c::CType::Int),
                                    Expr::new_struct_access(Expr::new_param(param1), 0, c::CType::Int),
                                )],
                            )
                        ),
                    ],
                    locals: vec![],
                    temps: vec![],
                    return_type: int,
                }
            }
            (Type::Int, "-1") => {
                let int = self.ty2c(Type::Int);
                let param = c::TypedParamId::new(ParamId(0), int.clone());
                c::Function {
                    parameters: vec![int.clone()],
                    body: vec![
                        c::Statement::Return(
                            Expr::new_struct_build(
                                self.get_int_struct(),
                                vec![Expr::new_minus(
                                    Expr::new_int(0),
                                    Expr::new_struct_access(Expr::new_param(param), 0, c::CType::Int),
                                )],
                            )
                        ),
                    ],
                    locals: vec![],
                    temps: vec![],
                    return_type: int,
                }
            }
            (Type::Function(tir::GlobalId::Intrinsic(0), _, _), "()") => {
                let string = self.ty2c(Type::String);
                let printf = self.get_external("printf");
                let char_ptr = c::CType::Pointer(Box::new(c::CType::Char));
                let typed_printf = TypedExternalId::new(printf, c::CType::Function(
                    vec![char_ptr.clone(), char_ptr.clone()],
                    Box::new(c::CType::Int))
                );
                let param = c::TypedParamId::new(ParamId(0), string.clone());
                c::Function {
                    parameters: vec![string.clone()],
                    body: vec![c::Statement::Expression(
                        Expr::new_call(
                            Expr::new_external(typed_printf),
                            vec![
                                Expr::new_string("%s".to_string()),
                                Expr::new_struct_access(Expr::new_param(param), 0, char_ptr),
                            ],
                        )
                    )],
                    locals: vec![],
                    temps: vec![],
                    return_type: string,
                }
            }
            (Type::Function(tir::GlobalId::Intrinsic(1), _, _), "()") => {
                let string = self.ty2c(Type::String);
                let printf = self.get_external("printf");
                let char_ptr = c::CType::Pointer(Box::new(c::CType::Char));
                let typed_printf = TypedExternalId::new(printf, c::CType::Function(
                    vec![char_ptr.clone(), char_ptr.clone()],
                    Box::new(c::CType::Int))
                );
                let param = c::TypedParamId::new(ParamId(0), string.clone());
                c::Function {
                    parameters: vec![string.clone()],
                    body: vec![c::Statement::Expression(
                        Expr::new_call(
                            Expr::new_external(typed_printf),
                            vec![
                                Expr::new_string("%s\n".to_string()),
                                Expr::new_struct_access(Expr::new_param(param), 0, char_ptr),
                            ],
                        )
                    )],
                    locals: vec![],
                    temps: vec![],
                    return_type: string,
                }
            }
            (Type::Function(tir::GlobalId::Function(id), args_ty, ret_ty), "()") => {
                let args_ty = args_ty.into_iter().filter_map(
                    |ty| if ty.is_zero_sized() { None } else { Some(self.ty2c(ty)) }
                ).collect();

                let ret_ty = if ret_ty.is_zero_sized() {
                    c::CType::Void
                } else {
                    self.ty2c(*ret_ty)
                };

                let mut builder = CExpressionBuilder::new(self);
                let body = builder.translate_function(id);

                c::Function {
                    parameters: args_ty,
                    return_type: ret_ty,
                    body,
                    locals: builder.locals,
                    temps: builder.temps,
                }
            }
            (ty, name) => panic!("Unknown method {:?}.{}", ty, name),
        }
    }
}

struct CExpressionBuilder<'m, 'tir> {
    module: &'m mut CBuilder<'tir>,
    locals: Vec<c::CType>,
    temps: Vec<c::CType>,
    registered_locals: Vec<Option<c::LocalId>>,
}

type StatementAndResult = (Vec<c::Statement>, Option<Expr>);

impl<'m, 'tir> CExpressionBuilder<'m, 'tir> {
    fn new(module: &'m mut CBuilder<'tir>) -> Self {
        let local_count = module.module.functions[0].locals.len();
        Self {
            module,
            locals: Vec::new(),
            temps: Vec::new(),
            registered_locals: vec![None; local_count],
        }
    }

    fn temp(&mut self, ty: c::CType) -> c::TypedTempId {
        let id = c::TempId(self.temps.len());
        self.temps.push(ty.clone());
        c::TypedTempId::new(id, ty)
    }

    fn ignore_result(&mut self, stmts: StatementAndResult) -> Vec<c::Statement> {
        let (mut statements, result) = stmts;
        if let Some(result) = result {
            statements.push(c::Statement::Expression(result));
        }

        statements
    }

    fn merge_results(
        &mut self,
        stmts: Vec<StatementAndResult>,
        types: Vec<c::CType>
    ) -> (Vec<c::Statement>, Vec<Expr>) {
        let mut statements = Vec::new();
        let mut results = Vec::new();
        for (stmt, ty) in stmts.into_iter().zip(types.into_iter()) {
            let (mut stmts, result) = stmt;
            statements.append(&mut stmts);
            if let Some(result) = result {
                let temp = self.temp(ty);
                statements.push(c::Statement::Expression(Expr::new_assign_temp(
                    temp.clone(),
                    result,
                )));
                results.push(Expr::new_temp(temp));
            }
        }

        (statements, results)
    }

    fn get_local(&mut self, local: tir::LocalId) -> c::LocalId {
        if let Some(id) = self.registered_locals[local.0] {
            id
        } else {
            let id = c::LocalId(self.locals.len());
            let ty = self.module.ty2c(self.module.module.functions[0].locals[local.0].clone());
            self.locals.push(ty);
            self.registered_locals[local.0] = Some(id);
            id
        }
    }

    fn translate_function(&mut self, function_id: usize) -> Vec<c::Statement> {
        let function = &self.module.module.functions[function_id];
        let body = &function.body;
        let (mut statements, result) = self.translate_expression(body);
        if let Some(result) = result {
            statements.push(c::Statement::Return(result));
        }

        statements
    }

    fn translate_expression(&mut self, expr: &tir::Typed) -> StatementAndResult {
        let mut result = Vec::new();
        let ty = &expr.ty;

        let last = match &*expr.expr {
            tir::Expression::Block(statements, last) => {
                for statement in statements {
                    let stmts = self.translate_expression(statement);
                    result.append(&mut self.ignore_result(stmts));
                }

                let (mut stmts, last) = self.translate_expression(last);
                result.append(&mut stmts);
                last
            }
            tir::Expression::Constant(constant) => {
                let constant = &self.module.module.constants[constant.0];
                let expr = match constant {
                    tir::Constant::Unit => return (Vec::new(), None),
                    tir::Constant::Int(int) => Expr::new_struct_build(
                        self.module.get_int_struct(),
                        vec![Expr::new_int(*int)],
                    ),
                    tir::Constant::String(string) => Expr::new_struct_build(
                        self.module.get_string_struct(),
                        vec![Expr::new_string(string.clone())],
                    ),
                };

                Some(expr)
            }
            tir::Expression::Local(local) => {
                if ty.is_zero_sized() {
                    None
                } else {
                    let local = self.get_local(*local);
                    Some(Expr::new_local(TypedLocalId::new(local, self.module.ty2c(ty.clone()))))
                }
            }
            tir::Expression::Global(_) => {
                if ty.is_zero_sized() {
                    None
                } else {
                    todo!("Standalone global expressions are not implemented yet")
                }
            }
            tir::Expression::Method {
                object: object @ tir::Typed { ty, .. },
                name: method_name,
                args,
            } => {
                let (mut obj_statements, obj) = self.translate_expression(object);
                result.append(&mut obj_statements);

                let id = self.module.get_method(ty.clone(), method_name);
                let arguments = args.iter().map(|arg| self.translate_expression(arg)).collect();
                let types = args.iter().map(|arg| self.module.ty2c(arg.ty.clone())).collect();
                let (mut stmts, mut args) = self.merge_results(arguments, types);
                result.append(&mut stmts);

                if !ty.is_zero_sized() {
                    args.insert(0, obj.expect("It must contain a value"));
                }

                Some(Expr::new_call(Expr::new_global(id), args))
            }
            tir::Expression::Assign {
                var: local,
                expr: value,
            } => {
                if ty.is_zero_sized() {
                    return (Vec::new(), None);
                } else {
                    let local = self.get_local(*local);
                    let (mut stmts, value) = self.translate_expression(value);
                    result.append(&mut stmts);
                    result.push(c::Statement::Expression(Expr::new_assign(
                        TypedLocalId::new(local, self.module.ty2c(ty.clone())),
                        value.expect("It must contain a value"),
                    )));
                    None
                }
            }
        };

        (result, last)
    }
}
