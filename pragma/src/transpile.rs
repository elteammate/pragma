use crate::c::{CType, Expr, ExternalId, FunctionId, LocalId, ParamId, StructFieldId, StructId, TempId, TypedExternalId, TypedFunctionId, TypedLocalId, TypedParamId};
use crate::ivec::{IIndex, IVec};
use crate::tir::Type;
use crate::{c, ivec, tir};
use std::collections::HashMap;
use crate::c::CType::Char;

pub fn transpile_to_c(module: tir::Module) -> c::Module {
    let mut cbuilder = CBuilder::new(&module);

    let (main_id, main) = module
        .functions
        .indexed_iter()
        .find(|(_, f)| f.ident == "main")
        .expect("Main function not found");

    assert_eq!(main.return_ty, Type::Unit);

    let main_ty = Type::Function(main_id, ivec![], Box::new(Type::Unit));

    let main = cbuilder.get_method(main_ty, "()");

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
    structs: IVec<StructId, c::Struct>,
    functions: IVec<FunctionId, c::Function>,
    externals: IVec<ExternalId, c::ExternalFunction>,
    includes: Vec<String>,
    int_wrap: Option<StructId>,
    string_wrap: Option<StructId>,
    generated_methods: HashMap<(Type, String), TypedFunctionId>,
}

impl<'tir> CBuilder<'tir> {
    fn new(module: &'tir tir::Module) -> Self {
        Self {
            module,
            structs: IVec::new(),
            functions: IVec::new(),
            externals: IVec::new(),
            includes: Vec::new(),
            int_wrap: None,
            string_wrap: None,
            generated_methods: HashMap::new(),
        }
    }

    fn add_struct(&mut self, fields: IVec<StructFieldId, CType>) -> StructId {
        self.structs.push(c::Struct { fields })
    }

    fn get_int_struct(&mut self) -> StructId {
        match self.int_wrap {
            Some(id) => id,
            None => {
                let id = self.add_struct(ivec![CType::Int]);
                self.int_wrap = Some(id);
                id
            }
        }
    }

    fn get_string_struct(&mut self) -> StructId {
        match self.string_wrap {
            Some(id) => id,
            None => {
                let id = self.add_struct(ivec![CType::Pointer(Box::new(CType::Char))]);
                self.string_wrap = Some(id);
                id
            }
        }
    }

    fn function(&mut self, function: c::Function) -> TypedFunctionId {
        let id = self.functions.push(function);
        let ty = CType::Function(
            self.functions[id].parameters.iter().map(|local_id|
                self.functions[id].locals[*local_id].clone()
            ).collect(),
            Box::new(self.functions[id].return_type.clone()),
        );
        TypedFunctionId::new(id, ty)
    }

    fn ty2c(&mut self, ty: Type) -> CType {
        match ty {
            ty if ty.is_zero_sized() => panic!("Zero-sized type"),
            Type::Int => CType::Struct(self.get_int_struct()),
            Type::String => CType::Struct(self.get_string_struct()),
            Type::Pointer(ty) => match *ty {
                ty if ty.is_zero_sized() => CType::Pointer(Box::new(CType::Void)),
                ty => CType::Pointer(Box::new(self.ty2c(ty))),
            },
            _ => panic!("Type {ty:?} is not zero sized, but does not have C representation")
        }
    }

    fn get_method(&mut self, object: Type, name: &str) -> TypedFunctionId {
        if let Some(id) = self
            .generated_methods
            .get(&(object.clone(), name.to_string()))
        {
            id.clone()
        } else {
            let method = self.generate_method(object.clone(), name);
            let id = self.function(method);
            self.generated_methods
                .insert((object, name.to_string()), id.clone());
            id
        }
    }

    fn include_if_needed(&mut self, name: String) {
        if !self.includes.contains(&name) {
            self.includes.push(name);
        }
    }

    fn get_external(&mut self, name: &str) -> ExternalId {
        if let Some((id, _fn)) = self.externals.indexed_iter().find(|(_, e)| e.name == name) {
            return id;
        }

        let (args_ty, ret_ty) = match name {
            "printf" => {
                let char_ptr = CType::Pointer(Box::new(CType::Char));
                self.include_if_needed("stdio.h".to_string());
                let mut params = IVec::new();
                params.push(char_ptr.clone());
                params.push(char_ptr.clone());
                (params, CType::Int)
            }
            _ => panic!("Unknown external function"),
        };

        self.externals.push(c::ExternalFunction {
            name: name.to_string(),
            parameters: args_ty,
            return_type: ret_ty,
        })
    }

    fn generate_method(&mut self, object: Type, name: &str) -> c::Function {
        // TODO: Workaround for the temporary implementation
        let field0 = StructFieldId::from_index(0);
        let int = self.ty2c(Type::Int);
        let string = self.ty2c(Type::String);

        match (object, name) {
            (Type::Int, "+2") => {
                let mut locals = ivec![];
                let a = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let b = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let params = ivec![a.id, b.id];

                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_plus(
                            Expr::new_struct_access(Expr::new_local(a), field0, CType::Int),
                            Expr::new_struct_access(Expr::new_local(b), field0, CType::Int),
                        )],
                    ))],
                    locals,
                    temps: IVec::new(),
                    return_type: int,
                }
            }
            (Type::Int, "*2") => {
                let mut locals = ivec![];
                let a = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let b = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let params = ivec![a.id, b.id];

                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_multiply(
                            Expr::new_struct_access(Expr::new_local(a), field0, CType::Int),
                            Expr::new_struct_access(Expr::new_local(b), field0, CType::Int),
                        )],
                    ))],
                    locals,
                    temps: ivec![],
                    return_type: int,
                }
            }
            (Type::Int, "-2") => {
                let mut locals = ivec![];
                let a = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let b = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let params = ivec![a.id, b.id];

                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_minus(
                            Expr::new_struct_access(Expr::new_local(a), field0, CType::Int),
                            Expr::new_struct_access(Expr::new_local(b), field0, CType::Int),
                        )],
                    ))],
                    locals,
                    temps: ivec![],
                    return_type: int,
                }
            }
            (Type::Int, "-1") => {
                let mut locals = ivec![];
                let a = TypedLocalId::new(locals.push(int.clone()), int.clone());
                let params = ivec![a.id];

                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_minus(
                            Expr::new_int(0),
                            Expr::new_struct_access(Expr::new_local(a), field0, CType::Int),
                        )],
                    ))],
                    locals,
                    temps: ivec![],
                    return_type: int,
                }
            }
            (Type::Intrinsic(id, _, _), "()") => match id.index() {
                0 => {
                    let printf = self.get_external("printf");
                    let char_ptr = CType::Pointer(Box::new(CType::Char));
                    let typed_printf = TypedExternalId::new(
                        printf,
                        CType::Function(
                            ivec![char_ptr.clone(), char_ptr.clone()],
                            Box::new(CType::Int),
                        ),
                    );
                    let mut locals = ivec![];
                    let s = TypedLocalId::new(locals.push(string.clone()), string.clone());
                    let params = ivec![s.id];

                    c::Function {
                        parameters: params,
                        body: vec![c::Statement::Expression(Expr::new_call(
                            Expr::new_external(typed_printf),
                            ivec![
                                Expr::new_string("%s".to_string()),
                                Expr::new_struct_access(Expr::new_local(s), field0, char_ptr),
                            ],
                        ))],
                        locals,
                        temps: ivec![],
                        return_type: CType::Void,
                    }
                }
                1 => {
                    let printf = self.get_external("printf");
                    let char_ptr = CType::Pointer(Box::new(CType::Char));
                    let typed_printf = TypedExternalId::new(
                        printf,
                        CType::Function(
                            ivec![char_ptr.clone(), char_ptr.clone()],
                            Box::new(CType::Int),
                        ),
                    );
                    let mut locals = ivec![];
                    let s = TypedLocalId::new(locals.push(string.clone()), string.clone());
                    let params = ivec![s.id];

                    c::Function {
                        parameters: params,
                        body: vec![c::Statement::Expression(Expr::new_call(
                            Expr::new_external(typed_printf),
                            ivec![
                                Expr::new_string("%s\n".to_string()),
                                Expr::new_struct_access(Expr::new_local(s), field0, char_ptr),
                            ],
                        ))],
                        locals,
                        temps: ivec![],
                        return_type: CType::Void,
                    }
                }
                _ => panic!("Unknown intrinsic"),
            },
            (Type::Function(id, _args_ty, ret_ty), "()") => {
                let ret_ty = if ret_ty.is_zero_sized() {
                    CType::Void
                } else {
                    self.ty2c(*ret_ty)
                };

                let mut builder = CExpressionBuilder::new(self, id);
                let body = builder.translate_function(id);

                c::Function {
                    parameters: builder.parameters,
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
    function: &'tir tir::Function,
    locals: IVec<LocalId, CType>,
    parameters: IVec<ParamId, LocalId>,
    temps: IVec<TempId, CType>,
    registered_locals: IVec<tir::LocalId, Option<LocalId>>,
}

type StatementAndResult = (Vec<c::Statement>, Option<Expr>);

impl<'m, 'tir> CExpressionBuilder<'m, 'tir> {
    fn new(module: &'m mut CBuilder<'tir>, function_id: tir::FunctionId) -> Self {
        let local_count = module.module.functions[function_id].locals.len();
        let function = &module.module.functions[function_id];
        Self {
            module,
            function,
            parameters: ivec![],
            locals: ivec![],
            temps: ivec![],
            registered_locals: ivec![None; local_count],
        }
    }

    fn temp(&mut self, ty: CType) -> c::TypedTempId {
        c::TypedTempId::new(self.temps.push(ty.clone()), ty)
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
        types: Vec<CType>,
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

    fn get_local(&mut self, local: tir::LocalId) -> LocalId {
        if let Some(id) = self.registered_locals[local] {
            id
        } else {
            let ty = self.module.ty2c(self.function.locals[local].clone());
            let id = self.locals.push(ty);
            self.registered_locals[local] = Some(id);
            id
        }
    }

    fn translate_function(&mut self, id: tir::FunctionId) -> Vec<c::Statement> {
        let function = &self.module.module.functions[id];

        self.parameters = function.args.iter().filter_map(|&local| {
            let ty = &function.locals[local];
            if ty.is_zero_sized() {
                return None;
            }
            Some(self.get_local(local))
        }).collect();

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
        let zero_sized = ty.is_zero_sized();

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
                let constant = &self.module.module.constants[*constant];
                let expr = match constant {
                    tir::Const::Unit => return (Vec::new(), None),
                    tir::Const::Int(int) => Expr::new_struct_build(
                        self.module.get_int_struct(),
                        ivec![Expr::new_int(*int)],
                    ),
                    tir::Const::String(string) => Expr::new_struct_build(
                        self.module.get_string_struct(),
                        ivec![Expr::new_string(string.clone())],
                    ),
                };

                Some(expr)
            }
            tir::Expression::Local(_) if zero_sized => None,
            tir::Expression::Local(local) => {
                let local = self.get_local(*local);
                Some(Expr::new_local(TypedLocalId::new(
                    local,
                    self.module.ty2c(ty.clone()),
                )))
            }
            tir::Expression::Function(_) | tir::Expression::Intrinsic(_) if zero_sized => None,
            tir::Expression::Function(_) | tir::Expression::Intrinsic(_) => {
                todo!("Expressions on functions are not implemented yet")
            }
            tir::Expression::Method {
                object: object @ tir::Typed { ty, .. },
                name: method_name,
                args,
            } => {
                let (mut obj_statements, obj) = self.translate_expression(object);
                result.append(&mut obj_statements);

                let id = self.module.get_method(ty.clone(), method_name);
                let arguments = args
                    .iter()
                    .map(|arg| self.translate_expression(arg))
                    .collect();
                let types = args
                    .iter()
                    .map(|arg| self.module.ty2c(arg.ty.clone()))
                    .collect();
                let (mut stmts, mut args) = self.merge_results(arguments, types);
                result.append(&mut stmts);

                if !ty.is_zero_sized() {
                    args.insert(0, obj.expect("It must contain a value"));
                }

                Some(Expr::new_call(Expr::new_global(id), args.into()))
            }
            tir::Expression::Assign { var: _, expr } if expr.ty.is_zero_sized() => {
                let translated = self.translate_expression(expr);
                result.append(&mut self.ignore_result(translated));
                None
            },
            tir::Expression::Assign {
                var: local,
                expr: value,
            } => {
                let ty = &self.function.locals[*local];
                let local = self.get_local(*local);
                let (mut stmts, value) = self.translate_expression(value);
                result.append(&mut stmts);
                result.push(c::Statement::Expression(Expr::new_assign(
                    TypedLocalId::new(local, self.module.ty2c(ty.clone())),
                    value.expect("It must contain a value"),
                )));
                None
            }
            tir::Expression::Return { value } if value.ty.is_zero_sized() => {
                let translated = self.translate_expression(value);
                result.append(&mut self.ignore_result(translated));
                result.push(c::Statement::ReturnVoid);
                None
            }
            tir::Expression::Return { value } => {
                let (mut stmts, value) = self.translate_expression(value);
                result.append(&mut stmts);
                result.push(c::Statement::Return(value.unwrap()));
                None
            }
            tir::Expression::Trap => {
                None
            }
            tir::Expression::Uninit if ty.is_zero_sized() => {
                None
            }
            tir::Expression::Uninit => {
                match ty {
                    Type::Int => Some(Expr::new_struct_build(
                        self.module.get_int_struct(),
                        ivec![],
                    )),
                    Type::String => Some(Expr::new_struct_build(
                        self.module.get_string_struct(),
                        ivec![],
                    )),
                    Type::Pointer(_) => {
                        let ty = self.module.ty2c(ty.clone());
                        Some(Expr::new_cast(Expr::new_int(0), ty))
                    },
                    _ => todo!("Uninitialized construction of arbitrary types"),
                }
            }
        };

        (result, last)
    }
}
