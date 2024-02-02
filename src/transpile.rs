use crate::c::{
    CType, Expr, ExternalId, FunctionId, LocalId, StructFieldId, StructId, TempId,
    TypedExternalId, TypedFunctionId, TypedLocalId,
};
use crate::ivec::{IIndex, IVec};
use crate::tir::Type;
use crate::{c, ivec, tir};
use std::collections::HashMap;

pub fn transpile_to_c(module: tir::Module) -> c::Module {
    let mut cbuilder = CBuilder::new(&module);
    let main = cbuilder.get_method(
        Type::Function(
            tir::FunctionId::from_index(0),
            ivec![],
            Box::new(Type::Unit),
        ),
        "()",
    );

    assert_eq!(main.ty(), CType::Function(ivec![], Box::new(CType::Void)));

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
    structs: IVec<c::Struct, StructId>,
    functions: IVec<c::Function, FunctionId>,
    externals: IVec<c::ExternalFunction, ExternalId>,
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

    fn add_struct(&mut self, fields: IVec<CType, StructFieldId>) -> StructId {
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
            self.functions[id].parameters.clone(),
            Box::new(self.functions[id].return_type.clone()),
        );
        TypedFunctionId::new(id, ty)
    }

    fn ty2c(&mut self, ty: Type) -> CType {
        match ty {
            Type::Int => CType::Struct(self.get_int_struct()),
            Type::String => CType::Struct(self.get_string_struct()),
            ty if ty.is_zero_sized() => panic!("Zero-sized type"),
            _ => todo!("Arbitrary types are not implemented yet"),
        }
    }

    fn get_method(&mut self, object: Type, name: &str) -> TypedFunctionId {
        if let Some(id) = self
            .generated_methods
            .get(&(object.clone(), name.to_string()))
        {
            return id.clone();
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
            return id
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
        match (object, name) {
            (Type::Int, "+2") => {
                let int = self.ty2c(Type::Int);
                let mut params = IVec::new();
                let param0 = c::TypedParamId::new(params.push(int.clone()), int.clone());
                let param1 = c::TypedParamId::new(params.push(int.clone()), int.clone());
                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_plus(
                            Expr::new_struct_access(Expr::new_param(param0), field0, CType::Int),
                            Expr::new_struct_access(Expr::new_param(param1), field0, CType::Int),
                        )],
                    ))],
                    locals: IVec::new(),
                    temps: IVec::new(),
                    return_type: int,
                }
            }
            (Type::Int, "*2") => {
                let int = self.ty2c(Type::Int);
                let mut params = IVec::new();
                let param0 = c::TypedParamId::new(params.push(int.clone()), int.clone());
                let param1 = c::TypedParamId::new(params.push(int.clone()), int.clone());
                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_multiply(
                            Expr::new_struct_access(Expr::new_param(param0), field0, CType::Int),
                            Expr::new_struct_access(Expr::new_param(param1), field0, CType::Int),
                        )],
                    ))],
                    locals: ivec![],
                    temps: ivec![],
                    return_type: int,
                }
            }
            (Type::Int, "-2") => {
                let int = self.ty2c(Type::Int);
                let mut params = IVec::new();
                let param0 = c::TypedParamId::new(params.push(int.clone()), int.clone());
                let param1 = c::TypedParamId::new(params.push(int.clone()), int.clone());
                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_minus(
                            Expr::new_struct_access(Expr::new_param(param0), field0, CType::Int),
                            Expr::new_struct_access(Expr::new_param(param1), field0, CType::Int),
                        )],
                    ))],
                    locals: ivec![],
                    temps: ivec![],
                    return_type: int,
                }
            }
            (Type::Int, "-1") => {
                let int = self.ty2c(Type::Int);
                let mut params = IVec::new();
                let param = c::TypedParamId::new(params.push(int.clone()), int.clone());
                c::Function {
                    parameters: params,
                    body: vec![c::Statement::Return(Expr::new_struct_build(
                        self.get_int_struct(),
                        ivec![Expr::new_minus(
                            Expr::new_int(0),
                            Expr::new_struct_access(Expr::new_param(param), field0, CType::Int),
                        )],
                    ))],
                    locals: ivec![],
                    temps: ivec![],
                    return_type: int,
                }
            }
            (Type::Intrinsic(id, _, _), "()") => match id.index() {
                0 => {
                    let string = self.ty2c(Type::String);
                    let printf = self.get_external("printf");
                    let char_ptr = CType::Pointer(Box::new(CType::Char));
                    let typed_printf = TypedExternalId::new(
                        printf,
                        CType::Function(
                            ivec![char_ptr.clone(), char_ptr.clone()],
                            Box::new(CType::Int),
                        ),
                    );
                    let mut params = IVec::new();
                    let param = c::TypedParamId::new(params.push(string.clone()), string.clone());
                    c::Function {
                        parameters: params,
                        body: vec![c::Statement::Expression(Expr::new_call(
                            Expr::new_external(typed_printf),
                            ivec![
                                Expr::new_string("%s".to_string()),
                                Expr::new_struct_access(Expr::new_param(param), field0, char_ptr),
                            ],
                        ))],
                        locals: ivec![],
                        temps: ivec![],
                        return_type: string,
                    }
                }
                1 => {
                    let string = self.ty2c(Type::String);
                    let printf = self.get_external("printf");
                    let char_ptr = CType::Pointer(Box::new(CType::Char));
                    let typed_printf = TypedExternalId::new(
                        printf,
                        CType::Function(
                            ivec![char_ptr.clone(), char_ptr.clone()],
                            Box::new(CType::Int),
                        ),
                    );
                    let mut params = IVec::new();
                    let param = c::TypedParamId::new(params.push(string.clone()), string.clone());
                    c::Function {
                        parameters: params,
                        body: vec![c::Statement::Expression(Expr::new_call(
                            Expr::new_external(typed_printf),
                            ivec![
                                Expr::new_string("%s\n".to_string()),
                                Expr::new_struct_access(Expr::new_param(param), field0, char_ptr),
                            ],
                        ))],
                        locals: ivec![],
                        temps: ivec![],
                        return_type: string,
                    }
                }
                _ => panic!("Unknown intrinsic"),
            },
            (Type::Function(id, args_ty, ret_ty), "()") => {
                let args_ty = args_ty
                    .into_iter()
                    .filter_map(|ty| {
                        if ty.is_zero_sized() {
                            None
                        } else {
                            Some(self.ty2c(ty))
                        }
                    })
                    .collect();

                let ret_ty = if ret_ty.is_zero_sized() {
                    CType::Void
                } else {
                    self.ty2c(*ret_ty)
                };

                let mut builder = CExpressionBuilder::new(self, id);
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
    function: &'tir tir::Function,
    locals: IVec<CType, LocalId>,
    temps: IVec<CType, TempId>,
    registered_locals: IVec<Option<LocalId>, tir::LocalId>,
}

type StatementAndResult = (Vec<c::Statement>, Option<Expr>);

impl<'m, 'tir> CExpressionBuilder<'m, 'tir> {
    fn new(module: &'m mut CBuilder<'tir>, function_id: tir::FunctionId) -> Self {
        let local_count = module.module.functions[function_id].locals.len();
        let function = &module.module.functions[function_id];
        Self {
            module,
            function,
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
            let ty = self
                .module
                .ty2c(self.function.locals[local].clone());
            let id = self.locals.push(ty);
            self.registered_locals[local] = Some(id);
            id
        }
    }

    fn translate_function(&mut self, id: tir::FunctionId) -> Vec<c::Statement> {
        let function = &self.module.module.functions[id];
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
            tir::Expression::Local(local) => {
                if ty.is_zero_sized() {
                    None
                } else {
                    let local = self.get_local(*local);
                    Some(Expr::new_local(TypedLocalId::new(
                        local,
                        self.module.ty2c(ty.clone()),
                    )))
                }
            }
            tir::Expression::Function(_) | tir::Expression::Intrinsic(_) => {
                if ty.is_zero_sized() {
                    None
                } else {
                    todo!("Expressions on functions are not implemented yet")
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
