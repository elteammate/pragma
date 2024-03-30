use crate::c::{
    CType, Expression, ExternalId, Function, FunctionId, LocalId, Module, ParamId, Precedence,
    Statement, Struct, StructFieldId, StructId, TempId,
};
use crate::ivec;
use crate::ivec::{IIndex, IVec};
use std::fmt::Write;

const ALPHABET: &[u8] = b"abcdefghijklmnopqrstuvwxyz\
      ABCDEFGHIJKLMNOPQRSTUVWXYZ\
      0123456789_";

const ALPHABET_LEN: usize = ALPHABET.len();

const GLOBAL_NAMES_OFFSET: usize = 26;
const GLOBAL_NAMES_RANGE: usize = 26;
const LOCAL_NAMES_OFFSET: usize = 0;
const LOCAL_NAMES_RANGE: usize = 26;
const STRUCT_NAMES_OFFSET: usize = 0;
const STRUCT_NAMES_RANGE: usize = 52;

fn nice_char(i: usize) -> char {
    assert!(i < ALPHABET_LEN);
    ALPHABET[i] as char
}

fn range_prefixed_string(range: usize, offset: usize, n: usize) -> String {
    if n < range {
        format!("{}", nice_char(offset + n))
    } else if n < range * ALPHABET_LEN {
        format!("{}{}", nice_char(offset + n % range), nice_char(n / range))
    } else if n < range * ALPHABET_LEN * ALPHABET_LEN {
        format!(
            "{}{}{}",
            nice_char(offset + n % range),
            nice_char(n / range % ALPHABET_LEN),
            nice_char(n / range / ALPHABET_LEN),
        )
    } else {
        panic!("No real program can be this big")
    }
}

struct Namer {
    offset: usize,
    range: usize,
    n: usize,
}

impl Namer {
    fn new(offset: usize, range: usize) -> Self {
        Self {
            offset,
            range,
            n: 0,
        }
    }
}

impl Iterator for Namer {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let result = range_prefixed_string(self.range, self.offset, self.n);
        self.n += 1;
        if matches!(&result[..], "if" | "for" | "do" | "int") {
            self.next()
        } else {
            Some(result)
        }
    }
}

struct Builder<'c> {
    result: Vec<u8>,
    module: &'c Module,
    struct_names: IVec<StructId, String>,
    function_names: IVec<FunctionId, String>,
    struct_fields: IVec<StructFieldId, String>,
    struct_field_namer: Namer,
}

impl<'c> Builder<'c> {
    fn new(module: &'c Module) -> Self {
        let mut namer = Namer::new(GLOBAL_NAMES_OFFSET, GLOBAL_NAMES_RANGE);
        let struct_names = module
            .structs
            .iter()
            .map(|_| namer.next().unwrap())
            .collect();
        let function_names = module
            .functions
            .iter()
            .map(|_| namer.next().unwrap())
            .collect();

        Self {
            result: Vec::new(),
            module,
            struct_names,
            function_names,
            struct_fields: ivec![],
            struct_field_namer: Namer::new(STRUCT_NAMES_OFFSET, STRUCT_NAMES_RANGE),
        }
    }

    fn commit(self) -> String {
        String::from_utf8(self.result).unwrap()
    }

    fn get_struct_name(&self, struct_id: StructId) -> String {
        self.struct_names[struct_id].clone()
    }

    fn get_function_name(&self, function_id: FunctionId) -> String {
        self.function_names[function_id].clone()
    }

    fn get_struct_field(&mut self, field_no: StructFieldId) -> String {
        while field_no.index() >= self.struct_fields.len() {
            self.struct_fields
                .push(self.struct_field_namer.next().unwrap());
        }
        self.struct_fields[field_no].clone()
    }

    fn get_external_name(&self, external_id: ExternalId) -> String {
        self.module.externals[external_id].name.clone()
    }
}

struct LocalNames {
    params: IVec<ParamId, String>,
    locals: IVec<LocalId, String>,
    temps: IVec<TempId, String>,
}

impl LocalNames {
    fn new(function: &Function) -> Self {
        let mut namer = Namer::new(LOCAL_NAMES_OFFSET, LOCAL_NAMES_RANGE);
        let params = function
            .parameters
            .iter()
            .map(|_| namer.next().unwrap())
            .collect();
        let locals = function
            .locals
            .iter()
            .map(|_| namer.next().unwrap())
            .collect();
        let temps = function
            .temps
            .iter()
            .map(|_| namer.next().unwrap())
            .collect();

        Self {
            params,
            locals,
            temps,
        }
    }

    fn get_param(&self, param_id: ParamId) -> String {
        self.params[param_id].clone()
    }

    fn get_local(&self, local_id: LocalId) -> String {
        self.locals[local_id].clone()
    }

    fn get_temp(&self, temp_id: TempId) -> String {
        self.temps[temp_id].clone()
    }
}

impl<'c> Write for Builder<'c> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.result.extend_from_slice(s.as_bytes());
        Ok(())
    }
}

pub fn emit(module: &Module) -> String {
    let mut builder = Builder::new(module);
    emit_module(&mut builder, module).expect("Errors while emitting module are unlikely");
    builder.commit()
}

fn emit_module<'c>(builder: &mut Builder<'c>, module: &'c Module) -> std::fmt::Result {
    for include in &module.includes {
        writeln!(builder, "#include <{}>", include)?;
    }

    for (id, struct_) in module.structs.indexed_iter() {
        emit_struct(builder, id, struct_)?;
    }

    for (id, function) in module.functions.indexed_iter() {
        emit_function(builder, id, function)?;
    }

    write!(builder, "int main(){{")?;
    if let Some(main) = module.main {
        write!(builder, "{}();", builder.get_function_name(main))?;
    }
    write!(builder, "}}")?;

    Ok(())
}

fn emit_struct(builder: &mut Builder, id: StructId, struct_: &Struct) -> std::fmt::Result {
    write!(builder, "typedef struct{{")?;
    for (id, field) in struct_.fields.indexed_iter() {
        emit_decl(builder, field.clone(), |builder| {
            let name = builder.get_struct_field(id);
            write!(builder, "{}", name)
        })?;
        write!(builder, ";")?;
    }
    write!(builder, "}}{};", builder.get_struct_name(id))?;
    Ok(())
}

fn emit_function<'c>(
    builder: &mut Builder<'c>,
    id: FunctionId,
    function: &'c Function,
) -> std::fmt::Result {
    emit_decl(builder, function.return_type.clone(), |b| {
        write!(b, "{}", b.get_function_name(id))
    })?;

    write!(builder, "(")?;
    let names = LocalNames::new(function);

    let mut declared_in_params: IVec<_, _> = 
        function.locals.iter().map(|_| false).collect();

    let mut first = true;
    for &id in function.parameters.iter() {
        if !first {
            write!(builder, ",")?;
        }
        first = false;
        declared_in_params[id] = true;
        let local = function.locals[id].clone();
        emit_decl(builder, local, |b| {
            let name = names.get_local(id);
            write!(b, "{}", name)
        })?;
    }

    write!(builder, "){{")?;

    for (id, local) in function.locals.indexed_iter() {
        if declared_in_params[id] {
            continue;
        }

        emit_decl(builder, local.clone(), |b| {
            let name = names.get_local(id);
            write!(b, "{}", name)
        })?;
        write!(builder, ";")?;
    }

    for (id, temp) in function.temps.indexed_iter() {
        emit_decl(builder, temp.clone(), |b| {
            let name = names.get_temp(id);
            write!(b, "{}", name)
        })?;
        write!(builder, ";")?;
    }

    for stmt in function.body.iter() {
        emit_statement(builder, &names, stmt)?;
    }

    write!(builder, "}}")?;

    Ok(())
}

fn emit_type(builder: &mut Builder, ty: CType) -> std::fmt::Result {
    match ty {
        CType::Int => write!(builder, "int"),
        CType::Char => write!(builder, "char"),
        CType::Void => write!(builder, "void"),
        CType::Struct(struct_id) => {
            let name = builder.get_struct_name(struct_id);
            write!(builder, "{}", name)
        }
        CType::Pointer(ty) => {
            // TODO: this is most likely incorrect
            emit_type(builder, *ty)?;
            write!(builder, "*")
        }
        CType::Function(_, _) => todo!("Cannot emit function type"),
    }
}

fn emit_decl<I>(builder: &mut Builder, ty: CType, ident: I) -> std::fmt::Result
where
    I: FnOnce(&mut Builder) -> std::fmt::Result,
{
    match ty {
        CType::Char | CType::Int | CType::Void | CType::Struct(_) => {
            emit_type(builder, ty)?;
            write!(builder, " ")?;
            ident(builder)
        }
        CType::Pointer(_) => {
            // TODO: this is most likely incorrect
            emit_type(builder, ty)?;
            ident(builder)
        }
        CType::Function(_, _) => todo!("Cannot emit function type"),
    }
}

fn emit_statement<'c>(
    builder: &mut Builder<'c>,
    names: &LocalNames,
    stmt: &'c Statement,
) -> std::fmt::Result {
    match stmt {
        Statement::Expression(expr) => {
            emit_expression(builder, names, &expr.expr, Precedence::Highest, false)?;
        }
        Statement::Return(expr) => {
            write!(builder, "return ")?;
            emit_expression(builder, names, &expr.expr, Precedence::Highest, false)?;
        }
        Statement::ReturnVoid => {
            write!(builder, "return")?;
        }
    }
    write!(builder, ";")
}

fn emit_comma_separated_list<'c>(
    builder: &mut Builder<'c>,
    names: &LocalNames,
    exprs: impl Iterator<Item = &'c Expression>,
) -> std::fmt::Result {
    let mut first = true;
    for expr in exprs {
        if !first {
            write!(builder, ",")?;
        }
        first = false;
        emit_expression(builder, names, expr, Precedence::Comma, true)?;
    }
    Ok(())
}

fn emit_expression<'c>(
    builder: &mut Builder<'c>,
    names: &LocalNames,
    expr: &'c Expression,
    context_prec: Precedence,
    strict: bool,
) -> std::fmt::Result {
    let prec = expr.prec();
    let needs_parens = context_prec < prec || !strict && context_prec == prec;
    if needs_parens {
        write!(builder, "(")?;
    }

    match expr {
        // TODO: lay out integers properly
        Expression::Int { x } => write!(builder, "{}", x)?,
        // TODO: format string properly
        Expression::String { s } => write!(builder, "{:?}", &s[..])?,
        Expression::Local { id } => write!(builder, "{}", names.get_local(*id))?,
        Expression::Temp { id } => write!(builder, "{}", names.get_temp(*id))?,
        Expression::Global { id } => write!(builder, "{}", builder.get_function_name(*id))?,
        Expression::External { id } => write!(builder, "{}", builder.get_external_name(*id))?,
        Expression::Call { f, args } => {
            emit_expression(builder, names, &f.expr, Precedence::SuffixUnary, false)?;
            write!(builder, "(")?;
            emit_comma_separated_list(builder, names, args.iter().map(|x| &x.expr))?;
            write!(builder, ")")?
        }
        Expression::Assign { lhs, rhs } => {
            write!(builder, "{}=", names.get_local(*lhs))?;
            emit_expression(builder, names, &rhs.expr, Precedence::Assign, false)?;
        }
        Expression::AssignTemp { lhs, rhs } => {
            write!(builder, "{}=", names.get_temp(*lhs))?;
            emit_expression(builder, names, &rhs.expr, Precedence::Assign, false)?;
        }
        Expression::Plus { lhs, rhs } => {
            emit_expression(builder, names, &lhs.expr, Precedence::Add, false)?;
            write!(builder, "+")?;
            emit_expression(builder, names, &rhs.expr, Precedence::Add, true)?;
        }
        Expression::Minus { lhs, rhs } => {
            emit_expression(builder, names, &lhs.expr, Precedence::Add, false)?;
            write!(builder, "-")?;
            emit_expression(builder, names, &rhs.expr, Precedence::Add, true)?;
        }
        Expression::Multiply { lhs, rhs } => {
            emit_expression(builder, names, &lhs.expr, Precedence::Multiply, false)?;
            write!(builder, "*")?;
            emit_expression(builder, names, &rhs.expr, Precedence::Multiply, true)?;
        }
        Expression::UnaryPlus { x } => {
            // it's always a no-op, right?
            // write!(builder, "+")?;
            emit_expression(builder, names, &x.expr, Precedence::PrefixUnary, true)?;
        }
        Expression::UnaryMinus { x } => {
            write!(builder, "-")?;
            emit_expression(builder, names, &x.expr, Precedence::PrefixUnary, true)?;
        }
        Expression::StructAccess { x, field } => {
            emit_expression(builder, names, &x.expr, Precedence::SuffixUnary, false)?;
            let field = builder.get_struct_field(*field);
            write!(builder, ".{}", field)?;
        }
        Expression::StructBuild { id, fields } => {
            write!(builder, "(")?;
            write!(builder, "{}", builder.get_struct_name(*id))?;
            write!(builder, "){{")?;
            emit_comma_separated_list(builder, names, fields.iter().map(|f| &f.expr))?;
            write!(builder, "}}")?;
        }
    }

    if needs_parens {
        write!(builder, ")")?;
    }

    Ok(())
}
