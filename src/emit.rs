use std::fmt::Write;
use crate::c::{CType, Expression, Function, GlobalId, LocalId, Module, ParamId, Precedence, Statement, Struct, StructId, TempId};

const ALPHABET: &'static [u8] =
    b"abcdefghijklmnopqrstuvwxyz\
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
        format!(
            "{}",
            nice_char(offset + n)
        )
    } else if n < range * ALPHABET_LEN {
        format!(
            "{}{}",
            nice_char(offset + n % range),
            nice_char(n / range)
        )
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
        if matches!(
            &result[..],
            "if" | "for" | "do" | "int"
        ) {
            self.next()
        } else {
            Some(result)
        }
    }
}

struct Builder<'c> {
    result: Vec<u8>,
    module: &'c Module,
    struct_names: Vec<String>,
    function_names: Vec<String>,
    struct_fields: Vec<String>,
    struct_field_namer: Namer,
}

impl<'c> Builder<'c> {
    fn new(module: &'c Module) -> Self {
        let mut namer = Namer::new(GLOBAL_NAMES_OFFSET, GLOBAL_NAMES_RANGE);
        let struct_names = module.structs.iter().map(|_| namer.next().unwrap()).collect();
        let function_names = module.functions.iter().map(|_| namer.next().unwrap()).collect();

        Self {
            result: Vec::new(),
            module,
            struct_names,
            function_names,
            struct_fields: Vec::new(),
            struct_field_namer: Namer::new(STRUCT_NAMES_OFFSET, STRUCT_NAMES_RANGE),
        }
    }

    fn commit(self) -> String {
        String::from_utf8(self.result).unwrap()
    }

    fn get_struct_name(&self, struct_id: StructId) -> String {
        self.struct_names[struct_id.0].clone()
    }

    fn get_function_name(&self, function_id: GlobalId) -> String {
        self.function_names[function_id.0].clone()
    }

    fn get_struct_field(&mut self, field_no: usize) -> String {
        while field_no >= self.struct_fields.len() {
            self.struct_fields.push(self.struct_field_namer.next().unwrap());
        }
        self.struct_fields[field_no].clone()
    }
}

struct LocalNames {
    params: Vec<String>,
    locals: Vec<String>,
    temps: Vec<String>,
}

impl LocalNames {
    fn new(function: &Function) -> Self {
        let mut namer = Namer::new(LOCAL_NAMES_OFFSET, LOCAL_NAMES_RANGE);
        let params = function.parameters.iter().map(|_| namer.next().unwrap()).collect();
        let locals = function.locals.iter().map(|_| namer.next().unwrap()).collect();
        let temps = function.temps.iter().map(|_| namer.next().unwrap()).collect();

        Self {
            params,
            locals,
            temps,
        }
    }

    fn get_param(&self, param_id: ParamId) -> String {
        self.params[param_id.0].clone()
    }

    fn get_local(&self, local_id: LocalId) -> String {
        self.locals[local_id.0].clone()
    }

    fn get_temp(&self, temp_id: TempId) -> String {
        self.temps[temp_id.0].clone()
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

    for (id, struct_) in module.structs.iter().enumerate() {
        emit_struct(builder, StructId(id), struct_)?;
    }

    for (id, function) in module.functions.iter().enumerate() {
        emit_function(builder, GlobalId(id), function)?;
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
    for field in struct_.fields.iter().cloned() {
        emit_decl(builder, field, |builder| {
            let name = builder.get_struct_field(0);
            write!(builder, "{}", name)
        })?;
    }
    write!(builder, "}}{};", builder.get_struct_name(id))?;
    Ok(())
}

fn emit_function<'c>(
    builder: &mut Builder<'c>,
    id: GlobalId,
    function: &'c Function
) -> std::fmt::Result {
    emit_decl(builder, function.return_type.clone(), |b| {
        write!(b, "{}", b.get_function_name(id))
    })?;

    write!(builder, "(")?;
    let names = LocalNames::new(function);

    for (i, arg) in function.parameters.iter().enumerate() {
        emit_decl(builder, arg.clone(), |b| {
            let name = names.get_param(ParamId(i));
            write!(b, "{}", name)
        })?;
        if i != function.parameters.len() - 1 {
            write!(builder, ",")?;
        }
    }

    write!(builder, "){{")?;

    for (i, local) in function.locals.iter().enumerate() {
        emit_decl(builder, local.clone(), |b| {
            let name = names.get_local(LocalId(i));
            write!(b, "{}", name)
        })?;
        write!(builder, ";")?;
    }

    for (i, temp) in function.temps.iter().enumerate() {
        emit_decl(builder, temp.clone(), |b| {
            let name = names.get_temp(TempId(i));
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
        },
        CType::Pointer(ty) => {
            // TODO: this is most likely incorrect
            emit_type(builder, *ty)?;
            write!(builder, "*")
        }
    }
}

fn emit_decl<I>(builder: &mut Builder, ty: CType, ident: I) -> std::fmt::Result
    where I: FnOnce(&mut Builder) -> std::fmt::Result
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
    }
}

fn emit_statement<'c>(
    builder: &mut Builder<'c>,
    names: &LocalNames,
    stmt: &'c Statement
) -> std::fmt::Result {
    match stmt {
        Statement::Expression(expr) => {
            emit_expression(builder, names, expr, Precedence::Highest, false)?;
        },
        Statement::Return(expr) => {
            write!(builder, "return ")?;
            emit_expression(builder, names, expr, Precedence::Highest, false)?;
        },
    }
    write!(builder, ";")
}

fn emit_comma_separated_list<'c>(
    builder: &mut Builder<'c>,
    names: &LocalNames,
    exprs: impl Iterator<Item=&'c Expression>,
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
    strict: bool
) -> std::fmt::Result {
    let prec = expr.prec();
    let needs_parens = context_prec < prec || !strict && context_prec == prec;
    if needs_parens {
        write!(builder, "(")?;
    }

    match expr {
        // TODO: lay out integers properly
        Expression::Int(i) => write!(builder, "{}", i)?,
        // TODO: format string properly
        Expression::String(s) => write!(builder, "{:?}", &s[..])?,
        Expression::Local(id) => write!(builder, "{}", names.get_local(*id))?,
        Expression::Temp(id) => write!(builder, "{}", names.get_temp(*id))?,
        Expression::Param(id) => write!(builder, "{}", names.get_param(*id))?,
        Expression::Global(id) => write!(builder, "{}", builder.get_function_name(*id))?,
        Expression::Call(id, args) => {
            write!(builder, "{}(", builder.get_function_name(*id))?;
            emit_comma_separated_list(builder, names, args.iter())?;
            write!(builder, ")")?
        },
        Expression::ExternalCall(id, args) => {
            write!(builder, "{}(", builder.module.externals[id.0].name)?;
            emit_comma_separated_list(builder, names, args.iter())?;
            write!(builder, ")")?
        },
        Expression::DynamicCall(func, args) => {
            emit_expression(builder, names, func, Precedence::SuffixUnary, false)?;
            write!(builder, "(")?;
            emit_comma_separated_list(builder, names, args.iter())?;
            write!(builder, ")")?
        },
        Expression::Assign(id, expr) => {
            write!(builder, "{}=", names.get_local(*id))?;
            emit_expression(builder, names, expr, Precedence::Assign, false)?;
        },
        Expression::AssignTemp(id, expr) => {
            write!(builder, "{}=", names.get_temp(*id))?;
            emit_expression(builder, names, expr, Precedence::Assign, false)?;
        },
        Expression::Plus(lhs, rhs) => {
            emit_expression(builder, names, lhs, Precedence::Add, false)?;
            write!(builder, "+")?;
            emit_expression(builder, names, rhs, Precedence::Add, true)?;
        },
        Expression::Minus(lhs, rhs) => {
            emit_expression(builder, names, lhs, Precedence::Add, false)?;
            write!(builder, "-")?;
            emit_expression(builder, names, rhs, Precedence::Add, true)?;
        },
        Expression::Multiply(lhs, rhs) => {
            emit_expression(builder, names, lhs, Precedence::Multiply, false)?;
            write!(builder, "*")?;
            emit_expression(builder, names, rhs, Precedence::Multiply, true)?;
        },
        Expression::UnaryPlus(expr) => {
            write!(builder, "+")?;
            emit_expression(builder, names, expr, Precedence::PrefixUnary, true)?;
        },
        Expression::UnaryMinus(expr) => {
            write!(builder, "-")?;
            emit_expression(builder, names, expr, Precedence::PrefixUnary, true)?;
        },
        Expression::StructAccess(expr, field_no) => {
            emit_expression(builder, names, expr, Precedence::SuffixUnary, false)?;
            let field = builder.get_struct_field(*field_no);
            write!(builder, ".{}", field)?;
        },
        Expression::StructBuild(_struct_id, fields) => {
            write!(builder, "{{")?;
            emit_comma_separated_list(builder, names, fields.iter())?;
            write!(builder, "}}")?;
        },

    }

    if needs_parens {
        write!(builder, ")")?;
    }

    Ok(())
}
