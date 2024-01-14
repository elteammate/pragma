#[derive(Debug)]
pub struct Module {
    pub constants: Vec<Constant>,
    pub functions: Vec<Function>,
}

#[derive(Debug, Copy, Clone)]
pub struct LocalId(pub usize);

#[derive(Debug, Copy, Clone)]
pub struct ConstId(pub usize);

#[derive(Debug, Copy, Clone)]
pub enum GlobalId {
    Intrinsic(usize),
    Function(usize),
}

#[derive(Debug)]
pub struct Function {
    pub locals: usize,
    pub ident: String,
    pub body: Expression,
}

#[derive(Debug)]
pub enum Constant {
    String(String),
    Int(i64),
    Unit,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Neg,
    Pos,
}

#[derive(Debug)]
pub enum Expression {
    Block(Vec<Expression>, Box<Expression>),
    Method {
        object: Box<Expression>,
        name: String,
        args: Vec<Expression>,
    },
    Const(ConstId),
    Local(LocalId),
    Global(GlobalId),
    Assign {
        var: LocalId,
        expr: Box<Expression>,
    },
}

mod pretty_print {
    use super::*;
    use std::fmt::*;

    impl Display for Module {
        fn fmt(&self, f: &mut Formatter) -> Result {
            writeln!(f, "Constants:")?;
            for (i, constant) in self.constants.iter().enumerate() {
                writeln!(f, "c{} = {}", i, constant)?;
            }

            writeln!(f, "Functions:")?;
            for (i, function) in self.functions.iter().enumerate() {
                writeln!(f, "f{} = fn {} {{", i, function.ident)?;
                for local in 0..function.locals {
                    writeln!(f, "    let l{};", local)?;
                }
                function.body.write(f, self, 0)?;
                writeln!(f, "}}")?;
            }
            Ok(())
        }
    }

    impl Display for LocalId {
        fn fmt(&self, f: &mut Formatter) -> Result {
            write!(f, "l{}", self.0)
        }
    }

    impl ConstId {
        fn write(&self, f: &mut Formatter, module: &Module) -> Result {
            write!(f, "c{}({})", self.0, module.constants[self.0])
        }
    }

    impl GlobalId {
        fn write(&self, f: &mut Formatter, module: &Module) -> Result {
            match self {
                GlobalId::Intrinsic(i) => write!(f, "i{}", i),
                GlobalId::Function(i) => write!(f, "f{}({})", i, module.functions[*i].ident),
            }
        }
    }

    impl Display for Constant {
        fn fmt(&self, f: &mut Formatter) -> Result {
            match self {
                Constant::String(s) => write!(f, "\"{}\"", s),
                Constant::Int(n) => write!(f, "{}", n),
                Constant::Unit => write!(f, "()"),
            }
        }
    }

    impl Display for BinaryOp {
        fn fmt(&self, f: &mut Formatter) -> Result {
            match self {
                BinaryOp::Add => write!(f, "+"),
                BinaryOp::Sub => write!(f, "-"),
                BinaryOp::Mul => write!(f, "*"),
            }
        }
    }

    impl Display for UnaryOp {
        fn fmt(&self, f: &mut Formatter) -> Result {
            match self {
                UnaryOp::Neg => write!(f, "-"),
                UnaryOp::Pos => write!(f, "+"),
            }
        }
    }

    impl Expression {
        pub fn write(&self, f: &mut Formatter, module: &Module, offset: usize) -> Result {
            match self {
                Expression::Block(exprs, expr) => {
                    writeln!(f, "{{")?;
                    for expr in exprs {
                        for _ in 0..offset + 1 {
                            write!(f, "    ")?;
                        }
                        expr.write(f, module, offset + 1)?;
                        writeln!(f, ";")?;
                    }
                    for _ in 0..offset + 1 {
                        write!(f, "    ")?;
                    }
                    expr.write(f, module, offset + 1)?;
                    write!(f, "\n")?;
                    for _ in 0..offset {
                        write!(f, "    ")?;
                    }
                    write!(f, "}}")
                }
                Expression::Method { object, name, args } => {
                    object.write(f, module, offset)?;
                    write!(f, ".{}(", name)?;
                    for arg in args {
                        arg.write(f, module, offset)?;
                        write!(f, ", ")?;
                    }
                    write!(f, ")")
                }
                Expression::Const(id) => id.write(f, module),
                Expression::Local(id) => write!(f, "{}", id),
                Expression::Global(id) => id.write(f, module),
                Expression::Assign { var, expr } => {
                    write!(f, "{} = ", var)?;
                    expr.write(f, module, offset)
                }
            }
        }
    }
}
