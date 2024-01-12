use std::iter::Peekable;
use logos::{Logos, Span, SpannedIter};

#[derive(logos::Logos, Debug, PartialEq)]
#[logos(skip r"\s+")]
enum Token<'s> {
    #[token(";")]
    Semi,
    #[token(":")]
    Colon,
    #[token("=")]
    Eq,
    #[token("fn")]
    Fn,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[regex(r"\d+")]
    Number(&'s str),
    #[regex(r#""(\\"|\\n|\\t|\\r|[^"\n])*""#, |lex| {
        let s = lex.slice();
        let s = &s[1..s.len()-1];
        s
    })]
    String(&'s str),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'s str),
}

#[derive(Debug)]
struct Module {
    items: Vec<Item>,
}

#[derive(Debug)]
enum Item {
    Function {
        ident: String,
        body: Expression,
    },
}

#[derive(Debug, Copy, Clone)]
enum BinaryOp {
    Add, Sub, Mul, Assign
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
enum Precedence {
    Lowest,
    Assign,
    Add,
    Mul,
    Unary,
}

impl BinaryOp {
    fn precedence(&self) -> Precedence {
        match self {
            BinaryOp::Assign => Precedence::Assign,
            BinaryOp::Add | BinaryOp::Sub => Precedence::Add,
            BinaryOp::Mul => Precedence::Mul,
        }
    }
}

#[derive(Debug)]
enum UnaryOp {
    Neg, Pos
}

#[derive(Debug)]
enum Expression {
    Block(Vec<Expression>, Option<Box<Expression>>),
    Call {
        callee: Box<Expression>,
        args: Vec<Expression>,
    },
    Literal(Literal),
    Ident(String),
    Binary {
        op: BinaryOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Definition {
        ident: String,
        expr: Box<Expression>,
    },
}

#[derive(Debug)]
enum Literal {
    Number(i64),
    String(String),
}

#[derive(Debug)]
enum ParsingError {
    UnknownToken(String, Span),
    UnexpectedToken(String, Span),
    IncorrectPattern(String, Expression),
    UnexpectedEof,
}

type ParsingResult<T> = Result<T, ParsingError>;
type Lex<'s> = Peekable<SpannedIter<'s, Token<'s>>>;

macro_rules! eat {
    ($lex:expr, $pattern:pat) => {
        eat!($lex, $pattern => ())
    };

    ($lex:expr, $($pattern:pat => $result:expr),+) => {
        match $lex.next() {
            $(Some((Ok($pattern), _)) => Ok($result),)+
            Some((Ok(..), span)) => Err(ParsingError::UnexpectedToken(
                format!("Unexpected token, expected {:?}", [$(stringify!($pattern)),+]),
                span
            )),
            Some((Err(..), span)) => Err(ParsingError::UnknownToken(
                format!("Unknown token, expected {:?}", [$(stringify!($pattern),)+]),
                span
            )),
            None => Err(ParsingError::UnexpectedEof),
        }
    };
}

macro_rules! maybe_eat {
    ($lex:expr, $($pattern:pat => $result:expr),+) => {
        match $lex.peek() {
            $(
                #[allow(unused_variables)]
                Some((Ok($pattern), _)) => {
                    match $lex.next() {
                        Some((Ok($pattern), _)) => Ok(Some($result)),
                        _ => unreachable!(),
                    }
                },
            )+
            Some((Ok(..), _)) => Ok(None),
            Some((Err(..), span)) => Err(ParsingError::UnknownToken(
                format!("Unknown token, expected {:?}", [$(stringify!($pattern),)+]),
                span.clone()
            )),
            None => Ok(None),
        }
    };
}

macro_rules! peek {
    ($lex:expr, $pattern:pat) => {
        match $lex.peek() {
            Some((Ok($pattern), _)) => Ok(true),
            Some((Ok(..), _)) => Ok(false),
            Some((Err(..), span)) => Err(ParsingError::UnknownToken(
                format!("Unknown token, expected {:?}", stringify!($pattern)),
                span.clone()
            )),
            None => Err(ParsingError::UnexpectedEof),
        }
    };
}

fn parse_program(lex: &mut Lex) -> ParsingResult<Module> {
    let mut items = Vec::new();

    while lex.peek().is_some() {
        items.push(parse_item(lex)?);
    }

    return Ok(Module { items });
}

fn parse_item(lex: &mut Lex) -> ParsingResult<Item> {
    eat!(lex, Token::Fn)?;
    let ident = eat!(lex, Token::Ident(s) => s)?.to_string();
    eat!(lex, Token::LParen)?;
    eat!(lex, Token::RParen)?;
    let body = parse_block(lex)?;
    Ok(Item::Function { ident, body })
}

fn parse_block(lex: &mut Lex) -> ParsingResult<Expression> {
    eat!(lex, Token::LBrace)?;
    let mut expressions = Vec::new();
    let mut last_expr = None;

    while !peek!(lex, Token::RBrace)? {
        let expr = parse_expression(lex)?;
        if peek!(lex, Token::Semi)? {
            eat!(lex, Token::Semi)?;
            expressions.push(expr);
        } else if peek!(lex, Token::RBrace)? {
            last_expr = Some(expr);
            break;
        } else {
            return Err(ParsingError::UnexpectedToken(
                "Expected semicolon or closing brace".to_string(),
                lex.peek().unwrap().1.clone()
            ));
        }
    }

    eat!(lex, Token::RBrace)?;
    Ok(Expression::Block(expressions, last_expr.map(Box::new)))
}

fn parse_expression(lex: &mut Lex) -> ParsingResult<Expression> {
    let expr = parse_primary(lex)?;
    if let Some(()) = maybe_eat!(lex, Token::Colon => ())? {
        let ident = match expr {
            Expression::Ident(s) => s,
            _ => return Err(ParsingError::IncorrectPattern(
                "Expected identifier".to_string(),
                expr
            )),
        };
        eat!(lex, Token::Eq)?;
        let expr = parse_expression(lex)?;
        return Ok(Expression::Definition {
            ident,
            expr: Box::new(expr),
        });
    }

    let mut expr_stack = vec![(expr, None)];

    while let Some(op) = maybe_eat!(
        lex,
        Token::Plus => BinaryOp::Add,
        Token::Minus => BinaryOp::Sub,
        Token::Star => BinaryOp::Mul,
        Token::Eq => BinaryOp::Assign
    )? {
        let prec = op.precedence();
        let rhs = parse_primary(lex)?;
        while let Some((_, Some((_, p)))) = expr_stack.last() {
            if prec > *p {
                break
            }

            let (prev_rhs, op_data) = expr_stack.pop().unwrap();
            let (prev_lhs, prev_op) = expr_stack.pop().unwrap();
            expr_stack.push((
                Expression::Binary {
                    op: op_data.unwrap().0,
                    lhs: Box::new(prev_lhs),
                    rhs: Box::new(prev_rhs),
                },
                prev_op,
            ));
        }

        expr_stack.push((rhs, Some((op, prec))));
    }

    while let Some((_, Some(_))) = expr_stack.last() {
        let (prev_rhs, op_data) = expr_stack.pop().unwrap();
        let (prev_lhs, prev_op) = expr_stack.pop().unwrap();
        expr_stack.push((
            Expression::Binary {
                op: op_data.unwrap().0,
                lhs: Box::new(prev_lhs),
                rhs: Box::new(prev_rhs),
            },
            prev_op,
        ));
    }

    assert!(matches!(expr_stack[..], [(_, None)]));
    Ok(expr_stack.pop().unwrap().0)
}

fn parse_primary(lex: &mut Lex) -> ParsingResult<Expression> {
    let expr = if let Some(e) = maybe_eat!(lex,
        Token::Number(s) => Expression::Literal(Literal::Number(s.parse().unwrap())),
        Token::String(s) => Expression::Literal(Literal::String(s.to_string())),
        Token::Ident(s) => Expression::Ident(s.to_string()),
        Token::Plus => {
            let expr = parse_primary(lex)?;
            Expression::Unary {
                op: UnaryOp::Pos,
                expr: Box::new(expr),
            }
        },
        Token::Minus => {
            let expr = parse_primary(lex)?;
            Expression::Unary {
                op: UnaryOp::Neg,
                expr: Box::new(expr),
            }
        },
        Token::LParen => {
            let expr = parse_expression(lex)?;
            eat!(lex, Token::RParen)?;
            expr
        }
    )? {
        e
    } else if peek!(lex, Token::LBrace)? {
        parse_block(lex)?
    } else {
        return Err(ParsingError::UnexpectedToken(
            "Expected expression".to_string(),
            lex.peek().unwrap().1.clone()
        ))
    };

    while peek!(lex, Token::LParen)? {
        let mut args = Vec::new();
        eat!(lex, Token::LParen)?;
        while !peek!(lex, Token::RParen)? {
            args.push(parse_expression(lex)?);
        }
        eat!(lex, Token::RParen)?;
        return Ok(Expression::Call {
            callee: Box::new(expr),
            args,
        });
    }

    Ok(expr)
}

fn main() {
    let program = std::fs::read_to_string("programs/hello.pragma").unwrap();

    let mut lex = Token::lexer(&program)
        .spanned()
        .peekable();

    match parse_program(&mut lex) {
        Ok(program) => println!("{:#?}", program),
        Err(ParsingError::UnexpectedEof) => println!("Unexpected end of file"),
        Err(ParsingError::UnknownToken(s, span)) => {
            println!("Unknown token: {} at {:?} ({:?})", s, span.clone(), &program[span]);
        },
        Err(ParsingError::UnexpectedToken(s, span)) => {
            println!("Unexpected token: {} at {:?} ({:?})", s, span.clone(), &program[span]);
        },
        Err(ParsingError::IncorrectPattern(s, expr)) => {
            println!("Incorrect pattern: {} ({:?})", s, expr);
        },
    }
}
