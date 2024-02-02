use crate::ast::*;
use crate::lexer::*;

type ParsingResult<T> = Result<T, ParsingError>;

macro_rules! eat {
    ($lex:expr, $pattern:pat) => {
        eat!($lex, $pattern => ())
    };

    ($lex:expr, $($pattern:pat => $result:expr),+) => {
        match $lex.next() {
            $(Some((Ok($pattern), span)) => Ok(Ast($result, span.into())),)+
            #[allow(unreachable_patterns)]
            Some((Ok(..), span)) => Err(ParsingError::UnexpectedToken(
                format!("Unexpected token, expected {:?}", [$(stringify!($pattern)),+]),
                span.into()
            )),
            Some((Err(..), span)) => Err(ParsingError::UnknownToken(
                format!("Unknown token, expected {:?}", [$(stringify!($pattern),)+]),
                span.into()
            )),
            None => Err(ParsingError::UnexpectedEof),
        }
    };
}

macro_rules! maybe_eat {
    ($lex:expr, $($pattern:pat, $span:ident => $result:expr),+) => {
        match $lex.peek() {
            $(
                #[allow(unused_variables)]
                Some((Ok($pattern), _)) => {
                    match $lex.next() {
                        Some((Ok($pattern), span)) => {
                            let $span: crate::span::Span = span.into();
                            Ok(Some($result))
                        }
                        _ => unreachable!(),
                    }
                }
            )+
            Some((Ok(..), _)) => Ok(None),
            Some((Err(..), span)) => Err(ParsingError::UnknownToken(
                format!("Unknown token, expected {:?}", [$(stringify!($pattern),)+]),
                span.clone().into()
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
                span.clone().into()
            )),
            None => Err(ParsingError::UnexpectedEof),
        }
    };
}

pub fn parse_program(lex: &mut Lex) -> ParsingResult<Module> {
    let mut items = Vec::new();

    while lex.peek().is_some() {
        items.push(parse_item(lex)?);
    }

    return Ok(Module { items });
}

fn parse_item(lex: &mut Lex) -> ParsingResult<Ast<Function>> {
    let start = eat!(lex, Token::Fn)?.1;
    let ident = eat!(lex, Token::Ident(s) => s)?.0.to_string();
    eat!(lex, Token::LParen)?;
    eat!(lex, Token::RParen)?;
    let body @ Ast(_, end) = parse_block(lex)?;
    Ok(Ast(Function { ident, body }, start.merge(end)))
}

fn parse_block(lex: &mut Lex) -> ParsingResult<Ast<Expression>> {
    let start = eat!(lex, Token::LBrace)?.1;
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
            let span = eat!(lex, _)?.1;
            return Err(ParsingError::UnexpectedToken(
                "Expected semicolon or closing brace".to_string(),
                span,
            ));
        }
    }

    let end = eat!(lex, Token::RBrace)?.1;
    Ok(Ast(Expression::Block(expressions, last_expr.map(Box::new)), start.merge(end)))
}

fn parse_expression(lex: &mut Lex) -> ParsingResult<Ast<Expression>> {
    let Ast(expr, start) = parse_primary(lex)?;
    if let Some(Ast(..)) = maybe_eat!(lex, Token::Colon, span => Ast((), span))? {
        let ident = match expr {
            Expression::Ident(s) => Ast(s, start),
            _ => return Err(ParsingError::IncorrectPattern(
                "Expected identifier".to_string(),
                expr,
            )),
        };
        eat!(lex, Token::Eq)?;
        let expr @ Ast(_, end) = parse_expression(lex)?;
        return Ok(Ast(Expression::Definition {
            ident,
            expr: Box::new(expr),
        }, start.merge(end)));
    }

    let mut expr_stack: Vec<(Ast<Expression>, Option<(Ast<BinaryOp>, Precedence)>)> =
        vec![(Ast(expr, start), None)];

    while let Some(op) = maybe_eat!(
        lex,
        Token::Plus, span => Ast(BinaryOp::Add, span),
        Token::Minus, span => Ast(BinaryOp::Sub, span),
        Token::Star, span => Ast(BinaryOp::Mul, span),
        Token::Eq, span => Ast(BinaryOp::Assign, span)
    )? {
        let prec = op.0.precedence();
        let rhs = parse_primary(lex)?;
        while let Some((_, Some((_, p)))) = expr_stack.last() {
            if prec > *p {
                break;
            }

            let (prev_rhs, op_data) = expr_stack.pop().unwrap();
            let (prev_lhs, prev_op) = expr_stack.pop().unwrap();
            let start = prev_lhs.1;
            let end = prev_rhs.1;
            expr_stack.push((Ast(
                Expression::Binary {
                    op: op_data.unwrap().0,
                    lhs: Box::new(prev_lhs),
                    rhs: Box::new(prev_rhs),
                },
                start.merge(end),
            ), prev_op));
        }

        expr_stack.push((rhs, Some((op, prec))));
    }

    while let Some((_, Some(_))) = expr_stack.last() {
        let (prev_rhs, op_data) = expr_stack.pop().unwrap();
        let (prev_lhs, prev_op) = expr_stack.pop().unwrap();
        let start = prev_lhs.1;
        let end = prev_rhs.1;
        expr_stack.push((
            Ast(Expression::Binary {
                op: op_data.unwrap().0,
                lhs: Box::new(prev_lhs),
                rhs: Box::new(prev_rhs),
            }, start.merge(end)),
            prev_op,
        ));
    }

    assert!(matches!(expr_stack[..], [(_, None)]));
    Ok(expr_stack.pop().unwrap().0)
}

fn parse_primary(lex: &mut Lex) -> ParsingResult<Ast<Expression>> {
    let expr = if let Some(e) = maybe_eat!(lex,
        Token::Number(s), span => Ast(Expression::Literal(Literal::Number(s.parse().unwrap())), span),
        Token::String(s), span => Ast(Expression::Literal(Literal::String(s.to_string())), span),
        Token::Ident(s), span => Ast(Expression::Ident(s.to_string()), span),
        Token::Plus, start => {
            let expr = parse_primary(lex)?;
            let end = expr.1;
            Ast(Expression::Unary {
                op: Ast(UnaryOp::Pos, start),
                expr: Box::new(expr),
            }, start.merge(end))
        },
        Token::Minus, start => {
            let expr = parse_primary(lex)?;
            let end = expr.1;
            Ast(Expression::Unary {
                op: Ast(UnaryOp::Neg, start),
                expr: Box::new(expr),
            }, start.merge(end))
        },
        Token::LParen, start => {
            let Ast(expr, _) = parse_expression(lex)?;
            let end = eat!(lex, Token::RParen)?.1;
            Ast(expr, start.merge(end))
        }
    )? {
        e
    } else if peek!(lex, Token::LBrace)? {
        parse_block(lex)?
    } else {
        let tok = eat!(lex, _)?;
        return Err(ParsingError::UnexpectedToken(
            "Expected expression".to_string(),
            tok.1,
        ));
    };

    while peek!(lex, Token::LParen)? {
        let start = expr.1;
        let mut args = Vec::new();
        eat!(lex, Token::LParen)?;
        while !peek!(lex, Token::RParen)? {
            args.push(parse_expression(lex)?);
        }
        let end = eat!(lex, Token::RParen)?.1;
        return Ok(Ast(Expression::Call {
            callee: Box::new(expr),
            args,
        }, start.merge(end)));
    }

    Ok(expr)
}
