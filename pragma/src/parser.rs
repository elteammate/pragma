use crate::ast::*;
use crate::lexer::*;

type ParsingResult<T> = Result<T, ParsingError>;

macro_rules! eat {
    ($lex:expr, $pattern:pat) => {
        eat!($lex, $pattern, span => Ast((), span))
    };

    ($lex:expr, $($pattern:pat, $span:ident => $result:expr),+) => {
        match $lex.next() {
            $(Some((Ok($pattern), span)) => {
                let $span: crate::span::Span = span.into();
                Ok($result)
            },)+
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
    ($lex:expr, $($pattern:pat, $span:ident => $result:expr),+ $(,)?) => {
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
                span.clone().into(),
            )),
            None => Err(ParsingError::UnexpectedEof),
        }
    };
}

pub fn parse_program(lex: &mut Lex) -> ParsingResult<Module> {
    let mut items = Vec::new();

    while lex.peek().is_some() {
        if peek!(lex, Token::Fn)? {
            items.push(parse_function(lex)?);
        } else {
            let span = eat!(lex, _)?.1;
            return Err(ParsingError::UnexpectedToken(
                "Expected function definition".to_string(),
                span,
            ));
        }
    }

    Ok(Module { items })
}

fn extract_pattern(expr: Ast<Expression>) -> ParsingResult<Ast<String>> {
    let Ast(expr, span) = expr;
    match expr {
        Expression::Ident(s) => Ok(Ast(s, span)),
        _ => Err(ParsingError::IncorrectPattern(
            "Expected identifier".to_string(),
            expr,
        )),
    }
}

fn parse_function(lex: &mut Lex) -> ParsingResult<Ast<Function>> {
    let start = eat!(lex, Token::Fn)?.1;
    let ident = eat!(lex, Token::Ident(s), span => Ast(s.to_string(), span))?;

    let mut args = Vec::new();
    eat!(lex, Token::LParen)?;
    while !peek!(lex, Token::RParen)? {
        let arg_name = extract_pattern(parse_primary(lex)?)?;
        eat!(lex, Token::Colon)?;
        let arg_ty = parse_type(lex)?;

        let arg_span = arg_name.1.merge(arg_ty.1);
        let arg = Ast(Argument { ident: arg_name, ty: arg_ty }, arg_span);
        args.push(arg);

        if peek!(lex, Token::RParen)? {
            break;
        }
        eat!(lex, Token::Comma)?;
    }
    eat!(lex, Token::RParen)?;

    let return_type = if peek!(lex, Token::Colon)? {
        eat!(lex, Token::Colon)?;
        Some(parse_type(lex)?)
    } else {
        None
    };

    let body @ Ast(_, end) = parse_block(lex)?;
    Ok(Ast(Function {
        ident,
        return_ty: return_type,
        args,
        body,
    }, start.merge(end)))
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
    Ok(Ast(
        Expression::Block(expressions, last_expr.map(Box::new)),
        start.merge(end),
    ))
}

fn parse_expression(lex: &mut Lex) -> ParsingResult<Ast<Expression>> {
    let Ast(expr, start) = parse_primary(lex)?;
    if let Some(Ast(..)) = maybe_eat!(lex, Token::Colon, span => Ast((), span))? {
        let ident = extract_pattern(Ast(expr, start))?;

        let ty = if !peek!(lex, Token::Eq)? {
            Some(parse_type(lex)?)
        } else {
            None
        };

        eat!(lex, Token::Eq)?;
        let expr @ Ast(_, end) = parse_expression(lex)?;
        return Ok(Ast(
            Expression::Definition {
                ident,
                ty,
                expr: Box::new(expr),
            },
            start.merge(end),
        ));
    }

    let mut expr_stack = vec![(Ast(expr, start), None)];

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
            expr_stack.push((
                Ast(
                    Expression::Binary {
                        op: op_data.unwrap().0,
                        lhs: Box::new(prev_lhs),
                        rhs: Box::new(prev_rhs),
                    },
                    start.merge(end),
                ),
                prev_op,
            ));
        }

        expr_stack.push((rhs, Some((op, prec))));
    }

    while let Some((_, Some(_))) = expr_stack.last() {
        let (prev_rhs, op_data) = expr_stack.pop().unwrap();
        let (prev_lhs, prev_op) = expr_stack.pop().unwrap();
        let start = prev_lhs.1;
        let end = prev_rhs.1;
        expr_stack.push((
            Ast(
                Expression::Binary {
                    op: op_data.unwrap().0,
                    lhs: Box::new(prev_lhs),
                    rhs: Box::new(prev_rhs),
                },
                start.merge(end),
            ),
            prev_op,
        ));
    }

    assert!(matches!(expr_stack[..], [(_, None)]));
    Ok(expr_stack.pop().unwrap().0)
}

fn parse_type(lex: &mut Lex) -> ParsingResult<Ast<TypeExpr>> {
    eat!(lex,
        Token::Ident(s), span => Ast(TypeExpr::Ident(s.to_string()), span),
        Token::LParen, start => {
            let end = eat!(lex, Token::RParen)?.1;
            Ast(TypeExpr::Unit, start.merge(end))
        }
    )
}

fn parse_primary(lex: &mut Lex) -> ParsingResult<Ast<Expression>> {
    let expr = if let Some(e) = maybe_eat!(lex,
        Token::Number(s), span => Ast(Expression::Literal(Literal::Number(s.parse().unwrap())), span),
        Token::String(s), span => Ast(Expression::Literal(Literal::String(s.to_string())), span),
        Token::Ident(s), span => Ast(Expression::Ident(s.to_string()), span),
        Token::Uninit, span => Ast(Expression::Literal(Literal::Uninit), span),
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
            if peek!(lex, Token::RParen)? {
                let end = eat!(lex, Token::RParen)?.1;
                Ast(Expression::Literal(Literal::Unit), start.merge(end))
            } else {
                let Ast(expr, _) = parse_expression(lex)?;
                let end = eat!(lex, Token::RParen)?.1;
                Ast(expr, start.merge(end))
            }
        },
        Token::Return, start => {
            if peek!(lex, Token::Semi | Token::RBrace | Token::RParen)? {
                Ast(Expression::ReturnUnit, start)
            } else {
                let expr = parse_expression(lex)?;
                let end = expr.1;
                Ast(Expression::Return(Box::new(expr)), start.merge(end))
            }
        },
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

    if peek!(lex, Token::LParen)? {
        let start = expr.1;
        let mut args = Vec::new();
        eat!(lex, Token::LParen)?;
        while !peek!(lex, Token::RParen)? {
            args.push(parse_expression(lex)?);
            if peek!(lex, Token::RParen)? {
                break;
            }
            eat!(lex, Token::Comma)?;
        }
        let end = eat!(lex, Token::RParen)?.1;
        return Ok(Ast(
            Expression::Call {
                callee: Box::new(expr),
                args,
            },
            start.merge(end),
        ));
    }

    Ok(expr)
}
