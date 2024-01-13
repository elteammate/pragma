use std::iter::Peekable;
use logos::SpannedIter;

#[derive(logos::Logos, Debug, PartialEq)]
#[logos(skip r"\s+")]
pub enum Token<'s> {
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

pub type Lex<'s> = Peekable<SpannedIter<'s, Token<'s>>>;
