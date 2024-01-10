use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"\s+")]
enum Token {
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
    Number,
    #[regex(r#""(\\"|\\n|\\t|\\r|.)*""#)]
    String,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
}

fn main() {
    let program = std::fs::read_to_string("programs/hello.pragma").unwrap();
    Token::lexer(&program).spanned().for_each(|(t, s)| {
        println!("{:?} {:?}", t, s);
    });
}
