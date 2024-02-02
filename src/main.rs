mod lexer;
mod ast;
mod parser;
mod hir;
mod names;
mod tir;
mod types;
mod c;
mod transpile;
mod span;
mod emit;
mod ivec;

#[macro_use(paste)]
extern crate paste;

use logos::Logos;
use ast::ParsingError;
use lexer::Token;
use crate::names::ast_to_hir;
use crate::types::solve_types;

fn main() {
    let program = std::fs::read_to_string("programs/hello.pragma").unwrap();

    let mut lex = Token::lexer(&program)
        .spanned()
        .peekable();

    let ast = match parser::parse_program(&mut lex) {
        Ok(ast) => ast,
        Err(ParsingError::UnexpectedEof) => {
            println!("Unexpected end of file");
            return;
        },
        Err(ParsingError::UnknownToken(s, span)) => {
            println!("Unknown token: {} at {:?} ({:?})", s, span.clone(), &program[span.range()]);
            return;
        },
        Err(ParsingError::UnexpectedToken(s, span)) => {
            println!("Unexpected token: {} at {:?} ({:?})", s, span.clone(), &program[span.range()]);
            return;
        },
        Err(ParsingError::IncorrectPattern(s, expr)) => {
            println!("Incorrect pattern: {} ({:?})", s, expr);
            return;
        },
    };

    println!("{:#?}", &ast);

    let intrinsics = vec![
        "print".to_string(),
        "println".to_string(),
    ];

    let hir = match ast_to_hir(intrinsics, ast) {
        Ok(hir) => hir,
        Err(names::HirError::UnknownName(s)) => {
            println!("Unknown name: {}", s);
            return;
        },
        Err(names::HirError::ExpectedIdentifier(expr)) => {
            println!("Expected identifier: {:?}", expr);
            return;
        },
    };

    println!("{:#?}", &hir);

    let typed = solve_types(hir);

    println!("{:#?}", &typed);

    let c = transpile::transpile_to_c(typed.unwrap());

    println!("{:#?}", &c);

    let c = emit::emit(&c);

    println!("{}\n", &c);

    println!("Size: {} bytes\n", c.len());
    println!("Writing to output/out.c");

    std::fs::write("output/out.c", c).unwrap();

    println!("\nCompiling output/out.c (gcc)");

    let output = std::process::Command::new("gcc")
        .arg("output/out.c")
        .arg("-o")
        .arg("output/out")
        .output()
        .expect("Failed to execute gcc");

    if !output.status.success() {
        println!("GCC failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    println!("\nRunning output/out");

    let output = std::process::Command::new("./output/out")
        .output()
        .expect("Failed to execute program");

    println!("Output:\n############################");
    println!("{}", String::from_utf8_lossy(&output.stdout));
    println!("############################");
}
