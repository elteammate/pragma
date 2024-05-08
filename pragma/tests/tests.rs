use rstest::rstest;
use std::path::PathBuf;
use pragma::lexer::lex;
use pragma::names::ast_to_hir;
use pragma::{emit, parser, transpile};
use pragma::types::solve_types;

enum ExpectedResult {
    Output(Box<[u8]>),
    ExitStatus(i32),
    ParseError,
    NameError,
    TypeError,
}

#[rstest]
fn test_file(#[files("tests/suite/**/*.pragma")] path: PathBuf) {
    let content = std::fs::read_to_string(path).unwrap();

    let top_comments = content.lines().take_while(|line| line.starts_with("//"));
    let mut expected = None;

    for line in top_comments {
        let line = line.trim_start_matches("//").trim();
        if line.starts_with("expect:") {
            if expected.is_some() {
                panic!("Malformed test file: multiple expect lines");
            }

            let line = line.trim_start_matches("expect:").trim();
            expected = Some(match line {
                "parse-error" => ExpectedResult::ParseError,
                "name-error" => ExpectedResult::NameError,
                "type-error" => ExpectedResult::TypeError,
                _ => {
                    if line.starts_with("exit:") {
                        ExpectedResult::ExitStatus(
                            line.trim_start_matches("exit:").trim().parse().unwrap(),
                        )
                    } else {
                        ExpectedResult::Output(
                            line.trim_start_matches('"')
                                .trim_end_matches('"')
                                .as_bytes()
                                .to_vec()
                                .into_boxed_slice(),
                        )
                    }
                }
            });
        }
    }

    let expected = expected.unwrap_or(ExpectedResult::ExitStatus(0));

    let mut lex = lex(&content);

    let ast = match (parser::parse_program(&mut lex), &expected) {
        (Ok(ast), ExpectedResult::ParseError) => {
            panic!("Expected parse error, got successful parse. AST dump:\n{:#?}", &ast);
        },
        (Err(_), ExpectedResult::ParseError) => {
            return;
        },
        (Ok(ast), _) => ast,
        (Err(e), _) => {
            panic!("Expected successful parse, got error: {:?}", e);
        },
    };

    // TODO: remove this
    let intrinsics = vec![
        "print".to_string(),
        "println".to_string(),
    ];

    let hir = match (ast_to_hir(intrinsics, ast), &expected) {
        (Ok(hir), ExpectedResult::NameError) => {
            panic!("Expected name error, got successful HIR. HIR dump:\n{:#?}", &hir);
        },
        (Err(_), ExpectedResult::NameError) => {
            return;
        },
        (Ok(hir), _) => hir,
        (Err(e), _) => {
            panic!("Expected successful HIR, got error: {:?}", &e);
        },
    };

    let typed = solve_types(hir);

    let typed = match (typed, &expected) {
        (Ok(typed), ExpectedResult::TypeError) => {
            panic!("Expected type error, got successful typed TIR. TIR dump:\n{:#?}", &typed);
        },
        (Err(_), ExpectedResult::TypeError) => {
            return;
        },
        (Ok(typed), _) => typed,
        (Err(e), _) => {
            panic!("Expected successful TIR, got error: {:?}", &e);
        },
    };

    let c = transpile::transpile_to_c(typed);
    let c = emit::emit(&c);

    let tempdir = tempfile::tempdir().unwrap();
    let c_path = tempdir.path().join("out.c");
    let out_path = tempdir.path().join("out");

    std::fs::write(&c_path, c).unwrap();

    let compiler = std::env::var("CC").expect("CC environment variable not set");

    let output = std::process::Command::new(compiler)
        .arg(&c_path)
        .arg("-o")
        .arg(&out_path)
        .output()
        .expect("Failed to execute gcc");

    if !output.status.success() {
        panic!("GCC failed: {}", String::from_utf8_lossy(&output.stderr));
    }

    let output = std::process::Command::new(&out_path)
        .output()
        .expect("Failed to execute program");

    match &expected {
        ExpectedResult::Output(expected) => {
            assert_eq!(output.stdout, expected[..]);
        },
        ExpectedResult::ExitStatus(expected) => {
            assert_eq!(output.status.code(), Some(*expected));
        },
        _ => unreachable!(),
    }
}
