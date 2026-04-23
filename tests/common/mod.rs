use lang::{
    ast::{Node, AST},
    lexer::{Kind, Lexer},
    parser::Parser,
    transpilers::python_transpiler::Transpiler,
};
use std::process::Command;

pub fn lex(source: &str) -> Vec<(String, String)> {
    let mut lexer = Lexer::new(source.to_string());
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push((format!("{:?}", token.kind), token.value));
    }

    tokens
}

pub fn lex_kinds(source: &str) -> Vec<Kind> {
    let mut lexer = Lexer::new(source.to_string());
    let mut kinds = Vec::new();

    while let Some(token) = lexer.next() {
        kinds.push(token.kind);
    }

    kinds
}

pub fn parse_ast(source: &str) -> AST {
    let mut parser = Parser::new(source.to_string());
    parser.parse();
    parser.ast
}

pub fn parse_nodes(source: &str) -> Vec<Node> {
    parse_ast(source)
        .get_scope()
        .into_iter()
        .filter(|node| !matches!(node, Node::Newline | Node::Empty))
        .collect()
}

pub fn transpile(source: &str) -> String {
    let ast = parse_ast(source);
    Transpiler::new().transpile(&ast)
}

pub fn transpile_body(source: &str) -> String {
    let code = transpile(source).replace("\r\n", "\n");
    match code.find("# End of custom builtins") {
        Some(index) => code[index..]
            .lines()
            .skip(2)
            .collect::<Vec<&str>>()
            .join("\n")
            .trim()
            .to_string(),
        None => code.trim().to_string(),
    }
}

pub fn assert_python_compiles(code: &str) {
    let output = python_output(&["-m", "py_compile", "-"], code);

    assert!(
        output.status.success(),
        "Python did not compile.\nstdout:\n{}\nstderr:\n{}\ncode:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
        code
    );
}

pub fn assert_python_runs(source: &str, expected_stdout: &str) {
    let code = transpile(source);
    let output = python_output(&["-c", &code], "");

    assert!(
        output.status.success(),
        "Python run failed.\nstdout:\n{}\nstderr:\n{}\ncode:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
        code
    );

    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), expected_stdout);
}

fn python_output(args: &[&str], stdin: &str) -> std::process::Output {
    let candidates = ["py", "python"];

    for candidate in candidates {
        let mut command = Command::new(candidate);
        command.args(args);

        if !stdin.is_empty() {
            use std::io::Write;
            command.stdin(std::process::Stdio::piped());
            let mut child = command.spawn().expect("failed to spawn Python process");
            let child_stdin = child.stdin.as_mut().expect("failed to open Python stdin");
            child_stdin
                .write_all(stdin.as_bytes())
                .expect("failed to write Python stdin");
            return child.wait_with_output().expect("failed to wait for Python");
        }

        if let Ok(output) = command.output() {
            return output;
        }
    }

    panic!("could not find py or python on PATH");
}
