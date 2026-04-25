#![allow(dead_code)]

use lang::{
    ast::{Node, AST},
    lexer::{Kind, Lexer},
    macros::{expand_source_tokens, MacroError},
    modules::{build_project, run_project, ModuleError},
    parser::Parser,
    transpilers::python_transpiler::Transpiler,
};
use rand::random;
use std::fs;
use std::path::{Path, PathBuf};
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

pub fn expand_tokens(source: &str) -> Result<Vec<(String, String)>, MacroError> {
    expand_source_tokens(source).map(|tokens| {
        tokens
            .into_iter()
            .map(|token| (format!("{:?}", token.kind), token.value))
            .collect()
    })
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
    let output = python_output(
        &[
            "-c",
            "import sys; compile(sys.stdin.read(), '<generated>', 'exec')",
        ],
        code,
    );

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

    let stdout = String::from_utf8_lossy(&output.stdout).replace("\r\n", "\n");
    assert_eq!(stdout.trim(), expected_stdout);
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

pub fn build_temp_project(
    files: &[(&str, &str)],
    entry: &str,
) -> (PathBuf, PathBuf) {
    let root = std::env::temp_dir().join(format!("lang_fixture_{}", random::<u64>()));
    fs::create_dir_all(&root).expect("failed to create temp project root");

    for (relative, contents) in files.iter() {
        let path = root.join(relative.replace('/', "\\"));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).expect("failed to create fixture directory");
        }
        fs::write(&path, contents).expect("failed to write fixture file");
    }

    (root.clone(), root.join(entry.replace('/', "\\")))
}

pub fn build_project_fixture(
    files: &[(&str, &str)],
    entry: &str,
) -> Result<PathBuf, ModuleError> {
    let (root, entry_path) = build_temp_project(files, entry);
    let output_root = root.join("__out__");
    build_project(entry_path, Some(root.clone()), &output_root)?;
    Ok(output_root)
}

pub fn assert_project_runs(
    files: &[(&str, &str)],
    entry: &str,
    expected_stdout: &str,
) {
    let (root, entry_path) = build_temp_project(files, entry);
    let output = run_project(entry_path, Some(root)).expect("project run should succeed");

    assert!(
        output.status.success(),
        "Project run failed.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout).replace("\r\n", "\n");
    assert_eq!(stdout.trim(), expected_stdout);
}

pub fn assert_project_runtime_error(
    files: &[(&str, &str)],
    entry: &str,
    expected_fragment: &str,
) {
    let (root, entry_path) = build_temp_project(files, entry);
    let output = run_project(entry_path, Some(root)).expect("project run should complete");

    assert!(
        !output.status.success(),
        "Project run unexpectedly succeeded.\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let stderr = String::from_utf8_lossy(&output.stderr).replace("\r\n", "\n");
    assert!(
        stderr.contains(expected_fragment),
        "expected runtime error containing '{}', got stderr:\n{}",
        expected_fragment,
        stderr
    );
}

pub fn assert_project_build_error(
    files: &[(&str, &str)],
    entry: &str,
    expected_fragment: &str,
) {
    let error = build_project_fixture(files, entry).expect_err("project build should fail");
    assert!(
        error.message.contains(expected_fragment),
        "expected error containing '{}', got '{}'",
        expected_fragment,
        error.message
    );
}

pub fn run_lang_binary(cwd: &Path, args: &[&str]) -> std::process::Output {
    Command::new(env!("CARGO_BIN_EXE_lang"))
        .current_dir(cwd)
        .args(args)
        .output()
        .expect("failed to run lang binary")
}
