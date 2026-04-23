mod common;

use common::{assert_python_compiles, assert_python_runs, transpile, transpile_body};

#[test]
fn emits_indented_function_body_with_implicit_return() {
    assert_eq!(
        transpile_body("fn add(a, b) { a + b }"),
        "def add(a, b):\n\treturn a + b"
    );
}

#[test]
fn emits_nested_control_flow_with_pass_for_empty_blocks() {
    assert_eq!(
        transpile_body("if true { loop {} } else {}"),
        "if True:\n\twhile True:\n\t\tpass\nelse:\n\tpass"
    );
}

#[test]
fn generated_python_compiles_for_core_syntax() {
    let code = transpile(
        r#"
fn add(a, b) { a + b }
value = add(1, 2)
if value == 3 { print(value) } else { print(0) }
"#,
    );

    assert_python_compiles(&code);
}

#[test]
fn generated_python_runs_core_scenarios() {
    assert_python_runs(
        r#"
fn add(a, b) { a + b }
print(add(2, 4))
"#,
        "6",
    );
}

#[test]
fn generated_python_runs_loop_over_range() {
    assert_python_runs(
        r#"
for x in 1..3 {
    print(x)
}
"#,
        "1\n2",
    );
}

#[test]
fn generated_python_runs_signal_updates() {
    assert_python_runs(
        r#"
$a = 1
$b = $a + 1
print($b)
$a = 2
print($b)
"#,
        "2\n3",
    );
}
