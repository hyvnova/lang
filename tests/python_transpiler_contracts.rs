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
fn generated_python_runs_basic_literals_and_comments() {
    assert_python_runs(
        r#"
// literal smoke test
flag = true
message = "ok"
if flag == true { print(message) } else { print("no") }
"#,
        "ok",
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

#[test]
fn generated_python_runs_arrays_and_indexing() {
    assert_python_runs(
        r#"
values = [1, 2, 3]
print(values[1])
"#,
        "2",
    );
}

#[test]
fn generated_python_runs_lambda_calls() {
    assert_python_runs(
        r#"
value = 2
print(((n) => n + 1)(value))
"#,
        "3",
    );
}

#[test]
fn generated_python_runs_raw_python_interop() {
    assert_python_runs(
        r#"
#[python]
class Counter:
    def __init__(self, value):
        self.value = value
#[endpython]

counter = Counter(5)
print(counter.value)
"#,
        "5",
    );
}
