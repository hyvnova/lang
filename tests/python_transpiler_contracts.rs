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
        "if Bool(True):\n\twhile True:\n\t\tpass\nelse:\n\tpass"
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

#[test]
fn generated_python_runs_primitive_raw_escape() {
    assert_python_runs(
        r#"
#[python]
def raw_type(value):
    return type(value).__name__
#[endpython]

value = "lamp"
print(raw_type(value))
print(raw_type(value.raw))
"#,
        "Str\nstr",
    );
}

#[test]
fn generated_python_runs_primitive_method_model() {
    assert_python_runs(
        r#"
text = " hello lang "
print(text.strip().upper())
print(text.strip().len)
print("a\nb".lines().len)
print("a b c".words().length)
print([1, 2, 3].len)
print([1, 2, 3].map((n) => n + 1).join(","))
fn add(acc, n) { acc + n }
print([1, 2, 3].fold(0, add))
data = Map()
data.set("age", 36)
data.set("name", "ada")
print(data.get("name").title())
print(data.len)
seen = Set([1, 2])
seen.add(3)
print(seen.contains(2))
print(seen.union(Set([3, 4])).len)
print("x".tap((value) => print(value)).pipe((value) => value + "y"))
"#,
        "HELLO LANG\n10\n2\n3\n3\n2,3,4\n6\nAda\n2\nTrue\n4\nx\nxy",
    );
}

#[test]
fn generated_python_runs_block_lambda_tap_and_pipe() {
    assert_python_runs(
        r#"
value = "ok"
    .tap((text) => {
        print(text)
        text
    })
    .pipe((text) => {
        text + "!"
    })

print(value)
"#,
        "ok\nok!",
    );
}
