mod common;

use common::{build_temp_project, run_lang_binary};

#[test]
fn lang_run_without_file_uses_manifest_entry() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "main.lang"

[run]
transpiler = "python"
"#,
            ),
            (
                "main.lang",
                r#"
print("manifest run")
"#,
            ),
        ],
        "main.lang",
    );

    let output = run_lang_binary(&root, &["run"]);
    assert!(output.status.success(), "stderr:\n{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "manifest run");
}

#[test]
fn lang_run_file_overrides_manifest_entry() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "main.lang"
"#,
            ),
            (
                "main.lang",
                r#"
print("main")
"#,
            ),
            (
                "override.lang",
                r#"
print("override")
"#,
            ),
        ],
        "main.lang",
    );

    let output = run_lang_binary(&root, &["run", "override.lang"]);
    assert!(output.status.success(), "stderr:\n{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "override");
}

#[test]
fn bare_executable_invocation_uses_manifest_entry() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "main.lang"
"#,
            ),
            (
                "main.lang",
                r#"
print("default run")
"#,
            ),
        ],
        "main.lang",
    );

    let output = run_lang_binary(&root, &[]);
    assert!(output.status.success(), "stderr:\n{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "default run");
}

#[test]
fn manifest_root_enables_absolute_imports_without_project_root_flag() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "app/main.lang"
"#,
            ),
            (
                "app/math/vec.lang",
                r#"
pub value = 7
"#,
            ),
            (
                "app/main.lang",
                r#"
from app.math.vec import value
print(value)
"#,
            ),
        ],
        "app/main.lang",
    );

    let output = run_lang_binary(&root, &["run"]);
    assert!(output.status.success(), "stderr:\n{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "7");
}

#[test]
fn cli_flags_override_manifest_root_and_transpiler() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "missing"
entry = "missing.lang"

[run]
transpiler = "ruby"
"#,
            ),
            (
                "app/math/vec.lang",
                r#"
pub value = 9
"#,
            ),
            (
                "app/main.lang",
                r#"
from app.math.vec import value
print(value)
"#,
            ),
        ],
        "app/main.lang",
    );

    let output = run_lang_binary(
        &root,
        &["run", "app/main.lang", "--project-root", ".", "--transpiler", "python"],
    );
    assert!(output.status.success(), "stderr:\n{}", String::from_utf8_lossy(&output.stderr));
    assert_eq!(String::from_utf8_lossy(&output.stdout).trim(), "9");
}
