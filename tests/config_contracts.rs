mod common;

use common::build_temp_project;
use lang::config::{discover_manifest, load_manifest, resolve_run_config};
use std::path::PathBuf;

#[test]
fn discovers_manifest_from_nested_paths() {
    let (root, entry_path) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "app/main.lang"

[run]
transpiler = "python"
"#,
            ),
            (
                "app/main.lang",
                r#"
print(1)
"#,
            ),
        ],
        "app/main.lang",
    );

    let manifest = discover_manifest(&entry_path).expect("manifest should be discovered");
    assert_eq!(manifest, root.join("lang.toml"));
}

#[test]
fn parses_valid_lang_manifest() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "app/main.lang"

[run]
transpiler = "python"
"#,
            ),
            (
                "app/main.lang",
                r#"
print(1)
"#,
            ),
        ],
        "app/main.lang",
    );

    let manifest = load_manifest(&root.join("lang.toml")).expect("manifest should parse");
    assert_eq!(manifest.project_root, root);
    assert_eq!(
        manifest.entry_file,
        Some(root.join("app").join("main.lang"))
    );
    assert_eq!(manifest.transpiler.as_deref(), Some("python"));
}

#[test]
fn rejects_missing_project_section() {
    let (root, _) = build_temp_project(
        &[(
            "lang.toml",
            r#"
[run]
transpiler = "python"
"#,
        )],
        "lang.toml",
    );

    let error = load_manifest(&root.join("lang.toml")).expect_err("manifest should fail");
    assert!(error.message.contains("missing the [project] section"));
}

#[test]
fn rejects_missing_entry_for_manifest_driven_execution() {
    let (root, _) = build_temp_project(
        &[(
            "lang.toml",
            r#"
[project]
root = "."
"#,
        )],
        "lang.toml",
    );

    let error = resolve_run_config(None, None, None, &root).expect_err("missing entry should fail");
    assert!(error
        .message
        .contains("Manifest-driven execution requires project.entry"));
}

#[test]
fn rejects_invalid_manifest_root() {
    let (root, _) = build_temp_project(
        &[(
            "lang.toml",
            r#"
[project]
root = "missing"
entry = "main.lang"
"#,
        )],
        "lang.toml",
    );

    let error = resolve_run_config(None, None, None, &root).expect_err("invalid root should fail");
    assert!(error.message.contains("Project root"));
}

#[test]
fn rejects_invalid_manifest_entry() {
    let (root, _) = build_temp_project(
        &[(
            "lang.toml",
            r#"
[project]
root = "."
entry = "missing.lang"
"#,
        )],
        "lang.toml",
    );

    let error = resolve_run_config(None, None, None, &root).expect_err("invalid entry should fail");
    assert!(error.message.contains("Entry file"));
}

#[test]
fn rejects_unsupported_manifest_transpilers() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "."
entry = "main.lang"

[run]
transpiler = "ruby"
"#,
            ),
            (
                "main.lang",
                r#"
print(1)
"#,
            ),
        ],
        "main.lang",
    );

    let error =
        resolve_run_config(None, None, None, &root).expect_err("bad transpiler should fail");
    assert!(error.message.contains("Unsupported transpiler 'ruby'"));
}

#[test]
fn explicit_file_can_override_manifest_entry_and_root() {
    let (root, _) = build_temp_project(
        &[
            (
                "lang.toml",
                r#"
[project]
root = "missing"
entry = "missing.lang"

[run]
transpiler = "python"
"#,
            ),
            (
                "app/main.lang",
                r#"
print(1)
"#,
            ),
        ],
        "app/main.lang",
    );

    let resolved = resolve_run_config(
        Some(PathBuf::from("app/main.lang")),
        Some(PathBuf::from(".")),
        None,
        &root,
    )
    .expect("cli values should override manifest root and entry");

    assert_eq!(resolved.entry_file, root.join("app").join("main.lang"));
    assert_eq!(resolved.project_root, Some(root.clone()));
}
