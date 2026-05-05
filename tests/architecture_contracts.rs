use std::fs;
use std::path::{Path, PathBuf};

fn repo_path(relative: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(relative)
}

#[test]
fn lsp_and_analysis_are_split_into_documented_modules() {
    for removed in ["src/lsp.rs", "src/analysis.rs"] {
        assert!(
            !repo_path(removed).exists(),
            "{removed} should stay split into focused modules"
        );
    }

    for module in [
        "src/analysis/mod.rs",
        "src/analysis/span.rs",
        "src/analysis/diagnostics.rs",
        "src/analysis/document.rs",
        "src/analysis/symbols.rs",
        "src/analysis/structs.rs",
        "src/analysis/imports.rs",
        "src/analysis/context.rs",
        "src/lsp/mod.rs",
        "src/lsp/server.rs",
        "src/lsp/capabilities.rs",
        "src/lsp/convert.rs",
        "src/lsp/features/completion.rs",
        "src/lsp/features/hover.rs",
        "src/lsp/features/inlay.rs",
        "src/lsp/features/signature.rs",
        "src/lsp/features/symbols.rs",
        "src/lsp/features/definition.rs",
        "src/lsp/features/semantic.rs",
    ] {
        let source = fs::read_to_string(repo_path(module))
            .unwrap_or_else(|error| panic!("expected {module}: {error}"));
        assert!(
            source.trim_start().starts_with("//!"),
            "{module} needs a module-level why-comment"
        );
    }
}

#[test]
fn rewritten_modules_stay_small_enough_to_read() {
    for module in [
        "src/analysis/mod.rs",
        "src/analysis/span.rs",
        "src/analysis/diagnostics.rs",
        "src/analysis/document.rs",
        "src/analysis/symbols.rs",
        "src/analysis/structs.rs",
        "src/analysis/imports.rs",
        "src/analysis/context.rs",
        "src/lsp/mod.rs",
        "src/lsp/server.rs",
        "src/lsp/capabilities.rs",
        "src/lsp/convert.rs",
        "src/lsp/features/completion.rs",
        "src/lsp/features/hover.rs",
        "src/lsp/features/inlay.rs",
        "src/lsp/features/signature.rs",
        "src/lsp/features/symbols.rs",
        "src/lsp/features/definition.rs",
        "src/lsp/features/semantic.rs",
    ] {
        let source = fs::read_to_string(repo_path(module))
            .unwrap_or_else(|error| panic!("expected {module}: {error}"));
        let line_count = source.lines().count();
        assert!(
            line_count <= 450,
            "{module} has {line_count} lines; split it before it turns feral"
        );
    }
}

#[test]
fn code_structure_guidelines_are_documented() {
    let source = fs::read_to_string(repo_path("docs/engineering/code-structure.md"))
        .expect("expected engineering structure docs");

    for required in [
        "Module Responsibility",
        "Why Comments",
        "Macros",
        "Line Budgets",
    ] {
        assert!(
            source.contains(required),
            "code-structure.md should document {required}"
        );
    }
}
