use lang::language_features::{
    all_features, load_syntax_feature_metadata, missing_syntax_metadata, syntax_coverage_report,
    SyntaxFeatureMetadata,
};
use std::path::Path;
use std::process::Command;

#[test]
fn syntax_metadata_covers_registered_parser_features() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let metadata = load_syntax_feature_metadata(root).expect("syntax metadata should load");
    let missing = missing_syntax_metadata(&metadata);

    assert!(
        missing.is_empty(),
        "syntax metadata is missing coverage for: {missing:#?}"
    );
}

#[test]
fn syntax_metadata_check_reports_missing_tree_sitter_feature() {
    let metadata = SyntaxFeatureMetadata {
        tree_sitter: Vec::new(),
        textmate: all_features()
            .iter()
            .map(|feature| feature.feature.key().to_string())
            .collect(),
    };
    let missing = missing_syntax_metadata(&metadata);

    assert!(
        missing
            .iter()
            .any(|item| item.tool == "tree_sitter" && item.feature.key() == "function_definition"),
        "expected function_definition to be reported missing from tree-sitter metadata, got {missing:#?}"
    );
}

#[test]
fn syntax_coverage_cli_passes_in_strict_mode() {
    let output = Command::new(env!("CARGO_BIN_EXE_lang"))
        .current_dir(env!("CARGO_MANIFEST_DIR"))
        .args(["check", "syntax-coverage", "--strict"])
        .output()
        .expect("failed to run lang binary");

    assert!(
        output.status.success(),
        "syntax coverage CLI failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("Syntax coverage is complete"),
        "expected success report, got stdout:\n{}",
        String::from_utf8_lossy(&output.stdout)
    );
}

#[test]
fn syntax_coverage_report_is_human_readable() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let (report, has_missing) = syntax_coverage_report(root).expect("syntax report should load");

    assert!(
        !has_missing,
        "expected complete syntax coverage, got:\n{report}"
    );
    assert!(report.contains("registered features"));
}
