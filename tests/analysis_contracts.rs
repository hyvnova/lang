use lang::analysis::{analyze_document, DiagnosticSeverity, SymbolKind};
use std::fs;

#[test]
fn malformed_source_returns_diagnostic_without_exiting() {
    let analysis = analyze_document("fn broken(", None, None);

    assert!(
        analysis
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.severity == DiagnosticSeverity::Error),
        "expected at least one error diagnostic, got {:#?}",
        analysis.diagnostics
    );
}

#[test]
fn analysis_collects_document_symbols() {
    let analysis = analyze_document(
        r#"
pub mod tools
pub struct User {
    name: Str,
}
trait Display {
    fn show(self) -> Str
}
fn greet(name: Str) -> Str {
    "hi"
}
value = 1
"#,
        None,
        None,
    );

    assert!(analysis
        .index
        .symbols
        .iter()
        .any(|symbol| symbol.name == "User" && symbol.kind == SymbolKind::Struct));
    assert!(analysis
        .index
        .symbols
        .iter()
        .any(|symbol| symbol.name == "Display" && symbol.kind == SymbolKind::Trait));
    assert!(analysis
        .index
        .symbols
        .iter()
        .any(|symbol| symbol.name == "greet" && symbol.kind == SymbolKind::Function));
    assert!(analysis
        .index
        .symbols
        .iter()
        .any(|symbol| symbol.name == "value" && symbol.kind == SymbolKind::Variable));
}

#[test]
fn missing_relative_import_surfaces_as_diagnostic() {
    let root = std::env::temp_dir().join(format!("lang_lsp_analysis_{}", rand::random::<u64>()));
    fs::create_dir_all(root.join("app")).expect("failed to create fixture root");
    let path = root.join("app").join("main.lang");
    let source = "import .missing";
    fs::write(&path, source).expect("failed to write fixture");

    let analysis = analyze_document(source, Some(path), Some(root));

    assert!(
        analysis
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.message.contains("Could not resolve module")),
        "expected missing module diagnostic, got {:#?}",
        analysis.diagnostics
    );
}

#[test]
fn analysis_understands_struct_literal_variable_values() {
    let analysis = analyze_document(
        r#"
struct Point {
    x: int,
    y: int,
}

a = Point { x: 1, y: 2 }
print(a.x)
"#,
        None,
        None,
    );

    let point = analysis
        .index
        .structs
        .iter()
        .find(|item| item.name == "Point")
        .expect("expected Point struct info");
    assert_eq!(point.fields.len(), 2);
    assert!(point
        .fields
        .iter()
        .any(|field| field.name == "x" && field.type_name == "int"));
    assert!(point
        .fields
        .iter()
        .any(|field| field.name == "y" && field.type_name == "int"));

    let variable = analysis
        .index
        .variables
        .iter()
        .find(|item| item.name == "a")
        .expect("expected variable a info");
    assert_eq!(variable.type_name, "Point");
    assert!(variable.fields.iter().any(|field| field.name == "x"
        && field.type_name.as_deref() == Some("int")
        && field.value == "1"));
    assert!(variable.fields.iter().any(|field| field.name == "y"
        && field.type_name.as_deref() == Some("int")
        && field.value == "2"));
}
