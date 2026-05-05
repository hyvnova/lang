use lang::analysis::analyze_document;
use lang::lsp::{
    completion_items, definition_at, document_symbols, hover_at, hover_at_position, inlay_hints,
    lsp_range, semantic_tokens, server_capabilities, signature_help_at,
};
use tower_lsp::lsp_types::{
    CompletionItemKind, HoverContents, SemanticTokenType, TextDocumentSyncCapability,
};

#[test]
fn server_capabilities_advertise_ide_heavy_mvp_features() {
    let capabilities = server_capabilities();

    assert!(matches!(
        capabilities.text_document_sync,
        Some(TextDocumentSyncCapability::Kind(_)) | Some(TextDocumentSyncCapability::Options(_))
    ));
    assert!(capabilities.hover_provider.is_some());
    assert!(capabilities.definition_provider.is_some());
    assert!(capabilities.document_symbol_provider.is_some());
    assert!(capabilities.completion_provider.is_some());
    assert!(capabilities.signature_help_provider.is_some());
    assert!(capabilities.semantic_tokens_provider.is_some());
    assert!(capabilities.inlay_hint_provider.is_some());

    let sync_kind = match capabilities.text_document_sync.unwrap() {
        TextDocumentSyncCapability::Kind(kind) => Some(kind),
        TextDocumentSyncCapability::Options(options) => options.change,
    };
    assert!(matches!(
        sync_kind,
        Some(tower_lsp::lsp_types::TextDocumentSyncKind::FULL)
    ));
}

#[test]
fn point_struct_literal_powers_field_completion_hover_and_inlay_hints() {
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

    let field_items = completion_items(&analysis, "a.");
    let x = field_items
        .iter()
        .find(|item| item.label == "x")
        .expect("expected x field completion");
    assert_eq!(x.kind, Some(CompletionItemKind::FIELD));
    assert_eq!(x.detail.as_deref(), Some("x: int"));

    let y = field_items
        .iter()
        .find(|item| item.label == "y")
        .expect("expected y field completion");
    assert_eq!(y.kind, Some(CompletionItemKind::FIELD));
    assert_eq!(y.detail.as_deref(), Some("y: int"));

    let hover = hover_at(&analysis, "a").expect("expected variable hover");
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markup hover");
    };
    assert!(markup.value.contains("a: Point"));
    assert!(markup.value.contains("x: int = 1"));
    assert!(markup.value.contains("y: int = 2"));

    let hints = inlay_hints(&analysis);
    assert!(hints.iter().any(|hint| match &hint.label {
        tower_lsp::lsp_types::InlayHintLabel::String(label) => label == ": Point",
        tower_lsp::lsp_types::InlayHintLabel::LabelParts(_) => false,
    }));

    let field_hover = hover_at_position(&analysis, 7, 8).expect("expected field hover");
    let HoverContents::Markup(markup) = field_hover.contents else {
        panic!("expected markup hover");
    };
    assert!(markup.value.contains("Point.x"));
    assert!(markup.value.contains("x: int = 1"));
}

#[test]
fn span_to_lsp_range_is_zero_based() {
    let range = lsp_range(lang::analysis::SourceSpan {
        path: None,
        line: 2,
        column: 3,
        length: 4,
    });

    assert_eq!(range.start.line, 1);
    assert_eq!(range.start.character, 2);
    assert_eq!(range.end.line, 1);
    assert_eq!(range.end.character, 6);
}

#[test]
fn completions_include_std_primitive_methods_and_local_symbols() {
    let analysis = analyze_document("fn greet() { }\nstd.", None, None);
    let labels: Vec<String> = completion_items(&analysis, "std.")
        .into_iter()
        .map(|item| item.label)
        .collect();

    assert!(labels.contains(&"fs".to_string()));
    assert!(labels.contains(&"io".to_string()));

    let primitive_labels: Vec<String> = completion_items(&analysis, "text.")
        .into_iter()
        .map(|item| item.label)
        .collect();
    assert!(primitive_labels.contains(&"strip".to_string()));
    assert!(primitive_labels.contains(&"words".to_string()));

    let local_items = completion_items(&analysis, "gre");
    assert!(local_items
        .iter()
        .any(|item| { item.label == "greet" && item.kind == Some(CompletionItemKind::FUNCTION) }));

    let read_text = completion_items(&analysis, "std.fs.")
        .into_iter()
        .find(|item| item.label == "read_text")
        .expect("expected read_text completion");
    let docs = match read_text
        .documentation
        .expect("expected read_text completion docs")
    {
        tower_lsp::lsp_types::Documentation::MarkupContent(markup) => markup.value,
        tower_lsp::lsp_types::Documentation::String(value) => value,
    };
    assert!(docs.contains("Result<Str, Err>"));
    assert!(docs.contains("```lang"));
}

#[test]
fn hover_returns_catalog_and_symbol_details() {
    let analysis = analyze_document("fn greet(name: Str) -> Str { name }\nstd.fs", None, None);

    let std_hover = hover_at(&analysis, "read_text").expect("expected std hover");
    let HoverContents::Markup(markup) = std_hover.contents else {
        panic!("expected markup hover");
    };
    assert!(markup
        .value
        .contains("std.fs.read_text(path: Str) -> Result<Str, Err>"));
    assert!(markup.value.contains("Errors"));
    assert!(markup.value.contains(".expect("));

    let std_object_hover = hover_at(&analysis, "std").expect("expected std hover");
    let HoverContents::Markup(markup) = std_object_hover.contents else {
        panic!("expected markup hover");
    };
    assert!(markup.value.contains("global `std` object"));

    let symbol_hover = hover_at(&analysis, "greet").expect("expected symbol hover");
    let HoverContents::Markup(markup) = symbol_hover.contents else {
        panic!("expected markup hover");
    };
    assert!(markup.value.contains("fn greet"));
}

#[test]
fn signature_help_supports_std_primitive_and_user_functions() {
    let analysis = analyze_document(
        "fn greet(name: Str) -> Str { name }\nstd.fs.read_text(",
        None,
        None,
    );

    let std_help = signature_help_at(&analysis, "std.fs.read_text(").expect("expected std help");
    assert_eq!(
        std_help.signatures[0].label,
        "std.fs.read_text(path: Str) -> Result<Str, Err>"
    );

    let primitive_help =
        signature_help_at(&analysis, "text.replace(").expect("expected primitive help");
    assert!(primitive_help.signatures[0].label.contains("Str.replace"));

    let user_help = signature_help_at(&analysis, "greet(").expect("expected user help");
    assert_eq!(user_help.signatures[0].label, "fn greet(name: Str) -> Str");
}

#[test]
fn semantic_tokens_classify_useful_language_surface() {
    let analysis = analyze_document(
        r#"# note
$tick = 1
std.fs.read_text("input.txt")
fn greet(name: Str) -> Str { name.upper() }
"#,
        None,
        None,
    );
    let semantic = semantic_tokens(&analysis);
    let legend = lang::lsp::semantic_token_legend();

    assert!(!semantic.data.is_empty());
    assert!(legend.token_types.contains(&SemanticTokenType::KEYWORD));
    assert!(legend.token_types.contains(&SemanticTokenType::FUNCTION));
    assert!(legend.token_types.contains(&SemanticTokenType::TYPE));
    assert!(legend.token_types.contains(&SemanticTokenType::NAMESPACE));
    assert!(legend.token_types.contains(&SemanticTokenType::METHOD));
    assert!(legend.token_types.contains(&SemanticTokenType::STRING));
    assert!(legend.token_types.contains(&SemanticTokenType::NUMBER));
    assert!(legend.token_types.contains(&SemanticTokenType::COMMENT));
    assert!(legend.token_types.contains(&SemanticTokenType::EVENT));
}

#[test]
fn document_symbols_and_definition_work_for_same_file_symbols() {
    let analysis = analyze_document(
        "fn greet() { }\nstruct Point {\n    x: int,\n}\nprint(greet())",
        None,
        None,
    );

    let symbols = document_symbols(&analysis);
    assert!(symbols.iter().any(|symbol| symbol.name == "greet"));
    assert!(symbols.iter().all(|symbol| {
        symbol.range.start.line < symbol.selection_range.start.line
            || (symbol.range.start.line == symbol.selection_range.start.line
                && symbol.range.start.character <= symbol.selection_range.start.character)
    }));
    assert!(symbols.iter().all(|symbol| {
        symbol.range.end.line > symbol.selection_range.end.line
            || (symbol.range.end.line == symbol.selection_range.end.line
                && symbol.range.end.character >= symbol.selection_range.end.character)
    }));

    let definition = definition_at(&analysis, "greet").expect("expected definition");
    assert_eq!(definition.name, "greet");
}
