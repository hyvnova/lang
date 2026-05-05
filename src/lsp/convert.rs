//! Conversion helpers keep tower-lsp types at the edge of the IDE layer.

use crate::analysis::{DiagnosticSeverity, LangDiagnostic, SourceSpan, SymbolKind};
use tower_lsp::lsp_types::{
    CompletionItemKind, Diagnostic, Documentation, Hover, HoverContents, MarkupContent, MarkupKind,
    NumberOrString, ParameterInformation, ParameterLabel, Range,
};

pub fn lsp_range(span: SourceSpan) -> Range {
    let line = span.line.saturating_sub(1) as u32;
    let start = span.column.saturating_sub(1) as u32;
    Range {
        start: tower_lsp::lsp_types::Position {
            line,
            character: start,
        },
        end: tower_lsp::lsp_types::Position {
            line,
            character: start + span.length.max(1) as u32,
        },
    }
}

pub(crate) fn markdown_hover(value: String) -> Hover {
    Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value,
        }),
        range: None,
    }
}

pub(crate) fn markdown_docs(value: String) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    })
}

pub(crate) fn lsp_diagnostic(diagnostic: &LangDiagnostic) -> Diagnostic {
    Diagnostic {
        range: lsp_range(diagnostic.span.clone()),
        severity: Some(match diagnostic.severity {
            DiagnosticSeverity::Error => tower_lsp::lsp_types::DiagnosticSeverity::ERROR,
            DiagnosticSeverity::Warning => tower_lsp::lsp_types::DiagnosticSeverity::WARNING,
            DiagnosticSeverity::Information => {
                tower_lsp::lsp_types::DiagnosticSeverity::INFORMATION
            }
            DiagnosticSeverity::Hint => tower_lsp::lsp_types::DiagnosticSeverity::HINT,
        }),
        code: diagnostic.code.clone().map(NumberOrString::String),
        source: Some(diagnostic.source.to_string()),
        message: diagnostic.message.clone(),
        ..Diagnostic::default()
    }
}

pub(crate) fn completion_kind(kind: SymbolKind) -> CompletionItemKind {
    match kind {
        SymbolKind::Function => CompletionItemKind::FUNCTION,
        SymbolKind::Struct => CompletionItemKind::STRUCT,
        SymbolKind::Trait => CompletionItemKind::INTERFACE,
        SymbolKind::Impl => CompletionItemKind::CLASS,
        SymbolKind::Module => CompletionItemKind::MODULE,
        SymbolKind::Variable => CompletionItemKind::VARIABLE,
    }
}

pub(crate) fn document_symbol_kind(kind: SymbolKind) -> tower_lsp::lsp_types::SymbolKind {
    match kind {
        SymbolKind::Function => tower_lsp::lsp_types::SymbolKind::FUNCTION,
        SymbolKind::Struct => tower_lsp::lsp_types::SymbolKind::STRUCT,
        SymbolKind::Trait => tower_lsp::lsp_types::SymbolKind::INTERFACE,
        SymbolKind::Impl => tower_lsp::lsp_types::SymbolKind::CLASS,
        SymbolKind::Module => tower_lsp::lsp_types::SymbolKind::MODULE,
        SymbolKind::Variable => tower_lsp::lsp_types::SymbolKind::VARIABLE,
    }
}

pub(crate) fn parameters_from_signature(signature: &str) -> Option<Vec<ParameterInformation>> {
    let params = signature.split_once('(')?.1.split_once(')')?.0;
    if params.trim().is_empty() {
        return Some(Vec::new());
    }
    Some(
        params
            .split(',')
            .map(|param| ParameterInformation {
                label: ParameterLabel::Simple(param.trim().to_string()),
                documentation: None,
            })
            .collect(),
    )
}
