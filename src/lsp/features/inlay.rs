//! Inlay hints expose inferred facts without making them part of source syntax.

use crate::analysis::DocumentAnalysis;
use crate::lsp::features::hover::variable_markdown;
use tower_lsp::lsp_types::{
    InlayHint, InlayHintKind, InlayHintLabel, InlayHintTooltip, MarkupContent, MarkupKind, Position,
};

pub fn inlay_hints(analysis: &DocumentAnalysis) -> Vec<InlayHint> {
    analysis
        .index
        .variables
        .iter()
        .map(|variable| InlayHint {
            position: Position {
                line: variable.span.line.saturating_sub(1) as u32,
                character: (variable.span.column.saturating_sub(1) + variable.name.len()) as u32,
            },
            label: InlayHintLabel::String(format!(": {}", variable.type_name)),
            kind: Some(InlayHintKind::TYPE),
            text_edits: None,
            tooltip: Some(InlayHintTooltip::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: variable_markdown(variable),
            })),
            padding_left: Some(false),
            padding_right: Some(true),
            data: None,
        })
        .collect()
}
