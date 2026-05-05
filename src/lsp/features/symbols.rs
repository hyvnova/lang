//! Document symbols are a view over the analysis symbol index.

use crate::analysis::DocumentAnalysis;
use crate::lsp::convert::{document_symbol_kind, lsp_range};
use tower_lsp::lsp_types::DocumentSymbol;

pub fn document_symbols(analysis: &DocumentAnalysis) -> Vec<DocumentSymbol> {
    analysis
        .index
        .symbols
        .iter()
        .map(|symbol| {
            #[allow(deprecated)]
            DocumentSymbol {
                name: symbol.name.clone(),
                detail: symbol.detail.clone(),
                kind: document_symbol_kind(symbol.kind),
                tags: None,
                deprecated: None,
                range: lsp_range(symbol.span.clone()),
                selection_range: lsp_range(symbol.selection_span.clone()),
                children: None,
            }
        })
        .collect()
}
