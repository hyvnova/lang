//! Document analysis is the stable contract between compiler-ish facts and IDEs.

use std::path::PathBuf;

use crate::analysis::diagnostics::{delimiter_diagnostics, LangDiagnostic};
use crate::analysis::imports::{
    import_definitions, import_diagnostics, import_references, imported_symbols,
};
use crate::analysis::structs::{collect_struct_literal_variables, collect_structs};
use crate::analysis::symbols::{collect_symbols, definitions_from_symbols, SymbolIndex};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DocumentAnalysis {
    pub source: String,
    pub path: Option<PathBuf>,
    pub project_root: Option<PathBuf>,
    pub diagnostics: Vec<LangDiagnostic>,
    pub index: SymbolIndex,
}

pub fn analyze_document(
    source: &str,
    path: Option<PathBuf>,
    project_root: Option<PathBuf>,
) -> DocumentAnalysis {
    let imports = import_references(source);
    let structs = collect_structs(source, path.as_deref());
    let variables = collect_struct_literal_variables(source, path.as_deref(), &structs);

    let mut diagnostics = Vec::new();
    diagnostics.extend(delimiter_diagnostics(source, path.as_deref()));
    diagnostics.extend(import_diagnostics(
        &imports,
        path.as_deref(),
        project_root.as_deref(),
    ));

    let mut symbols = collect_symbols(source, path.as_deref(), &variables);
    symbols.extend(imported_symbols(
        &imports,
        path.as_deref(),
        project_root.as_deref(),
    ));

    let mut definitions = definitions_from_symbols(&symbols);
    definitions.extend(import_definitions(
        &imports,
        path.as_deref(),
        project_root.as_deref(),
    ));

    DocumentAnalysis {
        source: source.to_string(),
        path,
        project_root,
        diagnostics,
        index: SymbolIndex::new(symbols, definitions, structs, variables, imports),
    }
}
