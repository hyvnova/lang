//! Symbol indexing keeps IDE providers from rediscovering names independently.

use regex::Regex;
use std::path::{Path, PathBuf};

use crate::analysis::span::SourceSpan;
use crate::analysis::structs::{StructInfo, VariableInfo};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Struct,
    Trait,
    Impl,
    Module,
    Variable,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangSymbol {
    pub name: String,
    pub kind: SymbolKind,
    pub span: SourceSpan,
    pub selection_span: SourceSpan,
    pub detail: Option<String>,
    pub public: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinitionTarget {
    pub name: String,
    pub path: Option<PathBuf>,
    pub span: SourceSpan,
    pub detail: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct SymbolIndex {
    pub symbols: Vec<LangSymbol>,
    pub definitions: Vec<DefinitionTarget>,
    pub structs: Vec<StructInfo>,
    pub variables: Vec<VariableInfo>,
    pub imports: Vec<crate::analysis::imports::ImportReference>,
}

impl SymbolIndex {
    pub(crate) fn new(
        symbols: Vec<LangSymbol>,
        definitions: Vec<DefinitionTarget>,
        structs: Vec<StructInfo>,
        variables: Vec<VariableInfo>,
        imports: Vec<crate::analysis::imports::ImportReference>,
    ) -> Self {
        Self {
            symbols,
            definitions,
            structs,
            variables,
            imports,
        }
    }
}

pub(crate) fn collect_symbols(
    source: &str,
    path: Option<&Path>,
    variables: &[VariableInfo],
) -> Vec<LangSymbol> {
    let function_re =
        Regex::new(r"^\s*(pub\s+)?fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*(?:->\s*([A-Za-z_][A-Za-z0-9_\.<>]*))?").unwrap();
    let struct_re = Regex::new(r"^\s*(pub\s+)?struct\s+([A-Za-z_][A-Za-z0-9_]*)").unwrap();
    let trait_re = Regex::new(r"^\s*(pub\s+)?trait\s+([A-Za-z_][A-Za-z0-9_]*)").unwrap();
    let impl_re = Regex::new(r"^\s*impl(?:\s+[A-Za-z_][A-Za-z0-9_\.<>]*)?\s+for\s+([A-Za-z_][A-Za-z0-9_\.<>]*)|^\s*impl\s+([A-Za-z_][A-Za-z0-9_\.<>]*)").unwrap();
    let mod_re = Regex::new(r"^\s*(pub\s+)?mod\s+([A-Za-z_][A-Za-z0-9_]*)").unwrap();
    let binding_re = Regex::new(r"^\s*(pub\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=").unwrap();

    let mut symbols = Vec::new();

    for (index, line) in source.lines().enumerate() {
        let line_number = index + 1;
        if let Some(captures) = function_re.captures(line) {
            let name = captures.get(2).unwrap();
            let params = captures.get(3).map(|param| param.as_str()).unwrap_or("");
            let return_type = captures.get(4).map(|value| value.as_str()).unwrap_or("Any");
            symbols.push(symbol(
                path,
                line_number,
                name.start() + 1,
                name.as_str(),
                SymbolKind::Function,
                captures.get(1).is_some(),
                Some(format!("fn {}({params}) -> {return_type}", name.as_str())),
            ));
            continue;
        }

        if let Some(item) = symbol_from_regex(
            &struct_re,
            line,
            path,
            line_number,
            SymbolKind::Struct,
            "struct",
        ) {
            symbols.push(item);
            continue;
        }
        if let Some(item) = symbol_from_regex(
            &trait_re,
            line,
            path,
            line_number,
            SymbolKind::Trait,
            "trait",
        ) {
            symbols.push(item);
            continue;
        }
        if let Some(item) =
            symbol_from_regex(&mod_re, line, path, line_number, SymbolKind::Module, "mod")
        {
            symbols.push(item);
            continue;
        }

        if let Some(captures) = impl_re.captures(line) {
            let name = captures.get(1).or_else(|| captures.get(2)).unwrap();
            symbols.push(symbol(
                path,
                line_number,
                name.start() + 1,
                name.as_str(),
                SymbolKind::Impl,
                false,
                Some(format!("impl {}", name.as_str())),
            ));
            continue;
        }

        if let Some(captures) = binding_re.captures(line) {
            let name = captures.get(2).unwrap();
            if !matches!(name.as_str(), "if" | "elif" | "while" | "for") {
                let detail = variables
                    .iter()
                    .find(|variable| variable.name == name.as_str())
                    .map(|variable| format!("let {}: {}", variable.name, variable.type_name))
                    .unwrap_or_else(|| format!("let {}", name.as_str()));
                symbols.push(symbol(
                    path,
                    line_number,
                    name.start() + 1,
                    name.as_str(),
                    SymbolKind::Variable,
                    captures.get(1).is_some(),
                    Some(detail),
                ));
            }
        }
    }

    symbols
}

pub(crate) fn definitions_from_symbols(symbols: &[LangSymbol]) -> Vec<DefinitionTarget> {
    symbols
        .iter()
        .map(|symbol| DefinitionTarget {
            name: symbol.name.clone(),
            path: symbol.span.path.clone(),
            span: symbol.selection_span.clone(),
            detail: symbol.detail.clone(),
        })
        .collect()
}

pub(crate) fn symbol(
    path: Option<&Path>,
    line: usize,
    column: usize,
    name: &str,
    kind: SymbolKind,
    public: bool,
    detail: Option<String>,
) -> LangSymbol {
    let selection_span = SourceSpan::new(path.map(Path::to_path_buf), line, column, name.len());
    LangSymbol {
        name: name.to_string(),
        kind,
        span: selection_span.clone(),
        selection_span,
        detail,
        public,
    }
}

fn symbol_from_regex(
    regex: &Regex,
    line: &str,
    path: Option<&Path>,
    line_number: usize,
    kind: SymbolKind,
    label: &str,
) -> Option<LangSymbol> {
    let captures = regex.captures(line)?;
    let name = captures.get(2).unwrap();
    Some(symbol(
        path,
        line_number,
        name.start() + 1,
        name.as_str(),
        kind,
        captures.get(1).is_some(),
        Some(format!("{label} {}", name.as_str())),
    ))
}
