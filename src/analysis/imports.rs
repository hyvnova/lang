//! Import analysis is intentionally shallow: enough for diagnostics and IDE links.

use regex::Regex;
use std::path::{Path, PathBuf};

use crate::analysis::diagnostics::{error, LangDiagnostic};
use crate::analysis::span::SourceSpan;
use crate::analysis::symbols::{collect_symbols, symbol, DefinitionTarget, LangSymbol, SymbolKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportReference {
    pub path: String,
    pub alias: Option<String>,
    pub line: usize,
    pub column: usize,
}

pub(crate) fn import_references(source: &str) -> Vec<ImportReference> {
    let import_re = Regex::new(
        r"^\s*import\s+([A-Za-z_\.][A-Za-z0-9_\.]*)(?:\s+as\s+([A-Za-z_][A-Za-z0-9_]*))?",
    )
    .unwrap();
    let from_re = Regex::new(r"^\s*from\s+([A-Za-z_\.][A-Za-z0-9_\.]*)\s+import\b").unwrap();
    let mut references = Vec::new();

    for (index, line) in source.lines().enumerate() {
        let line_number = index + 1;
        if let Some(captures) = import_re.captures(line) {
            let path = captures.get(1).unwrap();
            references.push(ImportReference {
                path: path.as_str().to_string(),
                alias: captures.get(2).map(|alias| alias.as_str().to_string()),
                line: line_number,
                column: path.start() + 1,
            });
        } else if let Some(captures) = from_re.captures(line) {
            let path = captures.get(1).unwrap();
            references.push(ImportReference {
                path: path.as_str().to_string(),
                alias: None,
                line: line_number,
                column: path.start() + 1,
            });
        }
    }

    references
}

pub(crate) fn import_diagnostics(
    references: &[ImportReference],
    path: Option<&Path>,
    project_root: Option<&Path>,
) -> Vec<LangDiagnostic> {
    references
        .iter()
        .filter_map(|reference| {
            if reference.path.starts_with("std") {
                return None;
            }

            resolve_module_path(&reference.path, path, project_root)
                .is_none()
                .then(|| {
                    error(
                        path,
                        reference.line,
                        reference.column,
                        reference.path.len(),
                        format!("Could not resolve module '{}'.", reference.path),
                        "missing-module",
                    )
                })
        })
        .collect()
}

pub(crate) fn import_definitions(
    references: &[ImportReference],
    path: Option<&Path>,
    project_root: Option<&Path>,
) -> Vec<DefinitionTarget> {
    references
        .iter()
        .filter_map(|reference| {
            let target_path = resolve_module_path(&reference.path, path, project_root)?;
            Some(DefinitionTarget {
                name: reference
                    .alias
                    .clone()
                    .unwrap_or_else(|| reference.path.rsplit('.').next().unwrap_or("").to_string()),
                path: Some(target_path.clone()),
                span: SourceSpan::new(Some(target_path), 1, 1, 1),
                detail: Some(format!("module {}", reference.path)),
            })
        })
        .collect()
}

pub(crate) fn imported_symbols(
    references: &[ImportReference],
    path: Option<&Path>,
    project_root: Option<&Path>,
) -> Vec<LangSymbol> {
    let mut symbols = Vec::new();

    for reference in references {
        let Some(target_path) = resolve_module_path(&reference.path, path, project_root) else {
            continue;
        };

        let module_name = reference
            .alias
            .clone()
            .unwrap_or_else(|| reference.path.rsplit('.').next().unwrap_or("").to_string());
        symbols.push(symbol(
            Some(&target_path),
            1,
            1,
            &module_name,
            SymbolKind::Module,
            true,
            Some(format!("module {}", reference.path)),
        ));

        let Ok(source) = std::fs::read_to_string(&target_path) else {
            continue;
        };
        symbols.extend(
            collect_symbols(&source, Some(&target_path), &[])
                .into_iter()
                .filter(|symbol| symbol.public),
        );
    }

    symbols
}

fn resolve_module_path(
    module_path: &str,
    source_path: Option<&Path>,
    project_root: Option<&Path>,
) -> Option<PathBuf> {
    let mut base = if module_path.starts_with('.') {
        source_path?.parent()?.to_path_buf()
    } else {
        project_root?.to_path_buf()
    };

    let segments = if module_path.starts_with('.') {
        let leading_dots = module_path.chars().take_while(|ch| *ch == '.').count();
        for _ in 1..leading_dots {
            base.pop();
        }
        module_path[leading_dots..]
            .split('.')
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
    } else {
        module_path
            .split('.')
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>()
    };

    for segment in segments {
        base.push(segment);
    }

    let file_candidate = base.with_extension("lang");
    if file_candidate.exists() {
        return Some(file_candidate);
    }

    let mod_candidate = base.join("mod.lang");
    if mod_candidate.exists() {
        return Some(mod_candidate);
    }

    None
}
