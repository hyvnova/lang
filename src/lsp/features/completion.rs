//! Completion chooses a typed target before creating editor items.

use crate::analysis::{context::receiver_before_dot, DocumentAnalysis, VariableInfo};
use crate::lsp::convert::{completion_kind, markdown_docs};
use crate::primitive_catalog::all_primitive_members;
use crate::std_catalog::STD_MODULES;
use tower_lsp::lsp_types::{CompletionItem, CompletionItemKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompletionTarget {
    Global,
    StdRoot,
    StdModule(String),
    ReceiverFields(String),
    PrimitiveMethods,
}

impl CompletionTarget {
    fn from_context(context: &str) -> Self {
        let trimmed = context.trim_end();
        if trimmed.ends_with("std.") {
            return CompletionTarget::StdRoot;
        }
        if let Some(module) = std_member_context(trimmed) {
            return CompletionTarget::StdModule(module.to_string());
        }
        if trimmed.ends_with('.') {
            return receiver_before_dot(trimmed)
                .map(|receiver| CompletionTarget::ReceiverFields(receiver.to_string()))
                .unwrap_or(CompletionTarget::PrimitiveMethods);
        }
        CompletionTarget::Global
    }
}

pub fn completion_items(analysis: &DocumentAnalysis, context: &str) -> Vec<CompletionItem> {
    match CompletionTarget::from_context(context) {
        CompletionTarget::StdRoot => std_root_items(),
        CompletionTarget::StdModule(module) => std_module_items(&module),
        CompletionTarget::ReceiverFields(receiver) => analysis
            .index
            .variables
            .iter()
            .find(|item| item.name == receiver)
            .map(field_completion_items)
            .unwrap_or_else(primitive_completion_items),
        CompletionTarget::PrimitiveMethods => primitive_completion_items(),
        CompletionTarget::Global => global_items(analysis),
    }
}

fn std_root_items() -> Vec<CompletionItem> {
    STD_MODULES
        .iter()
        .filter(|module| module.name != "prelude")
        .map(|module| CompletionItem {
            label: module.name.to_string(),
            kind: Some(CompletionItemKind::MODULE),
            detail: Some(format!("std.{}", module.name)),
            documentation: Some(markdown_docs(module.docs.markdown())),
            ..CompletionItem::default()
        })
        .collect()
}

fn std_module_items(module_name: &str) -> Vec<CompletionItem> {
    STD_MODULES
        .iter()
        .find(|module| module.name == module_name)
        .map(|module| {
            module
                .exports
                .iter()
                .map(|export| CompletionItem {
                    label: export.name.to_string(),
                    kind: Some(CompletionItemKind::FUNCTION),
                    detail: Some(export.docs.signature.to_string()),
                    documentation: Some(markdown_docs(export.docs.markdown())),
                    ..CompletionItem::default()
                })
                .collect()
        })
        .unwrap_or_default()
}

fn global_items(analysis: &DocumentAnalysis) -> Vec<CompletionItem> {
    let mut items = keyword_completion_items();
    items.push(CompletionItem {
        label: "std".to_string(),
        kind: Some(CompletionItemKind::MODULE),
        detail: Some("Lang standard library".to_string()),
        documentation: Some(markdown_docs(
            crate::lsp::features::hover::std_global_markdown(),
        )),
        ..CompletionItem::default()
    });

    for symbol in &analysis.index.symbols {
        items.push(CompletionItem {
            label: symbol.name.clone(),
            kind: Some(completion_kind(symbol.kind)),
            detail: symbol.detail.clone(),
            ..CompletionItem::default()
        });
    }

    items.extend(primitive_completion_items());
    dedupe_completion_items(items)
}

fn primitive_completion_items() -> Vec<CompletionItem> {
    all_primitive_members()
        .map(|member| CompletionItem {
            label: member.name.to_string(),
            kind: Some(CompletionItemKind::METHOD),
            detail: Some(member.signature()),
            documentation: Some(markdown_docs(member.markdown())),
            ..CompletionItem::default()
        })
        .collect()
}

fn field_completion_items(variable: &VariableInfo) -> Vec<CompletionItem> {
    variable
        .fields
        .iter()
        .map(|field| {
            let type_name = field.type_name.as_deref().unwrap_or("Any");
            CompletionItem {
                label: field.name.clone(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(format!("{}: {type_name}", field.name)),
                documentation: Some(markdown_docs(format!(
                    "Field on `{}`.\n\n```lang\n{}: {type_name} = {}\n```",
                    variable.type_name, field.name, field.value
                ))),
                ..CompletionItem::default()
            }
        })
        .collect()
}

fn keyword_completion_items() -> Vec<CompletionItem> {
    [
        "fn", "struct", "trait", "impl", "for", "while", "loop", "if", "elif", "else", "return",
        "break", "continue", "pub", "mod", "import", "from", "use", "as", "true", "false",
    ]
    .into_iter()
    .map(|keyword| CompletionItem {
        label: keyword.to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        ..CompletionItem::default()
    })
    .collect()
}

fn dedupe_completion_items(items: Vec<CompletionItem>) -> Vec<CompletionItem> {
    let mut seen = std::collections::BTreeSet::new();
    items
        .into_iter()
        .filter(|item| seen.insert(item.label.clone()))
        .collect()
}

fn std_member_context(context: &str) -> Option<&str> {
    let context = context.strip_suffix('.')?;
    let parts = context.rsplit('.').take(2).collect::<Vec<_>>();
    if parts.len() == 2 && parts[1] == "std" {
        Some(parts[0])
    } else {
        None
    }
}
