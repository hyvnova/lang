//! Hover resolves a typed target first, then renders Markdown in one place.

use crate::analysis::{member_access_at, word_at, DocumentAnalysis, FieldValueInfo, VariableInfo};
use crate::lsp::convert::markdown_hover;
use crate::primitive_catalog::primitive_member;
use crate::std_catalog::{std_export, std_module, STD_MODULES};
use tower_lsp::lsp_types::Hover;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HoverTarget {
    Std,
    StdModule(String),
    StdExport(String),
    PrimitiveMember(String),
    LocalSymbol(String),
    StructField { receiver: String, field: String },
    Variable(String),
}

pub fn hover_at(analysis: &DocumentAnalysis, word: &str) -> Option<Hover> {
    hover_target_for_word(analysis, word).and_then(|target| render_hover_target(analysis, target))
}

pub fn hover_at_position(
    analysis: &DocumentAnalysis,
    line: usize,
    character: usize,
) -> Option<Hover> {
    if let Some((receiver, field)) = member_access_at(&analysis.source, line, character) {
        return render_hover_target(analysis, HoverTarget::StructField { receiver, field });
    }

    let word = word_at(&analysis.source, line, character)?;
    hover_at(analysis, &word)
}

pub fn variable_markdown(variable: &VariableInfo) -> String {
    let mut markdown = format!("```lang\n{}: {}\n```", variable.name, variable.type_name);
    if !variable.fields.is_empty() {
        markdown.push_str("\n\n**Fields**\n");
        for field in &variable.fields {
            let type_name = field.type_name.as_deref().unwrap_or("Any");
            markdown.push_str(&format!(
                "\n- `{}: {type_name} = {}`",
                field.name, field.value
            ));
        }
    }
    markdown
}

pub fn std_global_markdown() -> String {
    "The global `std` object is Lang's standard-library namespace. Use it directly without imports for file IO, env, time, random, terminal helpers, and compatibility facades.\n\n```lang\ntext = std.fs.read_text(\"input.txt\")\n    .expect(\"Failed to read input.txt\")\n    .strip()\n\nprint(text.words().len)\n```"
        .to_string()
}

fn hover_target_for_word(analysis: &DocumentAnalysis, word: &str) -> Option<HoverTarget> {
    if word == "std" {
        return Some(HoverTarget::Std);
    }
    if std_module(word).is_some() {
        return Some(HoverTarget::StdModule(word.to_string()));
    }
    for module in STD_MODULES {
        if std_export(module.name, word).is_some() {
            return Some(HoverTarget::StdExport(word.to_string()));
        }
    }
    if primitive_member(word).is_some() {
        return Some(HoverTarget::PrimitiveMember(word.to_string()));
    }
    if analysis
        .index
        .variables
        .iter()
        .any(|item| item.name == word)
    {
        return Some(HoverTarget::Variable(word.to_string()));
    }
    if analysis.index.symbols.iter().any(|item| item.name == word) {
        return Some(HoverTarget::LocalSymbol(word.to_string()));
    }
    None
}

fn render_hover_target(analysis: &DocumentAnalysis, target: HoverTarget) -> Option<Hover> {
    let markdown = match target {
        HoverTarget::Std => Some(std_global_markdown()),
        HoverTarget::StdModule(module) => std_module(&module).map(|module| module.docs.markdown()),
        HoverTarget::StdExport(export_name) => STD_MODULES
            .iter()
            .find_map(|module| std_export(module.name, &export_name))
            .map(|export| export.docs.markdown()),
        HoverTarget::PrimitiveMember(member) => {
            primitive_member(&member).map(|member| member.markdown())
        }
        HoverTarget::Variable(variable) => analysis
            .index
            .variables
            .iter()
            .find(|item| item.name == variable)
            .map(variable_markdown),
        HoverTarget::LocalSymbol(symbol_name) => analysis
            .index
            .symbols
            .iter()
            .find(|symbol| symbol.name == symbol_name)
            .and_then(|symbol| symbol.detail.clone())
            .map(|detail| format!("```lang\n{detail}\n```")),
        HoverTarget::StructField { receiver, field } => analysis
            .index
            .variables
            .iter()
            .find(|variable| variable.name == receiver)
            .and_then(|variable| {
                variable
                    .fields
                    .iter()
                    .find(|item| item.name == field)
                    .map(|field| field_markdown(variable, field))
            }),
    };
    markdown.map(markdown_hover)
}

fn field_markdown(variable: &VariableInfo, field: &FieldValueInfo) -> String {
    let type_name = field.type_name.as_deref().unwrap_or("Any");
    format!(
        "```lang\n{}.{}\n{}: {type_name} = {}\n```",
        variable.type_name, field.name, field.name, field.value
    )
}
