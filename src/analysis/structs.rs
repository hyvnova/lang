//! Struct facts power IDE help before Lang has a full type checker.

use regex::Regex;
use std::path::Path;

use crate::analysis::span::SourceSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<StructFieldInfo>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructFieldInfo {
    pub name: String,
    pub type_name: String,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariableInfo {
    pub name: String,
    pub type_name: String,
    pub fields: Vec<FieldValueInfo>,
    pub span: SourceSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldValueInfo {
    pub name: String,
    pub type_name: Option<String>,
    pub value: String,
    pub span: SourceSpan,
}

pub(crate) fn collect_structs(source: &str, path: Option<&Path>) -> Vec<StructInfo> {
    let start_re = Regex::new(r"^\s*(?:pub\s+)?struct\s+([A-Za-z_][A-Za-z0-9_]*)").unwrap();
    let field_re =
        Regex::new(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([A-Za-z_][A-Za-z0-9_\.<>]*)\s*,?")
            .unwrap();
    let mut structs = Vec::new();
    let mut current: Option<StructInfo> = None;

    for (index, line) in source.lines().enumerate() {
        let line_number = index + 1;

        if let Some(mut item) = current.take() {
            if line.contains('}') {
                structs.push(item);
                continue;
            }

            if let Some(captures) = field_re.captures(line) {
                let name = captures.get(1).unwrap();
                let type_name = captures.get(2).unwrap();
                item.fields.push(StructFieldInfo {
                    name: name.as_str().to_string(),
                    type_name: type_name.as_str().to_string(),
                    span: SourceSpan::new(
                        path.map(Path::to_path_buf),
                        line_number,
                        name.start() + 1,
                        name.as_str().len(),
                    ),
                });
            }

            current = Some(item);
            continue;
        }

        if let Some(captures) = start_re.captures(line) {
            let name = captures.get(1).unwrap();
            current = Some(StructInfo {
                name: name.as_str().to_string(),
                fields: Vec::new(),
                span: SourceSpan::new(
                    path.map(Path::to_path_buf),
                    line_number,
                    name.start() + 1,
                    name.as_str().len(),
                ),
            });
        }
    }

    if let Some(item) = current {
        structs.push(item);
    }

    structs
}

pub(crate) fn collect_struct_literal_variables(
    source: &str,
    path: Option<&Path>,
    structs: &[StructInfo],
) -> Vec<VariableInfo> {
    let binding_re = Regex::new(
        r"^\s*(?:pub\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=\s*([A-Za-z_][A-Za-z0-9_]*)\s*\{(.*)\}",
    )
    .unwrap();
    let mut variables = Vec::new();

    for (index, line) in source.lines().enumerate() {
        let line_number = index + 1;
        let Some(captures) = binding_re.captures(line) else {
            continue;
        };

        let name = captures.get(1).unwrap();
        let type_name = captures.get(2).unwrap().as_str().to_string();
        let fields_source = captures.get(3).map(|value| value.as_str()).unwrap_or("");
        let struct_info = structs.iter().find(|item| item.name == type_name);

        variables.push(VariableInfo {
            name: name.as_str().to_string(),
            type_name,
            fields: parse_struct_literal_fields(
                fields_source,
                path,
                line_number,
                line,
                struct_info,
            ),
            span: SourceSpan::new(
                path.map(Path::to_path_buf),
                line_number,
                name.start() + 1,
                name.as_str().len(),
            ),
        });
    }

    variables
}

fn parse_struct_literal_fields(
    fields_source: &str,
    path: Option<&Path>,
    line_number: usize,
    full_line: &str,
    struct_info: Option<&StructInfo>,
) -> Vec<FieldValueInfo> {
    fields_source
        .split(',')
        .filter_map(|part| {
            let (name, value) = part.split_once(':')?;
            let name = name.trim();
            let value = value.trim();
            if name.is_empty() || value.is_empty() {
                return None;
            }

            let type_name = struct_info.and_then(|item| {
                item.fields
                    .iter()
                    .find(|field| field.name == name)
                    .map(|field| field.type_name.clone())
            });
            let column = full_line.find(name).map(|column| column + 1).unwrap_or(1);

            Some(FieldValueInfo {
                name: name.to_string(),
                type_name,
                value: value.to_string(),
                span: SourceSpan::new(path.map(Path::to_path_buf), line_number, column, name.len()),
            })
        })
        .collect()
}
