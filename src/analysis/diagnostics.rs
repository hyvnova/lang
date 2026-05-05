//! Diagnostics are compiler-facing facts first; LSP conversion happens later.

use std::path::Path;

use crate::analysis::span::SourceSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Information,
    Hint,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangDiagnostic {
    pub severity: DiagnosticSeverity,
    pub message: String,
    pub span: SourceSpan,
    pub source: &'static str,
    pub code: Option<String>,
}

pub(crate) fn error(
    path: Option<&Path>,
    line: usize,
    column: usize,
    length: usize,
    message: String,
    code: &str,
) -> LangDiagnostic {
    LangDiagnostic {
        severity: DiagnosticSeverity::Error,
        message,
        span: SourceSpan::new(path.map(Path::to_path_buf), line, column, length),
        source: "lang",
        code: Some(code.to_string()),
    }
}

pub(crate) fn delimiter_diagnostics(source: &str, path: Option<&Path>) -> Vec<LangDiagnostic> {
    let mut diagnostics = Vec::new();
    let mut stack: Vec<(char, usize, usize)> = Vec::new();
    let mut in_string: Option<(char, usize, usize)> = None;

    for (line_index, line) in source.lines().enumerate() {
        let line_number = line_index + 1;
        let mut chars = line.chars().enumerate().peekable();

        while let Some((column_index, ch)) = chars.next() {
            let column = column_index + 1;

            if let Some((quote, _, _)) = in_string {
                if ch == '\\' {
                    let _ = chars.next();
                    continue;
                }
                if ch == quote {
                    in_string = None;
                }
                continue;
            }

            if ch == '"' || ch == '\'' {
                in_string = Some((ch, line_number, column));
                continue;
            }

            if ch == '#' {
                break;
            }

            match ch {
                '(' | '{' | '[' => stack.push((ch, line_number, column)),
                ')' | '}' | ']' => match stack.pop() {
                    Some((open, _, _)) if delimiters_match(open, ch) => {}
                    Some((open, open_line, open_column)) => diagnostics.push(error(
                        path,
                        line_number,
                        column,
                        1,
                        format!(
                            "Mismatched closing delimiter '{ch}'. Opened '{open}' at line {open_line}, column {open_column}."
                        ),
                        "delimiter-mismatch",
                    )),
                    None => diagnostics.push(error(
                        path,
                        line_number,
                        column,
                        1,
                        format!("Unexpected closing delimiter '{ch}'."),
                        "unexpected-closing-delimiter",
                    )),
                },
                _ => {}
            }
        }
    }

    if let Some((quote, line, column)) = in_string {
        diagnostics.push(error(
            path,
            line,
            column,
            1,
            format!("Missing closing quote for string literal opened with '{quote}'."),
            "unclosed-string",
        ));
    }

    for (open, line, column) in stack.into_iter().rev() {
        diagnostics.push(error(
            path,
            line,
            column,
            1,
            format!("Missing closing delimiter for '{open}'."),
            "unclosed-delimiter",
        ));
    }

    diagnostics
}

fn delimiters_match(open: char, close: char) -> bool {
    matches!((open, close), ('(', ')') | ('{', '}') | ('[', ']'))
}
