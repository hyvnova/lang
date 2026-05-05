//! Semantic tokens are lexical by design until parser recovery gets richer.

use crate::analysis::DocumentAnalysis;
use crate::primitive_catalog::all_primitive_members;
use tower_lsp::lsp_types::{
    SemanticToken, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
};

pub fn semantic_token_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::KEYWORD,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::TYPE,
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::METHOD,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::COMMENT,
            SemanticTokenType::EVENT,
            SemanticTokenType::MACRO,
        ],
        token_modifiers: vec![],
    }
}

pub fn semantic_tokens(analysis: &DocumentAnalysis) -> SemanticTokens {
    let mut raw = Vec::new();
    for (line_index, line) in analysis.source.lines().enumerate() {
        collect_line_semantic_tokens(line, line_index as u32, &mut raw);
    }
    raw.sort_by_key(|token| (token.line, token.start));

    let mut previous_line = 0;
    let mut previous_start = 0;
    let mut data = Vec::new();
    for token in raw {
        let delta_line = token.line - previous_line;
        let delta_start = if delta_line == 0 {
            token.start - previous_start
        } else {
            token.start
        };
        data.push(SemanticToken {
            delta_line,
            delta_start,
            length: token.length,
            token_type: token.kind,
            token_modifiers_bitset: 0,
        });
        previous_line = token.line;
        previous_start = token.start;
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}

#[derive(Debug)]
struct RawSemanticToken {
    line: u32,
    start: u32,
    length: u32,
    kind: u32,
}

fn collect_line_semantic_tokens(line: &str, line_number: u32, tokens: &mut Vec<RawSemanticToken>) {
    if let Some(comment_start) = line.find('#') {
        tokens.push(raw_token(
            line_number,
            comment_start,
            line.len() - comment_start,
            7,
        ));
    }

    for keyword in [
        "fn", "struct", "trait", "impl", "for", "while", "loop", "if", "elif", "else", "return",
        "break", "continue", "pub", "mod", "import", "from", "use", "as", "true", "false",
    ] {
        collect_word_tokens(line, line_number, keyword, 0, tokens);
    }
    for primitive in [
        "Str", "Num", "Bool", "Vec", "Map", "Set", "Result", "Option",
    ] {
        collect_word_tokens(line, line_number, primitive, 2, tokens);
    }
    collect_word_tokens(line, line_number, "std", 3, tokens);

    for member in all_primitive_members() {
        collect_member_tokens(line, line_number, member.name, 4, tokens);
    }

    collect_string_tokens(line, line_number, tokens);
    collect_number_tokens(line, line_number, tokens);
    collect_signal_tokens(line, line_number, tokens);

    for symbol in ["fn", "struct", "trait"] {
        if let Some(index) = line.find(symbol) {
            let rest = &line[index + symbol.len()..];
            if let Some((offset, len)) = next_identifier(rest) {
                let kind = if symbol == "fn" { 1 } else { 2 };
                tokens.push(raw_token(
                    line_number,
                    index + symbol.len() + offset,
                    len,
                    kind,
                ));
            }
        }
    }
}

fn collect_word_tokens(
    line: &str,
    line_number: u32,
    word: &str,
    kind: u32,
    tokens: &mut Vec<RawSemanticToken>,
) {
    for (index, _) in line.match_indices(word) {
        let before = index
            .checked_sub(1)
            .and_then(|i| line.as_bytes().get(i))
            .copied();
        let after = line.as_bytes().get(index + word.len()).copied();
        if !is_identifier_byte(before) && !is_identifier_byte(after) {
            tokens.push(raw_token(line_number, index, word.len(), kind));
        }
    }
}

fn collect_member_tokens(
    line: &str,
    line_number: u32,
    word: &str,
    kind: u32,
    tokens: &mut Vec<RawSemanticToken>,
) {
    for (index, _) in line.match_indices(&format!(".{word}")) {
        tokens.push(raw_token(line_number, index + 1, word.len(), kind));
    }
}

fn collect_string_tokens(line: &str, line_number: u32, tokens: &mut Vec<RawSemanticToken>) {
    let mut start = None;
    for (index, ch) in line.char_indices() {
        if ch == '"' {
            match start {
                Some(open) => {
                    tokens.push(raw_token(line_number, open, index + 1 - open, 5));
                    start = None;
                }
                None => start = Some(index),
            }
        }
    }
}

fn collect_number_tokens(line: &str, line_number: u32, tokens: &mut Vec<RawSemanticToken>) {
    let mut start = None;
    for (index, ch) in line.char_indices() {
        if ch.is_ascii_digit() {
            start.get_or_insert(index);
        } else if let Some(open) = start.take() {
            tokens.push(raw_token(line_number, open, index - open, 6));
        }
    }
    if let Some(open) = start {
        tokens.push(raw_token(line_number, open, line.len() - open, 6));
    }
}

fn collect_signal_tokens(line: &str, line_number: u32, tokens: &mut Vec<RawSemanticToken>) {
    for (index, _) in line.match_indices('$') {
        let rest = &line[index + 1..];
        if let Some((_, len)) = next_identifier(rest) {
            tokens.push(raw_token(line_number, index, len + 1, 8));
        }
    }
}

fn next_identifier(source: &str) -> Option<(usize, usize)> {
    let start = source.find(|ch: char| ch == '_' || ch.is_ascii_alphabetic())?;
    let len = source[start..]
        .chars()
        .take_while(|ch| *ch == '_' || ch.is_ascii_alphanumeric())
        .map(char::len_utf8)
        .sum();
    Some((start, len))
}

fn is_identifier_byte(byte: Option<u8>) -> bool {
    matches!(
        byte,
        Some(b'_') | Some(b'a'..=b'z') | Some(b'A'..=b'Z') | Some(b'0'..=b'9')
    )
}

fn raw_token(line: u32, start: usize, length: usize, kind: u32) -> RawSemanticToken {
    RawSemanticToken {
        line,
        start: start as u32,
        length: length.max(1) as u32,
        kind,
    }
}
