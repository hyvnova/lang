//! Cursor context preserves syntax around a word so IDE features answer precisely.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CursorContext {
    Empty,
    BareWord(String),
    MemberAccess { receiver: String, member: String },
    StdPath(Vec<String>),
    CallSite { callee: String, args: String },
}

impl CursorContext {
    pub fn at(source: &str, line: usize, character: usize) -> Self {
        let line_text = match source.lines().nth(line) {
            Some(line_text) => line_text,
            None => return CursorContext::Empty,
        };
        let prefix = line_text.chars().take(character).collect::<String>();

        if let Some((callee, args)) = prefix.rsplit_once('(') {
            return CursorContext::CallSite {
                callee: callee.trim().to_string(),
                args: args.to_string(),
            };
        }

        if let Some((receiver, member)) = member_access_at(source, line, character) {
            return CursorContext::MemberAccess { receiver, member };
        }

        let trimmed = prefix.trim_end();
        if trimmed == "std" || trimmed.starts_with("std.") {
            return CursorContext::StdPath(trimmed.split('.').map(str::to_string).collect());
        }

        word_at(source, line, character)
            .map(CursorContext::BareWord)
            .unwrap_or(CursorContext::Empty)
    }
}

pub fn word_at(source: &str, line: usize, character: usize) -> Option<String> {
    let line_text = source.lines().nth(line)?;
    let byte_index = char_to_byte_index(line_text, character);
    let (start, end) = identifier_bounds(line_text, byte_index)?;
    Some(line_text[start..end].to_string())
}

pub fn completion_context(source: &str, line: usize, character: usize) -> String {
    source
        .lines()
        .nth(line)
        .map(|line_text| {
            line_text
                .chars()
                .take(character)
                .collect::<String>()
                .trim_end()
                .to_string()
        })
        .unwrap_or_default()
}

pub fn member_access_at(source: &str, line: usize, character: usize) -> Option<(String, String)> {
    let line_text = source.lines().nth(line)?;
    let byte_index = char_to_byte_index(line_text, character);
    let (start, end) = identifier_bounds(line_text, byte_index)?;

    if start == 0 || !line_text[..start].ends_with('.') {
        return None;
    }

    let receiver_end = start - 1;
    let receiver_start = line_text[..receiver_end]
        .rfind(|ch: char| !is_ident_char(ch))
        .map(|index| index + 1)
        .unwrap_or(0);
    let receiver = line_text[receiver_start..receiver_end].trim();
    let member = line_text[start..end].trim();

    (!receiver.is_empty() && !member.is_empty()).then(|| (receiver.to_string(), member.to_string()))
}

pub fn receiver_before_dot(context: &str) -> Option<&str> {
    let context = context.strip_suffix('.')?.trim_end();
    let end = context.len();
    let start = context[..end]
        .rfind(|ch: char| !is_ident_char(ch))
        .map(|index| index + 1)
        .unwrap_or(0);
    let receiver = &context[start..end];
    (!receiver.is_empty()).then_some(receiver)
}

fn identifier_bounds(line_text: &str, byte_index: usize) -> Option<(usize, usize)> {
    let mut start = byte_index.min(line_text.len());
    while start > 0 {
        let previous = line_text[..start].char_indices().last();
        match previous {
            Some((index, ch)) if is_ident_char(ch) => start = index,
            _ => break,
        }
    }

    let mut end = byte_index.min(line_text.len());
    while end < line_text.len() {
        let mut iter = line_text[end..].char_indices();
        match iter.next() {
            Some((_, ch)) if is_ident_char(ch) => end += ch.len_utf8(),
            _ => break,
        }
    }

    (start != end).then_some((start, end))
}

fn char_to_byte_index(line_text: &str, character: usize) -> usize {
    line_text
        .char_indices()
        .nth(character)
        .map(|(index, _)| index)
        .unwrap_or(line_text.len())
}

fn is_ident_char(ch: char) -> bool {
    ch == '_' || ch.is_ascii_alphanumeric()
}
