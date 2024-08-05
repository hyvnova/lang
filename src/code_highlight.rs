use syntect::easy::HighlightLines;
use syntect::highlighting::{ThemeSet, Style};
use syntect::parsing::SyntaxSet;
use syntect::util::{as_24_bit_terminal_escaped, LinesWithEndings};

fn lang_to_ext(s: &str) -> &'static str {
    match s {
        "python" => "py",
        _ => "txt",
    }
}

pub fn highlight_code(language: &str, code: &str) -> String {
    // Load syntax set and theme set
    let ps = SyntaxSet::load_defaults_newlines();
    let ts = ThemeSet::load_defaults();

    // Choose a syntax and theme
    let syntax = ps.find_syntax_by_extension(lang_to_ext(language)).unwrap();
    let mut h = HighlightLines::new(syntax, &ts.themes["base16-ocean.dark"]);

    // Highlight the lines
    let mut highlighted = String::new();
    for line in LinesWithEndings::from(code) {
        let ranges: Vec<(Style, &str)> = h.highlight_line(line, &ps).unwrap();
        highlighted.push_str(&as_24_bit_terminal_escaped(&ranges[..], true));
    }

    add_line_numbers(&highlighted)
}



fn add_line_numbers(source: &str) -> String {
    source
        .lines()
        .enumerate()
        .map(|(i, line)| format!("{:>4} | {}", i + 1, line))
        .collect::<Vec<String>>()
        .join("\n")
}