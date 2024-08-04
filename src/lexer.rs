use phf::phf_map;

use crate::log_utils::log_error;
use std::{borrow::Borrow, path::PathBuf};

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum TokenKind {
    // Special
    EOF,      // End of file
    NEW_LINE, // New line

    // Statement
    _stmt_start, // index to start of statement tokens
    //   Instructions
    DEF,
    ASSIGN,   // =
    IMPORT,   // import
    CONTINUE, // continue
    BREAK,    // break
    LOOP,     // loop
    FROM,     // from
    AS,       // as  (for aliasing)

    PYTHON, // Python code. When this keyword appears, everything after it is considered python code until it appears again

    _stmt_end, // index to end of statement tokens

    // Expressions
    _expr_start, // index to start of expression tokens
    //   Parenthesis
    L_PARENT,
    R_PARENT,

    //   Brackets
    L_BRACKET,
    R_BRACKET,

    //   Square brackets
    L_SQUARE_BRACKET,
    R_SQUARE_BRACKET,

    //   Values
    _value_start, // index to start of value tokens

    IDENTIFIER,
    NUMBER,
    STRING,

    _value_end, // index to end of value tokens

    //   Quotes
    SINGLE_QUOTE, // '
    DOUBLE_QUOTE, // "
    BACK_TICK,    // `

    //   Arithmetic
    _op_start, // index to start of operator tokens

    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    POW,
    MOD,

    // Logical
    //   Comparison
    EQ, // ==
    NE, // !=
    LT, // <
    LE, // <=
    GT, // >
    GE, // >=

    _bitwise_start, // index to start of bitwise operator tokens

    NEG, // !
    AND, // &
    OR,  // |
    XOR, // ^
    NOT, // ~

    _op_end, // index to end of operator tokens

    // Syntax
    SEMICOLON, // ;
    COLON,     // :
    COMMA,     // ,
    D_DOT,      // .. (double dot - range operator)
    DOT,       // .
    HASH,      // #
    AT,        // @

    _expr_end, // index to end of expression tokens
}

const KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "loop" => TokenKind::LOOP,
    "continue" => TokenKind::CONTINUE,
    "break" => TokenKind::BREAK,
    "def" => TokenKind::DEF,
    "import" => TokenKind::IMPORT,
    "from" => TokenKind::FROM,
    "as" => TokenKind::AS,
};

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        *self as i16 == *other as i16
    }
}

///     Represents a single token contains it's associated value and metadata
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "<Token {:?} = {} >", self.kind, self.value)
    }
}

/// Chekcing equallity between 2 Tokens
impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

/// A lexer reads the given path and returns a callable that can use to "consume" the next token
/// In other words, the lexer works lazily, only reading the next token when it's needed
pub struct Lexer {
    source: String,

    pub current_char_index: usize,

    pub line: usize,
    pub column: usize,

    // Token value (if needed)
    current_token_value: Option<String>,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Lexer {
            source,
            current_char_index: 0,

            line: 1,
            column: 0,

            current_token_value: None,
        }
    }

    pub fn from_path(path: PathBuf) -> Self {
        let source = std::fs::read_to_string(path).expect("Could not read source file");
        Lexer::new(source)
    }

    pub fn next(&mut self) -> Option<Token> {
        let raw = self.next_raw_token();

        if raw == TokenKind::EOF {
            return None;
        }

        return Some(Token {
            kind: raw,
            value: self.current_token_value.take().unwrap(),
            line: self.line,
            column: self.current_char_index,
        });
    }

    fn next_raw_token(&mut self) -> TokenKind {
        let ch: Option<char> = self.next_char();

        if ch.is_none() {
            return TokenKind::EOF;
        }

        let ch = ch.unwrap();

        // Skip whitespaces
        if ch.is_whitespace() {
            // New line
            if ch == '\n' {
                self.line += 1;
                self.column = 0;
                self.current_token_value = Some("\n".to_string());
                return TokenKind::NEW_LINE;
            }

            return self.next_raw_token();
        }

        // * Number
        if ch.is_numeric() {
            self.capture(ch, |char| char.is_numeric() || char == '.');

            // Ensure only 1 dot is present
            if self
                .current_token_value
                .as_ref()
                .unwrap()
                .matches('.')
                .count()
                > 1
            {
                log_error(
                    &["Invalid number".to_string()],
                    self.line,
                    self.current_char_index,
                );
            }

            return TokenKind::NUMBER;
        }

        // * Identifier
        if Lexer::is_identifier(ch) {
            self.capture(ch, Lexer::is_identifier);

            // ! Keywords
            return KEYWORDS
                .get(self.current_token_value.as_ref().unwrap().as_str())
                .copied()
                .unwrap_or(TokenKind::IDENTIFIER);
        }

        // * Special characters
        self.current_token_value = Some(ch.to_string());

        match ch {
            // Comparison
            '=' if self.peek_next_char() == Some('=') => {
                self.current_char_index += 1; // Move past =
                self.current_token_value = Some("==".to_string());
                return TokenKind::EQ;
            }

            '<' if self.peek_next_char() == Some('=') => {
                self.current_char_index += 1; // Move past =
                self.current_token_value = Some("<=".to_string());
                return TokenKind::LE;
            }

            '>' if self.peek_next_char() == Some('=') => {
                self.current_char_index += 1; // Move past =
                self.current_token_value = Some(">=".to_string());
                return TokenKind::GE;
            }

            '!' if self.peek_next_char() == Some('=') => {
                self.current_char_index += 1; // Move past =
                self.current_token_value = Some("!=".to_string());
                return TokenKind::NE;
            }

            '!' => return TokenKind::NEG,
            '<' => return TokenKind::LT,
            '>' => return TokenKind::GT,

            // Bitwise
            '&' => return TokenKind::AND,
            '|' => return TokenKind::OR,
            '^' => return TokenKind::XOR,
            '~' => return TokenKind::NOT,

            // Instruction
            '=' => return TokenKind::ASSIGN,

            // Parenthesis
            '(' => return TokenKind::L_PARENT,
            ')' => return TokenKind::R_PARENT,

            // Brackets
            '{' => return TokenKind::L_BRACKET,
            '}' => return TokenKind::R_BRACKET,

            // Square brackets
            '[' => return TokenKind::L_SQUARE_BRACKET,
            ']' => return TokenKind::R_SQUARE_BRACKET,

            // Arithmetic
            '+' => return TokenKind::ADD,
            '-' => return TokenKind::SUBTRACT,
            _ if ch == '*' && self.peek_next_char() == Some('*') => {
                self.current_char_index += 1; // Move past *
                self.column += 1;
                self.current_token_value = Some("**".to_string());
                return TokenKind::POW;
            }

            '*' => return TokenKind::MULTIPLY,
            '/' => return TokenKind::DIVIDE,
            '%' => return TokenKind::MOD,

            // Syntax
            ';' => return TokenKind::SEMICOLON,
            ':' => return TokenKind::COLON,
            ',' => return TokenKind::COMMA,
                
            '.' if self.peek_next_char() == Some('.') => {
                self.current_char_index += 1; // Move past .
                self.column += 1;
                self.current_token_value = Some("..".to_string());
                return TokenKind::D_DOT;
            }
            
            '.' => return TokenKind::DOT,
            '#' => {
                // ! Special keywords
                self.capture(' ', |char| !&[' ', '\n'].contains(&char));

                if self.current_token_value.is_none() {
                    return TokenKind::HASH;
                }

                match self.current_token_value.as_ref().unwrap().trim() {
                    "[python]" => {
                        self.current_token_value = Some("".to_string());

                        // Capture python code until #[python]
                        let mut python_code = String::new();
                        loop {
                            let ch = self.next_char();
                            if ch.is_none() {
                                break;
                            }

                            if ch.unwrap() == '#' {
                                let mut buffer: Vec<char> = Vec::with_capacity(11);
                                for _ in 0..11 {
                                    let ch = self.next_char();
                                    if ch.is_none() {
                                        break;
                                    }
                                    buffer.push(ch.unwrap());
                                }

                                let buff_str = buffer.iter().collect::<String>();
                                println!("Buffer str  {}", buff_str);
                                if buff_str == "[endpython]" {
                                    break;
                                } else {
                                    python_code.push('#');
                                    python_code.push_str(&buff_str);
                                }
                            }
                            python_code.push(ch.unwrap());
                        }
                        self.current_token_value = Some(python_code);
                        return TokenKind::PYTHON;
                    }

                    _ => {
                        // move back to the start of the capture
                        self.current_char_index -= self.current_token_value.as_ref().unwrap().len();

                        self.current_token_value = Some("#".to_string());
                        return TokenKind::HASH;
                    }
                }
            }
            '@' => return TokenKind::AT,

            // Quotes
            '"' => {
                self.capture(' ', |char| char != '"');
                self.current_char_index += 1; // Move past "
                self.column += 1;
                return TokenKind::STRING;
            }
            '\'' => {
                self.capture(' ', |char| char != '\'');
                return TokenKind::STRING;
            }

            // Reserved for syntax
            '`' => return TokenKind::BACK_TICK,

            _ => log_error(
                &[format!("Unexpected character: {}", ch)],
                self.line,
                self.current_char_index,
            ),
        }
    }

    /// Returns true if char could be part of an identifier
    fn is_identifier(ch: char) -> bool {
        ch.is_alphabetic() || ch == '_' || ch.is_numeric()
    }

    /// Gets the next character from source (if any)
    /// Consumes `current_char_index` then moves to the next character
    fn next_char(&mut self) -> Option<char> {
        let ch = self.source.chars().nth(self.current_char_index);
        self.current_char_index += 1;
        self.column += 1;
        ch
    }

    /// "peeks" next character by consuming `current_char_index` and then moving back
    fn peek_next_char(&mut self) -> Option<char> {
        let ch = self.next_char();
        self.current_char_index -= 1;
        self.column -= 1;
        ch
    }

    /// Captures characters while a condition is met
    /// Capture is placed at current_token_value
    /// If ch given as parameter  is a space it will not be placed in value
    fn capture(&mut self, ch: char, condition: fn(char) -> bool) {
        let mut value = String::new();
        if ch != ' ' {
            value.push(ch);
        }

        loop {
            let char = self.next_char();

            if char.is_none() || !condition(char.unwrap()) {
                self.current_char_index -= 1; // Move back, since char is not part of value
                self.column -= 1;
                break;
            }

            value.push(char.unwrap());
        }

        self.current_token_value = Some(value);
    }

    // ! Helper functions to determine what kind of token is being read

    ///  Expression tokens are anything that can be part of an expression -- almost everything
    pub fn is_expression_token(token: &Token) -> bool {
        return token.kind as i16 > TokenKind::_expr_start as i16
            && (token.kind as i16) < TokenKind::_expr_end as i16;
    }

    /// Value tokens are tokens that represent a value, such as a number or a string
    pub fn is_value_token(token: &Token) -> bool {
        return token.kind as i16 > TokenKind::_value_start as i16
            && (token.kind as i16) < TokenKind::_value_end as i16;
    }

    /// Operators tokens represent an operator, such as +, -, *, /, etc.
    pub fn is_operator_token(token: &Token) -> bool {
        return token.kind as i16 > TokenKind::_op_start as i16
            && (token.kind as i16) < TokenKind::_op_end as i16;
    }

    /// Statement token represent an instruction, such as `def`, `assign`, etc.
    pub fn is_statement_token(token: &Token) -> bool {
        return token.kind as i16 > TokenKind::_stmt_start as i16
            && (token.kind as i16) < TokenKind::_stmt_end as i16;
    }

    pub fn is_bitwise_operator(token: &Token) -> bool {
        return token.kind as i16 > TokenKind::_bitwise_start as i16;
    }
}
