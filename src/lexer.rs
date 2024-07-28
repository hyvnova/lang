use crate::log_utils::error;
use std::path::PathBuf;

#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum TokenKind {
    // Special -- -199/-100
    EOF = -199,      // End of file
    NEW_LINE = -198, // New line

    // Statement -- 100/199
    //   Instructions
    DEF = 100,
    ASSING = 101, //

    // Expressions -- >=200
    //   Parenthesis
    L_PARENT = 200,
    R_PARENT = 201,

    //   Brackets
    L_BRACKET = 202,
    R_BRACKET = 203,

    //   Square brackets
    L_SQUARE_BRACKET = 204,
    R_SQUARE_BRACKET = 205,

    //   Values -- 210/249
    IDENTIFIER = 210,
    NUMBER = 211,
    STRING = 212,

    //   Quotes 280/289
    SINGLE_QUOTE = 280, // '
    DOUBLE_QUOTE = 281, // "
    BACK_TICK = 282,    // `

    //   Arithmetic -- 290/299
    ADD = 290,
    SUBTRACT = 291,
    MULTIPLY = 293,
    DIVIDE = 294,
    POW = 295,

    // Logical -- 300/399
    //   Comparinson
    EQ = 300,
    NOT = 301,
    LT = 302,
    GT = 303,

    // Syntax -- 400/499
    SEMICOLON = 400,
    COLON = 401,
    COMMA = 402,
    DOT = 403,
    HASH = 404,
}

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
        write!(
            f,
            "<Token {:?} = {} >",
            self.kind, self.value
        )
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
    pub fn new(path: PathBuf) -> Self {
        let source = std::fs::read_to_string(path)
        .expect("Could not read source file");

        Lexer {
            source,
            current_char_index: 0,
            
            line: 1,
            column: 0,

            current_token_value: None,
        }
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
        if ch.is_numeric() || ch == '.' {
            self.capture(ch, |char| char.is_numeric() || char == '.');

            // Ensure only 1 dot is present
            if self.current_token_value.as_ref().unwrap().matches('.').count() > 1 {
                error(
                    &[
                        "Invalid number".to_string(),
                    ],
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
            match self.current_token_value.as_ref().unwrap().as_str() {
                "def" => return TokenKind::DEF,

                _ => return TokenKind::IDENTIFIER,
            }
        }

        // * Special characters
        self.current_token_value = Some(ch.to_string());


        // TODO: Theres a little error.
        // The value of a `Token` is set above, but 
        // If some tokenkind use 2 characters, these tokens will only
        // have the first character as value.

        match ch {
            
            // Comparison
            '=' if self.peek_next_char() == Some('=') => {
                self.current_char_index += 1; // Move past =
                return TokenKind::EQ
            },
            '!' => return TokenKind::NOT,
            '<' => return TokenKind::LT,
            '>' => return TokenKind::GT,

            // Instruction
            '=' => {
                return TokenKind::ASSING
            },

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
                return TokenKind::POW
            }
                ,
            '*' => return TokenKind::MULTIPLY,
            '/' => return TokenKind::DIVIDE,

            // Syntax
            ';' => return TokenKind::SEMICOLON,
            ':' => return TokenKind::COLON,
            ',' => return TokenKind::COMMA,
            '.' => return TokenKind::DOT,
            '#' => return TokenKind::HASH,

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

            _ => error(
                &[
                    format!("Unexpected character: {}", ch),
                ],
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
        return token.kind as i16 >= 200;
    }

    /// Value tokens are tokens that represent a value, such as a number or a string
    pub fn is_value_token(token: &Token) -> bool {
        return token.kind as i16 >= 210 && token.kind as i16 <= 249;
    }

    /// Operators tokens represent an operator, such as +, -, *, /, etc.
    pub fn is_operator_token(token: &Token) -> bool {
        return token.kind as i16 >= 290 && token.kind as i16 <= 299;
    }

    /// Statement token represent an instruction, such as `def`, `assign`, etc.
    pub fn is_statement_token(token: &Token) -> bool {
        return 100 <= token.kind as i16 && token.kind as i16 <= 199
    }
}


