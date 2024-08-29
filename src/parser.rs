use std::{fs, path::PathBuf};

use crate::{
    ast::{Node, AST},
    error,
    lexer::{Lexer, Token, Kind},
    log,
};

use std::collections::HashSet;

fn filter_repeated_strings(vec: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    vec.into_iter()
        .filter(|s| seen.insert(s.clone())) // insert returns false if the value was already in the set
        .collect()
}

/// Box-It, bi!
/// Used to box an expression
macro_rules! bi {
    // Box it
    ($x:expr) => {
        Box::new($x)
    };
}

/// By default, the parser will stop at a semicolon or a new line.
const EXPR_END: [Kind; 2] = [Kind::SEMICOLON, Kind::NEW_LINE];

pub struct Parser<'stop_arr> {
    // Token that was read but not processed
    remainder_token: Option<Token>,

    // lexer
    pub lexer: Lexer,

    // ast
    pub ast: AST,

    // Travel variables -- used to keep track of control flow
    stopped_at: Vec<Kind>, // if the parser stopped at a token, this will be the token. Works as a Stack.

    // Vector of "stops at", works as a stack.
    // When a handler needs to stop at a token, it will push an array of tokens to this vector.
    // When the parser reaches a token in one of the arrays, it will stop and pop the array.
    // The token that stopped the parser will be pushed to the `stopped_at` vector.
    stops: Vec<&'stop_arr [Kind]>, 

    // Makes parse, parse_expr, parse_statement, etc. stop at a token.
    // used to globally control the parser flow. Ex. parse_block needs to stop at R_BRACKET
    // In difference of `stops`, this is a single buffer of tokens, meaning that it will stop at the first token in the buffer.
    // This allows control over stops while letting handlers to push their own stops.
    global_stop : Option<Vec<Kind>>, 

    capturing_sequence: bool, // if the parser is capturing a sequence, parse_expr_from_buffer will return a sequence once parse_expr finds a expr end token

    // When a handler wants to capture signals, it will push a new vector to this variable.
    // The vector will be filled with the signals that the parser captures.
    // Used to keep track of "dependencies" of signals, so we can later on generate the correct code.
    capturing_signals: Vec<Vec<String>>,

    // Used to differentiate between signal definition and signal update
    // If a signal is defined, any type of assignment will be considered as a signal update
    defined_signals: Vec<String>,
}

impl<'stop_arr> Parser<'stop_arr> {
    pub fn new(source: String) -> Self {
        let lexer = Lexer::new(source);

        Parser {
            remainder_token: None,
            lexer,
            ast: AST::new(),

            stopped_at: Vec::new(),
            global_stop: None,
            stops: Vec::new(),
            
            capturing_sequence: false,
            capturing_signals: Vec::new(),

            defined_signals: Vec::new(),
        }
    }

    pub fn from_path(source_file: PathBuf) -> Self {
        let source: String = fs::read_to_string(&source_file)
            .expect(&format!("Couldn't read file: {:?}", source_file));

        Parser::new(source)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(token) = self.remainder_token.take() {
            return Some(token);
        }

        self.lexer.next()
    }

    /// Puts back a token that was read but not processed
    pub fn put_back(&mut self, token: Token) {
        // println!("[put_back] {:?}", token);
        self.remainder_token = Some(token);
    }

    /// Returns the next token without consuming it
    /// (It consumes the token but put's it back)
    pub fn peek_token(&mut self) -> Option<Token> {
        // print!("[peek_token]\n\t");
        let token = self.next_token();
        let copy = token.clone();

        if token.is_none() {
            return None;
        }

        self.put_back(token.unwrap());
        copy
    }

    /// Parses until one `stop_at` token is found.
    /// Returns the scope of the tokens that were parsed until the stop token was found.
    /// Does not ensure that it stopped at one of the stop tokens.
    pub fn parse_until(&mut self, stop_at: &'stop_arr [Kind]) -> Vec<Node> {
        self.stops.push(stop_at);
        self.ast.new_scope();
        self.parse();
        let scope = self.ast.pop_scope().unwrap_or_else(|| {
            error!(&self.lexer, "No scope found. Probably scope was popped by another handler.")
        });
        dbg!(&scope); // TODO: loggin scope because currently we don't ahve a way to handle more than 1 Node in scope
        scope
    }


    /// Parses parenthesis
    /// Should be called when it is expected to parse a parenthesis
    /// Returns a WrappedSequence or a Group
    /// (1, 2, 3) -> WrappedSequence
    /// ( 1 + 2 ) -> Group
    fn parse_paren(&mut self) -> Node {
        // Look for opening parenthesis
        if self.next_token().map(|t| t.kind) != Some(Kind::L_PARENT) {
            error!(&self.lexer, "Expected an opening parenthesis.");
        }

        // Parse until closing parenthesis
        let scope = self.parse_until(&[Kind::R_PARENT]);

        // Ensure closing parenthesis was found
        if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_PARENT {
            error!(&self.lexer, "Expected a closing parenthesis.");
        }

        // If there's only one element, it's a group
        if scope.len() == 1 {
            return Node::Group(scope.pop().unwrap());
        }
        // Otherwise, it's a wrapped sequence
        return Node::WrappedSequence(scope);
    }

    /// Parses a binary operation
    /// Should be called when encountering a binary operator, 
    /// - `op`: the operator. Ex. `+`, `-`, `*`, `/`, `==`, `!=`, `>`, `<`, `>=`, `<=`
    /// Expects a left hand side and a right hand side
    /// Returns a BinOp node
    fn parse_binop(&mut self, current_stop: &[Kind], op: String) {
        // LHS should be the last node in the scope
        let lhs = self.ast.pop_node().unwrap_or_else(|| {
            error!(&self.lexer, "Expected an expression before binary operator.")
        });

        // Parse RHS
        let rhs = self.parse_until(current_stop).into_iter().next().unwrap_or_else(|| {
            error!(&self.lexer, "Expected an expression after binary operator.")
        });

        self.ast.add_node(Node::BinOp { lhs: bi!(lhs), op, rhs: bi!(rhs) });
    }

    pub fn parse(&mut self) {
        // * Parse start
        while let Some(token) = self.next_token() {
            // * Global stop at
            if let Some(stop_at) = &self.global_stop {
                if stop_at.contains(&token.kind) {
                    self.stopped_at.push(token.kind);
                    return;
                } 
            }

            // Used to make all handlers stop where they're supposed to
            // In other words, if I handler sets a stop this will make sure that the parser stops at that token
            let current_stop: &[Kind] = self.stops.pop().unwrap_or(&EXPR_END);

            // * Stop at
            if current_stop.contains(&token.kind) {
                self.stopped_at.push(token.kind);
                return;
            }

            
            use Kind::*;
            match token.kind {
                EOF => {
                    break;
                }

                // * Comments
                COMMENT | ML_COMMENT => {
                    self.ast.add_node(Node::Comment(token.value.clone()));
                }

                NEW_LINE | SEMICOLON => {
                    self.ast.add_node(Node::Newline);
                }
            
                // * Funciton Definition
                // `def {ident}( {[{ident},]* ) {block}`
                FN_DEF => {
                    // parse function name
                    let ident_scope = self.parse_until(&[L_PARENT]).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an identifier for function name.")
                    });
                    
                    // Ensure token in scope was an identifier
                    let name = match ident_scope {
                        Node::Identifier(name) => name,
                        other => error!(&self.lexer, format!("Expected an identifier for function name. Got: {:?}", other)),
                    };

                    // Ensure there's a parenthesis after the function name
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::L_PARENT {
                        error!(&self.lexer, "Expected a parenthesis after function name.");
                    }

                    // Parse function arguments
                    let args_scope: Vec<Node> = self.parse_until(&[R_PARENT]);

                    // Args must a WrappedSequence or a Group
                    let args: Vec<Node> = match args_scope {
                        Node::WrappedSequence(args) 
                        | Node::Group(args) => args,
                        other => error!(&self.lexer, format!("Expected a sequence of arguments for function. Got: {:?}", other)),
                    };

                    // Ensure parenthesis was closed
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_PARENT {
                        error!(&self.lexer, "Expected a closing parenthesis after function arguments.");
                    }

                    // Ensure there's a block after the arguments
                    if !self.peek_token().map(|t| t.kind == Kind::L_BRACKET).unwrap_or(false) {
                        error!(&self.lexer, "Expected a block after function arguments.");
                    }

                    self.next_token(); // Consume the L_BRACKET

                    // Parse function body
                    let body_scope: Vec<Node> = self.parse_until(&[R_BRACKET]);
                    let body = Node::Block(body_scope);

                    // Ensure block was closed
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_BRACKET {
                        error!(&self.lexer, "Expected a closing bracket to close function body.");
                    }

                    // Add function to AST
                    self.ast.add_node(Node::FunctionDef { name, args, body });
                    continue;
                }
                
                CONTINUE => todo!(),
                BREAK => todo!(),
                LOOP => todo!(),
                FOR => todo!(),
                WHILE => todo!(),
                PYTHON => todo!(),
                
                IF => todo!(),
                ELSE => todo!(),
                ELIF => todo!(),
                ASSIGN => todo!(),
                
                L_PARENT => todo!(),
                R_PARENT => todo!(),
                L_BRACKET => todo!(),
                R_BRACKET => todo!(),
                L_SQUARE_BRACKET => todo!(),
                R_SQUARE_BRACKET => todo!(),

                IDENTIFIER => { self.ast.add_node(Node::Identifier(token.value.clone())); }
                NUMBER => { self.ast.add_node(Node::Number(token.value.clone())); }
                STRING => { self.ast.add_node(Node::Str(token.value.clone())); }

                SINGLE_QUOTE => todo!(),
                DOUBLE_QUOTE => todo!(),
                BACK_TICK => todo!(),

                // * Unary Operators / Bitwise
                // {op}{expr}
                NOT // "!" not
                | MULTIPLY // "*" unpack
                | BIT_AND
                | BIT_OR
                | BIT_XOR
                | BIT_NOT => {

                    // If token is MULTIPLY and current scope is not empty, it's a binary operator, not unpack
                    if token.kind == MULTIPLY && !self.ast.current_scope().is_empty() {
                        self.parse_binop(current_stop, token.value.clone());
                        continue;
                    }

                    let expr = self.parse_until(current_stop).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an expression after unary operator.")
                    });

                    self.ast.add_node(Node::UnaryOp { op: token.value.clone(), expr: bi!(expr) });
                    continue;   
                }   
                
                // * Binary Operators
                // Compare, Arithmetic, Logical
                AND
                | OR
                | EQ
                | NE
                | LT
                | LE
                | GT
                | GE
                | ADD
                | MULTIPLY
                | SUBTRACT
                | DIVIDE
                | POW
                | MOD => {
                    self.parse_binop(current_stop, token.value.clone());
                    continue;
                }

                AT => todo!(),
                COLON => todo!(),

                COMMA => { continue; }
                D_DOT => todo!(),
                DOT => todo!(),
                HASH => todo!(),
                DOLLAR_SING => todo!(),
                PIPE_RIGHT => todo!(),
                PIPE_LEFT => todo!(),
                L_ARROW => todo!(),
                R_ARROW => todo!(),
                FAT_ARROW => todo!(),
            }   
        }
    }
}
