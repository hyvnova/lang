use std::{fs, path::PathBuf};

use crate::{
    ast::{Node, AST},
    error,
    lexer::{Kind, Lexer, Token},
    log, signal::clean_signals,
};

use std::collections::HashSet;

fn filter_repeated_strings(vec: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    vec.into_iter()
        .filter(|s| seen.insert(s.clone())) // insert returns false if the value was already in the set
        .collect()
}

/// Box-It, bi!
/// Used to box an Nodeession
macro_rules! bi {
    // Box it
    ($x:expr) => {
        Box::new($x)
    };
}

/// By default, the parser will stop at a semicolon or a new line.
/// This is probably bad for performance, but I really can't care less
const EXPR_END: &[Kind] = &[Kind::SEMICOLON, Kind::NEW_LINE];

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

    // Makes parse, parse_Node, parse_statement, etc. stop at a token.
    // used to globally control the parser flow. Ex. parse_block needs to stop at R_BRACKET
    // In difference of `stops`, this is a single buffer of tokens, meaning that it will stop at the first token in the buffer.
    // This allows control over stops while letting handlers to push their own stops.
    global_stop : Option<Vec<Kind>>, 

    capturing_sequence: bool, // if the parser is capturing a sequence, parse_Node_from_buffer will return a sequence once parse_Node finds a Node end token

    // When a handler wants to capture signals, it will push a new vector to this variable.
    // The vector will be filled with the signals that the parser captures.
    // Used to keep track of "dependencies" of signals, so we can later on generate the correct code.
    capturing_signals: Vec<HashSet<String>>,

    // Used to differentiate between signal definition and signal update
    // If a signal is defined, any type of assignment will be considered as a signal update
    defined_signals: HashSet<String>,
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

            defined_signals: HashSet::new()
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
        dbg!(&scope.len()); // TODO: loggin scope because currently we don't ahve a way to handle more than 1 Node in scope
        scope
    }


    /// Parses parenthesis.
    /// Should be called after encountering an opening parenthesis.
    /// Returns a WrappedSequence or a Group \
    /// (1, 2, 3) -> WrappedSequence.
    /// ( 1 + 2 ) -> Group
    fn parse_paren(&mut self) -> Node {
        // Parse until closing parenthesis
        let mut scope = self.parse_until(&[Kind::R_PARENT]);

        // Ensure closing parenthesis was found
        if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_PARENT {
            error!(&self.lexer, "Expected a closing parenthesis.");
        }

        // If there's 1 or less elements in the scope, it's a group
        if scope.len() < 2 {
            return Node::Group(match scope.pop() {
                Some(node) => Some(bi!(node)),
                None => None
            });
        }
        // Otherwise, it's a wrapped sequence
        return Node::WrappedSequence(scope);
    }

    /// Parses a binary operation
    /// Should be called when encountering a binary operator, 
    /// - `op`: the operator. Ex. `+`, `-`, `*`, `/`, `==`, `!=`, `>`, `<`, `>=`, `<=`
    /// Expects a left hand side and a right hand side
    /// Returns a BinOp node
    fn parse_binop(&mut self, current_stop: &'stop_arr [Kind], op: String) {
        // LHS should be the last node in the scope
        let lhs = self.ast.pop_node().unwrap_or_else(|| {
            error!(&self.lexer, "Expected an Nodeession before binary operator.")
        });

        // Parse RHS
        let rhs = self.parse_until(current_stop).into_iter().next().unwrap_or_else(|| {
            error!(&self.lexer, "Expected an Nodeession after binary operator.")
        });

        self.ast.add_node(Node::BinOp { lhs: bi!(lhs), op, rhs: bi!(rhs) });
    }


    /// Parses a block.
    /// Should be called after encountering an opening bracket.
    /// Ensures that the block is closed.
    /// Returns a Block node.
    fn parse_block(&mut self) -> Node {
        // Parse until closing bracket
        let scope = self.parse_until(&[Kind::R_BRACKET]);

        // Ensure closing bracket was found
        if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_BRACKET {
            error!(&self.lexer, "Expected a closing bracket.");
        }

        Node::Block(scope)
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
            // In other words, if a handler sets a stop this will make sure that the parser stops at that token
            let current_stop: &[Kind] = match self.stops.last() {
                Some(stop) => *stop,
                None => EXPR_END,
            };

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
                    let args = bi!(self.parse_paren()); 

                    // Ensure parenthesis was closed
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_PARENT {
                        error!(&self.lexer, "Expected a closing parenthesis after function arguments.");
                    }

                    // Ensure there's a block after the arguments
                    if !self.next_token().map(|t| t.kind == Kind::L_BRACKET).unwrap_or(false) {
                        error!(&self.lexer, "Expected a block after function arguments.");
                    }

                    // Parse function body
                    let body = bi!(self.parse_block());

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
                
                // * Parenthesis
                L_PARENT => {
                    let expr = self.parse_paren();
                    self.ast.add_node(expr);
                }
                R_PARENT => error!(&self.lexer, "Unexpected closing parenthesis."),


                // * Brackets / Blocks / Dictionaries
                L_BRACKET => todo!(),
                R_BRACKET => todo!(),
                
                // * Indexing / Arrays
                L_SQUARE_BRACKET => todo!(),
                R_SQUARE_BRACKET => todo!(),

                IDENTIFIER => { self.ast.add_node(Node::Identifier(token.value.clone())); }
                NUMBER => { self.ast.add_node(Node::Number(token.value.clone())); }
                STRING => { self.ast.add_node(Node::Str(token.value.clone())); }

                SINGLE_QUOTE => todo!(),
                DOUBLE_QUOTE => todo!(),
                BACK_TICK => todo!(),

                // * Unary Operators / Bitwise
                // {op}{Node}
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
                        error!(&self.lexer, "Expected an Nodeession after unary operator.")
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

                // * Signal
                // ${ident}
                DOLLAR_SING => {
                    // Parse node after "$"
                    // If Identifier -> Signal definition/update
                    // If Block -> Reactive Statement
                    // Else -> Error
                    self.capturing_signals.push(HashSet::new()); // Capture signal deps in case is block

                    let node = self.parse_until(current_stop).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected a name (signal identifier) or block (reactive statement) after \"$\". Got nothing.")
                    });

                    let deps = self.capturing_signals.pop().unwrap_or_else(|| {
                        error!(&self.lexer, "Couldn't get dependencies for Reactive Statement. -- Prob double pop somewhere")
                    });


                    match node {
                        // Signal Defition/Update
                        Node::Identifier(name) => {

                            // If capturing signals, regist in
                            if let Some(signals) = self.capturing_signals.last_mut() {
                                signals.insert(name.clone());
                            }

                            self.ast.add_node(Node::Signal(name));
                        },

                        // Reactive Statement
                        Node::Block(block) => {
                            // Fetch reactive block dependencies
                            let dependencies: HashSet<String> = clean_signals(&self.ast, deps);
                            self.ast.add_node(Node::ReactiveStmt { block, dependencies })
                        }

                        other => error!(&self.lexer, format!("Expected a name (signal identifier) or block (reactive statement) after \"$\". Got: {:?}", other))

                    }
                    continue;

                },
                
                PIPE_RIGHT => todo!(),
                PIPE_LEFT => todo!(),
                L_ARROW => todo!(),
                R_ARROW => todo!(),
                FAT_ARROW => todo!(),
            }   
        }
    }
}
