use std::{fs, path::PathBuf};

use crate::{
    ast::{Node, AST}, error, lexer::{Kind, Lexer, Token}, log, signal::clean_signals
};

use std::collections::HashSet;


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
    parsing_paren: bool, // If the parser is parsing a parenthesis, used to differentiate between Named Arguments and normal assignments

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
            parsing_paren: false,

            defined_signals: HashSet::new()

        }
    }

    pub fn from_path(source_file: PathBuf) -> Self {
        let source: String = fs::read_to_string(&source_file)
            .expect(&format!("Couldn't read file: {:?}", source_file));

        Parser::new(source)
    }

    fn next_token(&mut self) -> Option<Token> {
        if let Some(token) = self.remainder_token.take() {
            return Some(token);
        }

        self.lexer.next()
    }

    /// Cleans-up a stop
    /// - Pops the last stop from the stops vector
    /// - Consumes next token, which is asumed to be a stop token.
    fn clean_stop(&mut self) {
        self.stops.pop();
        self.next_token();
    }
    
    /// Puts back a token that was read but not processed
    fn put_back(&mut self, token: Token) {
        // println!("[put_back] {:?}", token);
        self.remainder_token = Some(token);
    }

    /// Returns the next token without consuming it
    /// (It consumes the token but put's it back)
    fn peek_token(&mut self) -> Option<Token> {
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
    /// ! If the stop token was found, make sure to consume it, since it is not consumed by this function.
    fn parse_until(&mut self, stop_at: Option<&'stop_arr [Kind]>) -> Vec<Node> {
        if let Some(stop_at) = stop_at {
            self.stops.push(stop_at);
        }
        
        self.ast.new_scope();
        self.parse_node();

        log!("END PARSE UNTIL", "Stopped at: {:?}", self.stopped_at);
        
        let scope: Vec<Node> = self.ast.pop_scope().unwrap_or_else(|| {
            error!(&self.lexer, "No scope found. Probably scope was popped by another handler.")
        });

        scope
    }


    /// Parses parenthesis.
    /// Should be called after encountering an opening parenthesis.
    /// Returns a WrappedSequence or a Group \
    /// (1, 2, 3) -> WrappedSequence.
    /// ( 1 + 2 ) -> Group
    fn parse_paren(&mut self) -> Node {
        // Parse until closing parenthesis
        self.parsing_paren = true;
        
        log!("PARSE PAREN");
        
        let mut scope: Vec<Node> = self.parse_until(Some(&[Kind::R_PARENT]));

        println!("PAREN: {:?}, stopped: {:?}", scope, self.stopped_at);

        // Ensure closing parenthesis was found
        if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_PARENT {
            error!(&self.lexer, "Expected a closing parenthesis.");
        }

        // handle stop
        self.clean_stop(); // Consume R_PARENT
        
        self.parsing_paren = false;

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
    fn parse_binop(&mut self, current_stop: Option<&'stop_arr [Kind]>, op: String) {
        // LHS should be the last node in the scope
        let lhs: Node = self.ast.pop_node().unwrap_or_else(|| {
            error!(&self.lexer, "Expected an Node before binary operator.")
        });
        
        log!("PARSE BINOP", "LHS: {:?} {:?}", lhs, op);

        // Parse RHS
        let rhs: Node = self.parse_until(current_stop).into_iter().next().unwrap_or_else(|| {
            error!(&self.lexer, "Expected an Node after binary operator.")
        });
        
        // We don't consume stop token because this handlers doens't sets any.

        log!("PARSE BINOP", "RHS: {:?}", rhs);
        
        self.ast.add_node(Node::BinOp { lhs: bi!(lhs), op, rhs: bi!(rhs) });
    }


    /// Parses a block.
    /// Should be called after encountering an opening bracket.
    /// Ensures that the block is closed.
    /// Returns a Block node.
    fn parse_block(&mut self) -> Node {

        // If next token is a closing bracket, it's an empty block
        if let Some(Token { kind: Kind::R_BRACKET, .. }) = self.peek_token() {
            self.next_token(); // Consume R_BRACKET
            return Node::Block(Vec::new());
        }

        // Parse until closing bracket
        let scope: Vec<Node> = self.parse_until(Some(&[Kind::R_BRACKET]));

        // Ensure closing bracket was found
        if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_BRACKET {
            error!(&self.lexer, "Expected a closing bracket.");
        }
        
        // Handle stop
        self.clean_stop(); // Consume R_BRACKET
        
        Node::Block(scope)
    }


    /// Parses the entire source code.
    pub fn parse(&mut self) {
        // Parse until EOF
        while let Some(next_token) = self.peek_token() {
            
            use Kind::*;
            match next_token.kind {
                EOF => {
                    self.next_token();
                    break;
                },
               
                NEW_LINE | SEMICOLON => {
                    self.next_token();
                    self.ast.add_node(Node::Newline);
                    continue;
                },
                
                _ => {            
                    self.parse_node();
                }
            }
        }
    }

    /// - This function performs the "actual" parsing.
    /// - Each node is a expression or statement.
    /// - When a stop token is encountered, it is added to `stopped_at` but not consumed.
    ///     The `stop` that contains the stop token is NOT popped. 
    fn parse_node(&mut self) {
        // * Parse start
        while let Some(next_token) = self.peek_token() {

            // * Global stop at
            if let Some(stop_at) = &self.global_stop {
                if stop_at.contains(&next_token.kind) {
                    self.stopped_at.push(next_token.kind);
                    return;
                } 
            }

            // Used to make all handlers stop where they're supposed to
            // In other words, if a handler sets a stop this will make sure that the parser stops at that token
            let current_stop: &[Kind] = match self.stops.last() {
                Some(stop) => *stop,
                None => EXPR_END,
            };

            println!("->{:?}  Stops: {:?}", next_token, self.stops);

            // * Stop at
            if current_stop.contains(&next_token.kind) {
                println!("\tStopping at: {:?}", next_token);

                if self.capturing_sequence {
                    self.capturing_sequence = false;

                    let seq: Vec<Node> = self.ast.pop_scope().unwrap_or_else(|| {
                        error!(&self.lexer, "No scope found. Probably scope was popped by another handler.")
                    });
                    
                    self.ast.add_node(Node::Sequence(seq));
                }

                self.stopped_at.push(next_token.kind);
                return;
            }
            
            // Since token exists and it is not a stop token, consume it
            let token = self.next_token().unwrap();

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
                    let ident_scope: Node = self.parse_until(Some(&[L_PARENT])).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an identifier for function name.")
                    });
                    
                    // Consume stop
                    self.stops.pop();
                    self.next_token(); // Consume L_PARENT
                    
                    // Ensure token in scope was an identifier
                    let name: String = match ident_scope {
                        Node::Identifier(name) => name,
                        other => error!(&self.lexer, format!("Expected an identifier for function name. Got: {:?}", other)),
                    };

                    // Ensure there's a parenthesis after the function name
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::L_PARENT {
                        error!(&self.lexer, "Expected a parenthesis after function name.");
                    }

                    // Parse function arguments
                    let args: Box<Node> = bi!(self.parse_paren()); 

                    // Ensure there's a block after the arguments
                    if !self.next_token().map(|t| t.kind == Kind::L_BRACKET).unwrap_or(false) {
                        error!(&self.lexer, "Expected a block after function arguments.");
                    }

                    // Parse function body
                    let body: Box<Node> = bi!(self.parse_block());

                    // Add function to AST
                    self.ast.add_node(Node::FunctionDef { name, args, body });
                    continue;
                },
                
                CONTINUE => todo!(),
                BREAK => todo!(),
                LOOP => todo!(),
                FOR => todo!(),
                WHILE => todo!(),

                PYTHON => {
                    self.ast.add_node(Node::Python(token.value.clone()));
                }
                
                // * If / Else / Elif
                // if {expr} {block} [elif {expr} {block}]* [else {block}]*
                IF =>{
                    println!("IF");
                    let condition: Node = self.parse_until(Some(&[L_BRACKET])).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an expression after \"if\".")
                    });

                    println!("Condition: {:?}", condition);

                    // Ensure we stop at R_BRACKET
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_BRACKET {
                        error!(&self.lexer, "Expected block after if condition.");
                    }

                    let block: Node = self.parse_block();
                    let mut elifs: Vec<(Node, Node)> = Vec::new();

                    // Elifs
                    while let Some(Token { kind: ELIF, .. }) = self.peek_token() {
                        self.next_token(); // Consume ELIF

                        let condition: Node = self.parse_until(Some(&[L_BRACKET])).into_iter().next().unwrap_or_else(|| {
                            error!(&self.lexer, "Expected an expression after \"elif\".")
                        });

                        // Ensure there's a block after the condition
                        if !self.next_token().map(|t| t.kind == Kind::L_BRACKET).unwrap_or(false) {
                            error!(&self.lexer, "Expected a block after elif condition.");
                        }

                        let block: Node = self.parse_block();
                        elifs.push((condition, block));
                    }

                    // Else
                    let mut else_body: Option<Box<Node>> = None;

                    if let Some(Kind::ELSE) = self.peek_token().map(|t| t.kind) {
                        self.next_token(); // Consume ELSE

                        // Ensure there's a block after the condition
                        if !self.next_token().map(|t| t.kind == Kind::L_BRACKET).unwrap_or(false) {
                            error!(&self.lexer, "Expected a block after else condition.");
                        }

                        let block: Node = self.parse_block();
                        else_body = Some(bi!(block));
                    }

                    println!("IF: {:?} {:?} {:?}", condition, block, elifs);

                    self.ast.add_node(Node::Conditional { 
                        condition: bi!(condition), 
                        body: bi!(block), 
                        elifs, 
                        else_body
                    });

                    continue;
                }
                ELSE => todo!(),
                ELIF => todo!(),

                // * Assingments
                // {ident} {  op = (=, +=, *=, ...) } {expr} [;]
                // * Multiple assignments
                // {sequence} { op } {sequence} [;]
                // * Unpacking
                // {sequence} { op } {expr} [;]
                // * Signal Definition/Update
                // {signal} { op } {expr} [;]
                // * Destructuring/Deconstruction
                // { dict } { op } {expr} [;]
                ASSIGN => {
                    let op: String = token.value.clone(); // Assignment operator. Ex. `=`, `+=`, `*=`, ...

                    let lhs: Node = self.ast.pop_node().unwrap_or_else(|| {
                        error!(&self.lexer, "No LHS found. Probably node was popped by another handler.");
                    });

                    // If LHS is a signal, then we're capturing signals/dependencies
                    if let Node::Signal(_) = &lhs {
                        self.capturing_signals.push(HashSet::new());
                    }

                    let rhs: Node = self.parse_until(None).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an value after assignment operator.")
                    });
                

                    // * If parsing parethensis it's a named arg
                    if self.parsing_paren {
                        println!("NAMED ARG: {:?} {:?} {:?}", lhs, op, rhs);
                        
                        match lhs {
                            Node::Identifier(name) => {
                                self.ast.add_node(Node::NamedArg(name, bi!(rhs)));
                            },
                            other => error!(&self.lexer, format!("Expected an Identifier for Named Argument. Got: {:?}", other))
                        }
                        
                        // If current_stop was achieved, end the parsing
                        if let Some(stop_token) = self.stopped_at.last() {
                            if current_stop.contains(&stop_token) {
                                return;
                            } 
                        }

                        // If not, continue parsing
                        continue;
                    }

                    println!("ASSIGN: {:?} {:?} {:?}", lhs, op, rhs);
                    
                    match (lhs, rhs) {
                    
                        // * Signal Definition/Update
                        (Node::Signal(name), rhs) => {
                            // Fetch dependencies
                            let dependencies: HashSet<String> = clean_signals(&self.ast, self.capturing_signals.pop().unwrap_or_else(|| {
                                error!(&self.lexer, "Couldn't get dependencies for Signal Definition/Update. -- Prob double pop somewhere")
                            }));

                            // If signal is defined, it's an update
                            if self.defined_signals.contains(&name) {
                                self.ast.add_node(Node::SignalUpdate { name, value: bi!(rhs), dependencies });
                            } else {
                                self.defined_signals.insert(name.clone());
                                self.ast.add_node(Node::SignalDef { name, value: bi!(rhs), dependencies });
                            }
                        }

                        // * Destructuring/Deconstruction
                        (Node::Dict { keys, values } , rhs) => {
                            self.ast.add_node(Node::Deconstruction { identifiers: keys, value: bi!(rhs), default_values: values });
                        }

                        // * Assingments / Multiple assignments / Unpacking
                        (lhs, rhs) => {
                            self.ast.add_node(Node::Assign { identifiers: vec![lhs], values: vec![rhs], op });
                        }
                    }
                    continue;
                }
                
                // * Parenthesis -> Group / WrappedSequence
                // * Function Call
                // ({expr}, {expr}, ...) -> WrappedSequence
                // ({expr}) -> Group
                // {expr}( {expr}, {expr}, ... ) -> Function Call
                L_PARENT => {
                    let expr: Node = self.parse_paren();

                    // If there's something in the scope, it's a function call
                    // TODO: This doens't check that "object" is a callable object, meaning it can be a SemiColon or NewLine
                    if !self.ast.current_scope().is_empty() {
                        let obj = self.ast.pop_node().unwrap();
                        self.ast.add_node(Node::FunctionCall { object: bi!(obj), args: bi!(expr) });
                        continue;
                    }

                    self.ast.add_node(expr);
                    continue;
                },
                R_PARENT => error!(&self.lexer, "Unexpected closing parenthesis."),


                // * Blocks / Dictionaries
                L_BRACKET => {
                    self.global_stop = Some(vec![Kind::R_BRACKET]);
                    self.ast.new_scope();
                    self.parse();
                    let scope: Vec<Node> = self.ast.pop_scope().unwrap_or_else(|| {
                        error!(&self.lexer, "No scope found. Probably scope was popped by another handler.")
                    });
                    self.ast.add_node(Node::Block(scope));
                    self.global_stop = None;
                    continue;
                },  
                R_BRACKET => error!(&self.lexer, "Unexpected closing bracket."),
                
                // * Indexing / Arrays
                L_SQUARE_BRACKET => {
                    let mut expr = self.parse_until(Some(&[R_SQUARE_BRACKET])).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an Expr after opening square bracket.")
                    });

                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != Kind::R_SQUARE_BRACKET {
                        error!(&self.lexer, "Expected a closing square bracket.");
                    }

                   // If there's someting in the scope, it's an index
                   // If `expr` is a Range and it doesn't have an end, end will be the length of object
                    if !self.ast.current_scope().is_empty() {
                        let object: Node = self.ast.pop_node().unwrap();

                        if let Node::Range { start, end, inclusive } = &expr {
                            if let Node::Empty = **end {
                                expr = Node::Range { start: start.clone(), end: bi!(Node::Len(bi!(object.clone()))), inclusive: *inclusive };
                            }
                        }
                    
                        self.ast.add_node(Node::Index { object: bi!(object), index: bi!(expr) });
                        continue;
                    }

                    // Array
                    self.ast.add_node(Node::Array(bi!(expr)));  
                    continue;                      
                },
                R_SQUARE_BRACKET => error!(&self.lexer, "Unexpected closing square bracket."),

                IDENTIFIER => { self.ast.add_node(Node::Identifier(token.value.clone())); }
                NUMBER => { self.ast.add_node(Node::Number(token.value.clone())); }
                STRING => { 
                    // String formatting is done at transpiler level.
                    self.ast.add_node(Node::Str(token.value.clone())); 
                }

                // Don't even know what to do with these
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
                        self.parse_binop(None, token.value.clone());
                        continue;
                    }

                    let expr = self.parse_until(None).into_iter().next().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an expr after unary operator.")
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
                    self.parse_binop(None, token.value.clone());
                    continue;
                }

                // * Decorators
                AT => todo!(),
                
                COLON => todo!(),

                COMMA => { 
                    if self.capturing_sequence {
                        continue;
                    }

                    self.capturing_sequence = true;
                    
                    // The node before "," is the start of the sequence
                    let seq_start = self.ast.pop_node().unwrap_or_else(|| {
                        error!(&self.lexer, "This \",\" is infront of nothing.")
                    });
                        
                    self.ast.new_scope();
                    self.ast.add_node(seq_start);
                    continue;
                },

                // * Ranges
                // {expr} .. {expr} (exclusive)
                // {expr} ..= {expr} (inclusive)
                D_DOT => {
                    let lhs: Node = self.ast.pop_node().unwrap_or_else(|| {
                        // If there's no LHS, it's a 0..{expr} range
                        Node::Number("0".to_string())
                    });
                    
                    // If next token is `=`, it's an inclusive range
                    // ASSING can be "+=", "-=", "*=", "/=", etc. potentially bad.
                    let inclusive: bool = if let Some(Kind::ASSIGN) = self.peek_token().map(|t| t.kind) {
                        self.next_token(); // Consume "="
                        true
                    } else {
                        false
                    };

                    let rhs: Node = self.parse_until(Some(current_stop)).into_iter().next().unwrap_or_else(|| {
                        // If there's no RHS, end will be the length of the thing being indexed
                        // This of course will only work if we indexing, otherwise it will be an error.
                        Node::Empty 
                    });

                    self.ast.add_node(Node::Range { start: bi!(lhs), end: bi!(rhs), inclusive });
                    continue;
                },

                DOT => todo!(),
                HASH => todo!(),

                // * Signal / Reactive Statement
                // ${ident}
                // ${block}
                DOLLAR_SING => {
                    // Parse node after "$"
                    // If Identifier -> Signal definition/update
                    // If Block -> Reactive Statement
                    // Else -> Error
                    self.capturing_signals.push(HashSet::new()); // Capture signal deps in case is block

                    let node: Node = match self.peek_token() {
                        // If Identifier
                        Some(Token { kind: Kind::IDENTIFIER, value, .. }) => {
                            self.next_token(); // Consume identifier
                            Node::Identifier(value.clone())
                        }
                        
                        // If not identifier, fetch next node, which should be a block 
                        _ => {
                            self.parse_until(Some(current_stop)).into_iter().next().unwrap_or_else(|| {
                                error!(&self.lexer, "Expected an Identifier or Block after \"$\" but got nothing.")
                            })
                        }
                    };

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
