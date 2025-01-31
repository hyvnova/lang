use core::panic;
use std::{fs, path::PathBuf, vec};

use clap::error;

use crate::{
    ast::{Node, AST}, error, lexer::{Kind, Lexer, Token}, log, parse_utils::{GetFirstOrElse, IsKind}, signal::clean_signals
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


    // When parsing a distribution makes sure parser tops before a "->" so a distribution cannon be used as input for another
    // And instead the following distribution is considered takes the previous distribution as input
    // All this is supposed to allow nested distributions
    parsing_distribution: bool,

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

            defined_signals: HashSet::new(),

            parsing_distribution: false,
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
    /// - Pops the last stopped_at token
    fn clean_stop(&mut self) {
        self.stops.pop();
        self.stopped_at.pop();
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
        log!("+ PARSE UNTIL", "Stopping at: {:?}", stop_at);
        
        if let Some(stop_at) = stop_at {
            self.stops.push(stop_at);
        }

        // Used to track a difference between the number of stops at the start and the end of the function
        // needed to remove the last stopped_at when no `stop_at` was provided, 
        // because last stop will be added by default when parser ends at EXPR_END
        let stops_at_count: usize = self.stopped_at.len();

        self.ast.new_scope();
        self.parse_node();

        if stop_at == None && stops_at_count != self.stopped_at.len() {
            self.stopped_at.pop();
        }
        log!("- END PARSE UNTIL", "Stopped at: {:?}", self.stopped_at);
        
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
        
        let mut scope = self.parse_until(Some(&[Kind::R_PARENT]));

        println!("PAREN: {:?}, stopped: {:?}", scope, self.stopped_at);

        // Ensure closing parenthesis was found
        if self.next_token().is_not(Kind::R_PARENT) {
            error!(&self.lexer, "Expected a closing parenthesis.");
        }

        // handle stop
        self.clean_stop(); // Remove R_PARENT stop
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
        let rhs: Node = self.parse_until(current_stop).get_first_or_else(|| {
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
    fn parse_block(&mut self, is_function: bool) -> Node {

        log!("PARSE BLOCK");

        // If next token is a closing bracket, it's an empty block
        if let Some(Token { kind: Kind::R_BRACKET, .. }) = self.peek_token() {
            self.next_token(); // Consume R_BRACKET
            return Node::Block(Vec::new());
        }

        // Parse until closing bracket
        let scope: Vec<Node> = self.parse_until(Some(&[Kind::R_BRACKET]));

        // Ensure closing bracket was found
        if self.next_token().is_not(Kind::R_BRACKET) {
            error!(&self.lexer, "Expected a closing bracket.");
        }
        // Handle stop
        self.clean_stop(); // Remove R_BRACKET stop
        
        if is_function {
            Node::FnBody(scope)
        } else {
            Node::Block(scope)
        }
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

                    // If last node was a newline, don't add another one
                    if let Some(Node::Newline) = self.ast.current_scope().last() {
                        continue;
                    }
                    self.ast.add_node(Node::Newline);
                    continue;
                },
                
                _ => {            
                    self.parse_node();
                }
            }
        }
    }

    /// Skips newlines.
    /// Consumes tokens until a token that is not a newline is found.
    fn skip_newlines(&mut self) {
         while let Some(Token { kind: Kind::NEW_LINE, .. }) = self.peek_token() {
            self.next_token();
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
                    // self.ast.add_node(Node::Newline);
                }
            
                // * Function Definition
                // `def {ident}( {expr},* ) {block}`
                // Ex. `def add(a, b) { return a + b; }`
                // |   `def add(a, b) { a + b }`
                FN_DEF => {
                    // parse function name
                    let ident_scope: Node = self.parse_until(Some(&[L_PARENT])).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an identifier for function name.")
                    });
                    
                    if self.next_token().is_not(Kind::L_PARENT) {
                        error!(&self.lexer, "Expected a parenthesis after function name.");
                    }
                    
                    self.clean_stop(); // Remove L_PARENT stop

                    // Ensure token in scope was an identifier
                    let name: String = match ident_scope {
                        Node::Identifier(name) => name,
                        other => error!(&self.lexer, format!("Expected an identifier for function name. Got: {:?}", other)),
                    };

                    // Parse function arguments
                    let args: Box<Node> = bi!(self.parse_paren()); 

                    // Ensure there's a block after the arguments
                    if self.next_token().is_not(Kind::L_BRACKET) {
                        error!(&self.lexer, "Expected a block after function arguments.");
                    }

                    // Parse function body
                    let body: Box<Node> = bi!(self.parse_block(true));

                    // Add function to AST
                    self.ast.add_node(Node::FunctionDef { name, args, body });
                    continue;
                },

                RETURN => {
                    let expr: Node = self.parse_until(Some(current_stop)).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an expression after \"return\".")
                    });

                    self.ast.add_node(Node::Return(bi!(expr)));
                    continue;
                }

                // * IN
                // Used in for loops (for {item} in {iter})
                IN => panic!("Unexpected IN token."),

                // * Loop
                // Repeats the body until explicitly stopped by a BREAK or an exit statement
                // loop {block} 
                LOOP => {
                    // Ensure there's a block after "loop"
                    if self.peek_token().is(Kind::L_BRACKET) {
                        self.next_token(); // Consume L_BRACKET
                    } else {
                        error!(&self.lexer, "Expected a block after \"loop\".");
                    }

                    let block: Node = self.parse_block(false);
                    self.ast.add_node(Node::Loop(bi!(block)));
                    continue;
                }
                
                CONTINUE => {
                    self.ast.add_node(Node::Continue);
                    continue;
                },
                BREAK => {
                    self.ast.add_node(Node::Break);
                    continue;
                },

                // * For Loop
                // for {item} in {iter} {block}
                FOR => {
                    let item: Node = self.parse_until(Some(&[IN])).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an identifier after \"for\".")
                    });

                    if self.next_token().is_not(Kind::IN) {
                        error!(&self.lexer, "Expected \"in\" after item.");
                    }
                   
                    self.clean_stop(); // Remove IN stop

                    let iterable: Node = self.parse_until(Some(&[L_BRACKET])).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an expression after \"in\".")
                    });

                    // Ensure there's a block after the expression
                    if self.next_token().is_not(Kind::L_BRACKET) {
                        error!(&self.lexer, "Expected a block after expression.");
                    }

                    self.clean_stop(); // Remove L_BRACKET stop

                    log!("FOR", "Item: {:?} Iterable: {:?}", item, iterable);

                    let block: Node = self.parse_block(false);

                    self.ast.add_node(Node::ForLoop { item: bi!(item), iterable: bi!(iterable), body: bi!(block) });
                    continue;
                }

                // * While loop UWU classic while loop. Who doesn't like a cute while loop?
                // while {expr} {block}
                WHILE => {
                    let condition: Node = self.parse_until(Some(&[L_BRACKET])).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an expression after \"while\".")
                    });

                    // Ensure there's a block after the expression
                    if self.next_token().is_not(Kind::L_BRACKET) {
                        error!(&self.lexer, "Expected a block after expression.");
                    }

                    self.clean_stop(); // Remove L_BRACKET stop

                    let block: Node = self.parse_block(false);

                    self.ast.add_node(Node::WhileLoop { condition: bi!(condition), body: bi!(block) });
                    continue;
                }

                PYTHON => {
                    self.ast.add_node(Node::Python(token.value.clone()));
                }
                
                // * If / Else / Elif
                // if {expr} {block} 
                // [elif {expr} {block}]* 
                // [else {block}]?

                IF =>{
                    log!("IF");
                    let condition: Node = self.parse_until(Some(&[L_BRACKET])).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an expression after \"if\".")
                    });

                    println!("Condition: {:?}", condition);

                    // Ensure we stop at L_BRACKET
                    if self.next_token().is_not(Kind::L_BRACKET) {
                        error!(&self.lexer, "Expected block after if condition.");
                    }

                    self.clean_stop(); // Remove L_BRACKET stop

                    let block: Node = self.parse_block(false);
                    let mut elifs: Vec<(Node, Node)> = Vec::new();


                    self.skip_newlines();

                    // Elifs
                    while let Some(Token { kind: ELIF, .. }) = self.peek_token() {
                        self.next_token(); // Consume ELIF

                        let condition: Node = self.parse_until(Some(&[L_BRACKET])).get_first_or_else(|| {
                            error!(&self.lexer, "Expected an expression after \"elif\".")
                        });

                        // Ensure there's a block after the condition
                        if self.next_token().is_not(Kind::L_BRACKET) {
                            error!(&self.lexer, "Expected a block after elif condition.");
                        }

                        let block: Node = self.parse_block(false);
                        elifs.push((condition, block));
                    }

                    // skip newlines
                    self.skip_newlines();

                    // Else
                    let mut else_body: Option<Box<Node>> = None;

                    if self.peek_token().is(Kind::ELSE) {
                        self.next_token(); // Consume ELSE

                        // Ensure there's a block after the condition
                        if self.next_token().is_not(Kind::L_BRACKET) {
                            error!(&self.lexer, "Expected a block after else condition.");
                        }

                        let block: Node = self.parse_block(false);
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
                ELSE => error!(&self.lexer, "Unexpected else statement."),
                ELIF => error!(&self.lexer, "Unexpected elif statement."),

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
                ASSIGN | WALRUS => {
                    // If token it's a walrus operator, then it's an assingment
                    // Even if we're capturing a sequence, it it's a walres it can't be a named arg
                    let is_walrus: bool = token.kind == WALRUS;

                    let op: String = token.value.clone(); // Assignment operator. Ex. `=`, `+=`, `*=`, ...

                    let lhs: Node = self.ast.pop_node().unwrap_or_else(|| {
                        error!(&self.lexer, "No LHS found. Probably node was popped by another handler.");
                    });

                    log!("ASSIGN", "LHS: {:?} {:?}", lhs, op);

                    // If LHS is a signal, then we're capturing signals/dependencies
                    if let Node::Signal(_) = &lhs {
                        self.capturing_signals.push(HashSet::new());
                    }

                    let rhs: Node = self.parse_until(None).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an value after assignment operator.")
                    });
                

                    // * If parsing parethensis it's a named arg
                    if self.parsing_paren && !is_walrus {
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


                    log!("ASSIGN", "\tRHS: {:?}", rhs);
                    
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
                    log!("Parsing BLOCK");

                    // ! This is technically the right way to do it, but... parse_until it's working fine...
                    // ! Needs to be a global stop so it is impossible to skip the stop token "}" by mistake
                    // ! Since a block will have a bunch of different stops
                    // --- OLD CODE ---
                    // self.global_stop = Some(vec![Kind::R_BRACKET]);
                    // Not needed, since parse_until will create a new scope
                    // self.ast.new_scope(); 
                    // let scope: Vec<Node> = self.ast.pop_scope().unwrap_or_else(|| {
                    //    error!(&self.lexer, "No scope found. Probably scope was popped by another handler.")
                    // });
                    // self.ast.add_node(Node::Block(scope));
                    // self.global_stop = None;

                    let block: Node = self.parse_block(false);
                    self.ast.add_node(block);
                    continue;
                },  
                R_BRACKET => error!(&self.lexer, "Unexpected closing bracket."),
                
                // * Indexing / Arrays 
                L_SQUARE_BRACKET => {

                    // The expr inside the square brackets can be anything, so we parse until a closing square bracket
                    let mut expr = self.parse_until(Some(&[R_SQUARE_BRACKET])).get_first_or_else(|| {
                        error!(&self.lexer, "Expected a closing square bracket at some point after opening one...")
                    });

                    // Ensure there's a closing square bracket
                    if self.next_token().is_not(Kind::R_SQUARE_BRACKET) {
                        error!(&self.lexer, "Expected a closing square bracket.");
                    }

                    self.clean_stop(); // Remove R_SQUARE_BRACKET stop

                    // If there's someting in the scope ( a node right before the [{expr}]), it's an index
                    // If `expr` is a Range and it doesn't have an end, end  will be the length of object
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
                    log!("ARRAY", "Expr: {:?}", expr);
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

                TRUE => { self.ast.add_node(Node::Bool(true)); }
                FALSE => { self.ast.add_node(Node::Bool(false)); }

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

                    let expr = self.parse_until(None).get_first_or_else(|| {
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
                    log!(" /  Capturing Sequence");
                    
                    // The node before "," is the start of the sequence
                    let seq_start: Node = self.ast.pop_node().unwrap_or_else(|| {
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

                    let rhs: Node = self.parse_until(Some(current_stop)).get_first_or_else(|| {
                        // If there's no RHS, end will be the length of the thing being indexed
                        // This of course will only work if we indexing, otherwise it will be an error.
                        Node::Empty 
                    });

                    if self.stopped_at.is_empty() || !current_stop.contains(&self.stopped_at.pop().unwrap()) {
                        error!(&self.lexer, "Expected a closing token for range.");
                    }

                    self.clean_stop(); // Remove stop token

                    self.ast.add_node(Node::Range { start: bi!(lhs), end: bi!(rhs), inclusive });
                    continue;
                },

                // * Member Acess / Dot Operator / Method Call
                DOT => {
                    // Parse property, shoulld be an identifier
                    let prop: Node = self.parse_until(None).get_first_or_else(|| {
                        error!(&self.lexer, "Expected a property after \".\".")
                    });

                    // Get object; value being accessed, should be the last node in the scope
                    let object: Node = self.ast.pop_node().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an object before \".\".\nEx. object.property \n -If you were trying to access a member of an object.")
                    });

                    // Otherwise, it's an member access
                    self.ast.add_node(Node::MemberAccess{ object: bi!(object), member: bi!(prop) });
                    continue;
                }
                HASH => todo!(),

                // * Signal / Reactive Statement
                // signal: ${ident}
                // reactive statement: ${block}
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
                            self.parse_until(None).get_first_or_else(|| {
                                error!(&self.lexer, "Expected an Identifier or Block after \"$\" but got nothing.")
                            })
                        }
                    };

                    let deps: HashSet<String> = self.capturing_signals.pop().unwrap_or_else(|| {
                        error!(&self.lexer, "Couldn't get dependencies for Reactive Statement. -- Prob double pop somewhere")
                    });

                    // node can be either an Identifier or a Block
                    // If it's an Identifier, it's a signal definition/update. If capturing signals, save signal
                    // If it's a Block, it's a reactive statement
                    match node {
                        // Signal Defition/Update
                        Node::Identifier(name) => {

                            // If capturing signals, regist in
                            if let Some(signals) = self.capturing_signals.last_mut() {
                                signals.insert(name.clone());
                            }

                            log!("+", "Signal - Name: {:?}", name);
                            self.ast.add_node(Node::Signal(name));
                        },

                        // Reactive Statement
                        Node::Block(block) => {
                            // Fetch reactive block dependencies
                            let dependencies: HashSet<String> = clean_signals(&self.ast, deps);
                            log!("+", "Reactive Block - Block: {:?}", block);
                            self.ast.add_node(Node::ReactiveStmt { block, dependencies });
                        }

                        other => error!(&self.lexer, format!("Expected a name (signal identifier) or block (reactive statement) after \"$\". Got: {:?}", other))

                    }
                    continue;

                },
                
                PIPE_RIGHT => todo!(),
                PIPE_LEFT => todo!(),
                L_ARROW => todo!(),

                // * Foward to / Distribution / Pipe
                // Sintactic Sugar that uses the value on the left and passes it as an argument to the function on the right
                // {expr} -> {expr}
                // Ex. names, ages -> print
                // This forwards names and ages to the print function
                // This operation can be chained
                // {expr} -> {expr} -> {expr}
                // Ex. names, ages -> zip -> print
                R_ARROW => {

                    // If parsing distribution, stop parsing to avoid conflict with nested distributions
                    if self.parsing_distribution {
                        self.clean_stop(); // Remove R_ARROW stop
                        self.put_back(token); // Put back R_ARROW so distribution can be parsed once the current distribution is done
                        return;
                    }

                    // LHS should be the last node in the scope
                    // Which should be a WrappedSequence or Sequence with the values to forward
                    let lhs = match self.ast.pop_node().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected an Node before \"->\".")
                    }) {
                        Node::WrappedSequence(seq)
                        | Node::Sequence(seq) =>  seq,

                        node => { vec![node] },
                    };

                    self.parsing_distribution = true;

                    // Skip newlines
                    self.skip_newlines();

                    // The RHS should be the next node
                    // The destination of the values
                    let rhs = match self.parse_until(None).get_first_or_else(|| {
                        error!(&self.lexer, "Expected an Node after \"->\".")
                    }) {
                        Node::WrappedSequence(seq)
                        | Node::Sequence(seq) =>  seq,

                        node => { vec![node] },
                    };

                    self.parsing_distribution = false;

                    log!("+ Distribution", "LHS: {:?} RHS: {:?}", lhs, rhs);

                    self.ast.add_node(Node::Distribution { args: lhs, recipients: rhs });
                    continue;
                }

                // * Anonymous Function / Lambda
                // ({args}*,) => {block}
                // ({args}*,) => {expr}
                FAT_ARROW =>  {
                    // Argument should be last node in the scope
                    // Node should be a WrappedSequence
                    let args: Node = self.ast.pop_node().unwrap_or_else(|| {
                        error!(&self.lexer, "Expected arguments before \"=>\".")
                    });

                    // Ensure there's a block or an expression after the arguments
                    let body: Node = self.parse_until(None).get_first_or_else(|| {
                        error!(&self.lexer, "Expected a block or an expression after arguments. Ex. \"=> return a + b;\"")
                    });

                    self.ast.add_node(Node::Lambda { args: bi!(args), body: bi!(body) });
                    continue;
                }
            }   
        }
    }
}
