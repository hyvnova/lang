use std::{fs, path::PathBuf};

use crate::{
    _ast::{Node, Node, Stmt, AST},
    error,
    _lexer::{Lexer, Token, TokenKind},
    log,
};

use std::collections::HashSet;

fn filter_repeated_strings(vec: Vec<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    vec.into_iter()
        .filter(|s| seen.insert(s.clone())) // insert returns false if the value was already in the set
        .collect()
}

/// Box-It, bit!
/// Used to box an Nodeession
macro_rules! bit {
    // Box it
    ($x:Node) => {
        Box::new($x)
    };
}


/// Macro to create a token kind array
macro_rules! tkarr {
    ($($x:ident),*) => {
        vec![$(TokenKind::$x),*]
    };
}

macro_rules! add_stop {
    ($self:Node, $($token:ident),*) => {
        $self.stops.push(tkarr![$($token),*]);
    };
}

pub struct Parser {
    // Token that was read but not processed
    remainder_token: Option<Token>,

    // lexer
    pub lexer: Lexer,

    // ast
    pub ast: AST,

    // Travel variables -- used to keep track of control flow
    stopped_at: Vec<TokenKind>, // if the parser stopped at a token, this will be the token. Works as a Stack.

    // Vector of "stops at", works as a stack.
    // When a handler needs to stop at a token, it will push an array of tokens to this vector.
    // When the parser reaches a token in one of the arrays, it will stop and pop the array.
    // The token that stopped the parser will be pushed to the `stopped_at` vector.
    stops: Vec<Vec<TokenKind>>, 

    // Makes parse, parse_Node, parse_statement, etc. stop at a token.
    // used to globally control the parser flow. Ex. parse_block needs to stop at R_BRACKET
    // In difference of `stops`, this is a single buffer of tokens, meaning that it will stop at the first token in the buffer.
    // This allows control over stops while letting handlers to push their own stops.
    global_stop : Option<Vec<TokenKind>>, 

    capturing_sequence: bool, // if the parser is capturing a sequence, parse_Node_from_buffer will return a sequence once parse_Node finds a Node end token

    // When a handler wants to capture signals, it will push a new vector to this variable.
    // The vector will be filled with the signals that the parser captures.
    // Used to keep track of "dependencies" of signals, so we can later on generate the correct code.
    capturing_signals: Vec<Vec<String>>,

    // Used to differentiate between signal definition and signal update
    // If a signal is defined, any type of assignment will be considered as a signal update
    defined_signals: Vec<String>,
}

impl Parser {
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

    pub fn parse(&mut self) {
        // * Parse start
        while let Some(token) = self.peek_token() {
            log!("parse", "{:?}", token);


            // * Global stop at
            if let Some(stop_at) = &self.global_stop {
                if stop_at.contains(&token.kind) {
                    self.next_token(); // Consume the token
                    self.stopped_at.push(token.kind); // Save the token that stopped the parser
                    break;
                } 
            }

            // * Stop at
            // If current token is in the stop buffer, stop the parser, pop the buffer and consume the token
            if let Some(stop_at) = self.stops.last() {
                if stop_at.contains(&token.kind) {
                    self.next_token(); // Consume the token
                    self.stopped_at.push(token.kind); // Save the token that stopped the parser
                    self.stops.pop(); // Remove the stop buffer
                    break;
                }
            }

            // New lines
            if token.kind == TokenKind::NEW_LINE {
                self.next_token();
                continue;
            }

            if Lexer::is_statement_token(&token) {
                let statement: Stmt = self.parse_statement();
                self.ast.add_node(Node::Stmt(statement));
            } else if Lexer::is_Nodeession_token(&token) {
                let Node: Node = self.parse_Node();

                log!("+ parse", "Adding Nodeession to AST: {:?}", Node);

                self.ast.add_node(Node::Node(Node));
            } else {
                log!("! parse", "Unknown token: {:?}", token);
            }

        }

    }

    /// Called when encountering a statement token.
    /// Statement token should be set as REMAINDER_TOKEN before calling this function.
    /// Captures the statement from token to the next semicolon.
    fn parse_statement(&mut self) -> Stmt {
        log!("parse_statement");

        while let Some(token) = self.next_token() {
            // * Global stop at
            if let Some(stop_at) = &self.global_stop {
                if stop_at.contains(&token.kind) {
                    // We put back token and end here so parse() can handle the stop
                    self.put_back(token);
                    return Stmt::Empty;
                } 
            }

            // * Stop at
            if let Some(stop_at) = self.stops.last() {
                if stop_at.contains(&token.kind) {
                    self.put_back(token);
                    return Stmt::Empty;
                }
            }

            if token.kind == TokenKind::SEMICOLON {
                break;
            }

            match token.kind {
                // * Conditional
                // `if {Node} {block} [elif {Node} {block} ...] [else {block}]`
                TokenKind::IF => {
                    let Node = self.parse_conditional();
                    match Node {
                        Node::Conditional { condition, body, elifs, else_body } => {
                            return Stmt::Conditional {
                                condition,
                                body,
                                elifs,
                                else_body,
                            };
                        }
                        _ => error!(&self.lexer, "Expected a conditional Nodeession."),
                    }
                }

                TokenKind::PYTHON => {
                    return Stmt::Python(token.value.clone());
                }

                // * Assingment & Deconstruction & Arithmetic assingments
                // {ident} = {Node};
                // {ident} += {Node};
                // { {ident}, {ident} } = {Node};
                TokenKind::ASSIGN => {
                    self.put_back(token);
                    let stmt = self.parse_assingment();
                    return stmt;
                }

                // * Function definition
                // `def {ident}({ident}, {ident}, ...) {block}`
                TokenKind::FN_DEF => {
                    // Parse function name
                    add_stop!(self, L_PARENT);
                    let name = self.get_Node(&Node::Identifier(String::new()));

                    // Since should stop at L_PARENT, if it's not there, raise an error
                    if self.stopped_at.is_empty() || self.stopped_at.pop().unwrap() != TokenKind::L_PARENT {
                        error!(
                            &self.lexer,
                            "Expected a parenthesis after function name.",
                            format!("def {:?} (...", name)
                        );
                    }

                    // Parse function arguments
                    let args = self.parse_paren();

                    // If next token is not a "{", then there's no body. Raise an error
                    if !self.peek_token().map(|t| t.kind == TokenKind::L_BRACKET).unwrap_or(false) {
                        error!(
                            &self.lexer,
                            "Expected a block after function arguments.",
                            format!("def {:?} ({:?})  <---", name, args)
                        );
                    }

                    self.next_token(); // Consume the L_BRACKET

                    // Parse function body
                    let body = self.parse_block();

                    log!(
                        "parse statement / DEF",
                        "{:?}( {:?} ) {:?} ",
                        name,
                        args,
                        body
                    );

                    return Stmt::FunctionDef { name, args, body };
                }

                // * Parsing Nodes inside statment
                _ if Lexer::is_Nodeession_token(&token) => {
                    self.put_back(token);
                    let Node = self.parse_Node();
                    self.ast.add_node(Node::Node(Node));

                    continue;
                }

                _ => unimplemented!("Token {} is not implemented in parse_statement.", token),
            }
        }

        unimplemented!("Statement parsing didn't reach a return statement at implementations.");
    }

    /// Called when encountering an Nodeession token.
    /// Nodeession token should be set as REMAINDER_TOKEN before calling this function.
    /// Captures the Nodeession from token to the next semicolon or new line.
    ///
    /// ### Arguments
    /// `stop_at` is a tuple of token types that should stop the Nodeession parsing.
    fn parse_Node(&mut self) -> Node {
        let mut buffer: Vec<Node> = Vec::new();

        let default_stop: Vec<TokenKind> = tkarr![NEW_LINE, SEMICOLON];
        let stop_at: Vec<TokenKind> = self.stops.last().cloned().unwrap_or(default_stop.clone());

        // * Capture all tokens that make up the Nodeession
        while let Some(token) = self.next_token() {
            log!(
                "parse Node",
                "{:?} stop_at={:?} buffer={:?} stopped_at={:?}",
                token,
                stop_at,
                buffer,
                self.stopped_at
            );

            // * global stop at
            if let Some(stop_at) = &self.global_stop {
                if stop_at.contains(&token.kind) {
                    log!("! global stop");
                    // We put back token and end here so parse() can handle the stop
                    self.put_back(token);
                    return self.parse_Node_from_buffer(buffer);
                } 
            }

            // * Stop at
            if stop_at.contains(&token.kind)  {
                if !default_stop.contains(&token.kind) {
                    self.stops.pop(); // Remove the stop buffer
                    self.stopped_at.push(token.kind);
                }
                return self.parse_Node_from_buffer(buffer);
            }

            // If it's not an Nodeession token
            if !Lexer::is_Nodeession_token(&token) {
                self.put_back(token); // Put back the token that stopped the Nodeession
                break;
            }

            match token.kind {
                // * Comments
                TokenKind::COMMENT | TokenKind::ML_COMMENT => {
                    self.ast.add_node(Node::Node(Node::Comment(token.value.clone())));
                    continue;
                }

                // * Conditional
                TokenKind::IF => {
                    let Node = self.parse_conditional();
                    buffer.push(Node);
                    continue;
                }

                // * Parse Signal
                // * Parse Reactive statement
                // ${ident}
                // $ { {block} }
                TokenKind::DOLLAR_SING => {

                    // * if next token is a "{" then it's a reactive statement
                    if let Some(TokenKind::L_BRACKET) = self.peek_token().map(|t| t.kind) {

                        log!("reactive statement");

                        self.next_token(); // Consume the L_BRACKET
                        self.capturing_signals.push(Vec::new()); // Start capturing signals

                        let block = match self.parse_block() {
                            Node::Block(block) => block,
                            _ => error!(&self.lexer, "Expected a block when declaring a reactive statement"),
                        };

                        let dependencies: Vec<String> = self.capturing_signals.pop().unwrap_or_else(|| {
                            error!(&self.lexer, "No signals captured when declaring a reactive statement / no buffer")
                        });

                        // Filter "dirty" signals
                        // dirty sginals are signals that depend on another signal which is already part of the dependencies
                        // or signals that are repeated
                        let dependencies: Vec<String> = filter_repeated_strings(dependencies.clone())
                            .iter()
                            .cloned()
                            .filter(|dep| {
                                // Check if the current dependency is already covered by any other dependency
                                !dependencies.iter().any(|signal| {
                                    // Skip if it's the same signal to avoid self-comparison
                                    if signal == dep {
                                        return false;
                                    }

                                    // Check if the current signal indirectly covers the dependency
                                    let signal_deps = self.ast.find_signal_deps(signal);
                                    signal_deps.contains(&dep)
                                })
                            })
                            .collect();

                        self.ast.add_node(Node::Stmt(Stmt::ReactiveStmt { block, dependencies }));
                        return Node::Empty;   
                    }

                    // * Otherwise, it's a signal
                    let ident = self.next_token().unwrap_or_else(|| {
                        error!(
                            &self.lexer,
                            "Expected an identifier (or block) after signal operator.",
                            format!("${:?}", token),
                            " ^^^ - This is not an identifier."
                        )
                    });

                    if ident.kind != TokenKind::IDENTIFIER {
                        error!(
                            &self.lexer,
                            "No name provided for signal.",
                            format!("${:?}", ident),
                            " ^^^ - This is not an identifier."
                        );
                    }

                    log!("signal", "{:?}", ident);

                    if !self.capturing_signals.is_empty() {
                        self.capturing_signals.last_mut().unwrap().push(ident.value.clone());
                    }

                    buffer.push(Node::Signal(ident.value.clone()));
                    continue;
                }

                // * Lambda function
                // {args} => {Node};
                // {args} => {block}
                TokenKind::FAT_ARROW => {

                    log!("Lambda function");

                    if buffer.is_empty() {
                        error!(
                            &self.lexer,
                            "Expected args definition for Lambda function.",
                            format!("-> ..."),
                            "\t^^^ - Missing arguments here."
                        );
                    }

                    let args = buffer.pop().unwrap();

                    if args != Node::WrappedSequence(Vec::new())
                        && args != Node::Group(bit!(Node::Empty))
                    {
                        error!(
                            &self.lexer,
                            "Expected args definition for Lambda function. This is not a valid argument definition.",
                            format!("-> {:?}", args)
                        );
                    }

                    // Parsing lambda body
                    let body = if let Some(TokenKind::L_BRACKET) = self.peek_token().map(|t| t.kind) {
                        
                        // Block
                        // TODO: Lambdas can't handle blocks as body
                        // This is because lambdas in python don't support blocks
                        self.next_token(); // Consume the L_BRACKET
                        let block = self.parse_block();

                        // If next token is stop token, added it as stop token
                        if let Some(t) = self.peek_token() {
                            if stop_at.contains(&t.kind) {
                                self.next_token(); // Consume the stop token
                                self.stopped_at.push(t.kind);
                            }
                        }

                        Node::Node(block)
                    } else {
                        // Node
                        // TODO: This is not good, since the body of the lambda can be a block or an Nodeession
                        // Unless lambda transpile as actual function, this is not good.
                        // self.stops.push(stop_at); 
                        let e = self.parse_Node();
                        match e {
                            // This is even worse... Seriously? getting the last stmt just to put something in the body?
                            Node::Empty => self.ast.pop_node().unwrap(),
                            _ => Node::Node(e),
                        }
                    };

                    if self.stopped_at.is_empty() {
                        error!(
                            &self.lexer,
                            "Expected a block or Nodeession after Lambda function arguments."
                        );
                    }

                    log!("Lambda function", "args={:?} body={:?}", args, body);

                    return Node::Lambda {
                        args: bit!(args),
                        body: bit!(body),
                    };
                }

                // * Parse Array
                // [Node, Node, ...]
                TokenKind::L_SQUARE_BRACKET => {
                    log!("parse array");
                    add_stop!(self, R_SQUARE_BRACKET);
                    let Node = self.parse_Node();

                    // If not closing braket
                    if self.stopped_at.is_empty()
                        || *self.stopped_at.last().unwrap() != TokenKind::R_SQUARE_BRACKET
                    {
                        error!(
                            &self.lexer,
                            "Expected a closing bracket after array.",
                            format!("[{:?} ... <-- Expected \"]\" here", Node)
                        );
                    }

                    self.stopped_at.pop(); // Consume stop, since we DID stop at an ]

                    if buffer.is_empty() {
                        buffer.push(Node::Array(Box::new(Node)));
                    }
                    else {
                        // * Indexing -- if there's something on buffer, it's indexing
                        let object = buffer.pop().unwrap();
                        buffer.push(Node::Index {
                            object: bit!(object),
                            index: bit!(Node),
                        });
                    }
                    log!("! parse braket done");
                    continue;
                }

                // * Distribution
                // | {sequence} -> {sequence};
                // |> {sequence} -> {sequence}
                TokenKind::PIPE => {
                    let Node = self.parse_distribution(false);
                    buffer.push(Node);
                    continue;
                }

                // * Iter distribution
                // |> {sequence} -> {sequence}
                TokenKind::PIPE_RIGHT => {
                    let Node = self.parse_distribution(true);
                    buffer.push(Node);
                    continue;
                }

                // * Parse Range
                // {start: Node}..{end: Node} (exclusive)
                // {start: Node}..={end: Node} (inclusive)
                TokenKind::D_DOT => {
                    // Get start of the range, if no start is provided, it's 0
                    let start = if buffer.is_empty() {
                        Node::Number("0".to_string())
                    } else {
                        buffer.pop().unwrap()
                    };

                    // if next token is "=" as in "..="
                    let mut inclusive: bool = false;
                    if let Some(TokenKind::ASSIGN) = self.peek_token().map(|t| t.kind)
                    {
                        self.next_token(); // Consume the "="
                        inclusive = true;
                    }

                    // Might not be needed since we just haven't reached the stop token
                    // self.stops.push(stop_at) 
                    let end = self.parse_Node(); // this ensures that we can break, since we are complaying with the stop 

                    buffer.push(Node::Range {
                        start: bit!(start),
                        end: bit!(end),
                        inclusive,
                    });

                    break; // since to reach here we stopped where we should've, break.
                }

                // * Parse Parenthesis (group, sequence)
                // (Node, Node, ...)
                // (Node * Node)
                TokenKind::L_PARENT => {

                    // If next token is R_PARENT, then it's an empty group
                    let Node = if let Some(TokenKind::R_PARENT) = self.peek_token().map(|t| t.kind) {
                        self.next_token(); // Consume the R_PARENT
                        Node::Group(bit!(Node::Empty))
                    } else {
                        // Otherwise, parse what's inside
                        self.parse_paren()
                    };

                    log!("Parenthesis", "collected={:?}, stopped_at={:?}", Node, self.stopped_at);

                    // * Function call
                    // If there's something in the buffer, it's a function call
                    if let Some(prev) = buffer.pop() {
                        buffer.push(Node::FunctionCall {
                            name: bit!(prev),
                            args: bit!(Node),
                        });

                        continue;
                    }
                    
                    // Group or Sequence
                    buffer.push(Node);
                    continue;
                }

                // * Dict
                TokenKind::L_BRACKET => {
                    let Node = self.parse_dict();
                    buffer.push(Node);
                    continue;
                }

                // * Sequenence
                // {Node}, {Node}, ...
                TokenKind::COMMA => {
                    self.capturing_sequence = true;
                    continue;
                }

                // * Parse Assingment 
                // * Parse Named Args
                // TODO: ASSING is =, -=, *=, ...
                // Thus, this acceps named args as ident *= value. 
                TokenKind::ASSIGN => {
                    log!("parse_Node > Assing | Named arg");

                    // Ensures there's a lhs
                    if buffer.is_empty() {
                        error!(
                            &self.lexer, 
                            "Unexpected \"=\". This should be in-front of a identifier most-likely."
                        );
                    }

                    // * parsing named arg
                    if self.capturing_sequence {

                        let ident = buffer.pop().unwrap();

                        // There must be an identifier before =

                        match ident {
                            Node::Identifier(name) 
                            | Node::Signal(name) => {
                                // Might not be needed since we just haven't reached the stop token
                                // self.stops.push(stop_at) 
                                let value = self.parse_Node();
                                buffer.push(Node::NamedArg(name, bit!(value)));
                            }

                            _ => error!(&self.lexer, "Expected an Identifer before \"=\", when parsing named argument.")
                        }

                        continue;

                    } else {
                        // * parsing assingment
                        // Solve buffer, since it is LHS and
                        let lhs = self.parse_Node_from_buffer(buffer.clone());
                        buffer.clear(); // Buffer was consumed in lhs

                        self.ast.add_node(Node::Node(lhs));

                        self.put_back(token); // put back assingment token since parse_assingment needs to know which type of assingment.
                        let stmt = self.parse_assingment();
                        self.ast.add_node(Node::Stmt(stmt));

                        break; // Finish parse_Node, since we parsed a stmt 
                    }
                }

                // * Member access
                TokenKind::DOT => {
                    // If no previous Node, no left hand value
                    if buffer.is_empty() {
                        error!(
                            &self.lexer,
                            "Expected an Nodeession before operator.".to_string(),
                            format!("{:?} ...", token),
                            "^^^ -- Expected something as: object.property".to_string()
                        );
                    }

                    let lhs = buffer.pop().unwrap();
                    // Might not be needed since we just haven't reached the stop token
                    // self.stops.push(stop_at) 
                    let member = self.parse_Node();

                    buffer.push(Node::MemberAccess {
                        object: bit!(lhs),
                        member: bit!(member),
                    });

                    continue;
                }

                // * Unary Operator
                // {op}{Node}
                // Buffer must be empty, since otherwise it isn't a unary operation
                _ if Lexer::is_bitwise_operator(&token) && buffer.is_empty() => {
                    log!("Unary Operator", "{:?}", token);

                    // Might not be needed since we just haven't reached the stop token
                    // self.stops.push(stop_at) 
                    let Node = self.parse_Node();

                    buffer.push(Node::UnaryOp {
                        op: token.value.clone(),
                        Node: bit!(Node),
                    });
                }

                // * Operator
                _ if Lexer::is_operator_token(&token) => {
                    // If no previous Node, no left hand value
                    if buffer.is_empty() {
                        // If operator is "-" then it's a negation
                        if token.kind == TokenKind::SUBTRACT {
                            // Might not be needed since we just haven't reached the stop token
                            // self.stops.push(stop_at) 
                            let Node = self.parse_Node();
                            return Node::UnaryOp {
                                op: '-'.to_string(),
                                Node: bit!(Node),
                            };
                        }

                        // If operapor is a "@" then it's a decorator
                        if token.kind == TokenKind::AT {
                            add_stop!(self, L_PARENT);
                            let name = self.get_Node(&Node::Identifier(String::new()));

                            if let Some(TokenKind::L_PARENT) = self.stopped_at.pop() {
                                let args = self.parse_paren();
                                return Node::Decorator {
                                    name: bit!(name),
                                    args: Some(bit!(args)),
                                };
                            } else {
                                return Node::Decorator {
                                    name: bit!(name),
                                    args: None,
                                };
                            }
                        }

                        error!(
                            &self.lexer,
                            "Expected an Nodeession before operator.",
                            format!("<!> {} ...", token.value),
                            "^^^ -- Nodeession missing here "
                        );
                    }
                    // Might not be needed since we just haven't reached the stop token
                    // self.stops.push(stop_at) 
                    let binop = self.parse_binop(buffer.pop().unwrap(), token);
                    buffer.push(binop);
                    continue;

                    // TODO: Remove if bin op works as expected
                    // If there was a stop token, check if it's the one we are looking for
                    // if !self.stopped_at.is_empty()
                    //     && stop_at.contains(self.stopped_at.last().unwrap())
                    // {
                    //     self.stopped_at.pop(); // Remove the stop token
                    //     return self.parse_Node_from_buffer(buffer);
                    // }
                }

                // * Value token
                _ => {
                    let res = self.parse_value_token(&token);
                    buffer.push(res);
                    continue;
                }
            }
        }

        log!(
            "end parse_Node",
            "{:?} stop_at={:?} stopped_at={:?}",
            buffer,
            stop_at,
            self.stopped_at
        );
        self.parse_Node_from_buffer(buffer)
    }

    /// Parse an Nodeession from a buffer of Nodeessiosn.
    /// Used to "condence" Nodeessions that are made up of multiple Nodeessions.
    /// Ex. (1 + 2) * (2 / 2) -> BinOp(BinOp(1, '+', 2), '*', BinOp(2, '/', 2))
    fn parse_Node_from_buffer(&mut self, buffer: Vec<Node>) -> Node {
        log!(
            "parse_Node_from_buffer",
            "buffer={:?}, stopped_at={:?}",
            buffer,
            self.stopped_at
        );

        if buffer.is_empty() {
            return Node::Empty;
        }

        if buffer.len() == 1 {
            return buffer[0].clone();
        }

        if self.capturing_sequence {
            self.capturing_sequence = false;
            return Node::Sequence(buffer);
        }

        error!(
            &self.lexer,
            "Couldn't parse Nodeession",
            format!("... {:?} ...", buffer),
            "Whatever this meant to be..."
        )
    }

    /// Parse a distrubution Nodeession.
    /// Should be called when encountering a PIPE token.
    /// | {sequence} -> {sequence};
    /// |> {sequence} -> {sequence}
    /// ---
    /// * Distribution - Distribute PI and Coords into the Direction and Distance functions.
    /// ```| PI, Coords -> Direction, Distance; ``````
    /// * Iter distribution - Distribute a arguments as iterable
    /// ```|> inputs, outputs -> f1, f2;```  Iterate over inputs and outputs and distribute them into f1 and f2
    fn parse_distribution(&mut self, is_iter: bool) -> Node {
        log!("parse_distribution");

        add_stop!(self, R_ARROW);
        let args_Node = self.parse_Node();

        if self.stopped_at.is_empty() || *self.stopped_at.last().unwrap() != TokenKind::R_ARROW {
            error!(
                &self.lexer,
                "Expected an arrow after arguments when using distribution operator.",
                format!("| {:?} ... <-- expected \"->\" here", args_Node)
            );
        }

        let args: Vec<Node> = match args_Node {
            Node::Sequence(seq) => seq,
            _ => vec![args_Node],
        };

        add_stop!(self, SEMICOLON, NEW_LINE);
        let rec_Node = self.parse_Node();

        if self.stopped_at.is_empty() {
            error!(
                &self.lexer,
                "Expected a semicolon or new line after recipients in distribution operator.",
                format!(
                    "| {:?} -> {:?} ... <- Can't end like this must be \";\" or new line.",
                    args, rec_Node
                )
            );
        }

        let recipients: Vec<Node> = match rec_Node {
            Node::Sequence(seq) => seq,
            _ => vec![rec_Node],
        };

        log!(
            "end parse_distribution",
            "args={:?} recipients={:?}",
            args,
            recipients
        );

        if is_iter {
            return Node::IterDistribution { args, recipients };
        }
        return Node::Distribution { args, recipients };
    }

    /// Parse a parenthesis Nodeession or a sequence of Nodeessions.
    /// Should be called when encountering a L_PAREN token.
    ///
    /// Group
    /// ({Node} {op} {Node} ...)
    ///
    /// Sequence
    /// ({Node}, {Node}, ...)
    fn parse_paren(&mut self) -> Node {
        log!("parse_paren");

        add_stop!(self, R_PARENT);
        let Node = self.parse_Node();

        self.capturing_sequence = true;

        if let Some(TokenKind::R_PARENT) = self.stopped_at.pop() {
            self.capturing_sequence = false;

            if let Node::Sequence(values) = Node {
                return Node::WrappedSequence(values);
            } else {
                return Node::Group(bit!(Node));
            }
        } else {
            error!(
                &self.lexer,
                "Expected closing parenthesis.",
                "( ... ",
                format!("stoppped_at={:?}", self.stopped_at)
            )
        }
    }

    /// Parse a block of code.
    /// ```txt
    /// {
    ///     statement | Nodeession
    ///     Nodeession Nodeession is the return value
    /// }
    /// ```
    fn parse_block(&mut self) -> Node {
        log!("parse_block");

        self.ast.new_scope();
        self.global_stop = Some(vec![TokenKind::R_BRACKET]);
        self.parse();

        // No stop token
        if self.stopped_at.is_empty() || *self.stopped_at.last().unwrap() != TokenKind::R_BRACKET {
            error!(
                &self.lexer,
                "Expected a closing bracket.",
                "{ ... <- Expected \"}\" here"
            );
        }
        self.global_stop = None;
        self.stopped_at.pop(); // Consume the R_BRACKET

        let block = Node::Block(self.ast.get_scope());
        // Go back to the previous scope
        self.ast.pop_scope();
        log!("end parse_block", "{:?}", block);
        
        block
    }

    /// Parse a dict
    /// {key: value, ...}
    /// { key, ... }
    /// {}
    /// should be called after encounterin a "{"
    fn parse_dict(&mut self) -> Node {
        let mut keys = Vec::new();
        let mut values = Vec::new();

        log!("parse_dict");

        loop {
            add_stop!(self, COLON, COMMA, R_BRACKET);
            let key = self.parse_Node();

            let stop_token = match self.stopped_at.pop() {
                Some(t) => t,
                None => error!(
                    &self.lexer,
                    "Unexpected end of file.",
                    "{{ ...",
                    "Expected a key or closing bracket, but got nothing instead."
                ),
            };

            log!("parse_dict", "key={:?} stop_token={:?}", key, stop_token);

            // No key, end of dict
            if key == Node::Empty {
                break;
            }

            match stop_token {
                // key, 
                // key}
                TokenKind::COMMA | TokenKind::R_BRACKET => {
                    if let Node::Identifier(_) = key {
                        keys.push(key.clone());
                        values.push(key);

                        if stop_token == TokenKind::R_BRACKET {
                            break;
                        }
                    } else {
                        error!(
                            &self.lexer,
                            "Key can't be used as value, expected an identifier (variable) instead.",
                            format!("{{...{:?}...}}", key),
                            "this should be an identifier."
                        );
                    }
                }

                // key : value
                TokenKind::COLON => {
                    add_stop!(self, COMMA, R_BRACKET);
                    let value = self.parse_Node();
                    log!("parse_dict", "key={:?} value={:?}", key, value);
                    keys.push(key);
                    values.push(value);

                    if let Some(TokenKind::R_BRACKET) = self.stopped_at.pop() {
                        break;
                    }
                }
                _ => break,
            }
        }

        log!("end parse_dict", "keys={:?} values={:?}", keys, values);
        Node::Dict { keys, values }
    }

    /// Parse a binary operator Nodeession.
    /// Should be called after encountering an operator token.
    fn parse_binop(&mut self, left: Node, operator: Token) -> Node {
        let right = self.parse_Node();
        Node::BinOp {
            left: Box::new(left),
            op: operator.value,
            right: Box::new(right),
        }
    }

    fn parse_value_token(&self, token: &Token) -> Node {
        log!("parse_value_token", "{:?}", token);
        match token.kind {
            TokenKind::NUMBER => Node::Number(token.value.clone()),
            TokenKind::STRING => Node::Str(token.value.clone()),
            TokenKind::IDENTIFIER => Node::Identifier(token.value.clone()),
            _ => error!(
                &self.lexer,
                "Can't parse token to value.",
                format!("Token: {:?}", token),
                "Expected a value token here."
            ),
        }
    }


    /// Parses a conditional statement or Nodeession.
    /// Should be called after encountering an IF token.
    /// ```txt
    /// if {Node} {block} [elif {Node} {block} ...] [else {block}]
    fn parse_conditional(&mut self) -> Node {
        log!("parse_conditional");

        add_stop!(self, L_BRACKET);
        let condition = self.parse_Node();

        if self.stopped_at.is_empty() || *self.stopped_at.last().unwrap() != TokenKind::L_BRACKET {
            error!(
                &self.lexer,
                "Expected a block after condition.",
                format!("if {:?} ...", condition)
            );
        }

        self.stopped_at.pop(); // Consume the L_BRACKET

        let block = self.parse_block();

        // Vec<(Condition, Block)>
        let mut elifs: Vec<(Node, Node)> = Vec::new();

        // Parse elifs
        while let Some(token) = self.peek_token() {
            if token.kind != TokenKind::ELIF {
                break;
            }

            self.next_token(); // Consume the ELIF token

            add_stop!(self, L_BRACKET);
            let condition = self.parse_Node();

            if self.stopped_at.is_empty() || *self.stopped_at.last().unwrap() != TokenKind::L_BRACKET {
                error!(
                    &self.lexer,
                    "Expected a block after condition.",
                    format!("elif {:?} ...", condition)
                );
            }

            let block = self.parse_block();
            elifs.push((condition, block));
        }

        // Parse else
        let mut else_block: Option<Box<Node>> = None;

        if let Some(TokenKind::ELSE) = self.peek_token().map(|t| t.kind) {
            self.next_token(); // Consume the ELSE token

            // If there's a block after else
            if let Some(TokenKind::L_BRACKET) = self.peek_token().map(|t| t.kind) {
                self.next_token(); // Consume the L_BRACKET
                let block = self.parse_block();
                else_block = Some(bit!(block));
            }

            // Otherwise, error
            error!(
                &self.lexer,
                "Expected a block after else.",
                format!("else ...")
            );
        }

        log!(
            "end parse_conditional",
            "condition={:?} block={:?} elifs={:?}",
            condition,
            block,
            elifs
        );

        Node::Conditional {
            condition: bit!(condition),
            body: bit!(block),
            elifs,
            else_body: else_block,
        }


    }

    /// Get an Nodeession of type `Node_type` from the token stream if it exists, otherwise return None.
    fn get_optional_Node(
        &mut self,
        Node_type: &Node,
    ) -> Option<Node> {
        let Node = self.parse_Node();

        if Node == Node::Empty || Node != *Node_type {
            error!(
                &self.lexer,
                format!("Expected an Nodeession of type {:?}.", Node_type),
                format!("... {:?}", Node)
            );
        }
        return Some(Node);
    }

    /// Get an Nodeession of type `Node_type` from the token stream. If not found, raise an error.
    fn get_Node(&mut self, Node_type: &Node) -> Node {
        let Node = self.get_optional_Node(&Node_type);

        if Node.is_none() {
            error!(
                &self.lexer,
                format!(
                    "Expected an Nodeession of type {:?}.\n Found: {:?}",
                    Node_type, Node
                )
            );
        }

        Node.unwrap()
    }

    /// Called when encountering a Assingment kind token. Needs the token.
    /// LHS (ident) should be in AST.current_scope.
    /// * Assingment statement.
    /// - `{ident} = {Node};`
    /// * Arithmetic assingment statement.
    /// - `{ident} += {Node};`
    /// * Multiple assingment statement.
    /// - `({ident}, {ident}, ...) = ({Node}, {Node}, ...)`
    /// * Deconstruction
    /// - `({ident}, {ident}, ...) = {Node}`
    fn parse_assingment(&mut self) -> Stmt {

        if self.ast.current_scope().is_empty() {
            error!(
                &self.lexer,
                "Expected an identifier before assingment operator.",
                format!("... = ..."),
                " ^^^ - This is not an identifier or not one present."
            );
        }

        let op = self.next_token().unwrap().value.clone();

        let ident: Node = self.ast.pop_node().unwrap().into();

        log!("parse_assingment", "{:?} {}", ident, op);

        // * Signal definition
        // {Signal} = {Node}
        if let Node::Signal(name) = ident {
            self.capturing_signals.push(Vec::new());

            let mut value = self.parse_Node();

            let mut dependencies = self.capturing_signals.pop().unwrap_or_else(|| {
                error!(&self.lexer, "No signals captured when declaring a signal / no buffer")
            });

            // Ensure there's no circular dependency
            dependencies.retain(|dep| dep != &name);

            // Circular dependency = Sa depends on Sb and Sb depends on Sa
            self.defined_signals
                .iter()
                .filter(|sig| dependencies.contains(sig)) // Check if this signal depends on any other signal
                .for_each(|sig| {
                    let sig_deps = self.ast.find_signal_deps(sig);

                    if sig_deps.contains(&name) {
                        error!(
                            &self.lexer,
                            "Circular dependency detected.",
                            format!("{:?} <-> {:?}", name, sig),
                            "These signals depend on each other."
                        );
                    }
                });

            // * Signal update
            // If signal is already defined, it's an update
            if self.defined_signals.contains(&name) {
                if op != "=" {
                    value = Node::BinOp {
                        left: Box::new(Node::Signal(name.clone())),
                        op: op.clone()[..op.len() - 1].to_string(), // Remove "=" because now it's a binop
                        right: Box::new(value.clone()),
                    };
                }

                return Stmt::SignalUpdate {
                    name,
                    value,
                    dependencies,
                };
            } else {
                self.defined_signals.push(name.clone());
                return Stmt::SignalDef {
                    name,
                    value,
                    dependencies,
                };
            }
        }

        let value = self.parse_Node();

        // Single assingment
        if ident == Node::Identifier(String::new())
             // Multiple assingment
            || ident == Node::Sequence(Vec::new())
        {
            return Stmt::Assign {
                identifiers: vec![ident],
                values: vec![value],
                op,
            };
        }
        // Deconstruction
        else if ident
            == (Node::Dict {
                keys: Vec::new(),
                values: Vec::new(),
            })
        {
            // Unpack keys from ident
            let keys = match ident {
                Node::Dict { keys, values: _ } => keys,
                _ => unreachable!(),
            };

            return Stmt::Deconstruction {
                identifiers: keys,
                value,
            };
        } else {
            error!(
                &self.lexer,
                "Expected an identifier or keys before assingment operator.",
                format!("{:?} = {:?}", ident, value),
                " ^^^ - This is not an identifier."
            );
        }
    }
}
