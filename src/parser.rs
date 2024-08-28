use std::{fs, path::PathBuf};

use crate::{
    ast::{Expr, Node, Stmt, AST},
    error,
    lexer::{Lexer, Token, TokenKind},
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
/// Used to box an expression
macro_rules! bit {
    // Box it
    ($x:expr) => {
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
    ($self:expr, $($token:ident),*) => {
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

    // Makes parse, parse_expr, parse_statement, etc. stop at a token.
    // used to globally control the parser flow. Ex. parse_block needs to stop at R_BRACKET
    // In difference of `stops`, this is a single buffer of tokens, meaning that it will stop at the first token in the buffer.
    // This allows control over stops while letting handlers to push their own stops.
    global_stop : Option<Vec<TokenKind>>, 

    capturing_sequence: bool, // if the parser is capturing a sequence, parse_expr_from_buffer will return a sequence once parse_expr finds a expr end token

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
            } else if Lexer::is_expression_token(&token) {
                let expr: Expr = self.parse_expr();

                log!("+ parse", "Adding expression to AST: {:?}", expr);

                self.ast.add_node(Node::Expr(expr));
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
                TokenKind::PYTHON => {
                    return Stmt::Python(token.value.clone());
                }

                // * Assingment & Deconstruction & Arithmetic assingments
                // {ident} = {expr};
                // {ident} += {expr};
                // { {ident}, {ident} } = {expr};
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
                    let name = self.get_expr(&Expr::Identifier(String::new()));

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

                // * Parsing exprs inside statment
                _ if Lexer::is_expression_token(&token) => {
                    self.put_back(token);
                    let expr = self.parse_expr();
                    self.ast.add_node(Node::Expr(expr));

                    continue;
                }

                _ => unimplemented!("Token {} is not implemented in parse_statement.", token),
            }
        }

        unimplemented!("Statement parsing didn't reach a return statement at implementations.");
    }

    /// Called when encountering an expression token.
    /// Expression token should be set as REMAINDER_TOKEN before calling this function.
    /// Captures the expression from token to the next semicolon or new line.
    ///
    /// ### Arguments
    /// `stop_at` is a tuple of token types that should stop the expression parsing.
    fn parse_expr(&mut self) -> Expr {
        let mut buffer: Vec<Expr> = Vec::new();

        let default_stop: Vec<TokenKind> = tkarr![NEW_LINE, SEMICOLON];
        let stop_at: Vec<TokenKind> = self.stops.last().cloned().unwrap_or(default_stop.clone());

        // * Capture all tokens that make up the expression
        while let Some(token) = self.next_token() {
            log!(
                "parse expr",
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
                    return self.parse_expr_from_buffer(buffer);
                } 
            }

            // * Stop at
            if stop_at.contains(&token.kind)  {
                if !default_stop.contains(&token.kind) {
                    self.stops.pop(); // Remove the stop buffer
                    self.stopped_at.push(token.kind);
                }
                return self.parse_expr_from_buffer(buffer);
            }

            // If it's not an expression token
            if !Lexer::is_expression_token(&token) {
                self.put_back(token); // Put back the token that stopped the expression
                break;
            }

            match token.kind {
                // * Comments
                TokenKind::COMMENT | TokenKind::ML_COMMENT => {
                    self.ast.add_node(Node::Expr(Expr::Comment(token.value.clone())));
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
                            Expr::Block(block) => block,
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
                        return Expr::Empty;   
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

                    buffer.push(Expr::Signal(ident.value.clone()));
                    continue;
                }

                // * Lambda function
                // {args} => {expr};
                // {args} => {block}
                TokenKind::BIG_R_ARROW => {

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

                    if args != Expr::WrappedSequence(Vec::new())
                        && args != Expr::Group(bit!(Expr::Empty))
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

                        Node::Expr(block)
                    } else {
                        // Expr
                        // TODO: This is not good, since the body of the lambda can be a block or an expression
                        // Unless lambda transpile as actual function, this is not good.
                        // self.stops.push(stop_at); 
                        let e = self.parse_expr();
                        match e {
                            // This is even worse... Seriously? getting the last stmt just to put something in the body?
                            Expr::Empty => self.ast.pop_node().unwrap(),
                            _ => Node::Expr(e),
                        }
                    };

                    if self.stopped_at.is_empty() {
                        error!(
                            &self.lexer,
                            "Expected a block or expression after Lambda function arguments."
                        );
                    }

                    log!("Lambda function", "args={:?} body={:?}", args, body);

                    return Expr::Lambda {
                        args: bit!(args),
                        body: bit!(body),
                    };
                }

                // * Parse Array
                // [expr, expr, ...]
                TokenKind::L_SQUARE_BRACKET => {
                    log!("parse array");
                    add_stop!(self, R_SQUARE_BRACKET);
                    let expr = self.parse_expr();

                    // If not closing braket
                    if self.stopped_at.is_empty()
                        || *self.stopped_at.last().unwrap() != TokenKind::R_SQUARE_BRACKET
                    {
                        error!(
                            &self.lexer,
                            "Expected a closing bracket after array.",
                            format!("[{:?} ... <-- Expected \"]\" here", expr)
                        );
                    }

                    self.stopped_at.pop(); // Consume stop, since we DID stop at an ]

                    if buffer.is_empty() {
                        buffer.push(Expr::Array(Box::new(expr)));
                    }
                    else {
                        // * Indexing -- if there's something on buffer, it's indexing
                        let object = buffer.pop().unwrap();
                        buffer.push(Expr::Index {
                            object: bit!(object),
                            index: bit!(expr),
                        });
                    }
                    log!("! parse braket done");
                    continue;
                }

                // * Distribution
                // | {sequence} -> {sequence};
                // |> {sequence} -> {sequence}
                TokenKind::PIPE => {
                    let expr = self.parse_distribution(false);
                    buffer.push(expr);
                    continue;
                }

                // * Iter distribution
                // |> {sequence} -> {sequence}
                TokenKind::PIPE_RIGHT => {
                    let expr = self.parse_distribution(true);
                    buffer.push(expr);
                    continue;
                }

                // * Parse Range
                // {start: expr}..{end: expr} (exclusive)
                // {start: expr}..={end: expr} (inclusive)
                TokenKind::D_DOT => {
                    // Get start of the range, if no start is provided, it's 0
                    let start = if buffer.is_empty() {
                        Expr::Number("0".to_string())
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
                    let end = self.parse_expr(); // this ensures that we can break, since we are complaying with the stop 

                    buffer.push(Expr::Range {
                        start: bit!(start),
                        end: bit!(end),
                        inclusive,
                    });

                    break; // since to reach here we stopped where we should've, break.
                }

                // * Parse Parenthesis (group, sequence)
                // (expr, expr, ...)
                // (expr * expr)
                TokenKind::L_PARENT => {

                    // If next token is R_PARENT, then it's an empty group
                    let expr = if let Some(TokenKind::R_PARENT) = self.peek_token().map(|t| t.kind) {
                        self.next_token(); // Consume the R_PARENT
                        Expr::Group(bit!(Expr::Empty))
                    } else {
                        // Otherwise, parse what's inside
                        self.parse_paren()
                    };

                    log!("Parenthesis", "collected={:?}, stopped_at={:?}", expr, self.stopped_at);

                    // * Function call
                    // If there's something in the buffer, it's a function call
                    if let Some(prev) = buffer.pop() {
                        buffer.push(Expr::FunctionCall {
                            name: bit!(prev),
                            args: bit!(expr),
                        });

                        continue;
                    }
                    
                    // Group or Sequence
                    buffer.push(expr);
                    continue;
                }

                // * Dict
                TokenKind::L_BRACKET => {
                    let expr = self.parse_dict();
                    buffer.push(expr);
                    continue;
                }

                // * Sequenence
                // {expr}, {expr}, ...
                TokenKind::COMMA => {
                    self.capturing_sequence = true;
                    continue;
                }

                // * Parse Assingment 
                // * Parse Named Args
                // TODO: ASSING is =, -=, *=, ...
                // Thus, this acceps named args as ident *= value. 
                TokenKind::ASSIGN => {
                    log!("parse_expr > Assing | Named arg");

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
                            Expr::Identifier(name) 
                            | Expr::Signal(name) => {
                                // Might not be needed since we just haven't reached the stop token
                                // self.stops.push(stop_at) 
                                let value = self.parse_expr();
                                buffer.push(Expr::NamedArg(name, bit!(value)));
                            }

                            _ => error!(&self.lexer, "Expected an Identifer before \"=\", when parsing named argument.")
                        }

                        continue;

                    } else {
                        // * parsing assingment
                        // Solve buffer, since it is LHS and
                        let lhs = self.parse_expr_from_buffer(buffer.clone());
                        buffer.clear(); // Buffer was consumed in lhs

                        self.ast.add_node(Node::Expr(lhs));

                        self.put_back(token); // put back assingment token since parse_assingment needs to know which type of assingment.
                        let stmt = self.parse_assingment();
                        self.ast.add_node(Node::Stmt(stmt));

                        break; // Finish parse_expr, since we parsed a stmt 
                    }
                }

                // * Member access
                TokenKind::DOT => {
                    // If no previous expr, no left hand value
                    if buffer.is_empty() {
                        error!(
                            &self.lexer,
                            "Expected an expression before operator.".to_string(),
                            format!("{:?} ...", token),
                            "^^^ -- Expected something as: object.property".to_string()
                        );
                    }

                    let lhs = buffer.pop().unwrap();
                    // Might not be needed since we just haven't reached the stop token
                    // self.stops.push(stop_at) 
                    let member = self.parse_expr();

                    buffer.push(Expr::MemberAccess {
                        object: bit!(lhs),
                        member: bit!(member),
                    });

                    continue;
                }

                // * Unary Operator
                // {op}{expr}
                // Buffer must be empty, since otherwise it isn't a unary operation
                _ if Lexer::is_bitwise_operator(&token) && buffer.is_empty() => {
                    log!("Unary Operator", "{:?}", token);

                    // Might not be needed since we just haven't reached the stop token
                    // self.stops.push(stop_at) 
                    let expr = self.parse_expr();

                    buffer.push(Expr::UnaryOp {
                        op: token.value.clone(),
                        expr: bit!(expr),
                    });
                }

                // * Operator
                _ if Lexer::is_operator_token(&token) => {
                    // If no previous expr, no left hand value
                    if buffer.is_empty() {
                        // If operator is "-" then it's a negation
                        if token.kind == TokenKind::SUBTRACT {
                            // Might not be needed since we just haven't reached the stop token
                            // self.stops.push(stop_at) 
                            let expr = self.parse_expr();
                            return Expr::UnaryOp {
                                op: '-'.to_string(),
                                expr: bit!(expr),
                            };
                        }

                        // If operapor is a "@" then it's a decorator
                        if token.kind == TokenKind::AT {
                            add_stop!(self, L_PARENT);
                            let name = self.get_expr(&Expr::Identifier(String::new()));

                            if let Some(TokenKind::L_PARENT) = self.stopped_at.pop() {
                                let args = self.parse_paren();
                                return Expr::Decorator {
                                    name: bit!(name),
                                    args: Some(bit!(args)),
                                };
                            } else {
                                return Expr::Decorator {
                                    name: bit!(name),
                                    args: None,
                                };
                            }
                        }

                        error!(
                            &self.lexer,
                            "Expected an expression before operator.",
                            format!("<!> {} ...", token.value),
                            "^^^ -- Expression missing here "
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
                    //     return self.parse_expr_from_buffer(buffer);
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
            "end parse_expr",
            "{:?} stop_at={:?} stopped_at={:?}",
            buffer,
            stop_at,
            self.stopped_at
        );
        self.parse_expr_from_buffer(buffer)
    }

    /// Parse an expression from a buffer of expressiosn.
    /// Used to "condence" expressions that are made up of multiple expressions.
    /// Ex. (1 + 2) * (2 / 2) -> BinOp(BinOp(1, '+', 2), '*', BinOp(2, '/', 2))
    fn parse_expr_from_buffer(&mut self, buffer: Vec<Expr>) -> Expr {
        log!(
            "parse_expr_from_buffer",
            "buffer={:?}, stopped_at={:?}",
            buffer,
            self.stopped_at
        );

        if buffer.is_empty() {
            return Expr::Empty;
        }

        if buffer.len() == 1 {
            return buffer[0].clone();
        }

        if self.capturing_sequence {
            self.capturing_sequence = false;
            return Expr::Sequence(buffer);
        }

        error!(
            &self.lexer,
            "Couldn't parse expression",
            format!("... {:?} ...", buffer),
            "Whatever this meant to be..."
        )
    }

    /// Parse a distrubution expression.
    /// Should be called when encountering a PIPE token.
    /// | {sequence} -> {sequence};
    /// |> {sequence} -> {sequence}
    /// ---
    /// * Distribution - Distribute PI and Coords into the Direction and Distance functions.
    /// ```| PI, Coords -> Direction, Distance; ``````
    /// * Iter distribution - Distribute a arguments as iterable
    /// ```|> inputs, outputs -> f1, f2;```  Iterate over inputs and outputs and distribute them into f1 and f2
    fn parse_distribution(&mut self, is_iter: bool) -> Expr {
        log!("parse_distribution");

        add_stop!(self, R_ARROW);
        let args_expr = self.parse_expr();

        if self.stopped_at.is_empty() || *self.stopped_at.last().unwrap() != TokenKind::R_ARROW {
            error!(
                &self.lexer,
                "Expected an arrow after arguments when using distribution operator.",
                format!("| {:?} ... <-- expected \"->\" here", args_expr)
            );
        }

        let args: Vec<Expr> = match args_expr {
            Expr::Sequence(seq) => seq,
            _ => vec![args_expr],
        };

        add_stop!(self, SEMICOLON, NEW_LINE);
        let rec_expr = self.parse_expr();

        if self.stopped_at.is_empty() {
            error!(
                &self.lexer,
                "Expected a semicolon or new line after recipients in distribution operator.",
                format!(
                    "| {:?} -> {:?} ... <- Can't end like this must be \";\" or new line.",
                    args, rec_expr
                )
            );
        }

        let recipients: Vec<Expr> = match rec_expr {
            Expr::Sequence(seq) => seq,
            _ => vec![rec_expr],
        };

        log!(
            "end parse_distribution",
            "args={:?} recipients={:?}",
            args,
            recipients
        );

        if is_iter {
            return Expr::IterDistribution { args, recipients };
        }
        return Expr::Distribution { args, recipients };
    }

    /// Parse a parenthesis expression or a sequence of expressions.
    /// Should be called when encountering a L_PAREN token.
    ///
    /// Group
    /// ({expr} {op} {expr} ...)
    ///
    /// Sequence
    /// ({expr}, {expr}, ...)
    fn parse_paren(&mut self) -> Expr {
        log!("parse_paren");

        add_stop!(self, R_PARENT);
        let expr = self.parse_expr();

        self.capturing_sequence = true;

        if let Some(TokenKind::R_PARENT) = self.stopped_at.pop() {
            self.capturing_sequence = false;

            if let Expr::Sequence(values) = expr {
                return Expr::WrappedSequence(values);
            } else {
                return Expr::Group(bit!(expr));
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
    ///     statement | expression
    ///     expression expression is the return value
    /// }
    /// ```
    fn parse_block(&mut self) -> Expr {
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

        let block = Expr::Block(self.ast.get_scope());
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
    fn parse_dict(&mut self) -> Expr {
        let mut keys = Vec::new();
        let mut values = Vec::new();

        log!("parse_dict");

        loop {
            add_stop!(self, COLON, COMMA, R_BRACKET);
            let key = self.parse_expr();

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
            if key == Expr::Empty {
                break;
            }

            match stop_token {
                // key, 
                // key}
                TokenKind::COMMA | TokenKind::R_BRACKET => {
                    if let Expr::Identifier(_) = key {
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
                    let value = self.parse_expr();
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
        Expr::Dict { keys, values }
    }

    /// Parse a binary operator expression.
    /// Should be called after encountering an operator token.
    fn parse_binop(&mut self, left: Expr, operator: Token) -> Expr {
        let right = self.parse_expr();
        Expr::BinOp {
            left: Box::new(left),
            op: operator.value,
            right: Box::new(right),
        }
    }

    fn parse_value_token(&self, token: &Token) -> Expr {
        log!("parse_value_token", "{:?}", token);
        match token.kind {
            TokenKind::NUMBER => Expr::Number(token.value.clone()),
            TokenKind::STRING => Expr::Str(token.value.clone()),
            TokenKind::IDENTIFIER => Expr::Identifier(token.value.clone()),
            _ => error!(
                &self.lexer,
                "Can't parse token to value.",
                format!("Token: {:?}", token),
                "Expected a value token here."
            ),
        }
    }


    /// Parses a conditional statement or expression.
    /// Should be called after encountering an IF token.
    /// ```txt
    /// if {expr} {block} [elif {expr} {block} ...] [else {block}]
    fn parse_conditional(&mut self) -> Expr {
        log!("parse_conditional");

        add_stop!(self, L_BRACKET);
        let condition = self.parse_expr();

        if self.stopped_at.is_empty() || *self.stopped_at.last().unwrap() != TokenKind::L_BRACKET {
            error!(
                &self.lexer,
                "Expected a block after condition.",
                format!("if {:?} ...", condition)
            );
        }

        let block = self.parse_block();

        // Vec<(Condition, Block)>
        let mut elifs: Vec<(Expr, Expr)> = Vec::new();

        // Parse elifs
        while let Some(token) = self.peek_token() {
            if token.kind != TokenKind::ELIF {
                break;
            }

            self.next_token(); // Consume the ELIF token

            add_stop!(self, L_BRACKET);
            let condition = self.parse_expr();

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
        let mut else_block: Option<Box<Expr>> = None;
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

        Expr::Conditional {
            condition: bit!(condition),
            body: bit!(block),
            elifs,
            else_body: else_block,
        }


    }

    /// Get an expression of type `expr_type` from the token stream if it exists, otherwise return None.
    fn get_optional_expr(
        &mut self,
        expr_type: &Expr,
    ) -> Option<Expr> {
        let expr = self.parse_expr();

        if expr == Expr::Empty || expr != *expr_type {
            error!(
                &self.lexer,
                format!("Expected an expression of type {:?}.", expr_type),
                format!("... {:?}", expr)
            );
        }
        return Some(expr);
    }

    /// Get an expression of type `expr_type` from the token stream. If not found, raise an error.
    fn get_expr(&mut self, expr_type: &Expr) -> Expr {
        let expr = self.get_optional_expr(&expr_type);

        if expr.is_none() {
            error!(
                &self.lexer,
                format!(
                    "Expected an expression of type {:?}.\n Found: {:?}",
                    expr_type, expr
                )
            );
        }

        expr.unwrap()
    }

    /// Called when encountering a Assingment kind token. Needs the token.
    /// LHS (ident) should be in AST.current_scope.
    /// * Assingment statement.
    /// - `{ident} = {expr};`
    /// * Arithmetic assingment statement.
    /// - `{ident} += {expr};`
    /// * Multiple assingment statement.
    /// - `({ident}, {ident}, ...) = ({expr}, {expr}, ...)`
    /// * Deconstruction
    /// - `({ident}, {ident}, ...) = {expr}`
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

        let ident: Expr = self.ast.pop_node().unwrap().into();

        log!("parse_assingment", "{:?} {}", ident, op);

        // * Signal definition
        // {Signal} = {expr}
        if let Expr::Signal(name) = ident {
            self.capturing_signals.push(Vec::new());

            let mut value = self.parse_expr();

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
                    value = Expr::BinOp {
                        left: Box::new(Expr::Signal(name.clone())),
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

        let value = self.parse_expr();

        // Single assingment
        if ident == Expr::Identifier(String::new())
             // Multiple assingment
            || ident == Expr::Sequence(Vec::new())
        {
            return Stmt::Assign {
                identifiers: vec![ident],
                values: vec![value],
                op,
            };
        }
        // Deconstruction
        else if ident
            == (Expr::Dict {
                keys: Vec::new(),
                values: Vec::new(),
            })
        {
            // Unpack keys from ident
            let keys = match ident {
                Expr::Dict { keys, values: _ } => keys,
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
