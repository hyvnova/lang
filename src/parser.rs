use core::panic;
use std::{ fs, path::PathBuf, vec };

use crate::{
    ast::{ FunctionParam, FunctionSignature, ImplMethod, Node, StructField, TypeRef, AST },
    error,
    lexer::{ Kind, Lexer, Token },
    log,
    parse_utils::{ GetFirstOrElse, IsKind },
    signal::clean_signals,
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
/// Like EXPR_END, but allows finishing an expression at the end of a block.
const EXPR_END_OR_R_BRACKET: &[Kind] = &[Kind::SEMICOLON, Kind::NEW_LINE, Kind::R_BRACKET];
const RANGE_END: &[Kind] = &[Kind::SEMICOLON, Kind::NEW_LINE, Kind::R_ARROW];
const RANGE_END_OR_R_BRACKET: &[Kind] = &[Kind::SEMICOLON, Kind::NEW_LINE, Kind::R_BRACKET, Kind::R_ARROW];
const RANGE_END_OR_L_BRACKET: &[Kind] = &[Kind::L_BRACKET, Kind::R_ARROW];
const RANGE_END_OR_R_PARENT: &[Kind] = &[Kind::R_PARENT, Kind::R_ARROW];
const RANGE_END_OR_R_SQUARE_BRACKET: &[Kind] = &[Kind::R_SQUARE_BRACKET, Kind::R_ARROW];
const FIELD_VALUE_END: &[Kind] = &[Kind::COMMA, Kind::R_BRACKET, Kind::NEW_LINE, Kind::SEMICOLON];

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
    global_stop: Option<Vec<Kind>>,

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

    // When true, prefer EXPR_END even if a block stop is active.
    // This prevents expressions (like RHS) from consuming the entire block.
    prefer_expr_end: bool,
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
            prefer_expr_end: false,
        }
    }

    pub fn from_path(source_file: PathBuf) -> Self {
        let source: String = fs
            ::read_to_string(&source_file)
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

        let prev_prefer_expr_end: bool = self.prefer_expr_end;
        if stop_at.is_none() {
            self.prefer_expr_end = true;
        }

        if let Some(stop_at) = stop_at {
            self.stops.push(stop_at);
        }

        // Used to track a difference between the number of stops at the start and the end of the function
        // needed to remove the last stopped_at when no `stop_at` was provided,
        // because last stop will be added by default when parser ends at EXPR_END
        let stops_at_count: usize = self.stopped_at.len();

        self.ast.new_scope();
        self.parse_node();

        self.prefer_expr_end = prev_prefer_expr_end;

        if stop_at == None && stops_at_count != self.stopped_at.len() {
            self.stopped_at.pop();
        }
        log!("- END PARSE UNTIL", "Stopped at: {:?}", self.stopped_at);

        let scope: Vec<Node> = self.ast
            .pop_scope()
            .unwrap_or_else(|| {
                error!(&self.lexer, "No scope found. Probably scope was popped by another handler.")
            });

        Self::normalize_scope(scope)
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

        log!("PAREN", "{:?}, stopped: {:?}", scope, self.stopped_at);

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
                None => None,
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
        let lhs: Node = self.ast
            .pop_node()
            .unwrap_or_else(|| { error!(&self.lexer, "Expected an Node before binary operator.") });

        log!("PARSE BINOP", "LHS: {:?} {:?}", lhs, op);

        // Parse RHS
        let rhs: Node = self
            .parse_until(current_stop)
            .get_first_or_else(|| {
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
                }

                NEW_LINE | SEMICOLON => {
                    self.next_token();

                    // If last node was a newline, don't add another one
                    if let Some(Node::Newline) = self.ast.current_scope().last() {
                        continue;
                    }
                    self.ast.add_node(Node::Newline);
                    continue;
                }

                _ => {
                    self.parse_node();
                }
            }
        }

        if let Some(scope) = self.ast.scopes.last_mut() {
            let current_scope = std::mem::take(scope);
            *scope = Self::normalize_scope(current_scope);
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
                Some(stop)
                    if self.prefer_expr_end && stop.len() == 1 && stop[0] == Kind::R_BRACKET =>
                {
                    EXPR_END_OR_R_BRACKET
                }
                Some(stop) => *stop,
                None => EXPR_END,
            };

            log!("TOKEN", "{:?}  Stops: {:?}", next_token, self.stops);

            // * Stop at
            if current_stop.contains(&next_token.kind) {
                log!("STOP", "{:?}", next_token);

                if self.capturing_sequence {
                    self.capturing_sequence = false;

                    let seq: Vec<Node> = self.ast
                        .pop_scope()
                        .unwrap_or_else(|| {
                            error!(
                                &self.lexer,
                                "No scope found. Probably scope was popped by another handler."
                            )
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
                    let ident_scope: Node = self
                        .parse_until(Some(&[L_PARENT]))
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an identifier for function name.")
                        });

                    if self.next_token().is_not(Kind::L_PARENT) {
                        error!(&self.lexer, "Expected a parenthesis after function name.");
                    }

                    log!("FN_DEF", "Ident Scope: {:?}", ident_scope);

                    self.clean_stop(); // Remove L_PARENT stop

                    // Ensure token in scope was an identifier
                    let name: String = match ident_scope {
                        Node::Identifier(name) => name,
                        other =>
                            error!(
                                &self.lexer,
                                format!(
                                    "Expected an identifier for function name. Got: {:?}",
                                    other
                                )
                            ),
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
                }

                STRUCT => {
                    let node = self.parse_struct_def();
                    self.ast.add_node(node);
                    continue;
                }
                TRAIT => {
                    let node = self.parse_trait_def();
                    self.ast.add_node(node);
                    continue;
                }
                IMPL => {
                    let node = self.parse_impl_block();
                    self.ast.add_node(node);
                    continue;
                }

                RETURN => {
                    let expr: Node = self
                        .parse_until(Some(current_stop))
                        .get_first_or_else(|| {
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
                }
                BREAK => {
                    self.ast.add_node(Node::Break);
                    continue;
                }

                // * For Loop
                // for {item} in {iter} {block}
                FOR => {
                    let item: Node = self
                        .parse_until(Some(&[IN]))
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an identifier after \"for\".")
                        });

                    if self.next_token().is_not(Kind::IN) {
                        error!(&self.lexer, "Expected \"in\" after item.");
                    }

                    self.clean_stop(); // Remove IN stop

                    let iterable: Node = self
                        .parse_until(Some(&[L_BRACKET]))
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an expression after \"in\".")
                        });

                    // Ensure there's a block after the expression
                    if self.next_token().is_not(Kind::L_BRACKET) {
                        error!(&self.lexer, "Expected a block after expression.");
                    }

                    self.clean_stop(); // Remove L_BRACKET stop

                    log!("FOR", "Item: {:?} Iterable: {:?}", item, iterable);

                    let block: Node = self.parse_block(false);

                    self.ast.add_node(Node::ForLoop {
                        item: bi!(item),
                        iterable: bi!(iterable),
                        body: bi!(block),
                    });
                    continue;
                }

                // * While loop UWU classic while loop. Who doesn't like a cute while loop?
                // while {expr} {block}
                WHILE => {
                    let condition: Node = self
                        .parse_until(Some(&[L_BRACKET]))
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an expression after \"while\".")
                        });

                    // Ensure there's a block after the expression
                    if self.next_token().is_not(Kind::L_BRACKET) {
                        error!(&self.lexer, "Expected a block after expression.");
                    }

                    self.clean_stop(); // Remove L_BRACKET stop

                    let block: Node = self.parse_block(false);

                    self.ast.add_node(Node::WhileLoop {
                        condition: bi!(condition),
                        body: bi!(block),
                    });
                    continue;
                }

                PYTHON => {
                    self.ast.add_node(Node::Python(token.value.clone()));
                }

                // * If / Else / Elif
                // if {expr} {block}
                // [elif {expr} {block}]*
                // [else {block}]?

                IF => {
                    log!("IF");
                    let condition: Node = self
                        .parse_until(Some(&[L_BRACKET]))
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an expression after \"if\".")
                        });

                    log!("IF", "Condition: {:?}", condition);

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

                        let condition: Node = self
                            .parse_until(Some(&[L_BRACKET]))
                            .get_first_or_else(|| {
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

                    log!("IF", "{:?} {:?} {:?}", condition, block, elifs);

                    self.ast.add_node(Node::Conditional {
                        condition: bi!(condition),
                        body: bi!(block),
                        elifs,
                        else_body,
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

                    let lhs: Node = self.ast.solve_pop(self.capturing_sequence).unwrap_or_else(|| {
                        error!(
                            &self.lexer,
                            "No LHS found. Probably node was popped by another handler."
                        );
                    });

                    log!("ASSIGN", "LHS: {:?} {:?}", lhs, op);

                    // If LHS is a signal, then we're capturing signals/dependencies
                    if let Node::Signal(_) = &lhs {
                        self.capturing_signals.push(HashSet::new());
                    }

                    let rhs: Node = self
                        .parse_until(None)
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an value after assignment operator.")
                        });

                    // * If parsing parethensis it's a named arg
                    if self.parsing_paren && !is_walrus {
                        log!("NAMED ARG", "{:?} {:?} {:?}", lhs, op, rhs);

                        match lhs {
                            Node::Identifier(name) => {
                                self.ast.add_node(Node::NamedArg(name, bi!(rhs)));
                            }
                            other =>
                                error!(
                                    &self.lexer,
                                    format!(
                                        "Expected an Identifier for Named Argument. Got: {:?}",
                                        other
                                    )
                                ),
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
                            let dependencies: HashSet<String> = clean_signals(
                                &self.ast,
                                self.capturing_signals
                                    .pop()
                                    .unwrap_or_else(|| {
                                        error!(
                                            &self.lexer,
                                            "Couldn't get dependencies for Signal Definition/Update. -- Prob double pop somewhere"
                                        )
                                    })
                            );

                            // If signal is defined, it's an update
                            if self.defined_signals.contains(&name) {
                                self.ast.add_node(Node::SignalUpdate {
                                    name,
                                    value: bi!(rhs),
                                    dependencies,
                                });
                            } else {
                                self.defined_signals.insert(name.clone());
                                self.ast.add_node(Node::SignalDef {
                                    name,
                                    value: bi!(rhs),
                                    dependencies,
                                });
                            }
                        }

                        // * Destructuring/Deconstruction
                        (Node::Dict { keys, values }, rhs) => {
                            self.ast.add_node(Node::Deconstruction {
                                identifiers: keys,
                                value: bi!(rhs),
                                default_values: values,
                            });
                        }

                        // * Assingments / Multiple assignments / Unpacking
                        (lhs, rhs) => {
                            self.ast.add_node(Node::Assign {
                                identifiers: vec![lhs],
                                values: vec![rhs],
                                op,
                            });
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
                }
                R_PARENT => error!(&self.lexer, "Unexpected closing parenthesis."),

                // * Blocks / Dictionaries
                L_BRACKET => {
                    log!("Parsing BLOCK");

                    if let Some(init_name) = self.pop_struct_init_name() {
                        let init = self.parse_struct_init(init_name);
                        self.ast.add_node(init);
                        continue;
                    }

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
                }
                R_BRACKET => error!(&self.lexer, "Unexpected closing bracket."),

                // * Indexing / Arrays
                L_SQUARE_BRACKET => {
                    // The expr inside the square brackets can be anything, so we parse until a closing square bracket
                    let mut expr = self
                        .parse_until(Some(&[R_SQUARE_BRACKET]))
                        .get_first_or_else(|| {
                            error!(
                                &self.lexer,
                                "Expected a closing square bracket at some point after opening one..."
                            )
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
                                expr = Node::Range {
                                    start: start.clone(),
                                    end: bi!(Node::Len(bi!(object.clone()))),
                                    inclusive: *inclusive,
                                };
                            }
                        }

                        self.ast.add_node(Node::Index { object: bi!(object), index: bi!(expr) });
                        continue;
                    }

                    // Array
                    log!("ARRAY", "Expr: {:?}", expr);
                    self.ast.add_node(Node::Array(bi!(expr)));
                    continue;
                }
                R_SQUARE_BRACKET => error!(&self.lexer, "Unexpected closing square bracket."),

                IDENTIFIER => {
                    self.ast.add_node(Node::Identifier(token.value.clone()));
                }
                NUMBER => {
                    self.ast.add_node(Node::Number(token.value.clone()));
                }
                STRING => {
                    // String formatting is done at transpiler level.
                    self.ast.add_node(Node::Str(token.value.clone()));
                }

                TRUE => {
                    self.ast.add_node(Node::Bool(true));
                }
                FALSE => {
                    self.ast.add_node(Node::Bool(false));
                }

                // Don't even know what to do with these
                SINGLE_QUOTE =>
                    error!(&self.lexer, "Unexpected quote token. Did you mean a string literal?"),
                DOUBLE_QUOTE =>
                    error!(&self.lexer, "Unexpected quote token. Did you mean a string literal?"),
                BACK_TICK =>
                    error!(&self.lexer, "Unexpected backtick. Raw strings are not supported."),

                // * Unary Operators / Bitwise
                // {op}{Node}
                | NOT // "!" not
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

                    let expr = self
                        .parse_until(None)
                        .get_first_or_else(|| {
                            error!(&self.lexer, "Expected an expr after unary operator.")
                        });

                    self.ast.add_node(Node::UnaryOp { op: token.value.clone(), expr: bi!(expr) });
                    continue;
                }

                // * Binary Operators
                // Compare, Arithmetic, Logical
                AND | OR | EQ | NE | LT | LE | GT | GE | ADD | SUBTRACT | DIVIDE | POW | MOD => {
                    self.parse_binop(None, token.value.clone());
                    continue;
                }

                // * Decorators
                AT => error!(&self.lexer, "Decorators are not supported yet."),

                COLON => error!(&self.lexer, "Unexpected ':'."),

                COMMA => {
                    if self.capturing_sequence {
                        continue;
                    }

                    self.capturing_sequence = true;
                    log!(" /  Capturing Sequence");

                    // The node before "," is the start of the sequence
                    let seq_start: Node = self.ast
                        .pop_node()
                        .unwrap_or_else(|| {
                            error!(&self.lexer, "This \",\" is infront of nothing.")
                        });

                    self.ast.new_scope();
                    self.ast.add_node(seq_start);
                    continue;
                }

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
                    let inclusive: bool = if
                        let Some(Kind::ASSIGN) = self.peek_token().map(|t| t.kind)
                    {
                        self.next_token(); // Consume "="
                        true
                    } else {
                        false
                    };

                    let rhs: Node = self.parse_until(Some(Self::range_stop(current_stop))).get_first_or_else(|| {
                        // If there's no RHS, end will be the length of the thing being indexed
                        // This of course will only work if we indexing, otherwise it will be an error.
                        Node::Empty
                    });

                    // * Shouldn't be reacheable, since parse_until will only stop where it is supposed to
                    // * Or throw an error, this will always be executed becuase if parse_until it's successfull
                    // * stopped_at will be empty, meaning parse_until found it's stop and also popped it
                    // if
                    //     self.stopped_at.is_empty() ||
                    //     !current_stop.contains(&self.stopped_at.pop().unwrap())
                    // {
                    //     // Debug stop at and current stop at
                    //     dbg!("RANGE", "Stopped at: {:?}, Current Stop: {:?}", &self.stopped_at, &current_stop);

                    //     error!(&self.lexer, "Expected a closing token for range.");
                    // }

                    self.clean_stop(); // Remove stop token

                    self.ast.add_node(Node::Range { start: bi!(lhs), end: bi!(rhs), inclusive });
                    continue;
                }

                // * Member Acess / Dot Operator / Method Call
                DOT => {
                    // Get object; value being accessed, should be the last node in the scope
                    let object: Node = self.ast
                        .pop_node()
                        .unwrap_or_else(|| {
                            error!(
                                &self.lexer,
                                "Expected an object before \".\".\nEx. object.property \n -If you were trying to access a member of an object."
                            )
                        });

                    let prop = match self.next_token() {
                        Some(Token { kind: Kind::IDENTIFIER, value, .. }) => Node::Identifier(value),
                        Some(other) =>
                            error!(
                                &self.lexer,
                                format!("Expected an identifier after \".\". Got: {:?}", other)
                            ),
                        None => error!(&self.lexer, "Expected a property after \".\"."),
                    };

                    // Otherwise, it's an member access
                    self.ast.add_node(Node::MemberAccess {
                        object: bi!(object),
                        member: bi!(prop),
                    });
                    continue;
                }

                HASH => error!(
                    &self.lexer,
                    "Unexpected '#'. Use #[python] ... #[endpython] for raw Python blocks."
                ),

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
                                error!(
                                    &self.lexer,
                                    "Expected an Identifier or Block after \"$\" but got nothing."
                                )
                            })
                        }
                    };

                    let deps: HashSet<String> = self.capturing_signals
                        .pop()
                        .unwrap_or_else(|| {
                            error!(
                                &self.lexer,
                                "Couldn't get dependencies for Reactive Statement. -- Prob double pop somewhere"
                            )
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
                        }

                        // Reactive Statement
                        Node::Block(block) => {
                            // Fetch reactive block dependencies
                            let dependencies: HashSet<String> = clean_signals(&self.ast, deps);
                            log!("+", "Reactive Block - Block: {:?}", block);
                            self.ast.add_node(Node::ReactiveStmt { block, dependencies });
                        }

                        other =>
                            error!(
                                &self.lexer,
                                format!(
                                    "Expected a name (signal identifier) or block (reactive statement) after \"$\". Got: {:?}",
                                    other
                                )
                            ),
                    }
                    continue;
                }

                PIPE_RIGHT => error!(&self.lexer, "Pipe operators are not supported yet."),
                PIPE_LEFT => error!(&self.lexer, "Pipe operators are not supported yet."),
                L_ARROW => error!(&self.lexer, "Left arrow '<-' is not supported yet."),

                // * Foward to / Distribution / Pipe
                // Sintactic Sugar that uses the value on the left and passes it as an argument to the function on the right
                // {expr} -> {expr}
                // Ex. names, ages -> print
                // This forwards names and ages to the print function
                // This operation can be chained
                // {expr} -> {expr} -> {expr}
                // Ex. names, ages -> zip -> print
                R_ARROW => {
                    // If already parsing a distribution, bail out to avoid conflicts
                    if self.parsing_distribution {
                        self.clean_stop();
                        self.put_back(token);
                        return;
                    }

                    // We'll parse one or more chained distributions here
                    self.parsing_distribution = true;

                    // 1) Pop the LHS out of the AST
                    let lhs_node = self.ast
                        .pop_until_non_space()
                        .unwrap_or_else(|| { error!(&self.lexer, "Expected a Node before '->'.") });
                    let lhs_nodes = match lhs_node {
                        Node::Assign { values, .. } => {
                            // Instead of wrapping the entire assignment, use its value(s)
                            values
                        }
                        Node::WrappedSequence(seq) | Node::Sequence(seq) => seq,
                        node => vec![node],
                    };

                    // 2) Parse the first RHS
                    self.skip_newlines();
                    let rhs_nodes: Vec<Node> = match
                        self
                            .parse_until(None)
                            .get_first_or_else(||
                                error!(&self.lexer, "Expected a Node after '->'.")
                            )
                    {
                        Node::WrappedSequence(seq) | Node::Sequence(seq) => seq,
                        node => vec![node],
                    };

                    // Build a distribution with the initial LHS, RHS
                    let mut distribution = Node::Distribution {
                        args: lhs_nodes,
                        recipients: rhs_nodes,
                    };

                    // 3) While the very next token is another '->', parse additional chained distributions
                    loop {
                        // Peek next token without consuming
                        let maybe_token = self.peek_token();
                        if maybe_token.is_not(Kind::R_ARROW) {
                            break;
                        }

                        // Actually consume the token now that we know it's R_ARROW
                        self.next_token();

                        // parse the next RHS (the new recipients)
                        self.skip_newlines();
                        let chained_rhs = match
                            self
                                .parse_until(None)
                                .get_first_or_else(||
                                    error!(&self.lexer, "Expected a Node after '->'.")
                                )
                        {
                            Node::WrappedSequence(seq) | Node::Sequence(seq) => seq,
                            node => vec![node],
                        };

                        // Our new LHS is the distribution we built in the previous iteration
                        distribution = Node::Distribution {
                            args: vec![distribution],
                            recipients: chained_rhs,
                        };
                    }

                    // Turn off parsing_distribution
                    self.parsing_distribution = false;

                    // 4) Finally, push the resulting (possibly chained) distribution back onto the AST
                    log!("+ Distribution (chained)", "{:?}", distribution);
                    self.ast.add_node(distribution);

                    // Continue on
                    continue;
                }

                // * Anonymous Function / Lambda
                // ({args}*,) => {block}
                // ({args}*,) => {expr}
                FAT_ARROW => {
                    // Argument should be last node in the scope
                    // Node should be a WrappedSequence
                    let args: Node = self.ast
                        .pop_node()
                        .unwrap_or_else(|| {
                            error!(&self.lexer, "Expected arguments before \"=>\".")
                        });

                    // Ensure there's a block or an expression after the arguments
                    let body: Node = self
                        .parse_until(None)
                        .get_first_or_else(|| {
                            error!(
                                &self.lexer,
                                "Expected a block or an expression after arguments. Ex. \"=> return a + b;\""
                            )
                        });

                    self.ast.add_node(Node::Lambda { args: bi!(args), body: bi!(body) });
                    continue;
                }
            }
        }
    }

    fn range_stop(current_stop: &[Kind]) -> &'static [Kind] {
        if current_stop == EXPR_END {
            RANGE_END
        } else if current_stop == EXPR_END_OR_R_BRACKET {
            RANGE_END_OR_R_BRACKET
        } else if current_stop == [Kind::L_BRACKET] {
            RANGE_END_OR_L_BRACKET
        } else if current_stop == [Kind::R_PARENT] {
            RANGE_END_OR_R_PARENT
        } else if current_stop == [Kind::R_SQUARE_BRACKET] {
            RANGE_END_OR_R_SQUARE_BRACKET
        } else {
            RANGE_END
        }
    }

    fn normalize_scope(scope: Vec<Node>) -> Vec<Node> {
        scope.into_iter().map(Self::normalize_node).collect()
    }

    fn normalize_node(node: Node) -> Node {
        match node {
            Node::MemberAccess { object, member } => Node::MemberAccess {
                object: bi!(Self::normalize_node(*object)),
                member: bi!(Self::normalize_node(*member)),
            },
            Node::Group(expr) => Node::Group(expr.map(|expr| bi!(Self::normalize_node(*expr)))),
            Node::BinOp { lhs, op, rhs } => Self::normalize_binop(*lhs, op, *rhs),
            Node::UnaryOp { op, expr } => Node::UnaryOp {
                op,
                expr: bi!(Self::normalize_node(*expr)),
            },
            Node::NamedArg(name, value) => Node::NamedArg(name, bi!(Self::normalize_node(*value))),
            Node::Array(values) => Node::Array(bi!(Self::normalize_node(*values))),
            Node::Index { object, index } => Node::Index {
                object: bi!(Self::normalize_node(*object)),
                index: bi!(Self::normalize_node(*index)),
            },
            Node::Sequence(values) => Node::Sequence(Self::normalize_scope(values)),
            Node::WrappedSequence(values) => Node::WrappedSequence(Self::normalize_scope(values)),
            Node::Block(nodes) => Node::Block(Self::normalize_scope(nodes)),
            Node::FnBody(nodes) => Node::FnBody(Self::normalize_scope(nodes)),
            Node::FunctionCall { object, args } => Node::FunctionCall {
                object: bi!(Self::normalize_node(*object)),
                args: bi!(Self::normalize_node(*args)),
            },
            Node::Return(value) => Node::Return(bi!(Self::normalize_node(*value))),
            Node::Dict { keys, values } => Node::Dict {
                keys: Self::normalize_scope(keys),
                values: Self::normalize_scope(values),
            },
            Node::Alias(value) => Node::Alias(bi!(Self::normalize_node(*value))),
            Node::Range { start, end, inclusive } => Node::Range {
                start: bi!(Self::normalize_node(*start)),
                end: bi!(Self::normalize_node(*end)),
                inclusive,
            },
            Node::Len(value) => Node::Len(bi!(Self::normalize_node(*value))),
            Node::Distribution { args, recipients } => Node::Distribution {
                args: Self::normalize_scope(args),
                recipients: Self::normalize_scope(recipients),
            },
            Node::IterDistribution { args, recipients } => Node::IterDistribution {
                args: Self::normalize_scope(args),
                recipients: Self::normalize_scope(recipients),
            },
            Node::Decorator { name, args } => Node::Decorator {
                name: bi!(Self::normalize_node(*name)),
                args: args.map(|args| bi!(Self::normalize_node(*args))),
            },
            Node::Lambda { args, body } => Node::Lambda {
                args: bi!(Self::normalize_node(*args)),
                body: bi!(Self::normalize_node(*body)),
            },
            Node::Conditional { condition, body, elifs, else_body } => Node::Conditional {
                condition: bi!(Self::normalize_node(*condition)),
                body: bi!(Self::normalize_node(*body)),
                elifs: elifs
                    .into_iter()
                    .map(|(condition, body)| {
                        (Self::normalize_node(condition), Self::normalize_node(body))
                    })
                    .collect(),
                else_body: else_body.map(|body| bi!(Self::normalize_node(*body))),
            },
            Node::Assign { identifiers, values, op } => Node::Assign {
                identifiers: Self::normalize_scope(identifiers),
                values: Self::normalize_scope(values),
                op,
            },
            Node::SignalDef { name, value, dependencies } => Node::SignalDef {
                name,
                value: bi!(Self::normalize_node(*value)),
                dependencies,
            },
            Node::SignalUpdate { name, value, dependencies } => Node::SignalUpdate {
                name,
                value: bi!(Self::normalize_node(*value)),
                dependencies,
            },
            Node::Deconstruction { identifiers, value, default_values } => Node::Deconstruction {
                identifiers: Self::normalize_scope(identifiers),
                value: bi!(Self::normalize_node(*value)),
                default_values: Self::normalize_scope(default_values),
            },
            Node::FunctionDef { name, args, body } => Node::FunctionDef {
                name,
                args: bi!(Self::normalize_node(*args)),
                body: bi!(Self::normalize_node(*body)),
            },
            Node::ImplBlock { generics, trait_ref, target, methods } => Node::ImplBlock {
                generics,
                trait_ref,
                target,
                methods: methods
                    .into_iter()
                    .map(|method| ImplMethod {
                        signature: method.signature,
                        body: bi!(Self::normalize_node(*method.body)),
                    })
                    .collect(),
            },
            Node::StructInit { name, fields } => Node::StructInit {
                name,
                fields: fields
                    .into_iter()
                    .map(|(name, value)| (name, Self::normalize_node(value)))
                    .collect(),
            },
            Node::ReactiveStmt { block, dependencies } => Node::ReactiveStmt {
                block: Self::normalize_scope(block),
                dependencies,
            },
            Node::Loop(body) => Node::Loop(bi!(Self::normalize_node(*body))),
            Node::ForLoop { item, iterable, body } => Node::ForLoop {
                item: bi!(Self::normalize_node(*item)),
                iterable: bi!(Self::normalize_node(*iterable)),
                body: bi!(Self::normalize_node(*body)),
            },
            Node::WhileLoop { condition, body } => Node::WhileLoop {
                condition: bi!(Self::normalize_node(*condition)),
                body: bi!(Self::normalize_node(*body)),
            },
            other => other,
        }
    }

    fn normalize_binop(lhs: Node, op: String, rhs: Node) -> Node {
        let lhs = Self::normalize_node(lhs);
        let rhs = Self::normalize_node(rhs);

        if let Node::BinOp { lhs: rhs_lhs, op: rhs_op, rhs: rhs_rhs } = rhs {
            if Self::should_rotate_binop(&op, &rhs_op) {
                let rotated_lhs = Node::BinOp {
                    lhs: bi!(lhs),
                    op,
                    rhs: rhs_lhs,
                };

                return Self::normalize_binop(rotated_lhs, rhs_op, *rhs_rhs);
            }

            return Node::BinOp {
                lhs: bi!(lhs),
                op,
                rhs: bi!(Node::BinOp { lhs: rhs_lhs, op: rhs_op, rhs: rhs_rhs }),
            };
        }

        Node::BinOp { lhs: bi!(lhs), op, rhs: bi!(rhs) }
    }

    fn should_rotate_binop(left_op: &str, right_op: &str) -> bool {
        if left_op == "**" {
            return false;
        }

        Self::precedence(left_op) >= Self::precedence(right_op)
    }

    fn precedence(op: &str) -> usize {
        match op {
            "||" => 1,
            "&&" => 2,
            "==" | "!=" | "<" | "<=" | ">" | ">=" => 3,
            "|" => 4,
            "^" => 5,
            "&" => 6,
            "+" | "-" => 7,
            "*" | "/" | "%" => 8,
            "**" => 9,
            _ => 0,
        }
    }

    fn parse_struct_def(&mut self) -> Node {
        let name = self.parse_identifier_name("Expected a struct name after \"struct\".");
        let generics = self.parse_generic_names();

        self.expect_token(Kind::L_BRACKET, "Expected a struct body after struct name.");

        let mut fields = Vec::new();
        loop {
            self.skip_newlines();

            if self.peek_token().is(Kind::R_BRACKET) {
                self.next_token();
                break;
            }

            let field_name = self.parse_identifier_name("Expected a field name in struct body.");
            self.expect_token(Kind::COLON, "Expected ':' after struct field name.");
            let type_ref = self.parse_type_ref();
            fields.push(StructField { name: field_name, type_ref });

            self.consume_decl_separator();
        }

        Node::StructDef { name, generics, fields }
    }

    fn parse_trait_def(&mut self) -> Node {
        let name = self.parse_identifier_name("Expected a trait name after \"trait\".");
        let generics = self.parse_generic_names();

        self.expect_token(Kind::L_BRACKET, "Expected a trait body after trait name.");

        let mut methods = Vec::new();
        loop {
            self.skip_newlines();

            if self.peek_token().is(Kind::R_BRACKET) {
                self.next_token();
                break;
            }

            self.expect_token(Kind::FN_DEF, "Expected a trait method signature.");
            methods.push(self.parse_function_signature());
            self.consume_decl_separator();
        }

        Node::TraitDef { name, generics, methods }
    }

    fn parse_impl_block(&mut self) -> Node {
        let generics = self.parse_generic_names();
        let first_type = self.parse_type_ref();

        let (trait_ref, target) = if self.peek_token().is(Kind::FOR) {
            self.next_token();
            (Some(first_type), self.parse_type_ref())
        } else {
            (None, first_type)
        };

        self.expect_token(Kind::L_BRACKET, "Expected an impl body.");

        let mut methods = Vec::new();
        loop {
            self.skip_newlines();

            if self.peek_token().is(Kind::R_BRACKET) {
                self.next_token();
                break;
            }

            self.expect_token(Kind::FN_DEF, "Expected a method definition in impl body.");
            let signature = self.parse_function_signature();
            self.expect_token(Kind::L_BRACKET, "Expected a method body after method signature.");
            let body = self.parse_block(true);
            methods.push(ImplMethod { signature, body: bi!(body) });

            self.consume_decl_separator();
        }

        Node::ImplBlock { generics, trait_ref, target, methods }
    }

    fn parse_struct_init(&mut self, name: TypeRef) -> Node {
        let mut fields = Vec::new();

        loop {
            self.skip_newlines();

            if self.peek_token().is(Kind::R_BRACKET) {
                self.next_token();
                break;
            }

            let field_name = self.parse_identifier_name("Expected a field name in struct literal.");
            self.expect_token(Kind::COLON, "Expected ':' after struct literal field name.");

            let value = self
                .parse_until(Some(FIELD_VALUE_END))
                .get_first_or_else(|| {
                    error!(&self.lexer, "Expected a value after struct literal field ':'.")
                });
            self.clean_stop();

            fields.push((field_name, value));

            match self.peek_token() {
                Some(Token { kind: Kind::COMMA | Kind::SEMICOLON | Kind::NEW_LINE, .. }) => {
                    self.next_token();
                }
                Some(Token { kind: Kind::R_BRACKET, .. }) => {
                    self.next_token();
                    break;
                }
                Some(other) =>
                    error!(
                        &self.lexer,
                        format!("Expected ',' or '}}' after struct literal field. Got: {:?}", other)
                    ),
                None => error!(&self.lexer, "Expected closing '}' for struct literal."),
            }
        }

        Node::StructInit { name, fields }
    }

    fn parse_function_signature(&mut self) -> FunctionSignature {
        let name = self.parse_identifier_name("Expected a function or method name.");
        let params = self.parse_function_params();
        let return_type = if self.peek_token().is(Kind::R_ARROW) {
            self.next_token();
            Some(self.parse_type_ref())
        } else {
            None
        };

        FunctionSignature { name, params, return_type }
    }

    fn parse_function_params(&mut self) -> Vec<FunctionParam> {
        self.expect_token(Kind::L_PARENT, "Expected '(' after function or method name.");

        let mut params = Vec::new();
        loop {
            self.skip_newlines();

            if self.peek_token().is(Kind::R_PARENT) {
                self.next_token();
                break;
            }

            let name = self.parse_identifier_name("Expected a parameter name.");
            let type_ref = if self.peek_token().is(Kind::COLON) {
                self.next_token();
                Some(self.parse_type_ref())
            } else {
                None
            };

            params.push(FunctionParam { name, type_ref });

            match self.peek_token() {
                Some(Token { kind: Kind::COMMA, .. }) => {
                    self.next_token();
                }
                Some(Token { kind: Kind::R_PARENT, .. }) => {
                    self.next_token();
                    break;
                }
                Some(other) =>
                    error!(
                        &self.lexer,
                        format!("Expected ',' or ')' after parameter. Got: {:?}", other)
                    ),
                None => error!(&self.lexer, "Expected ')' after parameters."),
            }
        }

        params
    }

    fn parse_type_ref(&mut self) -> TypeRef {
        let name = self.parse_identifier_name("Expected a type name.");
        let generics = if self.peek_token().is(Kind::LT) {
            self.next_token();
            let mut generics = Vec::new();

            loop {
                generics.push(self.parse_type_ref());

                match self.peek_token() {
                    Some(Token { kind: Kind::COMMA, .. }) => {
                        self.next_token();
                    }
                    Some(Token { kind: Kind::GT, .. }) => {
                        self.next_token();
                        break;
                    }
                    Some(other) =>
                        error!(
                            &self.lexer,
                            format!("Expected ',' or '>' in generic type arguments. Got: {:?}", other)
                        ),
                    None => error!(&self.lexer, "Expected '>' after generic type arguments."),
                }
            }

            generics
        } else {
            Vec::new()
        };

        TypeRef::new(name, generics)
    }

    fn parse_generic_names(&mut self) -> Vec<String> {
        if self.peek_token().is_not(Kind::LT) {
            return Vec::new();
        }

        self.next_token();
        let mut generics = Vec::new();

        loop {
            generics.push(self.parse_identifier_name("Expected a generic parameter name."));

            match self.peek_token() {
                Some(Token { kind: Kind::COMMA, .. }) => {
                    self.next_token();
                }
                Some(Token { kind: Kind::GT, .. }) => {
                    self.next_token();
                    break;
                }
                Some(other) =>
                    error!(
                        &self.lexer,
                        format!("Expected ',' or '>' in generic parameter list. Got: {:?}", other)
                    ),
                None => error!(&self.lexer, "Expected '>' after generic parameter list."),
            }
        }

        generics
    }

    fn parse_identifier_name(&mut self, message: &str) -> String {
        match self.next_token() {
            Some(Token { kind: Kind::IDENTIFIER, value, .. }) => value,
            Some(other) => error!(&self.lexer, format!("{} Got: {:?}", message, other)),
            None => error!(&self.lexer, message),
        }
    }

    fn expect_token(&mut self, kind: Kind, message: &str) {
        if self.next_token().is_not(kind) {
            error!(&self.lexer, message);
        }
    }

    fn consume_decl_separator(&mut self) {
        loop {
            match self.peek_token() {
                Some(Token { kind: Kind::COMMA | Kind::SEMICOLON | Kind::NEW_LINE, .. }) => {
                    self.next_token();
                }
                _ => break,
            }
        }
    }

    fn pop_struct_init_name(&mut self) -> Option<TypeRef> {
        let node = self.ast.current_scope().last()?;

        match node {
            Node::Identifier(_) => {
                let node = self.ast.pop_node()?;
                Self::type_ref_from_node(node)
            }
            _ => None,
        }
    }

    fn type_ref_from_node(node: Node) -> Option<TypeRef> {
        match node {
            Node::Identifier(name) => Some(TypeRef::new(name, Vec::new())),
            _ => None,
        }
    }
}
