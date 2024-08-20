use std::{cell::RefCell, fs, path::PathBuf, rc::Rc};

use crate::{
    ast::{Expr, Node, Stmt, AST},
    error,
    lexer::{Lexer, Token, TokenKind},
    log,
};

/// Box-It, bit!
/// Used to box an expression
macro_rules! bit {
    // Box it
    ($x:expr) => {
        Box::new($x)
    };
}

/// Creates an array of TokenKind. Thus tk (TokenKind) arr (array).
/// `tokarr![NEW_LINE, SEMICOLON]` -> `Some(&[TokenKind::NEW_LINE, TokenKind::SEMICOLON])`
/// This macro is supposed to be an easier way of passing `stop_at` argument to handlers.
macro_rules! tkarr {
    ($($x:ident),*) => {
        Some(&[$(TokenKind::$x),*])
    };
}

pub struct Parser {
    // Token that was read but not processed
    remanider_token: Option<Token>,

    // lexer
    pub lexer: Lexer,

    // ast
    pub ast: AST,

    // Travel variables -- used to keep track of control flow
    stopped_at: Option<TokenKind>, // token in which a parser stopped
    capturing_sequence: bool, // if the parser is capturing a sequence, parse_expr_from_buffer will return a sequence once parse_expr finds a expr end token

    // if the parser is capturing signals, this will be a list of signals.
    // Used to keep track of "dependencies" of signals, so we can later on generate the correct code.
    capturing_signals: Option<Vec<String>>,

    // Used to differentiate between signal definition and singal update
    // If a signal is defined, any type of assignment will be considered as a signal update
    defined_signals: Vec<String>,
}

impl Parser {
    pub fn new(source: String) -> Self {
        let lexer = Lexer::new(source);

        Parser {
            remanider_token: None,
            lexer,
            ast: AST::new(),

            stopped_at: None,
            capturing_sequence: false,
            capturing_signals: None,
            defined_signals: Vec::new(),
        }
    }

    pub fn from_path(source_file: PathBuf) -> Self {
        let source: String = fs::read_to_string(&source_file)
            .expect(&format!("Couldn't read file: {:?}", source_file));

        Parser::new(source)
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if let Some(token) = self.remanider_token.take() {
            // println!("[next_token/remainder] {:?}", token);
            return Some(token);
        }

        let token = self.lexer.next();
        // println!("[next_token] {:?}", token);
        token
    }

    /// Puts back a token that was read but not processed
    pub fn put_back(&mut self, token: Token) {
        // println!("[put_back] {:?}", token);
        self.remanider_token = Some(token);
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

    pub fn parse(&mut self) -> &AST {
        // * Parse start
        while let Some(token) = self.peek_token() {
            log!("parse", "{:?}", token);

            // New lines
            if token.kind == TokenKind::NEW_LINE {
                self.ast.add(Node::Expr(Expr::Newline));
                self.next_token();
                continue;
            }

            if Lexer::is_statement_token(&token) {
                let statement: Stmt = self.parse_statement();
                self.ast.add(Node::Stmt(statement));
            } else if Lexer::is_expression_token(&token) {
                let expr: Expr = self.parse_expr(None);

                log!("+ parse", "Adding expression to AST: {:?}", expr);

                self.ast.add(Node::Expr(expr));
            } else {
                log!("! parse", "Unknown token: {:?}", token);
            }

            if self.stopped_at.is_some() && self.stopped_at.unwrap() == TokenKind::NEW_LINE {
                self.ast.add(Node::Expr(Expr::Newline));
            }
        }

        &self.ast
    }

    /// Called when encountering a statement token.
    /// Statement token should be set as REMAINDER_TOKEN before calling this function.
    /// Captures the statement from token to the next semicolon.
    fn parse_statement(&mut self) -> Stmt {
        log!("parse_statement", "");

        while let Some(token) = self.next_token() {
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
                TokenKind::DEF => {
                    let name = self.get_expr(&Expr::Identifier(String::new()), tkarr![L_PARENT]);

                    if self.stopped_at.take().is_none() {
                        error!(
                            &self.lexer,
                            "Expected a parenthesis after function name.",
                            format!("def {:?} (...", name)
                        );
                    }

                    let args = self.parse_paren();
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

                // * Import
                // `import {expr}[as {expr}{; or new_line}`
                TokenKind::IMPORT => {
                    let expr = self.parse_expr(tkarr![SEMICOLON, NEW_LINE, AS]);

                    if let Some(TokenKind::AS) = self.stopped_at {
                        // let alias: Expr = self.get_expr(&Expr::Identifier(String::new()), None);
                        let alias = self.parse_expr(None);

                        return Stmt::Import(expr, Some(alias));
                    }

                    return Stmt::Import(expr, None);
                }

                // * From import
                // `from {expr} import {expr}[as {expr}{; or new_line}`]
                TokenKind::FROM => {
                    let name = self.get_expr(&Expr::Identifier(String::new()), tkarr![IMPORT]);

                    if self.stopped_at.is_none() {
                        error!(
                            &self.lexer,
                            "Expected 'import' after 'from'.".to_string(),
                            format!("from {:?} ...", name)
                        );
                    }

                    let thing = self.get_expr(
                        &Expr::Identifier(String::new()),
                        tkarr![SEMICOLON, NEW_LINE, AS],
                    );

                    if let Some(_) = self.stopped_at {
                        let alias: Expr = self.get_expr(&Expr::Identifier(String::new()), None);
                        return Stmt::From(name, thing, Some(alias));
                    }

                    return Stmt::From(name, thing, None);
                }

                // * Parsing exprs inside statment
                _ if Lexer::is_expression_token(&token) => {
                    self.put_back(token);
                    let expr = self.parse_expr(None);
                    self.ast.add(Node::Expr(expr));

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
    fn parse_expr(&mut self, stop_at: Option<&[TokenKind]>) -> Expr {
        let mut buffer: Vec<Expr> = Vec::new();

        let stop_at = stop_at.or(tkarr![NEW_LINE, SEMICOLON]).unwrap();

        // * Capture all tokens that make up the expression
        while let Some(token) = self.next_token() {
            log!(
                "parse expr",
                "{:?} stop_at={:?} buffer={:?}",
                token,
                stop_at,
                buffer
            );

            if token.kind == TokenKind::NEW_LINE {
                self.ast.add(Node::Expr(Expr::Newline));
            }

            // If should stop
            if stop_at.contains(&token.kind) {
                self.stopped_at = Some(token.kind);
                return self.parse_expr_from_buffer(buffer);
            }

            // Skip newlines that ar not in stop_at, since they are not part of the expression
            if token.kind == TokenKind::NEW_LINE {
                continue;
            }

            // If it's not an expression token
            if !Lexer::is_expression_token(&token) {
                self.put_back(token); // Put back the token that stopped the expression
                break;
            }

            match token.kind {
                // * Comments
                TokenKind::COMMENT | TokenKind::ML_COMMENT => {
                    self.ast.add(Node::Expr(Expr::Comment(token.value.clone())));
                    continue;
                }

                // * Signal
                // ${ident}
                TokenKind::DOLLAR_SING => {
                    let ident = self.next_token().unwrap_or_else(|| {
                        error!(
                            &self.lexer,
                            "Expected an identifier after signal operator.",
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

                    if let Some(signals) = &mut self.capturing_signals {
                        signals.push(ident.value.clone());
                    }

                    buffer.push(Expr::Signal(ident.value.clone()));
                    continue;
                }

                // * Anonymous function
                // {args} -> {expr};
                // {args} -> {block}
                TokenKind::R_ARROW => {

                    log!("Anonymous function");

                    if buffer.is_empty() {
                        error!(
                            &self.lexer,
                            "Expected args definition for anonymous function.",
                            format!("-> ..."),
                            " ^^^ - Missing arguments here."
                        );
                    }

                    let args = buffer.pop().unwrap();

                    if args != Expr::WrappedSequence(Vec::new())
                        && args != Expr::Group(bit!(Expr::Empty))
                    {
                        error!(
                            &self.lexer,
                            "Expected args definition for anonymous function. This is not a valid argument definition.",
                            format!("-> {:?}", args)
                        );
                    }

                    // Parse block or expr
                    let body = self.parse_expr(Some(stop_at));

                    if self.stopped_at.is_none() && body != Expr::Block(Vec::new()) {
                        error!(
                            &self.lexer,
                            "Expected a block or expression after anonymous function arguments."
                        );
                    }

                    return Expr::AnonFunction {
                        args: bit!(args),
                        body: bit!(body),
                    };
                }

                // * Array
                // [expr, expr, ...]
                TokenKind::L_SQUARE_BRACKET => {
                    let expr = self.parse_expr(tkarr![R_SQUARE_BRACKET]);

                    if self.stopped_at.is_none()
                        || self.stopped_at.unwrap() != TokenKind::R_SQUARE_BRACKET
                    {
                        error!(
                            &self.lexer,
                            "Expected a closing bracket after array.",
                            format!("[{:?} ... <-- Expected \"]\" here", expr)
                        );
                    }

                    buffer.push(expr);
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

                // * Range
                // {start: expr}..{end: expr} (exclusive)
                // {start: expr}..={end: expr} (inclusive)
                TokenKind::D_DOT => {
                    // Get start of the range, if no start is provided, it's 0
                    let start = if buffer.is_empty() {
                        Expr::Number("0".to_string())
                    } else {
                        buffer.pop().unwrap()
                    };

                    // if next token is "="
                    let mut inclusive: bool = false;
                    if let Some(Token {
                        kind: TokenKind::ASSIGN,
                        ..
                    }) = self.peek_token()
                    {
                        self.next_token(); // Consume the "="
                        inclusive = true;
                    }

                    let end = self.parse_expr(Some(stop_at));

                    buffer.push(Expr::Range {
                        start: bit!(start),
                        end: bit!(end),
                        inclusive,
                    });

                    continue;
                }

                // * Parentheses (group, sequence)
                // (expr, expr, ...)
                // (expr * expr)
                TokenKind::L_PARENT => {
                    let expr = self.parse_paren();

                    // * Function call
                    // If there's an identifier or anonymous function def before, then it's a function call
                    if let Some(prev) = buffer.pop() {
                        if prev == Expr::Identifier(String::new())
                            || prev == (Expr::AnonFunction { args: bit!(Expr::Empty), body: bit!(Expr::Empty) })
                            || prev == Expr::Group(bit!(Expr::Empty))
                        {
                            buffer.push(Expr::FunctionCall {
                                name: bit!(prev),
                                args: bit!(expr),
                            });

                            continue;
                        } 
                    }
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

                // * Assingment
                TokenKind::ASSIGN => {
                    // Solve buffer, since it is LHS and
                    let lhs = self.parse_expr_from_buffer(buffer.clone());
                    buffer.clear(); // Buffer was consumed in lhs

                    self.ast.add(Node::Expr(lhs));

                    let stmt = self.parse_assingment();
                    self.ast.add(Node::Stmt(stmt));

                    continue;
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
                    let member = self.parse_expr(Some(stop_at));

                    buffer.push(Expr::MemberAccess {
                        object: bit!(lhs),
                        member: bit!(member),
                    });

                    continue;
                }

                // * Unary Operator
                // {op}{expr}
                _ if Lexer::is_bitwise_operator(&token) => {
                    log!("Unary Operator", "{:?}", token);
                    let expr = self.parse_expr(Some(stop_at));

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
                            let expr = self.parse_expr(Some(stop_at));
                            return Expr::UnaryOp {
                                op: '-'.to_string(),
                                expr: bit!(expr),
                            };
                        }

                        // If operapor is a "@" then it's a decorator
                        if token.kind == TokenKind::AT {
                            let name =
                                self.get_expr(&Expr::Identifier(String::new()), tkarr!(L_PARENT));

                            if let Some(TokenKind::L_PARENT) = self.stopped_at {
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

                    let binop = self.parse_binop(buffer.pop().unwrap(), token, Some(stop_at));
                    buffer.push(binop);

                    if self.stopped_at.is_some()
                        && stop_at.contains(&self.stopped_at.as_ref().unwrap())
                    {
                        return self.parse_expr_from_buffer(buffer);
                    }
                }

                // * Value token
                _ => {
                    let res = self.parse_value_token(&token);
                    buffer.push(res);
                }
            }

            continue;
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
            "buffer={:?}, stop_token={:?}",
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

        let args_expr = self.parse_expr(tkarr![R_ARROW]);

        if self.stopped_at.is_none() || self.stopped_at.unwrap() != TokenKind::R_ARROW {
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

        let rec_expr = self.parse_expr(tkarr![SEMICOLON, NEW_LINE]);

        if self.stopped_at.is_none() {
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
        let mut buffer: Vec<Expr> = Vec::new();

        log!("parse_paren");

        loop {
            let expr = self.parse_expr(tkarr![R_PARENT, COMMA]);
            let stop_token = self.stopped_at.take();

            if stop_token.is_none() {
                error!(
                    &self.lexer,
                    "Unexpected end of file.",
                    "( ...",
                    "Expected a value or expression here or closing parenthesis, but got nothing instead."
                );
            }

            buffer.push(expr);

            match stop_token.unwrap() {
                TokenKind::COMMA => {
                    continue;
                }

                TokenKind::R_PARENT => {
                    if buffer.len() == 1 {
                        // An expression surrounded by parenthesis. Ex. (1 + 2 * a)
                        return Expr::Group(Box::new(buffer.pop().unwrap()));
                    } else {
                        // A sequence of expressions. Ex. (1, 2, 3)
                        return Expr::WrappedSequence(buffer);
                    }
                }
                _ => unreachable!("Unexpected stop token in parse_paren. {:?}", stop_token),
            }
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

        self.ast.current_scope = Rc::new(RefCell::new(Vec::new()));

        while let Some(token) = self.peek_token() {
            if token.kind == TokenKind::R_BRACKET {
                self.next_token(); // Consume the R_BRACKET
                break; // End of block
            }

            if Lexer::is_statement_token(&token) {
                let stmt = self.parse_statement();
                self.ast.add(Node::Stmt(stmt));
            } else if Lexer::is_expression_token(&token) {
                let expr = self.parse_expr(tkarr![R_BRACKET]);
                log!("+ parse_block", "expr={:?}", expr);

                self.ast.add(Node::Expr(expr));

                if let Some(t) = self.stopped_at.take() {
                    if t == TokenKind::R_BRACKET {
                        break;
                    }
                }
            }
        }
        let block = Expr::Block(self.ast.current_scope.borrow().clone());
        log!("end parse_block", "{:?}", block);

        self.ast.current_scope = self.ast.children.clone();
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
            let key = self.parse_expr(tkarr![COLON, COMMA, R_BRACKET]);

            let stop_token = match self.stopped_at.take() {
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
                TokenKind::COLON => {
                    let value = self.parse_expr(tkarr![COMMA, R_BRACKET]);
                    log!("parse_dict", "key={:?} value={:?}", key, value);
                    keys.push(key);
                    values.push(value);

                    if let Some(TokenKind::R_BRACKET) = self.stopped_at {
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
    fn parse_binop(&mut self, left: Expr, operator: Token, stop_at: Option<&[TokenKind]>) -> Expr {
        let right = self.parse_expr(stop_at);
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

    /// Get an expression of type `expr_type` from the token stream if it exists, otherwise return None.
    /// `stop_at` is a tuple of token types that should stop the expression parsing.
    fn get_optional_expr(
        &mut self,
        expr_type: &Expr,
        stop_at: Option<&[TokenKind]>,
    ) -> Option<Expr> {
        let expr = self.parse_expr(stop_at);

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
    /// `stop_at` is a tuple of token types that should stop the expression parsing.
    fn get_expr(&mut self, expr_type: &Expr, stop_at: Option<&[TokenKind]>) -> Expr {
        let expr = self.get_optional_expr(&expr_type, stop_at);

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
        if self.ast.current_scope.borrow().is_empty() {
            error!(
                &self.lexer,
                "Expected an identifier before assingment operator.",
                format!("... = ..."),
                " ^^^ - This is not an identifier or not one present."
            );
        }

        let op = self.next_token().unwrap().value.clone();

        let ident: Expr = self.ast.pop().unwrap().into();

        // Signal definition
        // {Signal} = {expr}
        if let Expr::Signal(name) = ident {
            self.capturing_signals = Some(vec![]);

            let mut value = self.parse_expr(None);

            let dependencies = self.capturing_signals.take().unwrap();

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

        let value = self.parse_expr(None);

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
