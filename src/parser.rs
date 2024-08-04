use std::{cell::RefCell, fs, path::PathBuf, rc::Rc};

use crate::{
    ast::{Expr, Node, Stmt, AST},
    error,
    lexer::{Lexer, Token, TokenKind},
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
            println!("[parse] {:?}", token);

            // * Single line commments
            if token.kind == TokenKind::HASH {
                while let Some(token) = self.next_token() {
                    if token.kind == TokenKind::NEW_LINE {
                        break;
                    }
                }
                continue; // Continue because token is now an unknown: new line or None
            }

            // New lines
            if token.kind == TokenKind::NEW_LINE {
                self.next_token();
                continue;
            }

            if Lexer::is_statement_token(&token) {
                let statement: Stmt = self.parse_statement();
                self.ast.add(Node::Stmt(statement));
            } else if Lexer::is_expression_token(&token) {
                let expr: Expr = self.parse_expr(None);

                println!("+ [parse] Adding expression to AST: {:?}", expr);

                self.ast.add(Node::Expr(expr));
            } else {
                eprintln!("( ! ) [parse] Unknown token: {:?}", token);
            }
        }

        &self.ast
    }

    /// Called when encountering a statement token.
    /// Statement token should be set as REMAINDER_TOKEN before calling this function.
    /// Captures the statement from token to the next semicolon.
    fn parse_statement(&mut self) -> Stmt {
        println!("[parse_statement]");

        while let Some(token) = self.next_token() {
            if token.kind == TokenKind::SEMICOLON {
                break;
            }

            match token.kind {
                TokenKind::PYTHON => {
                    return Stmt::Python(token.value.clone());
                }

                TokenKind::ASSIGN => {
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

                    println!("[parse_statement/DEF] {:?}( {:?} ) {:?} ", name, args, body);

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

                    if let Some(Token) = self.stopped_at {
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
            println!(
                "[parse_expr] {:?} stop_at={:?} buffer={:?}",
                token, stop_at, buffer
            );

            // If should stop
            if stop_at.contains(&token.kind) {
                self.stopped_at = Some(token.kind);
                return self.parse_expr_from_buffer(buffer);
            }

            // If it's not an expression token
            if !Lexer::is_expression_token(&token) {
                self.put_back(token); // Put back the token that stopped the expression
                break;
            }

            match token.kind {
                // Range
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

                TokenKind::L_PARENT => {
                    let expr = self.parse_paren();
                    buffer.push(expr);
                    continue;
                }

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
                    println!("Unary Operator: {:?}", token);
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

                        // error!(
                        //         "Expected an expression before operator.".to_string(),
                        //         ("{:?} ...", token),
                        //     self.lexer.line,
                        //     self.lexer.column,
                        // );
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

        self.parse_expr_from_buffer(buffer)
    }

    /// Parse an expression from a buffer of expressiosn.
    /// Used to "condence" expressions that are made up of multiple expressions.
    /// Ex. (1 + 2) * (2 / 2) -> BinOp(BinOp(1, '+', 2), '*', BinOp(2, '/', 2))
    fn parse_expr_from_buffer(&mut self, buffer: Vec<Expr>) -> Expr {
        println!("[parse_expr_from_buffer] buffer={:?}", buffer);

        if buffer.is_empty() {
            return Expr::Empty;
        }

        if buffer.len() == 1 {
            return buffer[0].clone();
        }

        let mut index = 0;
        while index < buffer.len() - 1 {
            let current_expr = &buffer[index];
            let next_expr = &buffer[index + 1];

            // * Function Call
            // {ident}{sequence | Group}
            if let Expr::Identifier(_) = current_expr {
                if let Expr::Sequence(_) | Expr::Group(_) = next_expr {
                    return Expr::FunctionCall {
                        name: bit!(current_expr.clone()),
                        args: bit!(next_expr.clone()),
                    };
                }
            }

            index += 1;
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

    /// Parse a parenthesis expression, sequence or function call.
    /// Should be called when encountering a L_PAREN token.
    ///
    /// Group
    /// ({expr} {op} {expr} ...)
    ///
    /// Sequence
    /// ({expr}, {expr}, ...)
    fn parse_paren(&mut self) -> Expr {
        let mut buffer: Vec<Expr> = Vec::new();

        println!("[parse_paren]");

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

            println!("[parse_paren] expr={:?} stop_token={:?}", expr, stop_token);
            buffer.push(expr);

            match stop_token.unwrap() {
                TokenKind::COMMA => {
                    continue;
                }
                TokenKind::R_PARENT => {
                    break;
                }
                _ => unreachable!(),
            }
        }

        if buffer.len() == 1 {
            Expr::Group(Box::new(buffer.pop().unwrap()))
        } else {
            Expr::Sequence(buffer)
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
        println!("[parse_block]");

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
                println!("+ [parse_block] expr={:?}", expr);

                self.ast.add(Node::Expr(expr));

                if let Some(t) = self.stopped_at.take() {
                    if t == TokenKind::R_BRACKET {
                        break;
                    }
                }
            }
        }
        let block = Expr::Block(self.ast.current_scope.borrow().clone());
        println!("[end parse_block] {:?}", block);

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

        println!("[parse_dict]");

        loop {
            let key = self.get_optional_expr(
                &Expr::Identifier(String::new()),
                tkarr![COMMA, R_BRACKET, COLON],
            );

            let stop_token = self.stopped_at.take();

            if stop_token.is_none() {
                error!(
                    &self.lexer,
                    "Unexpected end of file.",
                    "{{ ...",
                    "Expected a key or closing bracket, but got nothing instead."
                );
            }

            let stop_token = stop_token.unwrap();

            if let Some(key) = key {
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

                        keys.push(key);
                        values.push(value);

                        if let Some(TokenKind::R_BRACKET) = self.stopped_at {
                            break;
                        }
                    }
                    _ => break,
                }
            } else {
                break;
            }
        }

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

        if expr == Expr::Empty {
            if expr != *expr_type {
                // <- This line right here
                error!(
                    &self.lexer,
                    format!("Expected an expression of type {:?}.", expr_type),
                    format!("... {:?}", expr)
                );
            }

            return Some(expr);
        }
        None
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

    /// Called when encountering a ASSING (=) token.
    /// LHS (ident) should be in AST.current_scope.
    /// * Assingment statement.
    /// - `{ident} = {expr};`
    /// * Multiple assingment statement.
    /// - `({ident}, {ident}, ...) = ({expr}, {expr}, ...)`
    fn parse_assingment(&mut self) -> Stmt {
        if self.ast.current_scope.borrow().is_empty() {
            error!(
                &self.lexer,
                "Expected an identifier before assingment operator.",
                format!("... = ..."),
                " ^^^ - This is not an identifier or not one present."
            );
        }

        let ident: Expr = self.ast.pop().unwrap().into();
        let value = self.parse_expr(None);

        // Single assingment
        if ident == Expr::Identifier(String::new())
             // Multiple assingment
            || ident == Expr::Sequence(Vec::new())
        {
            return Stmt::Assign {
                identifiers: vec![ident],
                values: vec![value],
            };
        } else {
            error!(
                &self.lexer,
                "Expected an identifier before assingment operator.",
                format!("{:?} = {:?}", ident, value),
                " ^^^ - This is not an identifier."
            );
        }
    }
}
