use std::{cell::RefCell, path::PathBuf, rc::Rc};

use crate::{
    ast::{Expr, Node, Stmt, AST},
    lexer::{self, Lexer, Token, TokenKind},
    log_utils::error,
};

/// Box-It, bit!
/// Used to box an expression
macro_rules! bit {
    // Box it
    ($x:expr) => {
        Box::new($x)
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
    stopped_at: Option<Token>, // token in which a parser stopped
    capturing_sequence: bool, // if the parser is capturing a sequence, parse_expr_from_buffer will return a sequence once parse_expr finds a expr end token
}

impl Parser {
    pub fn new(source_file: PathBuf) -> Self {
        let lexer = Lexer::from_path(source_file);

        Parser {
            remanider_token: None,
            lexer,
            ast: AST::new(),

            stopped_at: None,
            capturing_sequence: false,
        }
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
                
                TokenKind::ASSING => {
                    let stmt = self.parse_assingment();
                    return stmt;
                }

                // Function definition
                // `def {ident}({ident}, {ident}, ...) {block}`
                TokenKind::DEF => {
                    let name = self.get_expr(
                        &Expr::Identifier(String::new()),
                        Some(&[TokenKind::L_PARENT]),
                    );

                    if self.stopped_at.take().is_none() {
                        error(
                            &[
                                "Expected a parenthesis after function name.".to_string(),
                                format!("def {:?} (...", name),
                            ],
                            self.lexer.line,
                            self.lexer.column,
                        );
                    }

                    let args = self.parse_paren();
                    let body = self.parse_block();

                    println!("[parse_statement/DEF] {:?}( {:?} ) {:?} ", name, args, body);

                    return Stmt::FunctionDef { name, args, body };
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

        let stop_at = stop_at.or(Some(&[TokenKind::NEW_LINE]));

        // * Capture all tokens that make up the expression
        while let Some(token) = self.next_token() {
            println!(
                "[parse_expr] {:?} stop_at={:?} buffer={:?}",
                token, stop_at, buffer
            );

            if !Lexer::is_expression_token(&token)
                && token.kind == TokenKind::SEMICOLON
                // If not stop at, stop at new_line
                || (stop_at.is_none() && token.kind == TokenKind::NEW_LINE)
            {
                self.put_back(token); // Put back the token that stopped the expression
                break;
            }

            if stop_at.is_some() && stop_at.unwrap().contains(&token.kind) {
                self.stopped_at = Some(token);
                return self.parse_expr_from_buffer(buffer);
            }

            match token.kind {
                TokenKind::L_PARENT => {
                    let expr = self.parse_paren();
                    buffer.push(expr);
                    continue;
                }

                TokenKind::L_BRACKET => {
                    let expr = self.parse_block();
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
                TokenKind::ASSING => {
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
                        error(
                            &[
                                "Expected an expression before operator.".to_string(),
                                format!("{:?} ...", token),
                                "^^^ -- Expected something as: object.property".to_string(),
                            ],
                            self.lexer.line,
                            self.lexer.column,
                        );
                    }

                    let lhs = buffer.pop().unwrap();
                    let member = self.parse_expr(stop_at);


                    buffer.push(Expr::MemberAccess {
                        object: bit!(lhs),
                        member: bit!(member),
                    });
                    
                    continue;
                }
                    
                        

                // * Operator
                _ if Lexer::is_operator_token(&token) => {
                    // If no previous expr, no left hand value
                    if buffer.is_empty() {
                        error(
                            &[
                                "Expected an expression before operator.".to_string(),
                                format!("{:?} ...", token),
                            ],
                            self.lexer.line,
                            self.lexer.column,
                        );
                    }

                    let binop = self.parse_binop(buffer.pop().unwrap(), token, stop_at);
                    buffer.push(binop);

                    if self.stopped_at.is_some()
                        && stop_at.is_some()
                        && stop_at
                            .unwrap()
                            .contains(&self.stopped_at.as_ref().unwrap().kind)
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

        error(
            &[
                "Couldn't parse expression".to_string(),
                format!("... {:?} ...", buffer),
                "Whatever this meant to be...".to_string(),
            ],
            self.lexer.line,
            self.lexer.column,
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
            let expr = self.parse_expr(Some(&[TokenKind::R_PARENT, TokenKind::COMMA]));
            let stop_token = self.stopped_at.take();

            if stop_token.is_none() {
                error(
                    &[
                        "Unexpected end of file.".to_string(),
                        "( ...".to_string(),
                        "Expected a value or expression here or closing parenthesis, but got nothing instead.".to_string(),
                    ],
                    self.lexer.line,
                    self.lexer.column,
                );
            }

            println!("[parse_paren] expr={:?} stop_token={:?}", expr, stop_token);
            buffer.push(expr);

            match stop_token.unwrap().kind {
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
                let expr = self.parse_expr(Some(&[TokenKind::R_BRACKET]));
                println!("+ [parse_block] expr={:?}", expr);

                self.ast.add(Node::Expr(expr));

                if let Some(t) = self.stopped_at.take() {
                    if t.kind == TokenKind::R_BRACKET {
                        break;
                    }
                }
            }
        }
        let block = Expr::Block(self.ast.current_scope.borrow().clone());
        println!("[end parse_block] {:?}", block);

        self.ast.current_scope = self.ast.childrem.clone();
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
                Some(&[TokenKind::COMMA, TokenKind::R_BRACKET, TokenKind::COLON]),
            );

            let stop_token = self.stopped_at.take();

            if let Some(key) = key {
                match stop_token {
                    Some(Token {
                        kind: TokenKind::COMMA,
                        ..
                    })
                    | Some(Token {
                        kind: TokenKind::R_BRACKET,
                        ..
                    }) => {
                        if let Expr::Identifier(_) = key {
                            keys.push(key.clone());
                            values.push(key);

                            if stop_token.unwrap().kind == TokenKind::R_BRACKET {
                                break;
                            }
                        } else {
                            error(
                                &[
                                    "Key can't be used as value, expected an identifier (variable) instead.".to_string(),
                                    format!("{{...{:?}...}}", key),
                                    "this should be an identifier.".to_string(),
                                ],
                                self.lexer.line,
                                self.lexer.column,
                            );
                        }
                    }
                    Some(Token {
                        kind: TokenKind::COLON,
                        ..
                    }) => {
                        let value =
                            self.parse_expr(Some(&[TokenKind::COMMA, TokenKind::R_BRACKET]));

                        keys.push(key);
                        values.push(value);

                        if let Some(Token {
                            kind: TokenKind::R_BRACKET,
                            ..
                        }) = stop_token
                        {
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
            _ => error(
                &[
                    "Can't parse token to value.".to_string(),
                    format!("Token: {:?}", token),
                    "Expected a value token here.".to_string(),
                ],
                token.line,
                token.column,
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
                error(
                    &[
                        format!("Expected an expression of type {:?}.", expr_type),
                        format!("... {:?}", expr),
                    ],
                    self.lexer.line,
                    self.lexer.column,
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
            error(
                &[
                    format!("Expected an expression of type {:?}.", expr_type),
                    "Found nothing.".to_string(),
                ],
                self.lexer.line,
                self.lexer.column,
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
            error(
                &[
                    "Expected an identifier before assingment operator.".to_string(),
                    format!("... = ..."),
                    " ^^^ - This is not an identifier or not one present.".to_string(),
                ],
                self.lexer.line,
                self.lexer.column,
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
            error(
                &[
                    "Expected an identifier before assingment operator.".to_string(),
                    format!("{:?} = {:?}", ident, value),
                    " ^^^ - This is not an identifier.".to_string(),
                ],
                self.lexer.line,
                self.lexer.column,
            );
        }
    }
}
