use std::collections::HashSet;

use crate::lexer::{Kind, Lexer, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroSpan {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroError {
    pub message: String,
    pub span: MacroSpan,
}

impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {}:{}", self.message, self.span.line, self.span.column)
    }
}

impl std::error::Error for MacroError {}

#[derive(Debug, Clone)]
pub struct AttrMacroInvocation {
    pub name: String,
    pub args_tokens: Vec<Token>,
    pub span: MacroSpan,
}

#[derive(Debug, Clone)]
pub struct MacroExpansion {
    pub prelude_items: Vec<Vec<Token>>,
    pub primary_item: Vec<Token>,
    pub emitted_items: Vec<Vec<Token>>,
}

pub trait AttrMacro {
    fn name(&self) -> &'static str;
    fn expand(
        &self,
        invocation: &AttrMacroInvocation,
        target: &CapturedItem
    ) -> Result<MacroExpansion, MacroError>;
}

pub trait DeriveHandler {
    fn name(&self) -> &'static str;
    fn expand(&self, target: &CapturedItem) -> Result<MacroExpansion, MacroError>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum CapturedItemKind {
    Struct(StructDescriptor),
    Trait(TraitDescriptor),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct StructDescriptor {
    name: String,
    generics: Vec<String>,
    fields: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TraitDescriptor {
    name: String,
    generics: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CapturedItem {
    kind: CapturedItemKind,
    tokens: Vec<Token>,
    span: MacroSpan,
}

pub fn expand_source_tokens(source: &str) -> Result<Vec<Token>, MacroError> {
    expand_tokens(lex_all(source))
}

pub fn expand_tokens(tokens: Vec<Token>) -> Result<Vec<Token>, MacroError> {
    let mut cursor = 0;
    let mut output = Vec::new();
    let mut injected_preludes = HashSet::new();

    while cursor < tokens.len() {
        if tokens[cursor].kind != Kind::HASH {
            output.push(tokens[cursor].clone());
            cursor += 1;
            continue;
        }

        let mut invocations = Vec::new();
        while cursor < tokens.len() && tokens[cursor].kind == Kind::HASH {
            let (invocation, next_cursor) = parse_attr_invocation(&tokens, cursor)?;
            invocations.push(invocation);
            cursor = next_cursor;

            while cursor < tokens.len() && tokens[cursor].kind == Kind::NEW_LINE {
                cursor += 1;
            }
        }

        let (mut current_item, next_cursor) = capture_attached_item(&tokens, cursor)?;
        cursor = next_cursor;

        let mut emitted_items = Vec::new();
        let mut prelude_items = Vec::new();

        for invocation in invocations.iter() {
            let expansion = expand_attr_macro(invocation, &current_item)?;
            current_item = parse_captured_item(&expansion.primary_item)?;
            prelude_items.extend(expansion.prelude_items);
            emitted_items.extend(expansion.emitted_items);
        }

        for prelude in prelude_items {
            let signature = token_signature(&prelude);
            if injected_preludes.insert(signature) {
                output.extend(prelude);
            }
        }

        output.extend(current_item.tokens.clone());
        for item in emitted_items {
            output.extend(item);
        }
    }

    Ok(output)
}

fn expand_attr_macro(
    invocation: &AttrMacroInvocation,
    target: &CapturedItem
) -> Result<MacroExpansion, MacroError> {
    match invocation.name.as_str() {
        "derive" => {
            let handler = DeriveAttrMacro;
            debug_assert_eq!(handler.name(), "derive");
            handler.expand(invocation, target)
        }
        other => Err(MacroError {
            message: format!("Unknown attribute macro '#{}(...)'.", other),
            span: invocation.span.clone(),
        }),
    }
}

struct DeriveAttrMacro;

impl AttrMacro for DeriveAttrMacro {
    fn name(&self) -> &'static str {
        "derive"
    }

    fn expand(
        &self,
        invocation: &AttrMacroInvocation,
        target: &CapturedItem
    ) -> Result<MacroExpansion, MacroError> {
        let derive_names = parse_derive_names(&invocation.args_tokens, &invocation.span)?;

        let mut current_item = target.clone();
        let mut prelude_items = Vec::new();
        let mut emitted_items = Vec::new();

        for derive_name in derive_names {
            let handler = derive_handler(&derive_name, &invocation.span)?;
            debug_assert_eq!(handler.name(), derive_name);

            let expansion = handler.expand(&current_item)?;
            current_item = parse_captured_item(&expansion.primary_item)?;
            prelude_items.extend(expansion.prelude_items);
            emitted_items.extend(expansion.emitted_items);
        }

        Ok(MacroExpansion {
            prelude_items,
            primary_item: current_item.tokens.clone(),
            emitted_items,
        })
    }
}

fn derive_handler(
    name: &str,
    span: &MacroSpan
) -> Result<Box<dyn DeriveHandler>, MacroError> {
    match name {
        "debug" => Ok(Box::new(DebugDeriveHandler)),
        other => Err(MacroError {
            message: format!("Unknown derive '{}'.", other),
            span: span.clone(),
        }),
    }
}

struct DebugDeriveHandler;

impl DeriveHandler for DebugDeriveHandler {
    fn name(&self) -> &'static str {
        "debug"
    }

    fn expand(&self, target: &CapturedItem) -> Result<MacroExpansion, MacroError> {
        let CapturedItemKind::Struct(descriptor) = &target.kind else {
            return Err(MacroError {
                message: "#derive(debug) only supports structs in v1.".to_string(),
                span: target.span.clone(),
            });
        };

        let generics_decl = generic_decl(&descriptor.generics);
        let target_ref = type_ref_source(&descriptor.name, &descriptor.generics);
        let debug_expr = build_debug_expr(&descriptor.name, &descriptor.fields);

        let debug_trait = lex_all(
            r#"
trait Debug {
    fn debug(self) -> str
}
"#,
        );

        let debug_impl = lex_all(
            format!(
                r#"
impl{generics_decl} Debug for {target_ref} {{
    fn debug(self) -> str {{
        {debug_expr}
    }}
}}
"#
            )
            .as_str(),
        );

        let inherent_impl = lex_all(
            format!(
                r#"
impl{generics_decl} {target_ref} {{
    fn __str__(self) -> str {{
        self.debug()
    }}

    fn __repr__(self) -> str {{
        self.debug()
    }}
}}
"#
            )
            .as_str(),
        );

        Ok(MacroExpansion {
            prelude_items: vec![debug_trait],
            primary_item: target.tokens.clone(),
            emitted_items: vec![debug_impl, inherent_impl],
        })
    }
}

fn parse_attr_invocation(
    tokens: &[Token],
    start: usize
) -> Result<(AttrMacroInvocation, usize), MacroError> {
    let hash = tokens.get(start).ok_or_else(|| MacroError {
        message: "Expected an attribute macro.".to_string(),
        span: MacroSpan { line: 1, column: 0 },
    })?;

    let span = MacroSpan {
        line: hash.line,
        column: hash.column,
    };

    let name_token = tokens.get(start + 1).ok_or_else(|| MacroError {
        message: "Expected a macro name after '#'.".to_string(),
        span: span.clone(),
    })?;
    if name_token.kind != Kind::IDENTIFIER {
        return Err(MacroError {
            message: "Expected a macro name after '#'.".to_string(),
            span,
        });
    }

    let l_paren = tokens.get(start + 2).ok_or_else(|| MacroError {
        message: format!("Expected '(' after macro name '{}'.", name_token.value),
        span: MacroSpan {
            line: name_token.line,
            column: name_token.column,
        },
    })?;
    if l_paren.kind != Kind::L_PARENT {
        return Err(MacroError {
            message: format!("Expected '(' after macro name '{}'.", name_token.value),
            span: MacroSpan {
                line: l_paren.line,
                column: l_paren.column,
            },
        });
    }

    let mut args_tokens = Vec::new();
    let mut depth = 1;
    let mut cursor = start + 3;

    while cursor < tokens.len() {
        let token = tokens[cursor].clone();
        match token.kind {
            Kind::L_PARENT => {
                depth += 1;
                args_tokens.push(token);
            }
            Kind::R_PARENT => {
                depth -= 1;
                if depth == 0 {
                    return Ok((
                        AttrMacroInvocation {
                            name: name_token.value.clone(),
                            args_tokens,
                            span,
                        },
                        cursor + 1,
                    ));
                }
                args_tokens.push(token);
            }
            _ => args_tokens.push(token),
        }
        cursor += 1;
    }

    Err(MacroError {
        message: format!("Expected ')' to close macro '{}'.", name_token.value),
        span,
    })
}

fn capture_attached_item(
    tokens: &[Token],
    start: usize
) -> Result<(CapturedItem, usize), MacroError> {
    let item_token = tokens.get(start).ok_or_else(|| MacroError {
        message: "Expected a struct or trait after attribute macro.".to_string(),
        span: MacroSpan { line: 1, column: 0 },
    })?;

    match item_token.kind {
        Kind::STRUCT | Kind::TRAIT => {}
        _ => {
            return Err(MacroError {
                message: "Expected a struct or trait after attribute macro.".to_string(),
                span: MacroSpan {
                    line: item_token.line,
                    column: item_token.column,
                },
            });
        }
    }

    let mut captured = Vec::new();
    let mut body_started = false;
    let mut brace_depth = 0;
    let mut cursor = start;

    while cursor < tokens.len() {
        let token = tokens[cursor].clone();
        match token.kind {
            Kind::L_BRACKET => {
                body_started = true;
                brace_depth += 1;
            }
            Kind::R_BRACKET => {
                brace_depth -= 1;
            }
            _ => {}
        }

        captured.push(token.clone());
        cursor += 1;

        if body_started && brace_depth == 0 {
            return Ok((parse_captured_item(&captured)?, cursor));
        }
    }

    Err(MacroError {
        message: "Expected a complete item body after attribute macro.".to_string(),
        span: MacroSpan {
            line: item_token.line,
            column: item_token.column,
        },
    })
}

fn parse_captured_item(tokens: &[Token]) -> Result<CapturedItem, MacroError> {
    let span = tokens
        .first()
        .map(|token| MacroSpan {
            line: token.line,
            column: token.column,
        })
        .unwrap_or(MacroSpan { line: 1, column: 0 });

    let mut cursor = TokenCursor::new(tokens);
    let first = cursor.next().ok_or_else(|| MacroError {
        message: "Expected a captured item.".to_string(),
        span: span.clone(),
    })?;

    let kind = match first.kind {
        Kind::STRUCT => CapturedItemKind::Struct(parse_struct_descriptor(&mut cursor, &span)?),
        Kind::TRAIT => CapturedItemKind::Trait(parse_trait_descriptor(&mut cursor, &span)?),
        _ => {
            return Err(MacroError {
                message: "Expected a struct or trait item.".to_string(),
                span,
            });
        }
    };

    Ok(CapturedItem {
        kind,
        tokens: tokens.to_vec(),
        span,
    })
}

fn parse_struct_descriptor(
    cursor: &mut TokenCursor<'_>,
    span: &MacroSpan
) -> Result<StructDescriptor, MacroError> {
    let name = cursor.expect_identifier("Expected a struct name.", span)?;
    let generics = cursor.parse_generic_names(span)?;
    cursor.expect_kind(Kind::L_BRACKET, "Expected a struct body after struct name.", span)?;

    let mut fields = Vec::new();
    loop {
        cursor.consume_decl_separators();

        if cursor.peek_kind(Kind::R_BRACKET) {
            cursor.next();
            break;
        }

        let field_name = cursor.expect_identifier("Expected a field name in struct body.", span)?;
        cursor.expect_kind(Kind::COLON, "Expected ':' after struct field name.", span)?;
        cursor.consume_type_ref(span)?;
        fields.push(field_name);
        cursor.consume_decl_separators();
    }

    Ok(StructDescriptor { name, generics, fields })
}

fn parse_trait_descriptor(
    cursor: &mut TokenCursor<'_>,
    span: &MacroSpan
) -> Result<TraitDescriptor, MacroError> {
    let name = cursor.expect_identifier("Expected a trait name.", span)?;
    let generics = cursor.parse_generic_names(span)?;
    cursor.expect_kind(Kind::L_BRACKET, "Expected a trait body after trait name.", span)?;

    let mut brace_depth = 1;
    while let Some(token) = cursor.next() {
        match token.kind {
            Kind::L_BRACKET => brace_depth += 1,
            Kind::R_BRACKET => {
                brace_depth -= 1;
                if brace_depth == 0 {
                    return Ok(TraitDescriptor { name, generics });
                }
            }
            _ => {}
        }
    }

    Err(MacroError {
        message: "Expected a closing '}' in trait body.".to_string(),
        span: span.clone(),
    })
}

fn parse_derive_names(
    tokens: &[Token],
    span: &MacroSpan
) -> Result<Vec<String>, MacroError> {
    let mut cursor = TokenCursor::new(tokens);
    let mut names = Vec::new();

    while cursor.peek().is_some() {
        cursor.skip_newlines();

        if cursor.peek().is_none() {
            break;
        }

        let name = cursor.expect_identifier("Expected a derive name inside #derive(...).", span)?;
        names.push(name);

        cursor.skip_newlines();
        if cursor.peek().is_none() {
            break;
        }

        cursor.expect_kind(Kind::COMMA, "Expected ',' between derive names.", span)?;
    }

    if names.is_empty() {
        return Err(MacroError {
            message: "Expected a derive name inside #derive(...).".to_string(),
            span: span.clone(),
        });
    }

    Ok(names)
}

fn generic_decl(generics: &[String]) -> String {
    if generics.is_empty() {
        String::new()
    } else {
        format!("<{}>", generics.join(", "))
    }
}

fn type_ref_source(name: &str, generics: &[String]) -> String {
    if generics.is_empty() {
        name.to_string()
    } else {
        format!("{}<{}>", name, generics.join(", "))
    }
}

fn build_debug_expr(type_name: &str, fields: &[String]) -> String {
    if fields.is_empty() {
        return format!("\"{} {{}}\"", type_name);
    }

    let mut expr = format!("\"{} {{ ", type_name);
    for (index, field) in fields.iter().enumerate() {
        if index == 0 {
            expr.push_str(&format!("{}: \" + str(self.{})", field, field));
        } else {
            expr.push_str(&format!(" + \", {}: \" + str(self.{})", field, field));
        }
    }
    expr.push_str(" + \" }\"");
    expr
}

fn lex_all(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source.to_string());
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push(token);
    }

    tokens
}

fn token_signature(tokens: &[Token]) -> String {
    tokens
        .iter()
        .map(|token| format!("{:?}:{}", token.kind, token.value))
        .collect::<Vec<String>>()
        .join("|")
}

struct TokenCursor<'a> {
    tokens: &'a [Token],
    index: usize,
}

impl<'a> TokenCursor<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        TokenCursor { tokens, index: 0 }
    }

    fn next(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.index)?;
        self.index += 1;
        Some(token)
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.index)
    }

    fn peek_kind(&self, kind: Kind) -> bool {
        matches!(self.peek(), Some(token) if token.kind == kind)
    }

    fn skip_newlines(&mut self) {
        while self.peek_kind(Kind::NEW_LINE) {
            self.next();
        }
    }

    fn consume_decl_separators(&mut self) {
        while let Some(token) = self.peek() {
            match token.kind {
                Kind::COMMA | Kind::SEMICOLON | Kind::NEW_LINE => {
                    self.next();
                }
                _ => break,
            }
        }
    }

    fn expect_identifier(
        &mut self,
        message: &str,
        span: &MacroSpan
    ) -> Result<String, MacroError> {
        match self.next() {
            Some(Token { kind: Kind::IDENTIFIER, value, .. }) => Ok(value.clone()),
            Some(token) => Err(MacroError {
                message: format!("{} Got: {:?}.", message, token.kind),
                span: MacroSpan {
                    line: token.line,
                    column: token.column,
                },
            }),
            None => Err(MacroError {
                message: message.to_string(),
                span: span.clone(),
            }),
        }
    }

    fn expect_kind(
        &mut self,
        kind: Kind,
        message: &str,
        span: &MacroSpan
    ) -> Result<(), MacroError> {
        match self.next() {
            Some(token) if token.kind == kind => Ok(()),
            Some(token) => Err(MacroError {
                message: message.to_string(),
                span: MacroSpan {
                    line: token.line,
                    column: token.column,
                },
            }),
            None => Err(MacroError {
                message: message.to_string(),
                span: span.clone(),
            }),
        }
    }

    fn parse_generic_names(&mut self, span: &MacroSpan) -> Result<Vec<String>, MacroError> {
        if !self.peek_kind(Kind::LT) {
            return Ok(Vec::new());
        }

        self.next();
        let mut generics = Vec::new();

        loop {
            generics.push(self.expect_identifier("Expected a generic parameter name.", span)?);

            match self.peek() {
                Some(Token { kind: Kind::COMMA, .. }) => {
                    self.next();
                }
                Some(Token { kind: Kind::GT, .. }) => {
                    self.next();
                    break;
                }
                Some(token) => {
                    return Err(MacroError {
                        message: "Expected ',' or '>' in generic parameter list.".to_string(),
                        span: MacroSpan {
                            line: token.line,
                            column: token.column,
                        },
                    });
                }
                None => {
                    return Err(MacroError {
                        message: "Expected '>' after generic parameter list.".to_string(),
                        span: span.clone(),
                    });
                }
            }
        }

        Ok(generics)
    }

    fn consume_type_ref(&mut self, span: &MacroSpan) -> Result<(), MacroError> {
        let mut consumed_any = false;
        let mut angle_depth = 0;

        while let Some(token) = self.peek() {
            match token.kind {
                Kind::LT => {
                    consumed_any = true;
                    angle_depth += 1;
                    self.next();
                }
                Kind::GT => {
                    consumed_any = true;
                    angle_depth -= 1;
                    self.next();
                }
                Kind::COMMA | Kind::SEMICOLON | Kind::NEW_LINE | Kind::R_BRACKET
                    if angle_depth == 0 =>
                {
                    break;
                }
                _ => {
                    consumed_any = true;
                    self.next();
                }
            }
        }

        if consumed_any {
            Ok(())
        } else {
            Err(MacroError {
                message: "Expected a type after ':'.".to_string(),
                span: span.clone(),
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{parse_derive_names, lex_all, MacroSpan};

    #[test]
    fn derive_name_parser_preserves_source_order() {
        let names = parse_derive_names(
            &lex_all("debug, trace, clone"),
            &MacroSpan { line: 1, column: 0 },
        )
        .expect("derive names should parse");

        assert_eq!(names, vec!["debug", "trace", "clone"]);
    }
}
