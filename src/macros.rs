//! Token-based expansion for Lang's built-in attached attribute macros.
//!
//! The macro system in v1 is intentionally narrow: it only understands built-in
//! attribute macros, it runs before normal item parsing, and it rewrites source
//! into ordinary tokens that continue through the existing parser and
//! transpilers.
//!
//! At a high level the pipeline is:
//!
//! 1. Scan the token stream and collect consecutive `#name(...)` attributes.
//! 2. Capture the `struct` or `trait` item attached to those attributes.
//! 3. Dispatch each invocation to its built-in handler.
//! 4. Inject any generated prelude items once per module.
//! 5. Append the rewritten primary item and any emitted helper items back into
//!    the output token stream.
//!
//! That shape keeps macro expansion isolated in one place while letting the
//! rest of the compiler operate on plain Lang items instead of special AST
//! nodes.

use std::collections::HashSet;

use crate::lexer::{Kind, Lexer, Token};

/// Source coordinates attached to macro parsing and expansion failures.
///
/// Macro expansion runs before the parser builds higher-level AST nodes, so the
/// macro subsystem needs its own lightweight location type for diagnostics.
/// Keeping line and column together in a dedicated struct makes those errors
/// easy to thread through parsing helpers and handler dispatch.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroSpan {
    /// One-based line number in the source being expanded.
    pub line: usize,
    /// Zero-based column offset within [`Self::line`].
    pub column: usize,
}

/// Error reported while parsing or expanding an attached macro invocation.
///
/// This is the common error type for the whole module. It keeps the human
/// message and the best available source span together so callers can surface a
/// precise failure without depending on later compiler phases.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MacroError {
    /// Human-readable explanation of what failed.
    pub message: String,
    /// Best-known source location for the failure.
    pub span: MacroSpan,
}

/// Formats a macro error as `message at line:column`.
impl std::fmt::Display for MacroError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} at {}:{}",
            self.message, self.span.line, self.span.column
        )
    }
}

/// Marks [`MacroError`] as a standard error value for higher-level callers.
impl std::error::Error for MacroError {}

/// Parsed representation of one `#name(...)` attribute attached to an item.
///
/// Expansion is split into two phases: first the raw invocation is parsed into
/// this transport type, then a handler decides how to rewrite the attached
/// item. Storing the name, raw argument tokens, and call-site span separately
/// keeps the dispatch layer simple and avoids reparsing the `#...(...)` shell.
#[derive(Debug, Clone)]
pub struct AttrMacroInvocation {
    /// Macro identifier immediately following `#`.
    pub name: String,
    /// Raw tokens found inside the invocation parentheses.
    pub args_tokens: Vec<Token>,
    /// Source location of the leading `#` token.
    pub span: MacroSpan,
}

/// Result of expanding one macro handler against one attached item.
///
/// Macro handlers can contribute output in three different places, which is why
/// expansion does not return a single flat token list.
///
/// ```text
/// #derive(debug)
/// struct Square { w: int, h: int }
///
/// prelude_items:
///   trait Debug { fn debug(self) -> str }
///
/// primary_item:
///   struct Square { w: int, h: int }
///
/// emitted_items:
///   impl Debug for Square { ... }
///   impl Square { fn __str__(...) -> str { ... } fn __repr__(...) -> str { ... } }
/// ```
///
/// The caller is responsible for de-duplicating preludes and stitching all
/// three sections back into the module token stream.
#[derive(Debug, Clone)]
pub struct MacroExpansion {
    /// Helper items that should appear before the rewritten target item.
    pub prelude_items: Vec<Vec<Token>>,
    /// Replacement tokens for the attached item itself.
    pub primary_item: Vec<Token>,
    /// Extra items emitted after the rewritten target item.
    pub emitted_items: Vec<Vec<Token>>,
}

/// Shared interface for built-in attached attribute macro handlers.
///
/// `AttrMacro` is the outer dispatch layer for attribute names such as
/// `#derive(...)`. Each implementation validates the invocation syntax that
/// belongs to that attribute and can chain into more specialized handlers.
pub trait AttrMacro {
    /// Returns the canonical attribute name handled by this implementation.
    fn name(&self) -> &'static str;

    /// Expands one parsed invocation against its attached item.
    ///
    /// Implementations may keep the item unchanged, rewrite it, inject preludes,
    /// and emit helper items after it. Failures should point back to the
    /// invocation or the captured target item.
    fn expand(
        &self,
        invocation: &AttrMacroInvocation,
        target: &CapturedItem,
    ) -> Result<MacroExpansion, MacroError>;
}

/// Shared interface for derives inside `#derive(...)`.
///
/// `#derive(...)` is itself an attribute macro, but each derive name inside the
/// argument list has its own behavior. This trait keeps those behaviors
/// independent so `#derive(debug, ...)` can apply multiple derives in source
/// order.
pub trait DeriveHandler {
    /// Returns the derive name accepted by this handler.
    fn name(&self) -> &'static str;

    /// Rewrites the captured item according to one derive's rules.
    fn expand(&self, target: &CapturedItem) -> Result<MacroExpansion, MacroError>;
}

/// Reduced semantic shape of an item that macro handlers are allowed to inspect.
///
/// Expansion in v1 does not use the full parser. Instead it captures just
/// enough structure to validate supported macro targets and generate helper
/// code.
#[derive(Debug, Clone, PartialEq, Eq)]
enum CapturedItemKind {
    /// Parsed description of a `struct` item.
    Struct(StructDescriptor),
    /// Parsed description of a `trait` item.
    Trait(TraitDescriptor),
}

/// Minimal description of a struct needed by the built-in derive handlers.
///
/// The debug derive only cares about the struct name, generic parameter names,
/// and declared field order. Keeping this smaller than the full AST parser makes
/// macro expansion cheap and deterministic.
#[derive(Debug, Clone, PartialEq, Eq)]
struct StructDescriptor {
    /// Struct identifier.
    name: String,
    /// Generic parameter names in declaration order.
    generics: Vec<String>,
    /// Field names in declaration order, used to build stable debug output.
    fields: Vec<String>,
}

/// Minimal description of a trait target recognized by the macro parser.
///
/// Traits are currently capturable so the expander can issue a targeted error
/// when a macro like `#derive(debug)` is attached to the wrong item kind.
#[derive(Debug, Clone, PartialEq, Eq)]
struct TraitDescriptor {
    /// Trait identifier.
    name: String,
    /// Generic parameter names in declaration order.
    generics: Vec<String>,
}

/// Token-backed item captured after one or more attribute lines.
///
/// This is the hand-off format between parsing helpers and macro handlers. It
/// preserves both the original tokens and a reduced semantic descriptor so
/// handlers can inspect the target without reparsing the raw item each time.
#[derive(Debug, Clone, PartialEq)]
pub struct CapturedItem {
    /// Reduced item classification used during dispatch and validation.
    kind: CapturedItemKind,
    /// Original tokens for the captured item.
    tokens: Vec<Token>,
    /// Source location of the first token in the captured item.
    span: MacroSpan,
}

/// Lexes raw source and expands any built-in attached macros it contains.
///
/// This helper exists for callers that naturally start from source text instead
/// of a pre-tokenized stream. It keeps the public API ergonomic while routing
/// all real expansion logic through [`expand_tokens`].
///
/// ```text
/// input source:
///   #derive(debug)
///   struct Square { w: int }
///
/// output token stream:
///   trait Debug { ... }
///   struct Square { w: int }
///   impl Debug for Square { ... }
///   impl Square { fn __str__(...) -> str { ... } fn __repr__(...) -> str { ... } }
/// ```
pub fn expand_source_tokens(source: &str) -> Result<Vec<Token>, MacroError> {
    expand_tokens(lex_all(source))
}

/// Expands built-in attached macros inside an existing token stream.
///
/// This is the module's main entry point. It walks the token stream once,
/// copies non-macro tokens through unchanged, and rewrites each contiguous block
/// of attribute macros plus its attached item into ordinary Lang items.
///
/// ```text
/// source tokens:
///   #derive(debug)
///   struct Square { w: int, h: int }
///
/// expanded tokens:
///   trait Debug { fn debug(self) -> str }
///   struct Square { w: int, h: int }
///   impl Debug for Square { fn debug(self) -> str { "Square { w: " + str(self.w) + ", h: " + str(self.h) + " }" } }
///   impl Square { fn __str__(self) -> str { self.debug() } fn __repr__(self) -> str { self.debug() } }
/// ```
///
/// Prelude items are de-duplicated by token signature so multiple uses of the
/// same derive in one module inject shared helpers only once.
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

/// Routes one parsed attribute invocation to the matching built-in handler.
///
/// Keeping dispatch in one function makes the supported macro surface explicit
/// and gives unknown names a single consistent error path.
fn expand_attr_macro(
    invocation: &AttrMacroInvocation,
    target: &CapturedItem,
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

/// Built-in handler for `#derive(...)`.
///
/// This layer parses the derive list and then delegates each derive name to a
/// [`DeriveHandler`]. The separation keeps `#derive(...)` responsible only for
/// attribute-level syntax while per-derive code generation lives elsewhere.
struct DeriveAttrMacro;

impl AttrMacro for DeriveAttrMacro {
    /// Returns the attribute name handled by this dispatcher.
    fn name(&self) -> &'static str {
        "derive"
    }

    /// Applies every derive inside one `#derive(...)` invocation in source order.
    fn expand(
        &self,
        invocation: &AttrMacroInvocation,
        target: &CapturedItem,
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

/// Resolves a derive name from `#derive(...)` into its built-in handler.
///
/// v1 supports only the names hard-coded here. That keeps the derive surface
/// obvious and prevents later stages from having to deal with unresolved derive
/// requests.
fn derive_handler(name: &str, span: &MacroSpan) -> Result<Box<dyn DeriveHandler>, MacroError> {
    match name {
        "debug" => Ok(Box::new(DebugDeriveHandler)),
        other => Err(MacroError {
            message: format!("Unknown derive '{}'.", other),
            span: span.clone(),
        }),
    }
}

/// Implements the built-in `debug` derive for structs.
///
/// The handler emits three pieces of code:
///
/// ```text
/// trait Debug { fn debug(self) -> str }
/// impl Debug for MyType { fn debug(self) -> str { ... } }
/// impl MyType { fn __str__(self) -> str { self.debug() } fn __repr__(self) -> str { self.debug() } }
/// ```
///
/// That gives Lang code one canonical string renderer plus the `__str__` and
/// `__repr__` hooks expected by the Python runtime.
struct DebugDeriveHandler;

impl DeriveHandler for DebugDeriveHandler {
    /// Returns the derive name implemented by this handler.
    fn name(&self) -> &'static str {
        "debug"
    }

    /// Generates the `Debug` trait, trait impl, and inherent string helpers.
    ///
    /// ```text
    /// input:
    ///   #derive(debug)
    ///   struct Square { w: int, h: int }
    ///
    /// runtime output:
    ///   Square { w: 3, h: 4 }
    /// ```
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

/// Parses one `#name(...)` attribute invocation starting at `tokens[start]`.
///
/// This helper owns the low-level token matching for the attribute shell so the
/// rest of the expander can work with a normalized [`AttrMacroInvocation`].
///
/// ```text
/// accepted input:
///   #derive(debug, clone)
///
/// failure cases guarded here:
///   #derive debug)
///   #derive(debug
///   #123(debug)
/// ```
fn parse_attr_invocation(
    tokens: &[Token],
    start: usize,
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

/// Captures the `struct` or `trait` that immediately follows one or more
/// attribute lines.
///
/// This function is the boundary between "macro syntax" and "item syntax". It
/// guarantees that handlers always receive a complete item body rather than a
/// partial token slice.
///
/// ```text
/// #derive(debug)
/// struct Square {
///     w: int
/// }
///
/// The returned slice starts at `struct` and ends after the matching `}`.
/// ```
fn capture_attached_item(
    tokens: &[Token],
    start: usize,
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

/// Converts a captured token slice into the reduced item model used by handlers.
///
/// This reparsing step runs after every derive so later derives see the current
/// primary item exactly as it will continue through expansion.
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

/// Parses the reduced struct metadata needed by derive handlers.
///
/// The parser intentionally records field names only. Field types are consumed
/// for validation and cursor advancement, but they are not retained because the
/// current derives do not inspect them.
fn parse_struct_descriptor(
    cursor: &mut TokenCursor<'_>,
    span: &MacroSpan,
) -> Result<StructDescriptor, MacroError> {
    let name = cursor.expect_identifier("Expected a struct name.", span)?;
    let generics = cursor.parse_generic_names(span)?;
    cursor.expect_kind(
        Kind::L_BRACKET,
        "Expected a struct body after struct name.",
        span,
    )?;

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

    Ok(StructDescriptor {
        name,
        generics,
        fields,
    })
}

/// Parses just enough of a trait declaration to classify it as a macro target.
///
/// The expander keeps trait support minimal because no built-in derive currently
/// rewrites trait bodies. The descriptor exists mainly so handlers can reject
/// unsupported targets with a precise message.
fn parse_trait_descriptor(
    cursor: &mut TokenCursor<'_>,
    span: &MacroSpan,
) -> Result<TraitDescriptor, MacroError> {
    let name = cursor.expect_identifier("Expected a trait name.", span)?;
    let generics = cursor.parse_generic_names(span)?;
    cursor.expect_kind(
        Kind::L_BRACKET,
        "Expected a trait body after trait name.",
        span,
    )?;

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

/// Parses the comma-separated derive names inside `#derive(...)`.
///
/// The returned order matters because derive handlers are applied sequentially.
/// That lets one derive rewrite the primary item before the next derive sees it.
fn parse_derive_names(tokens: &[Token], span: &MacroSpan) -> Result<Vec<String>, MacroError> {
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

/// Formats generic parameter names for an `impl` or item declaration header.
///
/// ```text
/// []        -> ""
/// ["T"]     -> "<T>"
/// ["K","V"] -> "<K, V>"
/// ```
fn generic_decl(generics: &[String]) -> String {
    if generics.is_empty() {
        String::new()
    } else {
        format!("<{}>", generics.join(", "))
    }
}

/// Formats a type reference using the declared generic parameter names.
///
/// This is used when generated code needs to refer back to the original target
/// type, for example `Square<T>` in derived impl blocks.
fn type_ref_source(name: &str, generics: &[String]) -> String {
    if generics.is_empty() {
        name.to_string()
    } else {
        format!("{}<{}>", name, generics.join(", "))
    }
}

/// Builds the Lang expression used by `#derive(debug)` to render a struct.
///
/// The expression preserves field declaration order so display output stays
/// stable across parser, transpiler, and runtime tests.
///
/// ```text
/// build_debug_expr("Square", ["w", "h"])
/// => "Square { w: " + str(self.w) + ", h: " + str(self.h) + " }"
///
/// runtime output for `Square { w: 3, h: 4 }`
/// => Square { w: 3, h: 4 }
/// ```
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

/// Fully lexes a source snippet into tokens.
///
/// Macro handlers use this to turn generated Lang source fragments back into
/// the token representation expected by the rest of the expansion pipeline.
fn lex_all(source: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(source.to_string());
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next() {
        tokens.push(token);
    }

    tokens
}

/// Produces a stable signature for a generated item based on token kind/value pairs.
///
/// Prelude items are compared by this signature so identical generated helpers,
/// such as the `Debug` trait, are injected only once per module.
fn token_signature(tokens: &[Token]) -> String {
    tokens
        .iter()
        .map(|token| format!("{:?}:{}", token.kind, token.value))
        .collect::<Vec<String>>()
        .join("|")
}

/// Small read-only cursor used by the macro parser's token-level helpers.
///
/// The full parser has its own machinery. This cursor keeps macro expansion
/// independent from that machinery while still giving helper functions a clear
/// way to consume and validate token sequences.
struct TokenCursor<'a> {
    /// Token slice being traversed.
    tokens: &'a [Token],
    /// Index of the next unread token.
    index: usize,
}

impl<'a> TokenCursor<'a> {
    /// Creates a cursor positioned at the start of `tokens`.
    fn new(tokens: &'a [Token]) -> Self {
        TokenCursor { tokens, index: 0 }
    }

    /// Returns the next token and advances the cursor by one position.
    fn next(&mut self) -> Option<&'a Token> {
        let token = self.tokens.get(self.index)?;
        self.index += 1;
        Some(token)
    }

    /// Returns the next token without advancing the cursor.
    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.index)
    }

    /// Checks whether the next token matches the requested kind.
    fn peek_kind(&self, kind: Kind) -> bool {
        matches!(self.peek(), Some(token) if token.kind == kind)
    }

    /// Consumes consecutive newline tokens.
    ///
    /// Derive lists allow line breaks between names, so this helper keeps that
    /// whitespace policy localized.
    fn skip_newlines(&mut self) {
        while self.peek_kind(Kind::NEW_LINE) {
            self.next();
        }
    }

    /// Consumes commas, semicolons, and newline separators between declarations.
    ///
    /// Struct fields can be separated by any of those tokens in Lang source, so
    /// descriptor parsing normalizes them here instead of branching repeatedly.
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

    /// Consumes and returns the next identifier token.
    ///
    /// On failure this reports the found token kind, which makes malformed
    /// macro targets much easier to debug than a generic "expected identifier".
    fn expect_identifier(&mut self, message: &str, span: &MacroSpan) -> Result<String, MacroError> {
        match self.next() {
            Some(Token {
                kind: Kind::IDENTIFIER,
                value,
                ..
            }) => Ok(value.clone()),
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

    /// Requires the next token to have the given kind.
    ///
    /// This is the basic structural assertion used throughout the item parsers.
    fn expect_kind(
        &mut self,
        kind: Kind,
        message: &str,
        span: &MacroSpan,
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

    /// Parses a simple `<T, U, ...>` generic parameter list.
    ///
    /// The macro system only needs parameter names, not bounds or defaults, so
    /// this helper intentionally accepts the small subset used by the current
    /// language grammar and generated code.
    fn parse_generic_names(&mut self, span: &MacroSpan) -> Result<Vec<String>, MacroError> {
        if !self.peek_kind(Kind::LT) {
            return Ok(Vec::new());
        }

        self.next();
        let mut generics = Vec::new();

        loop {
            generics.push(self.expect_identifier("Expected a generic parameter name.", span)?);

            match self.peek() {
                Some(Token {
                    kind: Kind::COMMA, ..
                }) => {
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

    /// Consumes a type reference without building a semantic representation.
    ///
    /// Macro derives currently only need to know that a field has some type and
    /// where that type ends. This helper advances through nested generic
    /// brackets until the next field separator or closing brace.
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

/// Targeted tests for the small token parsers in this module.
#[cfg(test)]
mod tests {
    use super::{lex_all, parse_derive_names, MacroSpan};

    /// Confirms that `#derive(a, b, c)` preserves the declared derive order.
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
