/// Transpile to Python.
///
/// Design goals:
/// - Centralize indentation in a single emitter (no ad-hoc `\n` in expressions).
/// - Separate statement emission from expression formatting.
/// - Support implicit return in function bodies.
/// - Keep global builtins imports for runtime behavior.
use crate::ast::{ Node, AST };
use itertools::Itertools;
use regex::Regex;
use std::fs;
use std::path::Path;

use crate::hyvnts_tools::strings::StrUtils;

/// Folder containing custom builtins modules for Python
const BUILTINS_PATH: &str = "./src/transpilers/python/custom_builtins/";

/// Helper that owns the output and handles indentation.
struct PyEmitter {
    lines: Vec<String>,
    indent: usize,
    indent_char: String,
}

impl PyEmitter {
    /// Create a new emitter with a given indent unit.
    fn new(indent_char: String) -> Self {
        PyEmitter {
            lines: Vec::new(),
            indent: 0,
            indent_char,
        }
    }

    /// Write a line with current indentation.
    fn line<S: AsRef<str>>(&mut self, line: S) {
        let line = line.as_ref();
        let indent = self.indent_char.repeat(self.indent);
        self.lines.push(format!("{}{}", indent, line));
    }

    /// Write a blank line.
    fn blank(&mut self) {
        self.lines.push(String::new());
    }

    /// Run a closure at +1 indentation, then restore.
    fn indented<F: FnOnce(&mut PyEmitter)>(&mut self, f: F) {
        self.indent += 1;
        f(self);
        if self.indent > 0 {
            self.indent -= 1;
        }
    }

    /// Consume the emitter and return the final code.
    fn finish(self) -> String {
        self.lines.join("\n")
    }
}

/// In charge of managing the transpilation process.
pub struct Transpiler {
    indent_char: String,

    // * Features / Flags
    // TODO: A way to toggle auto-vars and other features
    /// If true, auto-vars will be enabled.
    /// This is be buggy, thus disabled by default.
    /// Auto-vars are variables that are automatically defined when they're used.
    /// Ex. `sum(a, b)` will translate into `( _: = sum(a, b) )`
    pub auto_vars: bool,

    /// If true, sequences will be automatically converted to iterators.
    /// No need to mention this is really bad for performance.
    /// Ex `(1, 2, 3)` -> `Iterator([1, 2, 3])`
    pub auto_sequence_to_iterator: bool,
}

impl Transpiler {
    pub fn new() -> Self {
        Transpiler {
            indent_char: "\t".to_string(),
            auto_vars: false,
            auto_sequence_to_iterator: true,
        }
    }

    /// Convert an AST into Python source.
    /// This function emits a preamble of custom builtins imports if available.
    pub fn transpile(&mut self, ast: &AST) -> String {
        let mut emitter = PyEmitter::new(self.indent_char.clone());

        // Include custom builtins
        // If the builtins directory doesn't exist, skip it
        if !Path::new(BUILTINS_PATH).exists() {
            eprintln!("[Python Transpiler] Custom builtins not found at: {}", BUILTINS_PATH);
            eprintln!("[Python Transpiler] Skipping custom builtins");
        } else {
            emitter.line("# Custom builtins");
            emitter.line("import sys");
            emitter.line(format!("sys.path.append('{}')", BUILTINS_PATH));

            // Iterate over the files in the directory
            for entry in fs
                ::read_dir(BUILTINS_PATH)
                .expect("Failed to read  custom builtin directory")
            {
                let entry: fs::DirEntry = entry.expect("Failed to read entry");
                let path: std::path::PathBuf = entry.path();

                // Check if it's a  python file and doesn't start
                if
                    path.is_file() &&
                    path.extension().unwrap_or_default() == "py" &&
                    !path.file_stem().unwrap().to_str().unwrap().starts_with('_')
                {
                    // Include the file
                    emitter.line(format!("from {} import * ", path.file_stem().unwrap().to_str().unwrap()));
                }
            }

            emitter.line("# End of custom builtins");
            emitter.blank();
        }

        for node in ast.get_scope() {
            self.emit_stmt(&mut emitter, &node);
        }

        emitter.finish()
    }

    /// Emit a statement. Expressions are only written if they are top-level.
    fn emit_stmt(&self, emitter: &mut PyEmitter, node: &Node) {
        use Node::*;
        match node {
            Empty => {}
            Newline => emitter.blank(),
            Comment(comment) => self.emit_comment(emitter, comment),

            Assign { identifiers, values, op } => {
                for (i, ident) in identifiers.iter().enumerate() {
                    let ident_str = self.emit_expr(emitter, ident);
                    let value_str = self.emit_expr(emitter, &values[i]);
                    emitter.line(format!("{} {} {}", ident_str, op, value_str));
                }
            }

            Return(value) => {
                let value_str = self.emit_expr(emitter, value);
                emitter.line(format!("return {}", value_str));
            }

            Deconstruction { identifiers, value, default_values } => {
                let value_str = self.emit_expr(emitter, value);
                for (i, ident) in identifiers.iter().enumerate() {
                    let ident_str = self.emit_expr(emitter, ident);
                    let default = if i < default_values.len() {
                        format!(", {}", self.emit_expr(emitter, &default_values[i]))
                    } else {
                        String::new()
                    };

                    emitter.line(format!(
                        "{} = {}.get('{}'{})",
                        ident_str,
                        value_str,
                        ident_str,
                        default
                    ));
                }
            }

            FunctionDef { name, args, body } => {
                let args_str = self.emit_arg_list(emitter, args);
                emitter.line(format!("def {}{}:", name, args_str));
                emitter.indented(|em| match body.as_ref() {
                    FnBody(nodes) => self.emit_fn_body(em, nodes),
                    Block(nodes) => self.emit_block(em, nodes),
                    other => self.emit_stmt(em, other),
                });
            }

            Conditional { condition, body, elifs, else_body } => {
                let cond_str = self.emit_expr(emitter, condition);
                emitter.line(format!("if {}:", cond_str));
                emitter.indented(|em| self.emit_block_node(em, body));

                for (cond, body) in elifs.iter() {
                    let cond_str = self.emit_expr(emitter, cond);
                    emitter.line(format!("elif {}:", cond_str));
                    emitter.indented(|em| self.emit_block_node(em, body));
                }

                if let Some(else_body) = else_body {
                    emitter.line("else:");
                    emitter.indented(|em| self.emit_block_node(em, else_body));
                }
            }

            Loop(body) => {
                emitter.line("while True:");
                emitter.indented(|em| self.emit_block_node(em, body));
            }

            ForLoop { item, iterable, body } => {
                let item_str = self.emit_expr(emitter, item);
                let iter_str = self.emit_expr(emitter, iterable);
                emitter.line(format!("for {} in {}:", item_str, iter_str));
                emitter.indented(|em| self.emit_block_node(em, body));
            }

            WhileLoop { condition, body } => {
                let cond_str = self.emit_expr(emitter, condition);
                emitter.line(format!("while {}:", cond_str));
                emitter.indented(|em| self.emit_block_node(em, body));
            }

            SignalDef { name, value, dependencies } => {
                let line = self.emit_signal_def(emitter, name, value, dependencies);
                emitter.line(line);
            }

            SignalUpdate { name, value, dependencies } => {
                let line = self.emit_signal_update(emitter, name, value, dependencies);
                emitter.line(line);
            }

            ReactiveStmt { block, dependencies } => {
                emitter.line("def __reactive_stmt():");
                emitter.indented(|em| self.emit_block(em, block));
                emitter.line(self.emit_reactive_stmt(dependencies));
            }

            Distribution { args, recipients } => {
                let line = self.emit_distribution_expr(emitter, args, recipients);
                emitter.line(line);
            }

            IterDistribution { args, recipients } => {
                let args_str = args
                    .iter()
                    .map(|arg| {
                        let val = self.emit_expr(emitter, arg);
                        format!(
                            "({} if hasattr({}, '__iter__') else itertools.cycle([{}]))",
                            val,
                            val,
                            val
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                emitter.line("import itertools");
                emitter.line(format!("for _lang_args in zip({}):", args_str));
                emitter.indented(|em| {
                    for func in recipients.iter() {
                        let func_str = self.emit_expr(em, func);
                        em.line(format!("{}(*_lang_args)", func_str));
                    }
                });
            }

            Continue => emitter.line("continue"),
            Break => emitter.line("break"),

            Python(code) => self.emit_python_raw(emitter, code),

            Block(nodes) => self.emit_block(emitter, nodes),
            FnBody(nodes) => self.emit_block(emitter, nodes),
            Decorator { name, args } => {
                let deco = if args.is_some() {
                    format!("@{}{}", self.emit_expr(emitter, name), self.emit_arg_list(emitter, args.as_ref().unwrap()))
                } else {
                    format!("@{}", self.emit_expr(emitter, name))
                };
                emitter.line(deco);
            }

            _ if self.is_expr_node(node) => {
                let expr = self.emit_expr(emitter, node);
                emitter.line(expr);
            }

            _ => {}
        }
    }

    /// Emit an expression as a Python string with no trailing newline.
    fn emit_expr(&self, emitter: &mut PyEmitter, node: &Node) -> String {
        use Node::*;
        match node {
            Number(n) => n.to_string(),
            Identifier(ident) => ident.to_string(),
            Str(value) => {
                // * String formatting
                // Simple: "Hello, %name" -> f"Hello {name}"
                // Complex: "Hello {name}" -> f"Hello {name}"
                let simple_format_re: Regex = Regex::new(r"%([$]?[a-zA-Z0-9_]+)").expect(
                    "Simple formatting regex failed"
                );
                let complex_format_re: Regex = Regex::new(
                    r"\{\s*([$]?[a-zA-Z0-9_]+)\s*(:\s*.*)?\s*}"
                ).expect("Complex formatting regex failed");

                let mut fstring: bool = false;
                let mut new_val: String = simple_format_re
                    .replace_all(value, r"{${1}}")
                    .to_string();
                if new_val != *value {
                    fstring = true;
                }
                new_val = complex_format_re.replace_all(&new_val, r"{${1}${2}}").to_string();
                if complex_format_re.is_match(&new_val) {
                    fstring = true;
                }

                if fstring {
                    format!("f\"{}\"", new_val)
                } else {
                    format!("\"{}\"", value)
                }
            }
            Bool(b) => b.to_string().capitalize(),

            MemberAccess { object, member } => {
                format!("{}.{}", self.emit_expr(emitter, object), self.emit_expr(emitter, member))
            }
            Group(expr) => format!(
                "({})",
                match expr {
                    Some(e) => self.emit_expr(emitter, e),
                    None => String::new(),
                }
            ),
            BinOp { lhs, op, rhs } => {
                format!(
                    "{} {} {}",
                    self.emit_expr(emitter, lhs),
                    op,
                    self.emit_expr(emitter, rhs)
                )
            }
            UnaryOp { op, expr } => format!("{}{}", op, self.emit_expr(emitter, expr)),
            NamedArg(name, value) => format!("{} = {}", name, self.emit_expr(emitter, value)),

            Array(values) => {
                let inner = self.emit_expr(emitter, values);
                if self.auto_sequence_to_iterator {
                    format!("Iterator([{}])", inner)
                } else {
                    format!("[{}]", inner)
                }
            }

            Index { object, index } => {
                format!("{}[{}]", self.emit_expr(emitter, object), self.emit_expr(emitter, index))
            }

            Sequence(values) => {
                values
                    .iter()
                    .map(|value| self.emit_expr(emitter, value))
                    .collect::<Vec<String>>()
                    .join(", ")
            }

            WrappedSequence(values) => {
                let inner = values
                    .iter()
                    .map(|value| self.emit_expr(emitter, value))
                    .collect::<Vec<String>>()
                    .join(", ");

                if self.auto_sequence_to_iterator {
                    format!("Iterator([{}])", inner)
                } else {
                    format!("({})", inner)
                }
            }

            FunctionCall { object, args } => {
                let call = format!("{}{}", self.emit_expr(emitter, object), self.emit_arg_list(emitter, args));
                if self.auto_vars {
                    format!("(_ := {})", call)
                } else {
                    call
                }
            }

            Dict { keys, values } => {
                let mut code: String = String::from("{");
                for (i, key) in keys.iter().enumerate() {
                    code.push_str(
                        format!(
                            "{}: {}",
                            self.emit_expr(emitter, key),
                            self.emit_expr(emitter, &values[i])
                        ).as_str()
                    );
                    if i < keys.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push('}');
                code
            }

            Alias(name) => format!("as {}", self.emit_expr(emitter, name)),
            Len(obj) => format!("len({})", self.emit_expr(emitter, obj)),

            Range { start, end, inclusive } => {
                format!(
                    "Iterator(tuple(range({}, {}{})))",
                    self.emit_expr(emitter, start),
                    self.emit_expr(emitter, end),
                    if *inclusive { "+1" } else { "" }
                )
            }

            Lambda { args, body } => {
                let mut args_str = self.emit_arg_list(emitter, args);
                if args_str.starts_with('(') && args_str.ends_with(')') {
                    args_str = args_str[1..args_str.len() - 1].to_string();
                }
                format!("lambda {}: {}", args_str, self.emit_expr(emitter, body))
            }

            Signal(name) => format!("{}.value", name),

            Distribution { args, recipients } => self.emit_distribution_expr(emitter, args, recipients),

            other => panic!("Node {:?} is not an expression", other),
        }
    }

    /// Emit a block of statements; empty blocks become `pass`.
    fn emit_block(&self, emitter: &mut PyEmitter, nodes: &[Node]) {
        if nodes.is_empty() {
            emitter.line("pass");
            return;
        }

        for node in nodes.iter() {
            self.emit_stmt(emitter, node);
        }
    }

    /// Emit a function body with implicit return for the last expression.
    fn emit_fn_body(&self, emitter: &mut PyEmitter, nodes: &[Node]) {
        let last_index = nodes
            .iter()
            .rposition(|node| !matches!(node, Node::Empty | Node::Newline));

        let last_index = match last_index {
            Some(idx) => idx,
            None => {
                emitter.line("pass");
                return;
            }
        };

        for (i, node) in nodes.iter().enumerate() {
            if i != last_index {
                self.emit_stmt(emitter, node);
                continue;
            }

            if matches!(node, Node::Return(_)) {
                self.emit_stmt(emitter, node);
            } else if self.is_expr_node(node) {
                let expr = self.emit_expr(emitter, node);
                emitter.line(format!("return {}", expr));
            } else {
                self.emit_stmt(emitter, node);
            }
        }
    }

    /// Emit any node that should behave like a block.
    fn emit_block_node(&self, emitter: &mut PyEmitter, node: &Node) {
        match node {
            Node::Block(nodes) => self.emit_block(emitter, nodes),
            Node::FnBody(nodes) => self.emit_block(emitter, nodes),
            other => self.emit_stmt(emitter, other),
        }
    }

    /// Emit a comment; supports multi-line comments.
    fn emit_comment(&self, emitter: &mut PyEmitter, comment: &str) {
        if comment.contains('\n') {
            for line in comment.lines() {
                emitter.line(format!("# {}", line));
            }
        } else {
            emitter.line(format!("# {}", comment));
        }
    }

    /// Emit raw Python code, respecting the current indentation level.
    fn emit_python_raw(&self, emitter: &mut PyEmitter, code: &str) {
        for line in code.lines() {
            if line.trim().is_empty() {
                emitter.blank();
            } else {
                emitter.line(line);
            }
        }
    }

    /// Format arguments as a Python argument list, always wrapped in parentheses.
    fn emit_arg_list(&self, emitter: &mut PyEmitter, args: &Node) -> String {
        match args {
            Node::Group(Some(expr)) => format!("({})", self.emit_expr(emitter, expr)),
            Node::Group(None) => "()".to_string(),
            Node::WrappedSequence(values) | Node::Sequence(values) => {
                let inner = values
                    .iter()
                    .map(|value| self.emit_expr(emitter, value))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("({})", inner)
            }
            other => format!("({})", self.emit_expr(emitter, other)),
        }
    }

    /// Build a distribution expression (no trailing newline).
    fn emit_distribution_expr(
        &self,
        emitter: &mut PyEmitter,
        args: &[Node],
        recipients: &[Node]
    ) -> String {
        let args_str = args
            .iter()
            .map(|arg| self.emit_expr(emitter, arg))
            .collect::<Vec<String>>()
            .join(", ");

        let mut code = String::new();

        if self.auto_vars {
            code.push_str("( _ :=");
        }

        code.push('(');
        for func in recipients.iter() {
            code.push_str(&format!("{}({}), ", self.emit_expr(emitter, func), args_str));
        }

        code.pop();
        code.pop();
        code.push(')');

        if self.auto_vars {
            code.push(')');
        }

        code
    }

    /// Build a signal definition assignment.
    fn emit_signal_def(
        &self,
        emitter: &mut PyEmitter,
        name: &str,
        value: &Node,
        dependencies: &std::collections::HashSet<String>
    ) -> String {
        let deps = dependencies.iter().join(", ");
        if deps.is_empty() {
            format!("{} = Signal(lambda {}: {})", name, name, self.emit_expr(emitter, value))
        } else {
            format!(
                "{} = Signal(lambda {}: {}, {})",
                name,
                name,
                self.emit_expr(emitter, value),
                deps
            )
        }
    }

    /// Build a signal update call.
    fn emit_signal_update(
        &self,
        emitter: &mut PyEmitter,
        name: &str,
        value: &Node,
        dependencies: &std::collections::HashSet<String>
    ) -> String {
        let deps = dependencies.iter().join(", ");
        if deps.is_empty() {
            format!("{}.update(lambda {}: {})", name, name, self.emit_expr(emitter, value))
        } else {
            format!(
                "{}.update(lambda {}: {}, {})",
                name,
                name,
                self.emit_expr(emitter, value),
                deps
            )
        }
    }

    /// Build a reactive statement instantiation.
    fn emit_reactive_stmt(&self, dependencies: &std::collections::HashSet<String>) -> String {
        let deps = dependencies.iter().join(", ");
        if deps.is_empty() {
            "ReactiveStmt(__reactive_stmt)".to_string()
        } else {
            format!("ReactiveStmt(__reactive_stmt, {})", deps)
        }
    }

    /// Return true if a node can be formatted as an expression.
    fn is_expr_node(&self, node: &Node) -> bool {
        matches!(
            node,
            Node::Number(_)
                | Node::Identifier(_)
                | Node::Str(_)
                | Node::Bool(_)
                | Node::MemberAccess { .. }
                | Node::Group(_)
                | Node::BinOp { .. }
                | Node::UnaryOp { .. }
                | Node::NamedArg(_, _)
                | Node::Array(_)
                | Node::Index { .. }
                | Node::Sequence(_)
                | Node::WrappedSequence(_)
                | Node::FunctionCall { .. }
                | Node::Dict { .. }
                | Node::Alias(_)
                | Node::Range { .. }
                | Node::Len(_)
                | Node::Lambda { .. }
                | Node::Signal(_)
        )
    }
}
