/// Transpile to Python
/// This mode implements the Transpile trait for the AST Node, Stmt and Node
/// It converts the AST to a string representation of the code in Python
use crate::ast::{Node, AST};
use itertools::Itertools;
use regex::Regex;
use std::{convert, fs};
use std::path::Path;

use crate::hyvnts_tools::strings::StrUtils;

trait Transpile {
    fn transpile(&self) -> String;
}




// * Features / Flags
// TODO: A way to toggle auto-vars and other features
/// If true, auto-vars will be enabled.
/// This is be buggy, thus disabled by default.
/// Auto-vars are variables that are automatically defined when they're used.
/// Ex. `sum(a, b)` will translate into `( _: = sum(a, b) )`
const AUTO_VARS: bool = false;

/// If true, sequences will be automatically converted to iterators.
/// No need to mention this is really bad for performance.
/// Ex `(1, 2, 3)` -> `Iterator([1, 2, 3])`
const AUTO_SEQUENCE_TO_ITERATOR: bool = true;

/// Folder containing custom builtins modules for Python
const BUILTINS_PATH: &str = "./src/transpilers/python/custom_builtins/";

pub fn transpile(ast: &AST) -> String {
    let mut code = String::new();

    // Include custom builtins
    if !Path::new(BUILTINS_PATH).exists() {
        eprintln!(
            "[Python Transpiler] Custom builtins not found at {}",
            BUILTINS_PATH
        );
        eprintln!("[Python Transpiler] Skipping custom builtins");
    } else {
        code.push_str("# Custom builtins\n");
        code.push_str("import sys\n");
        code.push_str(format!("sys.path.append('{}')\n", BUILTINS_PATH).as_str());

        // Iterate over the files in the directory
        for entry in fs::read_dir(BUILTINS_PATH).expect("Failed to read  custom builtin directory")
        {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();

            // Check if it's a  python file and doesn't start
            if path.is_file()
                && path.extension().unwrap_or_default() == "py"
                && !path.file_stem().unwrap().to_str().unwrap().starts_with('_')
            {
                // Include the file
                code.push_str(&format!(
                    "from {} import * \n",
                    path.file_stem().unwrap().to_str().unwrap()
                ));
            }
        }

        code.push_str("# End of custom builtins\n\n");
    }

    for node in ast.get_scope() {
        code.push_str(&format!("{}\n", node.transpile()));
    }

    code
}

impl Transpile for Node {
    fn transpile(&self) -> String {
        use Node::*;
        match self {
            Empty => "".to_string(),
            Newline => "\n".to_string(),
            Comment(comment) => {
                // Multiline comments
                if comment.contains("\n") {
                    comment
                        .clone()
                        .lines()
                        .map(|line| format!("# {}\n", line))
                        .collect()
                // Single line comments
                } else {
                    format!("# {}\n", comment)
                }
            }

            Number(n) => n.to_string(),
            Identifier(ident) => ident.to_string(),
            Str(value) => {
                // * String formatting
                // Simple
                // "Hello, {name}" -> f"Hello, {name}"

                // Complex
                // "Hello %name" -> f"Hello {name}"
                // "Some {var:args}" -> f"Some {var:args}"

                // Captures "%var_name" and "%$signal_name" in strings
                let simple_format_re: Regex =
                    Regex::new(r"%([$]?[a-zA-Z0-9_]+)").expect("Simple formatting regex failed");

                // Captures "{name} and {name:args}" in strings
                let complex_format_re: Regex =
                    Regex::new(r"\{\s*([$]?[a-zA-Z0-9_]+)\s*(:\s*.*)?\s*}")
                        .expect("Complex formatting regex failed");

                let mut fstring: bool = false;

                // Apply simple formatting
                let mut new_val: String =
                    simple_format_re.replace_all(value, r"{${1}}").to_string();
                if new_val != *value {
                    fstring = true;
                }

                // Apply complex formatting
                new_val = complex_format_re
                    .replace_all(&new_val, r"{${1}${2}}")
                    .to_string();
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
                format!("{}.{}", object.transpile(), member.transpile())
            }

            Group(expr) => format!(
                "({})",
                match expr {
                    Some(e) => e.transpile(),
                    None => "".to_string(),
                }
            ),

            BinOp { lhs, op, rhs } => {
                format!("{} {} {}", lhs.transpile(), op, rhs.transpile())
            }

            UnaryOp { op, expr } => format!("{}{}", op, expr.transpile()),

            NamedArg(name, value) => format!("{} = {}", name, value.transpile()),

            Array(values) => {
                format!("[{}]", values.transpile())
            }

            Index { object, index } => {
                format!("{}[{}]", object.transpile(), index.transpile())
            }

            Sequence(values) => {

                let mut code = if AUTO_SEQUENCE_TO_ITERATOR {
                    String::from("Iterator([")
                } else {
                    String::from("")
                };
                for (i, value) in values.iter().enumerate() {
                    code.push_str(&value.transpile());
                    if i < values.len() - 1 {
                        code.push_str(", ");
                    }
                }

                if AUTO_SEQUENCE_TO_ITERATOR {
                    code.push_str("])");
                }

                code
            }

            WrappedSequence(values) => {
                let mut code = String::from("(");
                for (i, value) in values.iter().enumerate() {
                    code.push_str(&value.transpile());
                    if i < values.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str(")");
                code
            }

            FnBody(nodes) => {
                let mut code = String::new();

                if nodes.is_empty() {
                    return format!("\tpass\n");
                }

                for node in nodes[0..nodes.len() - 1].iter() {
                    code.push_str(format!("\t{}", node.transpile()).as_str());
                }

                // Last node is return
                match nodes.last() {
                    Some(node) => {
                        if node == &Node::Return(Box::new(Node::Empty)) {
                            code.push_str(format!("\t{}", node.transpile()).as_str());
                        } else {
                            code.push_str(format!("\treturn {}", node.transpile()).as_str());
                        }
                    }
                    None => {}
                }

                code
            }

            Block(nodes) => {
                let mut code = String::new();

                if nodes.is_empty() {
                    return format!("\tpass\n");
                }

                for node in nodes {
                    code.push_str(format!("\t{}\n", node.transpile()).as_str());
                }

                code
            }

            // FunctionCall
            // name(args)
            //
            // If enabled Auto-var: Result of the function is assigned to `_`
            FunctionCall { object: name, args } => {

                if AUTO_VARS {
                    format!("(_ := {}{})", name.transpile(), args.transpile()) // Somehow parenthesis are not needed here since args it's a sequence
                } else {
                    format!("{}{}", name.transpile(), args.transpile())
                }
            }

            Return(value) => format!("\treturn {}", value.transpile()),

            Dict { keys, values } => {
                let mut code: String = String::from("{");
                for (i, key) in keys.iter().enumerate() {
                    code.push_str(
                        format!("{}: {}", key.transpile(), values[i].transpile()).as_str(),
                    );
                    if i < keys.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("}");
                code
            }

            Alias(name) => format!("as {}", name.transpile()),

            Len(obj) => format!("len({})", obj.transpile()),

            Range {
                start,
                end,
                inclusive,
            } => {
                format!(
                    "Iterator(tuple(range({}, {}{})))",
                    start.transpile(),
                    end.transpile(),
                    if *inclusive { "+1" } else { "" }
                )
            }

            // * Distribution
            // Distribution pipes args into functions
            // So, `| a, b -> fn1, fn1;` -> `fn1(a, b); fn2(a, b);`
            Distribution { args, recipients } => {
                let mut code = String::new();

                let args_str = args
                    .iter()
                    .map(|arg| arg.transpile())
                    .collect::<Vec<String>>()
                    .join(", ");

                for func in recipients.iter() {
                    code.push_str(&format!("{}({});\n", func.transpile(), args_str));
                }

                code
            }

            // * Iterative Distribution
            // Iterative Distribution pipes iteration of args into functions
            // So, `|> nums, chars -> fn1, fn1;` -> for num, char in zip(nums, chars): fn1(num, char); fn2(num, char);
            // We iterate over the args and pass them to the functions
            IterDistribution { args, recipients } => {
                let mut code = String::new();

                let args_str = args
                    .iter()
                    .map(|arg| {
                        // Some python magic here --
                        // If values is not iterable in python we will put it inside a tuple
                        let val = arg.transpile();
                        format!(
                            "({} if hasattr({}, '__iter__') else itertools.cycle([{}]))",
                            val, val, val
                        )
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                // Import itertools, to use cycle
                code.push_str("import itertools\n");

                code.push_str(format!("for _lang_args in zip({}):\n", args_str).as_str());
                for func in recipients.iter() {
                    code.push_str(&format!("\t{}(*_lang_args);\n", func.transpile()));
                }

                code
            }

            Decorator { name, args } => {
                format!(
                    "@{}{}",
                    name.transpile(),
                    if args.is_some() {
                        args.clone().unwrap().transpile()
                    } else {
                        "".to_string()
                    }
                )
            }

            Lambda { args, body } => {
                let args_str = args.transpile();
                format!(
                    "lambda {}: {}",
                    args_str[1..args_str.len() - 1].to_string(),
                    body.transpile()
                )
            }

            Signal(name) => format!("{}.value", name),

            Conditional {
                condition,
                body,
                elifs,
                else_body,
            } => {
                let mut code = String::new();

                // if
                code.push_str(
                    format!("if {}:\n{}\n", condition.transpile(), body.transpile()).as_str(),
                );

                // elifs
                for (cond, body) in elifs.iter() {
                    code.push_str(
                        format!("elif {}:{}\n", cond.transpile(), body.transpile()).as_str(),
                    );
                }

                // else
                if let Some(else_body) = else_body {
                    code.push_str(format!("else:{}\n", else_body.transpile()).as_str());
                }

                code
            }

            Python(code) => code.clone(),
            Assign {
                identifiers,
                values,
                op,
            } => {
                let mut code = String::new();

                for (i, ident) in identifiers.iter().enumerate() {
                    code.push_str(
                        format!("{} {} {}\n", ident.transpile(), op, values[i].transpile())
                            .as_str(),
                    );
                }

                code
            }

            SignalDef {
                name,
                value,
                dependencies,
            } => {
                format!(
                    "{0} = Signal(lambda {0}: {1}, {2})",
                    name,
                    value.transpile(),
                    dependencies.iter().join(", ")
                )
            }

            SignalUpdate {
                name,
                value,
                dependencies,
            } => {
                format!(
                    "{0}.update(lambda {0}: {1}, {2})",
                    name,
                    value.transpile(),
                    dependencies.iter().join(", ")
                )
            }

            // * Deconstruction
            // Deconstruction is a way to assign values to multiple variables at once
            // {a, b} = {Node} -> a = Node.a and b = Node.b
            // {name="John"} = {Node} -> name = Node.get("name", "John")
            Deconstruction {
                identifiers,
                value,
                default_values,
            } => {
                let mut code = String::new();
                let value_str = value.transpile();

                for (i, ident) in identifiers.iter().enumerate() {
                    let default = if i < default_values.len() {
                        format!(", {}", default_values[i].transpile())
                    } else {
                        "".to_string()
                    };

                    code.push_str(
                        format!(
                            "{} = {}.get('{}'{})\n",
                            ident.transpile(),
                            value_str,
                            ident.transpile(),
                            default
                        )
                        .as_str(),
                    );
                }

                code
            }

            FunctionDef { name, args, body } => {
                format!("def {}{}:{}", name, args.transpile(), body.transpile())
            }

            ReactiveStmt {
                block,
                dependencies,
            } => {
                // Create function, which is called when dependencies change
                let mut code = String::from(format!("def __reactive_stmt():\n"));
                for stmt in block.iter() {
                    code.push_str(format!("\t{}\n", stmt.transpile()).as_str());
                }

                // Create reactive statement
                code.push_str(
                    format!(
                        "ReactiveStmt(__reactive_stmt, {})",
                        dependencies.iter().join(", ")
                    )
                    .as_str(),
                );
                code
            }

            Continue => "continue".to_string(),
            Break => "break".to_string(),

            // * Loop
            Loop(body) => {
                format!("while True:\n{}", body.transpile())
            }

            // * For Loop
            ForLoop {
                item,
                iterable,
                body,
            } => {
                format!(
                    "for {} in {}:\n{}",
                    item.transpile(),
                    iterable.transpile(),
                    body.transpile()
                )
            }

            // * While Loop
            // While loop is a loop that repeats the body until the condition is false
            WhileLoop { condition, body } => {
                format!("while {}:\n{}", condition.transpile(), body.transpile())
            }
        }
    }
}
