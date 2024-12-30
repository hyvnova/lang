/// Transpile to Python
/// This mode implements the Transpile trait for the AST Node, Stmt and Node
/// It converts the AST to a string representation of the code in Python
use crate::ast::{Node, AST};
use itertools::Itertools;
use regex::Regex;
use std::cell::RefCell;
use std::fs;
use std::path::Path;

use crate::hyvnts_tools::strings::StrUtils;

trait Transpile {
    fn transpile(&self, transpiler: &Transpiler) -> String;
}


/// Folder containing custom builtins modules for Python
const BUILTINS_PATH: &str = "./src/transpilers/python/custom_builtins/";


/// In charge or managing all the transpilation process, mainly serves as a way for transpilable objects to talk to each other.
/// Ex. handling scopes and indentation
pub struct Transpiler {
    indent: RefCell<usize>,
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
            indent: RefCell::new(0),
            indent_char: "\t".to_string(),
            auto_vars: false,
            auto_sequence_to_iterator: true,
        }
    }

    pub fn transpile(&mut self, ast: &AST) -> String {
        let mut code = String::new();

        // Include custom builtins
        // If the builtins directory doesn't exist, skip it
        if !Path::new(BUILTINS_PATH).exists() {
            eprintln!(
                "[Python Transpiler] Custom builtins not found at: {}",
                BUILTINS_PATH
            );
            eprintln!("[Python Transpiler] Skipping custom builtins");
            
        // Otherwise, include the builtins
        } else {
            code.push_str("# Custom builtins\n");
            code.push_str("import sys\n");
            code.push_str(format!("sys.path.append('{}')\n", BUILTINS_PATH).as_str());

            // Iterate over the files in the directory
            for entry in fs::read_dir(BUILTINS_PATH).expect("Failed to read  custom builtin directory")
            {
                let entry: fs::DirEntry = entry.expect("Failed to read entry");
                let path: std::path::PathBuf = entry.path();

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

            // * Indentation
            // This really sucks, I wanterd to have a "general" way to know which nodes modify the indent but, I couldn't find a way to do it
            // Macros would be nice here so I don't need to manually check for each node.
            // match node {
            //     Node::Block(_) | Node::FnBody(_) | Node::Conditional { .. } => {
            //         self.indent += 1;
            //     }
            //     _ => { break; }
            // }

            code.push_str(&format!("{}\n", node.transpile(&self)));
        }

        code
    }


    /// Returns the current indent string
    /// Ex. if indent is 2 and indent_char is "\t", this will return "\t\t"
    pub fn indent(&self) -> String {
        self.indent_char.repeat(*self.indent.borrow())
    }

    /// Updates the indent by aadding the given amount
    /// This is a nasty hack to not need a bunch of mutable borrows
    pub fn update_indent(&self, amount: isize) {
        let mut indent = self.indent.borrow_mut();
        *indent = (*indent as isize + amount) as usize;
    }

}

impl Transpile for Node {
    fn transpile(&self, transpiler: &Transpiler) -> String {
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
                format!("{}.{}", object.transpile(&transpiler), member.transpile(&transpiler))
            }

            Group(expr) => format!(
                "({})",
                match expr {
                    Some(e) => e.transpile(&transpiler),
                    None => "".to_string(),
                }
            ),

            BinOp { lhs, op, rhs } => {
                format!("{} {} {}", lhs.transpile(&transpiler), op, rhs.transpile(&transpiler))
            }

            UnaryOp { op, expr } => format!("{}{}", op, expr.transpile(&transpiler)),

            NamedArg(name, value) => format!("{} = {}", name, value.transpile(&transpiler)),

            Array(values) => {
                let mut code = String::new();

                // If auto_sequence_to_iterator is enabled, convert the sequence to an iterator
                if transpiler.auto_sequence_to_iterator {
                    code.push_str("Iterator([");
                } else {
                    code.push_str("[");
                }

                code.push_str(format!("{}", values.transpile(&transpiler)).as_str());

                if transpiler.auto_sequence_to_iterator {
                    code.push_str("])");
                } else {
                    code.push_str("]");
                }

                code
            }

            Index { object, index } => {
                format!("{}[{}]", object.transpile(&transpiler), index.transpile(&transpiler))
            }

            Sequence(values) => {

                let mut code = String::new();

                for (i, value) in values.iter().enumerate() {
                    code.push_str(&value.transpile(&transpiler));
                    if i < values.len() - 1 {
                        code.push_str(", ");
                    }
                }

                code
            }

            WrappedSequence(values) => {
                let mut code = String::new();

                if transpiler.auto_sequence_to_iterator {
                    code.push_str("Iterator([");
                } else {
                    code.push_str("(");
                }

                for (i, value) in values.iter().enumerate() {
                    code.push_str(&value.transpile(&transpiler));
                    if i < values.len() - 1 {
                        code.push_str(", ");
                    }
                }
                
                if transpiler.auto_sequence_to_iterator {
                    code.push_str("])");
                } else {
                    code.push_str(")");
                }

                code
            }

            FnBody(nodes) => {
                let mut code = String::from("\n"); 

                transpiler.update_indent(1);

                if nodes.is_empty() {
                    return format!("{}pass\n", transpiler.indent());
                }

                for node in nodes[0..nodes.len() - 1].iter() {
                    code.push_str(format!("{}{}\n", transpiler.indent(), node.transpile(&transpiler)).as_str());
                }

                // Last node is return
                match nodes.last() {
                    Some(node) => {
                        if node == &Node::Return(Box::new(Node::Empty)) {
                            code.push_str(format!("{}{}", transpiler.indent(), node.transpile(&transpiler)).as_str());
                        } else {
                            code.push_str(format!("{}return {}", transpiler.indent(), node.transpile(&transpiler)).as_str());
                        }
                    }
                    None => {}
                }

                transpiler.update_indent(-1);

                code
            }

            Block(nodes) => {
                let mut code = String::new();

                if nodes.is_empty() {
                    return format!("{}pass\n", transpiler.indent());
                }

                for node in nodes {
                    code.push_str(format!("{}{}\n", transpiler.indent(), node.transpile(&transpiler)).as_str());
                }

                code
            }

            // FunctionCall
            // fn_name(args)
            // If enabled Auto-var: Result of the function is assigned to `_`
            FunctionCall { object: name, args } => {
                if transpiler.auto_vars {
                    format!("(_ := {}{})", name.transpile(&transpiler), args.transpile(&transpiler)) // Somehow parenthesis are not needed here since args it's a sequence
                } else {
                    format!("{}{}", name.transpile(&transpiler), args.transpile(&transpiler))
                }
            }

            Return(value) => format!("return {}", value.transpile(&transpiler)),

            Dict { keys, values } => {
                let mut code: String = String::from("{");
                for (i, key) in keys.iter().enumerate() {
                    code.push_str(
                        format!("{}: {}", key.transpile(&transpiler), values[i].transpile(&transpiler)).as_str(),
                    );
                    if i < keys.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("}");
                code
            }

            Alias(name) => format!("as {}", name.transpile(&transpiler)),

            Len(obj) => format!("len({})", obj.transpile(&transpiler)),

            Range {
                start,
                end,
                inclusive,
            } => {
                format!(
                    "Iterator(tuple(range({}, {}{})))",
                    start.transpile(&transpiler),
                    end.transpile(&transpiler),
                    if *inclusive { "+1" } else { "" }
                )
            }

            // * Distribution / Foward To
            // Distribution/pipes/fowards args into functions
            // So, ` a, b -> fn1, fn2;` -> `fn1(a, b); fn2(a, b);`
            Distribution { args, recipients } => {
                let mut code = String::new();

                let args_str = args
                    .iter()
                    .map(|arg| arg.transpile(&transpiler))
                    .collect::<Vec<String>>()
                    .join(", ");


                // Auto-var
                // A tuple with the results of the function is assigned to `_`
                // Ex. ( _ := (fn1(a, b), fn2(a, b)) )

                if transpiler.auto_vars {
                    code.push_str("( _ :=");
                }

                // Start a tuple to store the results of the functions
                code.push_str("(");

                for func in recipients.iter() {
                    code.push_str(&format!("{}({}), ", func.transpile(&transpiler), args_str));
                }

                // Remove the last comma and space
                code.pop(); code.pop();

                // Close the tuple
                code.push_str(")");

                // If auto-vars is enabled, close the auto-var
                if transpiler.auto_vars {
                    code.push_str(")");
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
                        let val = arg.transpile(&transpiler);
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
                    code.push_str(&format!("\t{}(*_lang_args);\n", func.transpile(&transpiler)));
                }

                code
            }

            Decorator { name, args } => {
                format!(
                    "@{}{}",
                    name.transpile(&transpiler),
                    if args.is_some() {
                        args.clone().unwrap().transpile(&transpiler)
                    } else {
                        "".to_string()
                    }
                )
            }

            Lambda { args, body } => {
                let args_str: String = args.transpile(&transpiler);

                // If args_str starts or ends with a parenthesis, remove it
                let args_str = if args_str.starts_with("(") && args_str.ends_with(")") {
                    args_str[1..args_str.len() - 1].to_string()
                } else {
                    args_str
                };

                format!(
                    "lambda {}: {}",
                    args_str,
                    body.transpile(&transpiler)
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
                    format!("if {}:\n{}\n", condition.transpile(&transpiler), body.transpile(&transpiler)).as_str(),
                );

                // elifs
                for (cond, body) in elifs.iter() {
                    code.push_str(
                        format!("elif {}:{}\n", cond.transpile(&transpiler), body.transpile(&transpiler)).as_str(),
                    );
                }

                // else
                if let Some(else_body) = else_body {
                    code.push_str(format!("else:{}\n", else_body.transpile(&transpiler)).as_str());
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
                        format!("{} {} {}\n", ident.transpile(&transpiler), op, values[i].transpile(&transpiler))
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
                    value.transpile(&transpiler),
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
                    value.transpile(&transpiler),
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
                let value_str = value.transpile(&transpiler);

                for (i, ident) in identifiers.iter().enumerate() {
                    let default = if i < default_values.len() {
                        format!(", {}", default_values[i].transpile(&transpiler))
                    } else {
                        "".to_string()
                    };

                    code.push_str(
                        format!(
                            "{} = {}.get('{}'{})\n",
                            ident.transpile(&transpiler),
                            value_str,
                            ident.transpile(&transpiler),
                            default
                        )
                        .as_str(),
                    );
                }

                code
            }

            FunctionDef { name, args, body } => {
                format!("def {}{}:{}", name, args.transpile(&transpiler), body.transpile(&transpiler))
            }

            ReactiveStmt {
                block,
                dependencies,
            } => {
                // Create function, which is called when dependencies change
                let mut code = String::from(format!("def __reactive_stmt():\n"));
                for stmt in block.iter() {
                    code.push_str(format!("\t{}\n", stmt.transpile(&transpiler)).as_str());
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
                format!("while True:\n{}", body.transpile(&transpiler))
            }

            // * For Loop
            ForLoop {
                item,
                iterable,
                body,
            } => {
                format!(
                    "for {} in {}:\n{}",
                    item.transpile(&transpiler),
                    iterable.transpile(&transpiler),
                    body.transpile(&transpiler)
                )
            }

            // * While Loop
            // While loop is a loop that repeats the body until the condition is false
            WhileLoop { condition, body } => {
                format!("while {}:\n{}", condition.transpile(&transpiler), body.transpile(&transpiler))
            }
        }
    }
}
