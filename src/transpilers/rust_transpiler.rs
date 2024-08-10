/// TODO! Transpiling to rust requires a more sophisticated approach. The "composition" method being used it's too simple and doesn't cover all the cases. Ex. imports, macros and other syntax-bound features.


/// Transpile to Rust
/// This mode implements the Transpile trait for the AST Node, Stmt and Expr
/// It converts the AST to a string representation of the code in Rust
use crate::ast::{Expr, Node, Stmt, AST};

use core::panic;
use std::collections::HashMap;


trait Transpile {
    fn transpile(&self) -> String;
    fn complex_transpile(&self, composition: &mut Composition, ident_level: usize) -> String;
}

/// Composition of the program, where each key is a function or scope
/// and the valie is a vector of strings that representseach statement/expr in the function/scope
struct Composition {
    composition: HashMap<String, Vec<String>>,
    position: String,
}

impl Composition {
    fn new() -> Self {
        let mut composition: HashMap<String, Vec<String>> = HashMap::new();

        // Imports
        composition.insert("imports".to_owned(), vec![]);

        composition.insert(
            "main".to_owned(),
            vec!["fn main() {".to_string(), "}".to_string()],
        );

        Composition { 
            composition,
            position: "main".to_owned(),    
        }
    }

    fn write(&mut self, position: &str, code: String, ident_level: usize) {
        let ident = "\t".repeat(ident_level);
        self.composition
            .get_mut(position)
            .unwrap()
            .push(format!("{}{}", ident, code));
    }

}

pub fn transpile(ast: &AST) -> String {
    let mut composition = Composition::new();

    // for node in ast.children.borrow().iter() {
    //     code.push_str(&node.transpile());
    // }
    String::new()
}

impl Transpile for Node {
    fn transpile(&self) -> String {
        match self {
            Node::Stmt(stmt) => stmt.transpile(),
            Node::Expr(expr) => expr.transpile(),
        }
    }

    fn complex_transpile(&self, composition: &mut Composition, ident_level: usize) -> String {
        match self {
            Node::Stmt(stmt) => stmt.complex_transpile(composition, ident_level),
            Node::Expr(expr) => expr.complex_transpile(composition, ident_level),
        }
    }
}

impl Transpile for Expr {
    fn transpile(&self) -> String {
        use Expr::*;
        
        match self {
            Empty => String::new(),
            
            Identifier(ident) => ident.to_owned(),
            Number(num) => num.to_string(),
            Str(s) => format!("\"{}\"", s),

            BinOp {
                left,
                op,
                right
            } => format!(
                "{} {} {}",
                left.transpile(),
                op,
                right.transpile()
            ),

            // as {expr} 
            Alias(name) => format!("as {};", name.transpile()),

            Array(values) => {
                let mut code = String::new();
                code.push_str("[");
                for (i, value) in values.iter().enumerate() {
                    code.push_str(&value.transpile());
                    if i < values.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("]");

                code
            },

            // Currently not parsed
            Block(nodes) => { String::from("\"A block\"") },

            // as {expr}
            Dict { keys, values } => { String::from("\"A dict\"") },
            
            Newline => String::from("\n"),
            MemberAccess { object, member } => {
                format!(
                    "{}.{}",
                    object.transpile(),
                    member.transpile()
                )
            },

            Group(expr) => {
                format!("({})", expr.transpile())
            },

            UnaryOp { op, expr } => {
                format!("{}{}", op, expr.transpile())
            },

            NamedArg(name, value) => panic!("NamedArg doens't exist in Rust"),

            Sequence(items) => {
                let mut code = String::new();
                for (i, item) in items.iter().enumerate() {
                    code.push_str(&item.transpile());
                    if i < items.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code
            },

            FunctionCall { name, args } => {
                format!(
                    "{}({})",
                    name.transpile(),
                    args.transpile()
                )
            }
                    
            Range { start, end, inclusive } => {
                format!("{}..{}{}", start.transpile(), if *inclusive { "=" } else { "" }, end.transpile())
            },

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
            },

            // * Iterative Distribution
            // Iterative Distribution pipes iteration of args into functions
            // So, `|> nums, chars -> fn1, fn1;` -> for num, char in zip(nums, chars): fn1(num, char); fn2(num, char);
            // We iterate over the args and pass them to the functions
            IterDistribution { args, recipients } => {
                let mut code = String::new();

                let args_str = args
                    .iter()
                    .map(|arg| {
                        arg.transpile()
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                for func in recipients.iter() {
                    code.push_str(&format!("for {} in {}: {}({});\n", args_str, args_str, func.transpile(), args_str));
                }

                code
            },
        }
    }
}

