/// Transpile to Python
/// This mode implements the Transpile trait for the AST Node, Stmt and Expr
/// It converts the AST to a string representation of the code in Python
use crate::ast::{Expr, Node, Stmt, AST};
use crate::transpilers::Transpile;

impl Transpile for Expr {
    fn transpile(&self) -> String {
        use Expr::*;
        match self {
            Empty => "".to_string(),
            Newline => "\n".to_string(),

            Number(n) => n.to_string(),
            Identifier(ident) => ident.to_string(),
            Str(value) => format!("\"{}\"", value),

            MemberAccess { object, member } => {
                format!("{}.{}", object.transpile(), member.transpile())
            }

            Group(expr) => format!("({})", expr.transpile()),

            BinOp { left, op, right } => {
                format!("{} {} {}", left.transpile(), op, right.transpile())
            }

            UnaryOp { op, expr } => format!("{}{}", op, expr.transpile()),

            NamedArg(name, value) => format!("{} = {}", name.transpile(), value.transpile()),

            Array(values) => {
                let mut code = String::from("[");
                for (i, value) in values.iter().enumerate() {
                    code.push_str(&value.transpile());
                    if i < values.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("]");
                code
            }

            Sequence(values) => {
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

            // TODO: Don't know how to implement this yet
            Block(nodes) => {
                let mut code = String::new();
                for node in nodes.iter() {
                    code.push_str(&node.transpile());
                }
                code
            }

            FunctionCall { name, args } => format!("{}{}", name.transpile(), args.transpile()), // Somehow parenthesis are not needed here since args it's a sequence

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

            Range {
                start,
                end,
                inclusive,
            } => {
                format!(
                    "range({}, {}{})",
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
                        format!("({} if hasattr({}, '__iter__') else itertools.cycle([{}]))", val, val, val)

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
        }
    }
}

impl Transpile for Stmt {
    fn transpile(&self) -> String {
        use Stmt::*;
        match self {
            Expr(expr) => expr.transpile(),

            Python(code) => code.to_string(),

            Assign {
                identifiers,
                values,
            } => {
                let mut code = String::new();

                for (i, ident) in identifiers.iter().enumerate() {
                    code.push_str(&ident.transpile());
                    code.push_str(" = ");
                    code.push_str(&values[i].transpile());
                    code.push_str("\n");
                }

                code
            }

            FunctionDef { name, args, body } => {
                let mut code = String::new();
                code.push_str("def ");
                code.push_str(&name.transpile());
                code.push_str(&args.transpile());
                code.push_str(":\n");
                code.push_str(&body.transpile());

                code
            }

            Import(name, alias) => format!(
                "import {}{}\n",
                name.transpile(),
                match alias {
                    Some(alias) => format!(" as {}", alias.transpile()),
                    None => "".to_string(),
                }
            ),
            From(name, thing, alias) => format!(
                "from {} import {}{}\n",
                name.transpile(),
                thing.transpile(),
                match alias {
                    Some(alias) => format!(" as {}", alias.transpile()),
                    None => "".to_string(),
                }
            ),
        }
    }
}

impl Transpile for Node {
    fn transpile(&self) -> String {
        match self {
            Node::Stmt(stmt) => stmt.transpile(),
            Node::Expr(expr) => expr.transpile(),
        }
    }
}

pub fn transpile(ast: &AST) -> String {
    let mut code = String::new();

    for node in ast.children.borrow().iter() {
        code.push_str(&node.transpile());
    }

    code
}
