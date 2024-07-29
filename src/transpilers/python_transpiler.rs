/// Transpile to Python 
/// This mode implements the Transpile trait for the AST Node, Stmt and Expr
/// It converts the AST to a string representation of the code in Python

use crate::ast::{Node, AST, Stmt, Expr};
use crate::transpilers::Transpile;

impl Transpile for Expr {
    fn transpile(&self) -> String {
        use Expr::*;
        match self {
            Empty => "".to_string(),
            Number(n) => n.to_string(),
            Identifier(ident) => ident.to_string(),
            Str(value) => format!("\"{}\"", value),
            
            MemberAccess {
                object,
                member,
            } => format!("{}.{}", object.transpile(), member.transpile()),
            
            Group(expr) => format!("({})", expr.transpile()),

            BinOp {
                left,
                op,
                right,
            } => format!("{} {} {}", left.transpile(), op, right.transpile()),

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
            },

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
            },

            // TODO: Don't know how to implement this yet 
            Block(nodes) => {
                let mut code = String::new();
                for node in nodes.iter() {
                    code.push_str(&node.transpile());
                }
                code
            },

            FunctionCall {
                name,
                args,
            } => format!("{}{}", name.transpile(), args.transpile()), // Somehow parenthesis are not needed here since args it's a sequence

            Dict {
                keys,
                values,
            } => {
                let mut code: String = String::from("{");
                for (i, key) in keys.iter().enumerate() {
                    code.push_str(format!("{}: {}", key.transpile(), values[i].transpile()).as_str());
                    if i < keys.len() - 1 {
                        code.push_str(", ");
                    }
                }
                code.push_str("}");
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

            _ => "None".to_string(),
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

    for node in ast.childrem.borrow().iter() {
        code.push_str(&node.transpile());
    }

    code
}
