pub mod python_transpiler;

/// Should be implement in Node, Stmt and Expr
/// Used to convert the AST to a string representation of the code in the desired language 
pub trait Transpile {
    fn transpile(&self) -> String;
}