/// ! All transpilers should be named as <language>_transpiler.rs
/// * This is necessary becays the macro `use_transpiler!` uses the name of the transpiler to import it
pub mod python_transpiler;
// pub mod rust_transpiler; 

/// Should be implement in Node, Stmt and Node
/// Used to convert the AST to a string representation of the code in the desired language 

#[macro_export]
/// Use a transpiler
/// ```rust
/// use_transpiler!("python", ast); // -> Returns code in Python
/// ```
macro_rules! use_transpiler {
    ($transpiler:expr, $ast:ident) => { 
        match $transpiler.as_str() {
            "python" => {
                crate::transpilers::python_transpiler::Transpiler::new().transpile(&$ast)
            }
            _ => {
                panic!("Transpiler {} not found", $transpiler);
            }
        }
    };
}