use crate::ast::AST;

/// The Analyzer is in charge of, well, analyzing the generated AST from parse and:
///  1. fix any logical errors  
///  2. optimize resulting AST
pub fn analyze(ast: &mut AST) {
    
}

fn signal_circular_dependency() {
}