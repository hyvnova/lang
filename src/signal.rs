/// This module provides Utilities to Parse and Analyze Signals

use std::collections::HashSet;

use crate::ast::AST;


/// Filster out signals that are covered by another.
pub fn clean_signals(ast: &AST, signals: HashSet<String>) -> HashSet<String> {
    signals.iter().cloned().filter(|sx| {
        for sy in signals.iter() {
            // Skip self 
            if sx == sy { continue; }

            // If covered by other signal -> remove
            if ast.find_signal_deps(sx).contains(sy) {
                return false;
            }
        }
        // If not covered -> keep
        return true;

    }).collect()
}