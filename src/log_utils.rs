//! This module provides utilities for error reporting and console output formatting.

use std::process;

/// A function type that takes a string slice and returns a String.
/// Used for creating utility functions that can modify or annotate error messages.
pub type LogUtilFunction<'a> = Box<dyn Fn(&str) -> String + 'a>;


/// Prints an error message to the console and exits the program.
///
/// # Arguments
///
/// * `lines` - A slice of strings to print to the console. These can be error messages or the results of `LogUtilFunction`s.
/// * `line` - An optional line number where the error occurred.
/// * `column` - An optional column number where the error occurred.
///
/// # Panics
///
/// This function does not return as it calls `process::exit(1)`.
pub fn error(lines: &[String], line: usize, column: usize) -> ! {

    // Red text
    eprintln!("\n\t \x1b[31mError\x1b[0m");
    for line in lines {
        eprintln!("{}", line);
    }

    eprintln!("At line: {}{}", line, if column > 0 { format!(", column: {}", column) } else { "".to_string() });
    eprintln!();
    
    process::exit(1);
}
