//! This module provides utilities for error reporting and console output formatting.

use std::process;

/// Prints an error message to the console and exits the program.
///
/// # Arguments
///
/// * `lines` - A slice of strings to print to the console. These can be error messages or the results of `LogUtilFunction`s.
/// * `line` - line number where the error occurred.
/// * `column` - column number where the error occurred.
///
/// # Panics
///
/// This function does not return as it calls `process::exit(1)`.
pub fn log_error(line: usize, column: usize, lines: &[String]) -> ! {
    // Red text for the "Error" header
    eprintln!("\x1b[31m{border}\x1b[0m", border = "━".repeat(50));
    eprintln!("\x1b[31;1m{:^50}\x1b[0m", "Error");
    eprintln!("\x1b[31m{border}\x1b[0m\n", border = "━".repeat(50));

    // Printing each line of the error message
    for line in lines {
        eprintln!("\t\x1b[31m{}\x1b[0m", line);
    }

    // Display line and column information
    eprintln!(
        "\n\t\x1b[31mAt line: {}\x1b[0m{}",
        line,
        if column > 0 {
            format!("\x1b[31m, column: {}\x1b[0m", column)
        } else {
            "".to_string()
        }
    );

    // Red text for the bottom border
    eprintln!("\x1b[31m{border}\x1b[0m", border = "━".repeat(50));
    eprintln!();

    process::exit(1);
}

#[macro_export]
/// A macro for creating error messages.
/// Each argument is a line of the error message.
macro_rules! error {
    (
        $line:expr,
        $col:expr,
        $($lines:expr),*
    ) => {
        crate::log_utils::log_error(
            $line, 
            $col,
            &[ $( $lines.to_string() ),* ]
        );
    };
}
