//! This module provides utilities for error reporting and console output formatting.

use std::process;

use crate::lexer::Lexer;

/// Prints an error message to the console and exits the program.
///
/// # Arguments
/// * `lexer` - The lexer... 
/// * `lines` - A slice of strings to print to the console. These can be error messages or the results of `LogUtilFunction`s.
/// # Panics
///
/// This function does not return as it calls `process::exit(1)`.
pub fn log_error(lexer: &Lexer, lines: &[String]) -> ! {
    // Determine the maximum width
    let max_error_width = lines.iter().map(|s| s.len()).max().unwrap_or(50);
    let source_line_width = lexer.source.lines().map(|s| s.len()).max().unwrap_or(0);
    let indicator_width = lexer.column;
    let width = max_error_width.max(source_line_width + indicator_width + 20);

    // Red text for the "Error" header
    eprintln!("\x1b[31m{border}\x1b[0m", border = "━".repeat(width));
    eprintln!("\x1b[31;1m{:^width$}\x1b[0m", "Error", width = width);
    eprintln!("\x1b[31m{border}\x1b[0m\n", border = "━".repeat(width));

    // Printing each line of the error message
    for line in lines {
        eprintln!("\t\x1b[31m{}\x1b[0m", line);
    }

    let line = lexer.line;
    let column = lexer.column;
    let source = &lexer.source;

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

    // Display the source code with the error highlighted
    let lines: Vec<&str> = source.lines().collect();
    if line > 0 && line <= lines.len() {
        let error_line = lines[line - 1];
        eprintln!("\n\x1b[37m{:>5} |\x1b[0m {}", line, error_line);
        eprintln!(
            "\x1b[37m      |\x1b[0m {:>width$}\x1b[31m^\x1b[0m", 
            "", 
            width = column
        );
    }

    // Red text for the bottom border
    eprintln!("\x1b[31m{border}\x1b[0m", border = "━".repeat(width));
    eprintln!();

    process::exit(1);
}

#[macro_export]
/// A macro for creating error messages.
/// Each argument is a line of the error message.
macro_rules! error {
    (
        $lexer:expr, // crate::lexer::Lexer
        $($lines:expr),*
    ) => {
        crate::log_utils::log_error(
            $lexer,
            &[ $( $lines.to_string() ),* ]
        )
    };
}




#[cfg(debug_assertions)]
#[macro_export]
/// A macro for creating debug log messages.
/// ```
/// log!("DEBUG", "This is a debug message");
/// ```
macro_rules! log {
    ($label:expr) => {{
        use colored::*;
        let log_width = 50; // Adjust width as needed

        // Format and color the label
        let formatted_label = format!("[{}]", $label).bold().cyan();
        
        // Align and print the log entry
        let padded_label = format!("{:<width$}", formatted_label, width = log_width);
        
        println!("{}", padded_label);
    }};

    ($label:expr, $($arg:tt)*) => {{
        use colored::*;
        let log_width = 50; // Adjust width as needed

        // Format the message with the provided arguments
        let formatted_message = format!($($arg)*).green();
        
        // Format and color the label
        let formatted_label = format!("[{}]", $label).bold().cyan();
        
        // Align and print the log entry
        let padded_label = format!("{:<width$}", formatted_label, width = log_width);
        let padded_message = format!("{:<width$}", formatted_message, width = log_width);
        
        println!("{}{}", padded_label, padded_message);
    }};
}

#[cfg(not(debug_assertions))]
#[macro_export]
macro_rules! log {
    ($label:expr) => {};
    ($label:expr, $($arg:tt)*) => {};
}