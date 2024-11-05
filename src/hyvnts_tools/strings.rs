/// # Strings
/// This module contains string realted helper functions.
/// Suchs impletations for the std::string::String struct for capitalization, and other string operations.

/// ### StrUtils
/// This trait provides additional methods for the `String` struct, similar to common Python `str` methods.
pub trait StrUtils {
    /// Make the first letter uppercase and the rest lowercase.
    fn capitalize(&self) -> String;
    /// Capitalize the first letter of each word in the string.
    fn title(&self) -> String;
    /// Swap the case of each letter in the string.
    fn swapcase(&self) -> String;
    /// Return `true` if all characters in the string are alphabetic.
    fn is_alpha(&self) -> bool;
    /// Return `true` if all characters in the string are digits.
    fn is_digit(&self) -> bool;
    /// Trim leading and trailing whitespace from the string.
    fn strip(&self) -> String;
}

impl StrUtils for String {
    fn capitalize(&self) -> String {
        if let Some(first) = self.chars().next() {
            format!("{}{}", first.to_uppercase(), self[1..].to_lowercase())
        } else {
            String::new()
        }
    }

    fn title(&self) -> String {
        self.split_whitespace()
            .map(|word| {
                if let Some(first) = word.chars().next() {
                    format!("{}{}", first.to_uppercase(), word[1..].to_lowercase())
                } else {
                    String::new()
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn swapcase(&self) -> String {
        self.chars()
            .map(|c| {
                if c.is_uppercase() {
                    c.to_lowercase().collect::<String>()
                } else {
                    c.to_uppercase().collect::<String>()
                }
            })
            .collect()
    }

    fn is_alpha(&self) -> bool {
        !self.is_empty() && self.chars().all(|c| c.is_alphabetic())
    }

    fn is_digit(&self) -> bool {
        !self.is_empty() && self.chars().all(|c| c.is_ascii_digit())
    }

    fn strip(&self) -> String {
        self.trim().to_string()
    }
}
