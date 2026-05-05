//! Source positions are shared by diagnostics, symbols, and LSP conversions.

use std::path::PathBuf;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceSpan {
    pub path: Option<PathBuf>,
    pub line: usize,
    pub column: usize,
    pub length: usize,
}

impl SourceSpan {
    pub fn new(path: Option<PathBuf>, line: usize, column: usize, length: usize) -> Self {
        Self {
            path,
            line,
            column,
            length,
        }
    }

    pub fn end_column(&self) -> usize {
        self.column + self.length.max(1)
    }
}
