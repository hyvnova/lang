//! Analysis exposes tolerant, typed facts for tools without invoking fatal compiler paths.

pub mod context;
pub mod diagnostics;
pub mod document;
pub mod imports;
pub mod span;
pub mod structs;
pub mod symbols;

pub use context::{completion_context, member_access_at, word_at, CursorContext};
pub use diagnostics::{DiagnosticSeverity, LangDiagnostic};
pub use document::{analyze_document, DocumentAnalysis};
pub use span::SourceSpan;
pub use structs::{FieldValueInfo, StructFieldInfo, StructInfo, VariableInfo};
pub use symbols::{DefinitionTarget, LangSymbol, SymbolIndex, SymbolKind};
