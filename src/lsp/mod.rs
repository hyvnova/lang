//! Lang's LSP is a thin protocol shell over typed analysis and feature providers.

pub mod capabilities;
pub mod convert;
pub mod features;
mod server;

pub use capabilities::server_capabilities;
pub use convert::lsp_range;
pub use features::completion::completion_items;
pub use features::definition::definition_at;
pub use features::hover::{hover_at, hover_at_position};
pub use features::inlay::inlay_hints;
pub use features::semantic::{semantic_token_legend, semantic_tokens};
pub use features::signature::signature_help_at;
pub use features::symbols::document_symbols;
pub use server::serve_stdio;
