//! Capabilities are declared in one place so editor behavior is auditable.

use tower_lsp::lsp_types::{
    CompletionOptions, HoverProviderCapability, InlayHintOptions, InlayHintServerCapabilities,
    OneOf, SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensServerCapabilities,
    ServerCapabilities, SignatureHelpOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
};

use crate::lsp::features::semantic::semantic_token_legend;

pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![".".to_string(), "(".to_string()]),
            ..CompletionOptions::default()
        }),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: Some(vec![",".to_string()]),
            ..SignatureHelpOptions::default()
        }),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                legend: semantic_token_legend(),
                full: Some(SemanticTokensFullOptions::Bool(true)),
                range: Some(false),
                ..SemanticTokensOptions::default()
            },
        )),
        inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
            InlayHintOptions {
                resolve_provider: Some(false),
                ..InlayHintOptions::default()
            },
        ))),
        definition_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        ..ServerCapabilities::default()
    }
}
