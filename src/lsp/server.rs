//! Server owns protocol state; feature logic lives in provider modules.

use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::analysis::{analyze_document, completion_context, word_at, DocumentAnalysis};
use crate::lsp::capabilities::server_capabilities;
use crate::lsp::convert::{lsp_diagnostic, lsp_range};
use crate::lsp::features::completion::completion_items;
use crate::lsp::features::definition::definition_at;
use crate::lsp::features::hover::hover_at_position;
use crate::lsp::features::inlay::inlay_hints;
use crate::lsp::features::semantic::semantic_tokens;
use crate::lsp::features::signature::signature_help_at;
use crate::lsp::features::symbols::document_symbols;

pub async fn serve_stdio() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: Arc::new(Mutex::new(HashMap::new())),
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}

#[derive(Debug, Clone)]
struct DocumentState {
    source: String,
    analysis: DocumentAnalysis,
}

struct Backend {
    client: Client,
    documents: Arc<Mutex<HashMap<Url, DocumentState>>>,
}

impl Backend {
    async fn update_document(&self, uri: Url, source: String) {
        let path = uri.to_file_path().ok();
        let project_root = path
            .as_ref()
            .and_then(|path| path.parent())
            .map(|path| path.to_path_buf());
        let analysis = analyze_document(&source, path, project_root);
        let diagnostics = analysis.diagnostics.iter().map(lsp_diagnostic).collect();

        self.documents
            .lock()
            .await
            .insert(uri.clone(), DocumentState { source, analysis });
        self.client
            .publish_diagnostics(uri, diagnostics, None)
            .await;
    }

    async fn document_state(&self, uri: &Url) -> Option<DocumentState> {
        self.documents.lock().await.get(uri).cloned()
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: server_capabilities(),
            server_info: Some(ServerInfo {
                name: "Lang LSP".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Lang language server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.update_document(params.text_document.uri, params.text_document.text)
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            self.update_document(params.text_document.uri, change.text)
                .await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.update_document(params.text_document.uri, text).await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let Some(state) = self.document_state(&uri).await else {
            return Ok(None);
        };
        let context = completion_context(
            &state.source,
            position.line as usize,
            position.character as usize,
        );
        Ok(Some(CompletionResponse::Array(completion_items(
            &state.analysis,
            &context,
        ))))
    }

    async fn completion_resolve(&self, item: CompletionItem) -> Result<CompletionItem> {
        Ok(item)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(state) = self.document_state(&uri).await else {
            return Ok(None);
        };
        Ok(hover_at_position(
            &state.analysis,
            position.line as usize,
            position.character as usize,
        ))
    }

    async fn signature_help(&self, params: SignatureHelpParams) -> Result<Option<SignatureHelp>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(state) = self.document_state(&uri).await else {
            return Ok(None);
        };
        let context = completion_context(
            &state.source,
            position.line as usize,
            position.character as usize,
        );
        Ok(signature_help_at(&state.analysis, &context))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let Some(state) = self.document_state(&params.text_document.uri).await else {
            return Ok(None);
        };
        Ok(Some(DocumentSymbolResponse::Nested(document_symbols(
            &state.analysis,
        ))))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let Some(state) = self.document_state(&params.text_document.uri).await else {
            return Ok(None);
        };
        Ok(Some(SemanticTokensResult::Tokens(semantic_tokens(
            &state.analysis,
        ))))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let Some(state) = self.document_state(&params.text_document.uri).await else {
            return Ok(None);
        };
        Ok(Some(inlay_hints(&state.analysis)))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let Some(state) = self.document_state(&uri).await else {
            return Ok(None);
        };
        let Some(word) = word_at(
            &state.analysis.source,
            position.line as usize,
            position.character as usize,
        ) else {
            return Ok(None);
        };
        let Some(definition) = definition_at(&state.analysis, &word) else {
            return Ok(None);
        };
        let target_uri = match definition.path.as_ref() {
            Some(path) => Url::from_file_path(path),
            None => Ok(uri),
        };
        let Ok(target_uri) = target_uri else {
            return Ok(None);
        };
        Ok(Some(GotoDefinitionResponse::Scalar(Location {
            uri: target_uri,
            range: lsp_range(definition.span),
        })))
    }
}
