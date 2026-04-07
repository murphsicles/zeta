//! LSP request and notification handlers

use serde_json::{Value, json};
use crate::lsp::protocol::*;
use crate::lsp::capabilities::{ServerCapabilities, TextDocumentSyncOptions, TextDocumentSyncKind, CompletionOptions};
use crate::lsp::LspResult;
use crate::lsp::server::ServerState;

/// Handle initialize request
pub fn handle_initialize(request: Request, _state: &ServerState) -> LspResult<Option<Response>> {
    log::info!("Handling initialize request");
    
    // Build server capabilities
    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(TextDocumentSyncKind::Full),
        }),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![
                ".".to_string(),
                ":".to_string(),
                "::".to_string(),
            ]),
            resolve_provider: Some(false),
        }),
        hover_provider: Some(true),
        definition_provider: Some(true),
        references_provider: Some(true),
        extra: json!({}),
    };
    
    let result = json!({
        "capabilities": capabilities,
        "serverInfo": {
            "name": "zeta-lsp",
            "version": "0.3.44"
        }
    });
    
    Ok(Some(Response::success(request.id, result)))
}

/// Handle initialized notification
pub fn handle_initialized(state: &mut ServerState) -> LspResult<()> {
    log::info!("Client initialized");
    // Perform any initialization that requires client capabilities
    Ok(())
}

/// Handle textDocument/didOpen notification
pub fn handle_did_open(notification: Notification, state: &mut ServerState) -> LspResult<()> {
    if let Some(params) = notification.params.as_object() {
        if let Some(text_document) = params.get("textDocument") {
            if let Some(uri) = text_document.get("uri").and_then(Value::as_str) {
                if let Some(text) = text_document.get("text").and_then(Value::as_str) {
                    log::info!("Document opened: {}", uri);
                    state.documents.insert(uri.to_string(), text.to_string());
                }
            }
        }
    }
    Ok(())
}

/// Handle textDocument/didChange notification
pub fn handle_did_change(notification: Notification, state: &mut ServerState) -> LspResult<()> {
    if let Some(params) = notification.params.as_object() {
        if let Some(text_document) = params.get("textDocument") {
            if let Some(uri) = text_document.get("uri").and_then(Value::as_str) {
                if let Some(content_changes) = params.get("contentChanges") {
                    if let Some(changes) = content_changes.as_array() {
                        if !changes.is_empty() {
                            // For full sync, take the last change
                            if let Some(last_change) = changes.last() {
                                if let Some(text) = last_change.get("text").and_then(Value::as_str) {
                                    log::debug!("Document changed: {}", uri);
                                    state.documents.insert(uri.to_string(), text.to_string());
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

/// Handle textDocument/didClose notification
pub fn handle_did_close(notification: Notification, state: &mut ServerState) -> LspResult<()> {
    if let Some(params) = notification.params.as_object() {
        if let Some(text_document) = params.get("textDocument") {
            if let Some(uri) = text_document.get("uri").and_then(Value::as_str) {
                log::info!("Document closed: {}", uri);
                state.documents.remove(uri);
            }
        }
    }
    Ok(())
}

/// Handle textDocument/completion request
pub fn handle_completion(request: Request, state: &ServerState) -> LspResult<Option<Response>> {
    log::info!("Handling completion request");
    
    // Parse completion parameters
    let params = if let Some(params) = request.params.as_object() {
        params
    } else {
        return Ok(Some(Response::error(
            request.id,
            crate::lsp::LspError::InvalidRequest("Missing parameters".to_string()),
        )));
    };
    
    let text_document = params.get("textDocument")
        .and_then(Value::as_object)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Missing textDocument".to_string()))?;
    
    let uri = text_document.get("uri")
        .and_then(Value::as_str)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Missing uri".to_string()))?;
    
    let position = params.get("position")
        .and_then(Value::as_object)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Missing position".to_string()))?;
    
    // Get document content
    let content = state.documents.get(uri)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Document not found".to_string()))?;
    
    // Simple completion based on context
    let completions = generate_completions(content, position);
    
    let result = json!({
        "isIncomplete": false,
        "items": completions
    });
    
    Ok(Some(Response::success(request.id, result)))
}

/// Handle textDocument/hover request
pub fn handle_hover(request: Request, state: &ServerState) -> LspResult<Option<Response>> {
    log::info!("Handling hover request");
    
    // Parse hover parameters
    let params = if let Some(params) = request.params.as_object() {
        params
    } else {
        return Ok(Some(Response::error(
            request.id,
            crate::lsp::LspError::InvalidRequest("Missing parameters".to_string()),
        )));
    };
    
    let text_document = params.get("textDocument")
        .and_then(Value::as_object)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Missing textDocument".to_string()))?;
    
    let uri = text_document.get("uri")
        .and_then(Value::as_str)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Missing uri".to_string()))?;
    
    let position = params.get("position")
        .and_then(Value::as_object)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Missing position".to_string()))?;
    
    // Get document content
    let content = state.documents.get(uri)
        .ok_or_else(|| crate::lsp::LspError::InvalidRequest("Document not found".to_string()))?;
    
    // Generate hover information
    let hover_info = generate_hover_info(content, position);
    
    let result = if let Some(info) = hover_info {
        json!({
            "contents": {
                "kind": "markdown",
                "value": info
            }
        })
    } else {
        Value::Null
    };
    
    Ok(Some(Response::success(request.id, result)))
}

/// Handle textDocument/definition request
pub fn handle_definition(request: Request, state: &ServerState) -> LspResult<Option<Response>> {
    log::info!("Handling definition request");
    
    // For now, return null (no definition found)
    // This will be implemented with proper symbol resolution
    Ok(Some(Response::success(request.id, Value::Null)))
}

/// Handle textDocument/references request
pub fn handle_references(request: Request, _state: &ServerState) -> LspResult<Option<Response>> {
    log::info!("Handling references request");
    
    // For now, return empty array
    // This will be implemented with proper symbol resolution
    Ok(Some(Response::success(request.id, json!([]))))
}

/// Handle shutdown request
pub fn handle_shutdown(request: Request, _state: &ServerState) -> LspResult<Option<Response>> {
    log::info!("Handling shutdown request");
    Ok(Some(Response::success(request.id, Value::Null)))
}

/// Handle exit notification
pub fn handle_exit(state: &mut ServerState) -> LspResult<()> {
    log::info!("Client exiting");
    // Clean up resources
    state.documents.clear();
    Ok(())
}

/// Generate completions based on document content and position
fn generate_completions(_content: &str, _position: &serde_json::Map<String, Value>) -> Vec<CompletionItem> {
    // Simple keyword completions for now
    // This will be enhanced with proper AST analysis
    let keywords = vec![
        "fn", "let", "const", "type", "struct", "enum", "impl", "trait",
        "if", "else", "match", "for", "while", "loop", "return", "break", "continue",
        "pub", "use", "mod", "as", "in", "where", "self", "Self",
    ];
    
    keywords.into_iter().map(|keyword| {
        CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::Keyword),
            documentation: Some(format!("Zeta keyword: {}", keyword)),
            detail: Some("keyword".to_string()),
        }
    }).collect()
}

/// Generate hover information based on document content and position
fn generate_hover_info(_content: &str, _position: &serde_json::Map<String, Value>) -> Option<String> {
    // Simple hover info for now
    // This will be enhanced with proper AST analysis
    Some("# Zeta Language\n\nThis is a Zeta source file.".to_string())
}