//! Server capabilities definition

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// Server capabilities
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct ServerCapabilities {
    /// Text document synchronization capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text_document_sync: Option<TextDocumentSyncOptions>,
    /// Completion capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub completion_provider: Option<CompletionOptions>,
    /// Hover capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hover_provider: Option<bool>,
    /// Definition capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub definition_provider: Option<bool>,
    /// References capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub references_provider: Option<bool>,
    /// Additional capabilities
    #[serde(flatten)]
    pub extra: Value,
}

/// Text document synchronization options
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct TextDocumentSyncOptions {
    /// Open/close notifications
    #[serde(skip_serializing_if = "Option::is_none")]
    pub open_close: Option<bool>,
    /// Change notifications
    #[serde(skip_serializing_if = "Option::is_none")]
    pub change: Option<TextDocumentSyncKind>,
}

/// Text document sync kind
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
#[repr(i32)]
pub enum TextDocumentSyncKind {
    /// No synchronization
    None = 0,
    /// Full synchronization
    Full = 1,
    /// Incremental synchronization
    Incremental = 2,
}

/// Completion options
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CompletionOptions {
    /// Trigger characters for completion
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trigger_characters: Option<Vec<String>>,
    /// Resolve provider for completion items
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resolve_provider: Option<bool>,
}