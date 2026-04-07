//! LSP protocol types and serialization

use serde::{Deserialize, Serialize};
use serde_json::Value;

/// LSP message types
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum LspMessage {
    Request(Request),
    Response(Response),
    Notification(Notification),
}

/// LSP request
#[derive(Debug, Deserialize, Serialize)]
pub struct Request {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Request ID
    pub id: Value,
    /// Method name
    pub method: String,
    /// Method parameters
    pub params: Value,
}

/// LSP response
#[derive(Debug, Deserialize, Serialize)]
pub struct Response {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Response ID (matches request ID)
    pub id: Value,
    /// Result or error
    #[serde(flatten)]
    pub result: ResponseResult,
}

impl Response {
    /// Create a success response
    pub fn success(id: Value, result: Value) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: ResponseResult::Success { result },
        }
    }
    
    /// Create an error response
    pub fn error<E: std::fmt::Display>(id: Value, error: E) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            id,
            result: ResponseResult::Error {
                error: ResponseError {
                    code: -32603, // Internal error
                    message: error.to_string(),
                    data: None,
                },
            },
        }
    }
}

/// LSP response result (either success or error)
#[derive(Debug, Deserialize, Serialize)]
#[serde(untagged)]
pub enum ResponseResult {
    Success {
        result: Value,
    },
    Error {
        error: ResponseError,
    },
}

/// LSP response error
#[derive(Debug, Deserialize, Serialize)]
pub struct ResponseError {
    /// Error code
    pub code: i32,
    /// Error message
    pub message: String,
    /// Optional error data
    pub data: Option<Value>,
}

/// LSP notification
#[derive(Debug, Deserialize, Serialize)]
pub struct Notification {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Method name
    pub method: String,
    /// Method parameters
    pub params: Value,
}

/// Position in a text document
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct Position {
    /// Line position in a document (zero-based)
    pub line: u32,
    /// Character offset on a line (zero-based)
    pub character: u32,
}

/// Range in a text document
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub struct Range {
    /// Start position
    pub start: Position,
    /// End position
    pub end: Position,
}

/// Location in a document
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Location {
    /// URI of the document
    pub uri: String,
    /// Range within the document
    pub range: Range,
}

/// Completion item
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct CompletionItem {
    /// Label of the completion item
    pub label: String,
    /// Kind of completion item
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<CompletionItemKind>,
    /// Documentation for the item
    #[serde(skip_serializing_if = "Option::is_none")]
    pub documentation: Option<String>,
    /// Detail for the item
    #[serde(skip_serializing_if = "Option::is_none")]
    pub detail: Option<String>,
}

/// Completion item kind
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
#[repr(i32)]
pub enum CompletionItemKind {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25,
}

/// Hover information
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct Hover {
    /// Contents of the hover
    pub contents: HoverContents,
    /// Optional range
    #[serde(skip_serializing_if = "Option::is_none")]
    pub range: Option<Range>,
}

/// Hover contents
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(untagged)]
pub enum HoverContents {
    PlainText(String),
    Markup(MarkupContent),
}

/// Markup content for documentation
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MarkupContent {
    /// Markup kind
    pub kind: MarkupKind,
    /// Content as string
    pub value: String,
}

/// Markup kind
#[derive(Debug, Clone, Copy, Deserialize, Serialize)]
pub enum MarkupKind {
    #[serde(rename = "plaintext")]
    PlainText,
    #[serde(rename = "markdown")]
    Markdown,
}