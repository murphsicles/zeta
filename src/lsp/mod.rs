//! Language Server Protocol implementation for Zeta
//!
//! This module provides LSP support for the Zeta compiler, enabling
//! code completion, hover information, go-to-definition, and other
//! IDE features.

mod capabilities;
mod handlers;
mod protocol;
mod server;

pub use capabilities::ServerCapabilities;
pub use protocol::LspMessage;
pub use server::LspServer;

/// LSP error types
#[derive(Debug)]
pub enum LspError {
    /// JSON-RPC protocol error
    Protocol(String),
    /// Invalid request
    InvalidRequest(String),
    /// Internal server error
    Internal(String),
    /// Feature not implemented
    NotImplemented(String),
}

impl std::fmt::Display for LspError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LspError::Protocol(msg) => write!(f, "Protocol error: {}", msg),
            LspError::InvalidRequest(msg) => write!(f, "Invalid request: {}", msg),
            LspError::Internal(msg) => write!(f, "Internal error: {}", msg),
            LspError::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
        }
    }
}

impl std::error::Error for LspError {}

/// LSP result type
pub type LspResult<T> = Result<T, LspError>;
