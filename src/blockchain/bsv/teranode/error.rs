//! Error types for Teranode RPC client

use thiserror::Error;
use serde_json::Error as JsonError;
use reqwest::Error as ReqwestError;

/// Teranode RPC client error
#[derive(Debug, Error)]
pub enum TeranodeError {
    /// Network connection error
    #[error("Network error: {0}")]
    Network(#[from] ReqwestError),
    
    /// JSON serialization/deserialization error
    #[error("JSON error: {0}")]
    Json(#[from] JsonError),
    
    /// RPC error response
    #[error("RPC error: code={code}, message={message}")]
    Rpc {
        /// Error code
        code: i32,
        /// Error message
        message: String,
    },
    
    /// Authentication error
    #[error("Authentication failed: {0}")]
    Auth(String),
    
    /// Invalid response format
    #[error("Invalid response format: {0}")]
    InvalidResponse(String),
    
    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),
    
    /// Timeout error
    #[error("Request timeout after {0} seconds")]
    Timeout(u64),
    
    /// Mining error
    #[error("Mining error: {0}")]
    Mining(String),
    
    /// Block validation error
    #[error("Block validation error: {0}")]
    BlockValidation(String),
    
    /// Other errors
    #[error("Teranode error: {0}")]
    Other(String),
}

impl TeranodeError {
    /// Create a new RPC error
    pub fn rpc(code: i32, message: &str) -> Self {
        TeranodeError::Rpc {
            code,
            message: message.to_string(),
        }
    }
    
    /// Check if error is a network error
    pub fn is_network_error(&self) -> bool {
        matches!(self, TeranodeError::Network(_))
    }
    
    /// Check if error is an authentication error
    pub fn is_auth_error(&self) -> bool {
        matches!(self, TeranodeError::Auth(_))
    }
    
    /// Check if error is a timeout error
    pub fn is_timeout_error(&self) -> bool {
        matches!(self, TeranodeError::Timeout(_))
    }
}