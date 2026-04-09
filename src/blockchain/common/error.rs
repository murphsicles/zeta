//! Common error types for blockchain operations

use std::fmt;
use std::error::Error;

/// Main error type for all blockchain operations
#[derive(Debug, Clone)]
pub enum BlockchainError {
    /// Configuration errors
    Config(String),
    
    /// Network errors
    Network(String),
    
    /// Cryptographic errors
    Crypto(String),
    
    /// Transaction errors
    Transaction(String),
    
    /// Wallet errors
    Wallet(String),
    
    /// Address format errors
    Address(String),
    
    /// Script errors
    Script(String),
    
    /// Key management errors
    Key(String),
    
    /// Storage errors
    Storage(String),
    
    /// Validation errors
    Validation(String),
    
    /// Not implemented errors
    NotImplemented(String),
    
    /// Unknown errors
    Unknown(String),
}

impl fmt::Display for BlockchainError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockchainError::Config(msg) => write!(f, "Configuration error: {}", msg),
            BlockchainError::Network(msg) => write!(f, "Network error: {}", msg),
            BlockchainError::Crypto(msg) => write!(f, "Cryptographic error: {}", msg),
            BlockchainError::Transaction(msg) => write!(f, "Transaction error: {}", msg),
            BlockchainError::Wallet(msg) => write!(f, "Wallet error: {}", msg),
            BlockchainError::Address(msg) => write!(f, "Address error: {}", msg),
            BlockchainError::Script(msg) => write!(f, "Script error: {}", msg),
            BlockchainError::Key(msg) => write!(f, "Key error: {}", msg),
            BlockchainError::Storage(msg) => write!(f, "Storage error: {}", msg),
            BlockchainError::Validation(msg) => write!(f, "Validation error: {}", msg),
            BlockchainError::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
            BlockchainError::Unknown(msg) => write!(f, "Unknown error: {}", msg),
        }
    }
}

impl Error for BlockchainError {}

/// Result type for blockchain operations
pub type BlockchainResult<T> = Result<T, BlockchainError>;

/// Helper functions for creating errors
impl BlockchainError {
    pub fn config(msg: impl Into<String>) -> Self {
        BlockchainError::Config(msg.into())
    }
    
    pub fn network(msg: impl Into<String>) -> Self {
        BlockchainError::Network(msg.into())
    }
    
    pub fn crypto(msg: impl Into<String>) -> Self {
        BlockchainError::Crypto(msg.into())
    }
    
    pub fn transaction(msg: impl Into<String>) -> Self {
        BlockchainError::Transaction(msg.into())
    }
    
    pub fn wallet(msg: impl Into<String>) -> Self {
        BlockchainError::Wallet(msg.into())
    }
    
    pub fn address(msg: impl Into<String>) -> Self {
        BlockchainError::Address(msg.into())
    }
    
    pub fn script(msg: impl Into<String>) -> Self {
        BlockchainError::Script(msg.into())
    }
    
    pub fn key(msg: impl Into<String>) -> Self {
        BlockchainError::Key(msg.into())
    }
    
    pub fn storage(msg: impl Into<String>) -> Self {
        BlockchainError::Storage(msg.into())
    }
    
    pub fn validation(msg: impl Into<String>) -> Self {
        BlockchainError::Validation(msg.into())
    }
    
    pub fn not_implemented(msg: impl Into<String>) -> Self {
        BlockchainError::NotImplemented(msg.into())
    }
    
    pub fn unknown(msg: impl Into<String>) -> Self {
        BlockchainError::Unknown(msg.into())
    }
}