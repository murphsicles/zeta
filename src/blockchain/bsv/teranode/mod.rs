//! Teranode RPC client for Bitcoin-compatible mining operations
//!
//! This module implements HTTP/JSON-RPC client for Teranode mining software.
//! It supports the Bitcoin protocol on port 18332 with critical commands:
//! - `getminingcandidate` - Get block template from Teranode
//! - `submitminingsolution` - Submit mined block to Teranode

pub mod client;
pub mod error;
pub mod types;
pub mod config;

// Re-export main components
pub use client::TeranodeClient;
pub use error::TeranodeError;
pub use types::*;
pub use config::TeranodeClientConfig;

/// Initialize Teranode module
pub fn init() -> Result<(), TeranodeError> {
    log::info!("Initializing Teranode RPC client module");
    Ok(())
}

/// Shutdown Teranode module
pub fn shutdown() -> Result<(), TeranodeError> {
    log::info!("Shutting down Teranode RPC client module");
    Ok(())
}