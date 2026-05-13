//! Solana program operations
//!
//! Implements the `Solana_Program_*` function family.

use crate::blockchain::common::config::BlockchainConfig;
use crate::blockchain::common::error::BlockchainError;

/// Initialize program module
pub fn init(_config: &BlockchainConfig) -> Result<(), BlockchainError> {
    log::debug!("Initializing Solana program module");
    Ok(())
}

/// Shutdown program module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::debug!("Shutting down Solana program module");
    Ok(())
}
