//! Solana network operations
//!
//! Implements the `Solana_Network_*` function family.

use crate::blockchain::common::config::BlockchainConfig;
use crate::blockchain::common::error::BlockchainError;

/// Initialize network module
pub fn init(_config: &BlockchainConfig) -> Result<(), BlockchainError> {
    log::debug!("Initializing Solana network module");
    Ok(())
}

/// Shutdown network module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::debug!("Shutting down Solana network module");
    Ok(())
}
