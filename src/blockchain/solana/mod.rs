//! Solana blockchain integration
//!
//! This module provides Solana blockchain functionality.
//! It implements the `Solana_` function family as specified in the API design.

pub mod address;
pub mod transaction;
pub mod account;
pub mod program;
pub mod network;

// Re-export Solana functionality
pub use address::*;
pub use transaction::*;
pub use account::*;
pub use program::*;
pub use network::*;

use crate::blockchain::common::config::BlockchainConfig;
use crate::blockchain::common::error::BlockchainError;

/// Initialize Solana module
pub fn init(config: &BlockchainConfig) -> Result<(), BlockchainError> {
    log::info!("Initializing Solana module v{}", version());
    
    // Check if Solana is enabled
    if !config.network.enable_solana {
        log::info!("Solana module disabled in configuration");
        return Ok(());
    }
    
    // Initialize address module
    address::init()?;
    
    // Initialize transaction module
    transaction::init()?;
    
    // Initialize account module
    account::init()?;
    
    // Initialize program module if enabled
    if config.solana.enable_token_program || 
       config.solana.enable_stake_program || 
       config.solana.enable_vote_program {
        program::init(config)?;
    }
    
    // Initialize network module
    network::init(config)?;
    
    log::info!("Solana module initialized successfully");
    Ok(())
}

/// Shutdown Solana module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::info!("Shutting down Solana module");
    
    // Shutdown network module
    network::shutdown()?;
    
    // Shutdown program module
    program::shutdown()?;
    
    // Shutdown other modules
    account::shutdown()?;
    transaction::shutdown()?;
    address::shutdown()?;
    
    log::info!("Solana module shutdown complete");
    Ok(())
}

/// Get Solana module version
pub fn version() -> &'static str {
    "0.3.50.0"
}

/// Check if Solana module is available
pub fn is_available() -> bool {
    true
}