//! Bitcoin SV (BSV) blockchain integration
//!
//! This module provides BSV blockchain functionality using Father's `nour` library.
//! It implements the `Bitcoin_` function family as specified in the API design.

pub mod address;
pub mod transaction;
pub mod script;
pub mod keys;
pub mod network;
pub mod mining;
pub mod teranode;

// Re-export BSV functionality
pub use address::*;
pub use transaction::*;
pub use script::*;
pub use keys::*;
pub use network::*;
pub use mining::*;
pub use teranode::*;

use crate::blockchain::common::config::BlockchainConfig;
use crate::blockchain::common::error::BlockchainError;

/// Initialize BSV module
pub fn init(config: &BlockchainConfig) -> Result<(), BlockchainError> {
    log::info!("Initializing BSV module v{}", version());
    
    // Initialize address module
    address::init()?;
    
    // Initialize transaction module
    transaction::init()?;
    
    // Initialize script module
    script::init()?;
    
    // Initialize keys module
    keys::init()?;
    
    // Initialize network module if enabled
    if config.network.enable_bsv {
        network::init(config)?;
    }
    
    // Initialize mining module if enabled
    if config.bsv.enable_teranode {
        mining::init(config)?;
    }
    
    log::info!("BSV module initialized successfully");
    Ok(())
}

/// Shutdown BSV module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::info!("Shutting down BSV module");
    
    // Shutdown mining module
    mining::shutdown()?;
    
    // Shutdown network module
    network::shutdown()?;
    
    // Shutdown other modules
    keys::shutdown()?;
    script::shutdown()?;
    transaction::shutdown()?;
    address::shutdown()?;
    
    log::info!("BSV module shutdown complete");
    Ok(())
}

/// Get BSV module version
pub fn version() -> &'static str {
    "0.3.50.0"
}

/// Check if BSV module is available
pub fn is_available() -> bool {
    true
}