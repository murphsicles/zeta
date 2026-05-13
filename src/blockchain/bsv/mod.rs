//! Bitcoin SV (BSV) blockchain integration
#![allow(non_snake_case)]
//!
//! This module provides BSV blockchain functionality using Father's `nour` library.
//! It implements the `Bitcoin_` function family as specified in the API design.

pub mod address;
pub mod keys;
pub mod mining;
pub mod network;
pub mod script;
pub mod teranode;
pub mod transaction;

// Re-export BSV functionality
#[allow(ambiguous_glob_reexports)]
pub use address::*;
pub use keys::*;
#[allow(ambiguous_glob_reexports)]
pub use mining::*;
pub use network::*;
pub use script::*;
#[allow(ambiguous_glob_reexports)]
pub use teranode::*;
pub use transaction::*;

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
