//! Blockchain integration module for Zeta v0.3.50+
//!
//! This module provides comprehensive blockchain support for:
//! - Bitcoin SV (BSV) via Father's `nour` library
//! - Solana blockchain
//! - Unified wallet management
//! - Cross-chain operations

// Re-export all blockchain modules
pub mod bsv;
pub mod solana;
pub mod common;
pub mod wallet;

// Re-export common types and traits
pub use common::error::BlockchainError;
pub use common::types::*;
pub use common::traits::*;
pub use common::config::BlockchainConfig;

// Re-export BSV functionality
pub use bsv::address::BtcAddress;
pub use bsv::transaction::BtcTransaction;
pub use bsv::script::BtcScript;
pub use bsv::keys::BtcKeyPair;

// Re-export Solana functionality
#[cfg(feature = "solana")]
pub use solana::address::SolanaAddress;
#[cfg(feature = "solana")]
pub use solana::transaction::SolanaTransaction;
#[cfg(feature = "solana")]
pub use solana::account::SolanaAccount;

// Re-export wallet functionality
pub use wallet::Wallet;
pub use wallet::KeyDerivation;
pub use wallet::EncryptedStorage;

/// Initialize blockchain subsystem with configuration
pub fn init(config: BlockchainConfig) -> Result<(), BlockchainError> {
    log::info!("Initializing blockchain subsystem v0.3.50");
    
    // Initialize BSV module
    bsv::init(&config)?;
    
    // Initialize Solana module if enabled
    #[cfg(feature = "solana")]
    solana::init(&config)?;
    
    // Initialize wallet module
    wallet::init(&config)?;
    
    log::info!("Blockchain subsystem initialized successfully");
    Ok(())
}

/// Shutdown blockchain subsystem
pub fn shutdown() -> Result<(), BlockchainError> {
    log::info!("Shutting down blockchain subsystem");
    
    // Shutdown wallet module
    wallet::shutdown()?;
    
    // Shutdown Solana module if enabled
    #[cfg(feature = "solana")]
    solana::shutdown()?;
    
    // Shutdown BSV module
    bsv::shutdown()?;
    
    log::info!("Blockchain subsystem shutdown complete");
    Ok(())
}

/// Get blockchain subsystem version
pub fn version() -> &'static str {
    "0.3.50.0"
}

/// Check if blockchain subsystem is available
pub fn is_available() -> bool {
    true
}