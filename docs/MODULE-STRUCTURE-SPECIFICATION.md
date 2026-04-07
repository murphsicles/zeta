# MODULE STRUCTURE SPECIFICATION
## Blockchain Integration for Zeta v0.3.50+

**Date:** 2026-04-02  
**Architect:** BLOCKCHAIN-ARCHITECT-AGENT  
**Status:** COMPREHENSIVE MODULE LAYOUT DEFINED

---

## 1. ROOT MODULE STRUCTURE

### 1.1 Top-level Organization

```
zetac/ (v0.3.50+)
├── Cargo.toml                    # Updated with blockchain dependencies
├── src/
│   ├── lib.rs                    # Updated exports
│   ├── main.rs                   # CLI entry point
│   ├── blockchain/              # NEW: Blockchain integration root
│   │   ├── lib.rs               # Re-export all blockchain modules
│   │   ├── error.rs             # Common blockchain errors
│   │   ├── types.rs             # Common blockchain types
│   │   ├── traits.rs            # Common blockchain traits
│   │   └── config.rs            # Blockchain configuration
│   ├── compiler/                # Existing compiler modules
│   ├── frontend/                # Existing frontend modules
│   ├── middle/                  # Existing middle-end modules
│   ├── backend/                 # Existing backend modules
│   └── std/                     # Standard library
│       └── blockchain/          # NEW: Zeta stdlib blockchain module
└── tests/
    └── blockchain/              # NEW: Blockchain integration tests
```

### 1.2 Cargo.toml Updates

```toml
[package]
name = "zetac"
version = "0.3.50.0"  # Major blockchain integration release
edition = "2024"

[dependencies]
# Existing dependencies...
# Blockchain-specific dependencies:
nour = { version = "1.0.0", path = "../nour" }  # Local during development
solana-sdk = { version = "2.0.0", optional = true }
solana-client = { version = "2.0.0", optional = true }
solana-program = { version = "2.0.0", optional = true }
secp256k1 = { version = "0.31.1", features = ["global-context"] }
ed25519-dalek = { version = "2.1.1", optional = true }
bip39 = { version = "2.0.0", features = ["chinese-simplified", "chinese-traditional", "french", "italian", "japanese", "korean", "spanish"] }
hdwallet = { version = "0.5.0", features = ["secp256k1"] }
base58 = "0.2.0"
hex = "0.4.3"
thiserror = "1.0.69"
tracing = { version = "0.1.40", features = ["log"] }

[features]
default = []
bitcoin-sv = ["nour"]  # Enable BSV support
bitcoin-sv-full = ["bitcoin-sv", "nour/async"]  # Full P2P support
solana = ["solana-sdk", "solana-client", "ed25519-dalek"]  # Enable Solana support
solana-full = ["solana", "solana-program"]  # Full program support
blockchain = ["bitcoin-sv", "solana"]  # Enable all blockchain features
blockchain-full = ["bitcoin-sv-full", "solana-full"]  # Full blockchain support
```

## 2. BLOCKCHAIN ROOT MODULE (`src/blockchain/`)

### 2.1 `lib.rs` - Module Exports

```rust
//! Blockchain integration module for Zeta compiler
//! Provides support for Bitcoin SV (via nour) and Solana

#![warn(missing_docs)]
#![warn(unused_crate_dependencies)]

// Re-export common types
pub use error::BlockchainError;
pub use types::*;
pub use config::BlockchainConfig;
pub use traits::BlockchainClient;

// Module declarations
pub mod error;
pub mod types;
pub mod traits;
pub mod config;

// Blockchain-specific modules (feature-gated)
#[cfg(feature = "bitcoin-sv")]
pub mod btc;

#[cfg(feature = "bitcoin-sv")]
pub mod bitcoin_sv;

#[cfg(feature = "solana")]
pub mod solana;

// Common infrastructure modules
pub mod wallet;
pub mod mining;
pub mod tokens;

/// Initialize blockchain subsystem with given configuration
pub fn init(config: BlockchainConfig) -> Result<(), BlockchainError> {
    // Initialize logging
    tracing::info!("Initializing blockchain subsystem v{}", env!("CARGO_PKG_VERSION"));
    
    // Validate configuration
    config.validate()?;
    
    // Initialize cryptography libraries
    init_crypto()?;
    
    // Initialize network components if needed
    if config.enable_networking {
        init_networking(&config)?;
    }
    
    tracing::info!("Blockchain subsystem initialized successfully");
    Ok(())
}

/// Cleanup blockchain subsystem
pub fn cleanup() -> Result<(), BlockchainError> {
    tracing::info!("Cleaning up blockchain subsystem");
    // Cleanup network connections
    // Clear sensitive data from memory
    Ok(())
}
```

### 2.2 `error.rs` - Common Blockchain Errors

```rust
//! Common error types for blockchain operations

use thiserror::Error;

/// Common blockchain error type
#[derive(Debug, Error)]
pub enum BlockchainError {
    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),
    
    /// Initialization error
    #[error("Initialization error: {0}")]
    Init(String),
    
    /// Cryptography error
    #[error("Cryptography error: {0}")]
    Crypto(String),
    
    /// Network error
    #[error("Network error: {0}")]
    Network(String),
    
    /// Serialization/deserialization error
    #[error("Serialization error: {0}")]
    Serialization(String),
    
    /// Validation error
    #[error("Validation error: {0}")]
    Validation(String),
    
    /// Not implemented error
    #[error("Feature not implemented: {0}")]
    NotImplemented(String),
    
    /// External library error
    #[error("External library error: {0}")]
    External(Box<dyn std::error::Error + Send + Sync>),
}

impl BlockchainError {
    /// Get error code for programmatic handling
    pub fn error_code(&self) -> u32 {
        match self {
            Self::Config(_) => 1000,
            Self::Init(_) => 1001,
            Self::Crypto(_) => 1002,
            Self::Network(_) => 1003,
            Self::Serialization(_) => 1004,
            Self::Validation(_) => 1005,
            Self::NotImplemented(_) => 1006,
            Self::External(_) => 1999,
        }
    }
    
    /// Check if error is recoverable
    pub fn is_recoverable(&self) -> bool {
        matches!(self, Self::Network(_) | Self::Validation(_))
    }
}
```

### 2.3 `types.rs` - Common Blockchain Types

```rust
//! Common type definitions for blockchain operations

use serde::{Deserialize, Serialize};
use std::fmt;

/// Blockchain network type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BlockchainNetwork {
    /// Main network (production)
    Mainnet,
    /// Test network
    Testnet,
    /// Development/regtest network
    Devnet,
    /// Simulation network (for testing)
    Simulation,
}

/// Blockchain type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BlockchainType {
    /// Bitcoin Core (legacy)
    BitcoinCore,
    /// Bitcoin SV (using nour)
    BitcoinSV,
    /// Solana
    Solana,
}

impl fmt::Display for BlockchainType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BitcoinCore => write!(f, "Bitcoin Core"),
            Self::BitcoinSV => write!(f, "Bitcoin SV"),
            Self::Solana => write!(f, "Solana"),
        }
    }
}

/// Transaction identifier (hash)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TransactionId(pub Vec<u8>);

impl fmt::Display for TransactionId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", hex::encode(&self.0))
    }
}

/// Block identifier (hash)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BlockId(pub Vec<u8>);

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", hex::encode(&self.0))
    }
}

/// Amount in smallest unit (satoshis, lamports, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct Amount(pub u64);

impl fmt::Display for Amount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Address type (blockchain-specific)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Address {
    /// Bitcoin-style address (Base58)
    Bitcoin(String),
    /// Solana-style address (Base58)
    Solana(String),
    /// Raw public key
    Pubkey(Vec<u8>),
}

/// Transaction status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransactionStatus {
    /// Transaction created but not broadcast
    Created,
    /// Transaction broadcast to network
    Pending,
    /// Transaction confirmed in block
    Confirmed(u32), // confirmation depth
    /// Transaction failed
    Failed(String),
}
```

### 2.4 `traits.rs` - Common Blockchain Traits

```rust
//! Common traits for blockchain operations

use crate::blockchain::types::*;
use crate::blockchain::error::BlockchainError;
use async_trait::async_trait;

/// Trait for blockchain client implementations
#[async_trait]
pub trait BlockchainClient: Send + Sync {
    /// Get blockchain type
    fn blockchain_type(&self) -> BlockchainType;
    
    /// Get network type
    fn network(&self) -> BlockchainNetwork;
    
    /// Get current block height
    async fn get_block_height(&self) -> Result<u64, BlockchainError>;
    
    /// Get balance for address
    async fn get_balance(&self, address: &Address) -> Result<Amount, BlockchainError>;
    
    /// Send transaction
    async fn send_transaction(&self, tx_data: &[u8]) -> Result<TransactionId, BlockchainError>;
    
    /// Get transaction status
    async fn get_transaction_status(&self, txid: &TransactionId) -> Result<TransactionStatus, BlockchainError>;
    
    /// Estimate transaction fee
    async fn estimate_fee(&self, tx_size: usize) -> Result<Amount, BlockchainError>;
}

/// Trait for transaction builders
pub trait TransactionBuilder {
    /// Create new transaction builder
    fn new() -> Self;
    
    /// Add input to transaction
    fn add_input(&mut self, txid: &TransactionId, vout: u32, amount: Amount) -> &mut Self;
    
    /// Add output to transaction
    fn add_output(&mut self, address: &Address, amount: Amount) -> &mut Self;
    
    /// Set transaction fee
    fn set_fee(&mut self, fee: Amount) -> &mut Self;
    
    /// Build transaction
    fn build(&self) -> Result<Vec<u8>, BlockchainError>;
}

/// Trait for wallet operations
#[async_trait]
pub trait Wallet: Send + Sync {
    /// Get wallet address for given index
    fn get_address(&self, index: u32) -> Result<Address, BlockchainError>;
    
    /// Sign transaction data
    fn sign_transaction(&self, tx_data: &[u8]) -> Result<Vec<u8>, BlockchainError>;
    
    /// Get wallet balance
    async fn get_balance(&self) -> Result<Amount, BlockchainError>;
    
    /// Create and send transaction
    async fn send(&self, to: &Address, amount: Amount) -> Result<TransactionId, BlockchainError>;
}
```

### 2.5 `config.rs` - Blockchain Configuration

```rust
//! Blockchain configuration management

use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Blockchain subsystem configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockchainConfig {
    /// Enable Bitcoin SV support
    pub enable_bitcoin_sv: bool,
    
    /// Enable Solana support
    pub enable_solana: bool,
    
    /// Enable networking (P2P/RPC)
    pub enable_networking: bool,
    
    /// Network type (mainnet/testnet/devnet)
    pub network: String,
    
    /// Data directory for blockchain data
    pub data_dir: PathBuf,
    
    /// RPC endpoints (if using RPC instead of P2P)
    pub rpc_endpoints: Vec<String>,
    
    /// Connection timeout in seconds
    pub connection_timeout: u64,
    
    /// Request timeout in seconds
    pub request_timeout: u64,
    
    /// Retry attempts for failed requests
    pub retry_attempts: u32,
    
    /// Enable debug logging
    pub debug_logging: bool,
    
    /// Security settings
    pub security: SecurityConfig,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    /// Require hardware wallet for high-value transactions
    pub require_hardware_wallet: bool,
    
    /// Maximum transaction value without confirmation
    pub max_tx_value_without_confirmation: u64,
    
    /// Enable multi-signature for large transactions
    pub enable_multisig: bool,
    
    /// Number of confirmations required
    pub required_confirmations: u32,
    
    /// Enable transaction monitoring
    pub enable_tx_monitoring: bool,
}

impl Default for BlockchainConfig {
    fn default() -> Self {
        Self {
            enable_bitcoin_sv: true,
            enable_solana: true,
            enable_networking: true,
            network: "testnet".to_string(),
            data_dir: dirs::data_dir()
                .unwrap_or_else(|| PathBuf::from(".zeta"))
                .join("blockchain"),
            rpc_endpoints: vec![],
            connection_timeout: 30,
            request_timeout: 60,
            retry_attempts: 3,
            debug_logging: false,
            security: SecurityConfig::default(),
        }
    }
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            require_hardware_wallet: false,
            max_tx_value_without_confirmation: 1_000_000, // 0.01 BTC/BSV
            enable_multisig: false,
            required_confirmations: 3,
            enable_tx_monitoring: true,
        }
    }
}

impl BlockchainConfig {
    /// Validate configuration
    pub fn validate(&self) -> Result<(), crate::blockchain::error::BlockchainError> {
        // Validate network type
        let valid_networks = ["mainnet", "testnet", "devnet", "simulation"];
        if !valid_networks.contains(&self.network.as_str()) {
            return Err(crate::blockchain::error::BlockchainError::Config(
                format!("Invalid network type: {}", self.network)
            ));
        }
        
        // Validate timeouts
        if self.connection_timeout == 0 {
            return Err(crate::blockchain::error::BlockchainError::Config(
                "Connection timeout must be greater than 0".to_string()
            ));
        }
        
        if self.request_timeout == 0 {
            return Err(crate::blockchain::error::BlockchainError::Config(
                "Request timeout must be greater than 0".to_string()
            ));
        }
        
        // Validate security settings
        if self.security.required_confirmations == 0 {
            return Err(crate::blockchain::error::BlockchainError::Config(
                "Required confirmations must be greater than 0".to_string()
            ));
        }
        
        Ok(())
    }
    
    /// Create configuration for test environment
    pub fn test_config() -> Self {
        Self {
            enable_bitcoin_sv: true,
            enable_solana: true,
            enable_networking: false, // Disable networking for tests
            network: "simulation".to_string(),
            data_dir: PathBuf::from("/tmp/zeta_test"),
            ..Default::default()
        }
    }
}
```

## 3. BTC MODULE (`src/blockchain/btc/`)

### 3.1 Module Structure

```
src/blockchain/btc/
├── lib.rs                    # Module exports and BTC_ function implementations
├── address.rs               # BTC_Address functions
├── transaction.rs           # BTC_Transaction functions
├── script.rs               # BTC_Script functions
├── wallet.rs               # BTC_Wallet functions
├── network.rs              # BTC_Network functions
├── error.rs                # BTC-specific errors
└── types.rs                # BTC-specific types
```

### 3.2 `lib.rs` - BTC Module Exports

```rust
//! Bitcoin Core (BTC) compatibility module
//! Provides BTC_ prefixed functions for legacy Bitcoin Core support

#![warn(missing_docs)]

pub use address::*;
pub use transaction::*;
pub use script::*;
pub use wallet::*;
pub use network::*;
pub use error::BtcError;
pub use types::*;

pub mod address;
pub mod transaction;
pub mod script;
pub mod wallet;
pub mod network;
pub mod error;
pub mod types;

/// Initialize BTC module
pub fn init() -> Result<(), BtcError> {
    tracing::debug!("Initializing BTC module");
    Ok(())
}

/// Cleanup BTC module
pub fn cleanup() -> Result<(), BtcError> {
    tracing::debug!("Cleaning up BTC module");
    Ok(())
}
```

### 3.3 `address.rs` - BTC_Address Functions

```rust
//! BTC_Address module - Bitcoin Core address functions

use crate::blockchain::error::BlockchainError;
use crate::blockchain::types::Address;
use bitcoin_hashes::{sha256d, Hash};
use base58::{FromBase58, ToBase58};

/// BTC-specific address type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtcAddress {
    /// Raw bytes (20-byte hash)
    pub hash: [u8; 20],
    /// Network prefix (0x00 for mainnet, 0x6f for testnet)
    pub prefix: u8,
    /// Address type (P2PKH or P2SH)
    pub addr_type: BtcAddressType,
}

/// BTC address type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BtcAddressType {
    /// Pay-to-Public-Key-Hash (P2PKH)
    P2PKH,
    /// Pay-to-Script-Hash (P2SH)
    P2SH,
}

/// Convert string to BTC address
pub fn BTC_Address_from_string(s: &str) -> Result<BtcAddress, BlockchainError> {
    // Decode Base58
    let decoded = s.from_base58()
        .map_err(|e| BlockchainError::Validation(format!("Invalid Base58: {}", e)))?;
    
    if decoded.len() < 25 {
        return Err(BlockchainError::Validation("Address too short".to_string()));
    }
    
    // Extract checksum
    let data = &decoded[..decoded.len() - 4];
    let checksum = &decoded[decoded.len() - 4..];
    
    // Verify checksum
    let hash1 = sha256d::Hash::hash(data);
    let hash2 = sha256d::Hash::hash(&hash1[..]);
    
    if &hash2[..4] != checksum {
        return Err(BlockchainError::Validation("Invalid checksum".to_string()));
    }
    
    // Determine address type from prefix
    let prefix = data[0];
    let addr_type = match prefix {
        0x00 => BtcAddressType::P2PKH,  // Mainnet P2PKH
        0x05 => BtcAddressType::P2SH,   // Mainnet P2SH
        0x6f => BtcAddressType::P2PKH,  // Testnet P2PKH
        0xc4 => BtcAddressType::P2SH,   // Testnet P2SH
        _ => return Err(BlockchainError::Validation(
            format!("Unknown address prefix: 0x{:02x}", prefix)
        )),
    };
    
    // Extract hash (20 bytes after prefix)
    let mut hash = [0u8; 20];
    hash.copy_from_slice(&data[1..21]);
    
    Ok(BtcAddress { hash, prefix, addr_type })
}

/// Convert BTC address to string
pub fn BTC_Address_to_string(addr: &BtcAddress) -> String {
    let mut data = Vec::with_capacity(21);
    data.push(addr.prefix);
    data.extend_from_slice(&addr.hash);
    
    // Calculate checksum
    let hash1 = sha256d::Hash::hash(&data);
    let hash2 = sha256d::Hash::hash(&hash1[..]);
    let checksum = &hash2[..4];
    
    data.extend_from_slice(checksum);
    data.to_base58()
}

/// Validate BTC address string
pub fn BTC_Address_validate(s: &str) -> bool {
    BTC_Address_from_string(s).is_ok()
}

/// Create BTC address from public key
pub fn BTC_Address_from_pubkey(pubkey: &[u8], network: &str) -> Result<BtcAddress, BlockchainError> {
    // Hash public key (SHA256 then RIPEMD160)
    let sha256_hash = sha256d::Hash::hash(pubkey);
    let ripemd160_hash = ripemd::Ripemd160::digest(&sha256_hash[..]);
    
    // Determine prefix based on network
    let (prefix, addr_type) = match network {
        "mainnet" => (0x00, BtcAddressType::P2PKH),
        "testnet" => (0x6f, BtcAddressType::P2PKH),
        _ => return Err(BlockchainError::Validation(
            format!("Unknown network: {}", network)
        )),
    };
    
    let mut hash = [0u8; 20];
    hash.copy_from_slice(&ripemd160_hash);
    
    Ok(BtcAddress { hash, prefix, addr_type })
}

/// Convert BTC address to common Address type
pub fn to_common_address(addr: &BtcAddress) -> Address {
    Address::Bitcoin(BTC_Address_to_string(addr))
}

/// Convert common Address to BTC address
pub fn from_common_address(addr: &Address) -> Result<BtcAddress, BlockchainError> {
    match addr {
        Address::Bitcoin(s) => BTC_Address_from_string(s),
        _ => Err(BlockchainError::Validation(
            "Address is not a Bitcoin address".to_string()
        )),
    }
}
```

## 4. BITCOIN_SV MODULE (`src/blockchain/bitcoin_sv/`)

### 4.1 Module Structure

```
src/blockchain/bitcoin_sv/
├── lib.rs                    # Module exports and Bitcoin_ function implementations
├── address.rs               # Bitcoin_Address functions (using nour)
├── transaction.rs           # Bitcoin_Transaction functions (using nour)
├── script.rs               # Bitcoin_Script functions (using nour opcodes)
├── wallet.rs               # Bitcoin_Wallet functions (using nour)
├── network.rs              # Bitcoin_Network functions (using nour::peer)
├── mining.rs               # Bitcoin_Mining functions (Teranode integration)
├── tokens.rs               # Bitcoin_Token functions (BSV-21/BRC100)
├── error.rs                # Bitcoin-specific errors
├── types.rs                # Bitcoin-specific types
└── conversion.rs           # Type conversions between Zeta and nour
```

### 4.2 `lib.rs` - Bitcoin_SV Module Exports

```rust
//! Bitcoin SV module using Father's nour library
//! Provides Bitcoin_ prefixed functions for BSV operations

#![warn(missing_docs)]

// Re-export nour types for convenience
pub use nour::{
    address::Address as NourAddress,
    transaction::Transaction as NourTransaction,
    script::{Script as NourScript, Opcode as NourOpcode},
    crypto::{PublicKey as NourPublicKey, PrivateKey as NourPrivateKey},
};

pub use address::*;
pub use transaction::*;
pub use script::*;
pub use wallet::*;
pub use network::*;
pub use mining::*;
pub use tokens::*;
pub use error::BitcoinError;
pub use types::*;
pub use conversion::*;

pub mod address;
pub mod transaction;
pub mod script;
pub mod wallet;
pub mod network;
pub mod mining;
pub mod tokens;
pub mod error;
pub mod types;
pub mod conversion;

/// Initialize Bitcoin SV module
pub fn init() -> Result<(), BitcoinError> {
    tracing::debug!("Initializing Bitcoin SV module with nour v{}", nour::VERSION);
    
    // Initialize nour library if needed
    // (nour may have its own initialization)
    
    tracing::info!("Bitcoin SV module initialized successfully");
    Ok(())
}

/// Cleanup Bitcoin SV module
pub fn cleanup() -> Result<(), BitcoinError> {
    tracing::debug!("Cleaning up Bitcoin SV module");
    // Cleanup network connections
    // Clear sensitive data
    Ok(())
}
```

### 4.3 `conversion.rs` - Type Conversions

```rust
//! Type conversion between Zeta types and nour types

use crate::blockchain::bitcoin_sv::error::BitcoinError;
use crate::blockchain::types::{Address as CommonAddress, Amount};
use nour::{address::Address as NourAddress, crypto::PublicKey as NourPublicKey};

/// Convert common Address to nour Address
pub fn common_address_to_nour(addr: &CommonAddress) -> Result<NourAddress, BitcoinError> {
    match addr {
        CommonAddress::Bitcoin(s) => {
            NourAddress::from_string(s)
                .map_err(|e| BitcoinError::AddressConversion(e.to_string()))
        }
        _ => Err(BitcoinError::AddressConversion(
            "Address is not a Bitcoin address".to_string()
        )),
    }
}

/// Convert nour Address to common Address
pub fn nour_address_to_common(addr: &NourAddress) -> CommonAddress {
    CommonAddress::Bitcoin(addr.to_string())
}

/// Convert Amount to nour satoshis
pub fn amount_to_satoshis(amount: Amount) -> u64 {
    amount.0
}

/// Convert nour satoshis to Amount
pub fn satoshis_to_amount(satoshis: u64) -> Amount {
    Amount(satoshis)
}

/// Convert public key bytes to nour PublicKey
pub fn bytes_to_nour_pubkey(bytes: &[u8]) -> Result<NourPublicKey, BitcoinError> {
    NourPublicKey::from_slice(bytes)
        .map_err(|e| BitcoinError::Crypto(format!("Invalid public key: {}", e)))
}

/// Convert nour PublicKey to bytes
pub fn nour_pubkey_to_bytes(pubkey: &NourPublicKey) -> Vec<u8> {
    pubkey.serialize().to_vec()
}

/// Convert script bytes to nour Script
pub fn bytes_to_nour_script(bytes: &[u8]) -> Result<nour::script::Script, BitcoinError> {
    nour::script::Script::from_bytes(bytes)
        .map_err(|e| BitcoinError::Script(format!("Invalid script: {}", e)))
}

/// Convert nour Script to bytes
pub fn nour_script_to_bytes(script: &nour::script::Script) -> Vec<u8> {
    script.to_bytes()
}
```

### 4.4 `address.rs` - Bitcoin_Address Functions (using nour)

```rust
//! Bitcoin_Address module - BSV address functions using nour

use crate::blockchain::bitcoin_sv::error::BitcoinError;
use crate::blockchain::types::Address as CommonAddress;
use nour::address::{Address as NourAddress, Network};

/// Bitcoin SV address type (wrapper around nour Address)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitcoinAddress {
    inner: NourAddress,
}

impl BitcoinAddress {
    /// Create from nour Address
    pub fn from_nour(addr: NourAddress) -> Self {
        Self { inner: addr }
    }
    
    /// Convert to nour Address
    pub fn to_nour(&self) -> NourAddress {
        self.inner.clone()
    }
    
    /// Get network
    pub fn network(&self) -> Network {
        self.inner.network()
    }
    
    /// Get address type (P2PKH, P2SH, etc.)
    pub fn address_type(&self) -> &'static str {
        self.inner.address_type()
    }
    
    /// Get hash (public key hash or script hash)
    pub fn hash(&self) -> &[u8] {
        self.inner.hash()
    }
}

/// Convert string to Bitcoin address using nour
pub fn Bitcoin_Address_from_string(s: &str) -> Result<BitcoinAddress, BitcoinError> {
    let nour_addr = NourAddress::from_string(s)
        .map_err(|e| BitcoinError::AddressConversion(e.to_string()))?;
    
    Ok(BitcoinAddress::from_nour(nour_addr))
}

/// Convert Bitcoin address to string using nour
pub fn Bitcoin_Address_to_string(addr: &BitcoinAddress) -> String {
    addr.inner.to_string()
}

/// Validate Bitcoin address string using nour
pub fn Bitcoin_Address_validate(s: &str) -> bool {
    NourAddress::from_string(s).is_ok()
}

/// Create Bitcoin address from public key using nour
pub fn Bitcoin_Address_from_pubkey(
    pubkey: &nour::crypto::PublicKey,
    network: Network,
) -> Result<BitcoinAddress, BitcoinError> {
    let nour_addr = NourAddress::from_pubkey(pubkey, network)
        .map_err(|e| BitcoinError::AddressConversion(e.to_string()))?;
    
    Ok(BitcoinAddress::from_nour(nour_addr))
}

/// Convert Bitcoin address to script hash
pub fn Bitcoin_Address_to_script_hash(addr: &BitcoinAddress) -> [u8; 20] {
    let hash = addr.hash();
    let mut result = [0u8; 20];
    result.copy_from_slice(hash);
    result
}

/// Convert to common Address type
pub fn to_common_address(addr: &BitcoinAddress) -> CommonAddress {
    CommonAddress::Bitcoin(Bitcoin_Address_to_string(addr))
}

/// Convert from common Address type
pub fn from_common_address(addr: &CommonAddress) -> Result<BitcoinAddress, BitcoinError> {
    match addr {
        CommonAddress::Bitcoin(s) => Bitcoin_Address_from_string(s),
        _ => Err(BitcoinError::AddressConversion(
            "Address is not a Bitcoin address".to_string()
        )),
    }
}
```

## 5. SOLANA MODULE (`src/blockchain/solana/`)

### 5.1 Module Structure

```
src/blockchain/solana/
├── lib.rs                    # Module exports and Solana_ function implementations
├── address.rs               # Solana_Address functions (Pubkey)
├── transaction.rs           # Solana_Transaction functions
├── program.rs               # Solana_Program functions
├── wallet.rs               # Solana_Wallet functions
├── network.rs              # Solana_Network functions (RPC)
├── tokens.rs               # Solana_Token functions (SPL)
├── accounts.rs             # Solana_Account functions
├── error.rs                # Solana-specific errors
├── types.rs                # Solana-specific types
└── crypto.rs               # Solana cryptography (Ed25519)
```

### 5.2 `lib.rs` - Solana Module Exports

```rust
//! Solana module using solana-sdk
//! Provides Solana_ prefixed functions for Solana operations

#![warn(missing_docs)]

// Re-export solana-sdk types for convenience
pub use solana_sdk::{
    pubkey::Pubkey,
    signature::{Signature, Keypair},
    transaction::Transaction as SolanaTransaction,
    instruction::Instruction,
    message::Message,
    account::Account,
    clock::Clock,
    sysvar::Sysvar,
};

pub use address::*;
pub use transaction::*;
pub use program::*;
pub use wallet::*;
pub use network::*;
pub use tokens::*;
pub use accounts::*;
pub use error::SolanaError;
pub use types::*;
pub use crypto::*;

pub mod address;
pub mod transaction;
pub mod program;
pub mod wallet;
pub mod network;
pub mod tokens;
pub mod accounts;
pub mod error;
pub mod types;
pub mod crypto;

/// Initialize Solana module
pub fn init() -> Result<(), SolanaError> {
    tracing::debug!("Initializing Solana module with solana-sdk v{}", solana_sdk::version!());
    
    // Initialize solana logger if debug mode
    #[cfg(feature = "debug")]
    {
        solana_logger::setup_with_default("info");
    }
    
    tracing::info!("Solana module initialized successfully");
    Ok(())
}

/// Cleanup Solana module
pub fn cleanup() -> Result<(), SolanaError> {
    tracing::debug!("Cleaning up Solana module");
    // Cleanup RPC connections
    Ok(())
}
```

### 5.3 `address.rs` - Solana_Address Functions

```rust
//! Solana_Address module - Solana address (Pubkey) functions

use crate::blockchain::solana::error::SolanaError;
use crate::blockchain::types::Address as CommonAddress;
use solana_sdk::pubkey::Pubkey;

/// Solana address type (wrapper around Pubkey)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SolanaAddress {
    inner: Pubkey,
}

impl SolanaAddress {
    /// Create from Pubkey
    pub fn from_pubkey(pubkey: Pubkey) -> Self {
        Self { inner: pubkey }
    }
    
    /// Convert to Pubkey
    pub fn to_pubkey(&self) -> Pubkey {
        self.inner
    }
    
    /// Check if address is on-curve
    pub fn is_on_curve(&self) -> bool {
        self.inner.is_on_curve()
    }
}

/// Convert string to Solana address
pub fn Solana_Address_from_string(s: &str) -> Result<SolanaAddress, SolanaError> {
    let pubkey = Pubkey::from_str(s)
        .map_err(|e| SolanaError::AddressConversion(e.to_string()))?;
    
    Ok(SolanaAddress::from_pubkey(pubkey))
}

/// Convert Solana address to string
pub fn Solana_Address_to_string(addr: &SolanaAddress) -> String {
    addr.inner.to_string()
}

/// Create address with seed
pub fn Solana_Address_create_with_seed(
    base: &Pubkey,
    seed: &str,
    program_id: &Pubkey,
) -> Result<Pubkey, SolanaError> {
    Pubkey::create_with_seed(base, seed, program_id)
        .map_err(|e| SolanaError::AddressConversion(e.to_string()))
}

/// Find program address
pub fn Solana_Address_find_program_address(
    seeds: &[&[u8]],
    program_id: &Pubkey,
) -> (Pubkey, u8) {
    Pubkey::find_program_address(seeds, program_id)
}

/// Convert to common Address type
pub fn to_common_address(addr: &SolanaAddress) -> CommonAddress {
    CommonAddress::Solana(Solana_Address_to_string(addr))
}

/// Convert from common Address type
pub fn from_common_address(addr: &CommonAddress) -> Result<SolanaAddress, SolanaError> {
    match addr {
        CommonAddress::Solana(s) => Solana_Address_from_string(s),
        _ => Err(SolanaError::AddressConversion(
            "Address is not a Solana address".to_string()
        )),
    }
}
```

## 6. WALLET MODULE (`src/blockchain/wallet/`)

### 6.1 Module Structure

```
src/blockchain/wallet/
├── lib.rs                    # Unified wallet exports
├── unified.rs               # Multi-chain wallet implementation
├── derivation.rs            # Key derivation paths (BIP-32/44, SLIP-0044)
├── storage.rs               # Encrypted wallet storage
├── hardware.rs              # Hardware wallet integration (Ledger/Trezor)
├── mnemonic.rs              # BIP-39 mnemonic generation/validation
├── encryption.rs            # AES-256-GCM encryption for private keys
├── backup.rs                # Wallet backup and recovery
├── error.rs                 # Wallet-specific errors
└── types.rs                 # Wallet-specific types
```

### 6.2 `unified.rs` - Multi-chain Wallet

```rust
//! Unified multi-chain wallet implementation

use crate::blockchain::wallet::error::WalletError;
use crate::blockchain::types::{Address, Amount, BlockchainType};
use crate::blockchain::wallet::derivation::{DerivationPath, DerivationPathType};
use crate::blockchain::wallet::mnemonic::Mnemonic;
use crate::blockchain::wallet::storage::EncryptedStorage;
use std::collections::HashMap;
use std::sync::Arc;

/// Unified multi-chain wallet
pub struct UnifiedWallet {
    /// Wallet identifier
    id: String,
    
    /// BIP-39 mnemonic
    mnemonic: Mnemonic,
    
    /// Master seed
    seed: [u8; 64],
    
    /// Passphrase (optional)
    passphrase: Option<String>,
    
    /// Derived keys per blockchain
    keys: HashMap<(BlockchainType, DerivationPath), WalletKey>,
    
    /// Wallet metadata
    name: String,
    created_at: chrono::DateTime<chrono::Utc>,
    last_used: chrono::DateTime<chrono::Utc>,
    
    /// Storage backend
    storage: Arc<EncryptedStorage>,
    
    /// Security settings
    security: WalletSecurity,
}

/// Wallet key (blockchain-specific)
pub enum WalletKey {
    /// BSV key (secp256k1)
    Bsv(nour::crypto::PrivateKey),
    /// Solana key (Ed25519)
    Solana(ed25519_dalek::Keypair),
    /// BTC key (secp256k1)
    Btc(bitcoin::PrivateKey),
}

/// Wallet security settings
pub struct WalletSecurity {
    /// Require password for spending
    require_spending_password: bool,
    /// Auto-lock timeout in seconds
    auto_lock_timeout: u64,
    /// Maximum transaction value without confirmation
    max_tx_value_without_confirmation: Amount,
    /// Enable biometric authentication
    enable_biometric: bool,
}

impl UnifiedWallet {
    /// Create new wallet with random mnemonic
    pub fn new(name: &str, passphrase: Option<&str>) -> Result<Self, WalletError> {
        // Generate random mnemonic
        let mnemonic = Mnemonic::generate(24)?; // 24 words for maximum security
        
        // Create wallet from mnemonic
        Self::from_mnemonic(&mnemonic, passphrase, name)
    }
    
    /// Create wallet from existing mnemonic
    pub fn from_mnemonic(
        mnemonic: &Mnemonic,
        passphrase: Option<&str>,
        name: &str,
    ) -> Result<Self, WalletError> {
        // Generate seed from mnemonic
        let seed = mnemonic.to_seed(passphrase.unwrap_or(""))?;
        
        // Create wallet instance
        let wallet = Self {
            id: uuid::Uuid::new_v4().to_string(),
            mnemonic: mnemonic.clone(),
            seed,
            passphrase: passphrase.map(|s| s.to_string()),
            keys: HashMap::new(),
            name: name.to_string(),
            created_at: chrono::Utc::now(),
            last_used: chrono::Utc::now(),
            storage: Arc::new(EncryptedStorage::new()?),
            security: WalletSecurity::default(),
        };
        
        // Save wallet to storage
        wallet.save()?;
        
        Ok(wallet)
    }
    
    /// Get address for specific blockchain and derivation path
    pub fn get_address(
        &self,
        blockchain: BlockchainType,
        derivation_path: &DerivationPath,
    ) -> Result<Address, WalletError> {
        // Check if key already derived
        let key = if let Some(key) = self.keys.get(&(blockchain, derivation_path.clone())) {
            key.clone()
        } else {
            // Derive new key
            let key = self.derive_key(blockchain, derivation_path)?;
            self.keys.insert((blockchain, derivation_path.clone()), key.clone());
            key
        };
        
        // Convert key to address based on blockchain type
        match (blockchain, key) {
            (BlockchainType::BitcoinSV, WalletKey::Bsv(private_key)) => {
                let public_key = private_key.public_key();
                let address = crate::blockchain::bitcoin_sv::address::Bitcoin_Address_from_pubkey(
                    &public_key,
                    nour::address::Network::Mainnet,
                )?;
                Ok(crate::blockchain::bitcoin_sv::address::to_common_address(&address))
            }
            (BlockchainType::Solana, WalletKey::Solana(keypair)) => {
                let address = crate::blockchain::solana::address::SolanaAddress::from_pubkey(
                    keypair.public,
                );
                Ok(crate::blockchain::solana::address::to_common_address(&address))
            }
            (BlockchainType::BitcoinCore, WalletKey::Btc(private_key)) => {
                // BTC address derivation
                let address = private_key.public_key(&bitcoin::secp256k1::Secp256k1::new()).to_string();
                Ok(Address::Bitcoin(address))
            }
            _ => Err(WalletError::UnsupportedBlockchain(
                format!("Unsupported blockchain: {:?}", blockchain)
            )),
        }
    }
    
    /// Derive private key for specific blockchain and path
    fn derive_key(
        &self,
        blockchain: BlockchainType,
        derivation_path: &DerivationPath,
    ) -> Result<WalletKey, WalletError> {
        // Convert derivation path to blockchain-specific format
        let path = derivation_path.to_blockchain_path(blockchain)?;
        
        match blockchain {
            BlockchainType::BitcoinSV => {
                // Derive BSV key using nour
                let extended_key = nour::wallet::ExtendedKey::from_seed(&self.seed)?;
                let derived_key = extended_key.derive_path(&path)?;
                Ok(WalletKey::Bsv(derived_key.private_key()))
            }
            BlockchainType::Solana => {
                // Derive Solana key using Ed25519
                let keypair = ed25519_dalek::Keypair::from_bytes(&self.seed[..64])
                    .map_err(|e| WalletError::Crypto(e.to_string()))?;
                Ok(WalletKey::Solana(keypair))
            }
            BlockchainType::BitcoinCore => {
                // Derive BTC key
                let master_key = bitcoin::bip32::Xpriv::new_master(
                    bitcoin::Network::Bitcoin,
                    &self.seed,
                )?;
                let derived_key = master_key.derive_priv(&bitcoin::secp256k1::Secp256k1::new(), &path)?;
                Ok(WalletKey::Btc(derived_key.private_key))
            }
        }
    }
    
    /// Sign transaction
    pub fn sign_transaction(
        &self,
        blockchain: BlockchainType,
        derivation_path: &DerivationPath,
        tx_data: &[u8],
    ) -> Result<Vec<u8>, WalletError> {
        // Get private key
        let key = self.derive_key(blockchain, derivation_path)?;
        
        // Sign based on blockchain type
        match (blockchain, key) {
            (BlockchainType::BitcoinSV, WalletKey::Bsv(private_key)) => {
                // Sign BSV transaction using nour
                let signature = private_key.sign(tx_data);
                Ok(signature.serialize_der().to_vec())
            }
            (BlockchainType::Solana, WalletKey::Solana(keypair)) => {
                // Sign Solana transaction
                let signature = keypair.sign(tx_data);
                Ok(signature.to_bytes().to_vec())
            }
            (BlockchainType::BitcoinCore, WalletKey::Btc(private_key)) => {
                // Sign BTC transaction
                let secp = bitcoin::secp256k1::Secp256k1::new();
                let msg = bitcoin::secp256k1::Message::from_slice(&sha256::digest(tx_data))
                    .map_err(|e| WalletError::Crypto(e.to_string()))?;
                let signature = secp.sign_ecdsa(&msg, &private_key.inner);
                Ok(signature.serialize_der().to_vec())
            }
            _ => Err(WalletError::UnsupportedBlockchain(
                format!("Unsupported blockchain: {:?}", blockchain)
            )),
        }
    }
    
    /// Save wallet to encrypted storage
    pub fn save(&self) -> Result<(), WalletError> {
        let serialized = serde_json::to_vec(self)
            .map_err(|e| WalletError::Serialization(e.to_string()))?;
        
        self.storage.save(&self.id, &serialized)?;
        Ok(())
    }
    
    /// Load wallet from storage
    pub fn load(id: &str, password: &str) -> Result<Self, WalletError> {
        let storage = EncryptedStorage::new()?;
        let data = storage.load(id, password)?;
        
        let wallet: Self = serde_json::from_slice(&data)
            .map_err(|e| WalletError::Deserialization(e.to_string()))?;
        
        Ok(wallet)
    }
    
    /// Get wallet balance across all blockchains
    pub async fn get_total_balance(&self) -> Result<HashMap<BlockchainType, Amount>, WalletError> {
        let mut balances = HashMap::new();
        
        // Get balances for each supported blockchain
        for blockchain in &[BlockchainType::BitcoinSV, BlockchainType::Solana] {
            if let Ok(balance) = self.get_balance(*blockchain).await {
                balances.insert(*blockchain, balance);
            }
        }
        
        Ok(balances)
    }
    
    /// Get balance for specific blockchain
    pub async fn get_balance(&self, blockchain: BlockchainType) -> Result<Amount, WalletError> {
        // Use default derivation path
        let derivation_path = DerivationPath::default_for(blockchain);
        
        // Get address
        let address = self.get_address(blockchain, &derivation_path)?;
        
        // Query balance from network
        match blockchain {
            BlockchainType::BitcoinSV => {
                let client = crate::blockchain::bitcoin_sv::network::BitcoinClient::new()?;
                let balance = client.get_balance(&address).await?;
                Ok(balance)
            }
            BlockchainType::Solana => {
                let client = crate::blockchain::solana::network::SolanaClient::new()?;
                let balance = client.get_balance(&address).await?;
                Ok(balance)
            }
            BlockchainType::BitcoinCore => {
                // BTC balance query
                Err(WalletError::UnsupportedBlockchain("BTC balance query not implemented".to_string()))
            }
        }
    }
}

impl Default for WalletSecurity {
    fn default() -> Self {
        Self {
            require_spending_password: true,
            auto_lock_timeout: 300, // 5 minutes
            max_tx_value_without_confirmation: Amount(1_000_000), // 0.01 BTC/BSV
            enable_biometric: false,
        }
    }
}
```

## 7. STANDARD LIBRARY INTEGRATION (`src/std/blockchain/`)

### 7.1 Module Structure

```
src/std/blockchain/
├── lib.ζ                    # Zeta stdlib blockchain module exports
├── btc.ζ                   # BTC_ functions in Zeta
├── bitcoin.ζ               # Bitcoin_ functions in Zeta
├── solana.ζ                # Solana_ functions in Zeta
├── wallet.ζ                # Wallet functions in Zeta
└── types.ζ                 # Blockchain types in Zeta
```

### 7.2 `lib.ζ` - Standard Library Exports

```zeta
//! Blockchain standard library for Zeta
//! Provides native Zeta functions for blockchain operations

module blockchain {
    // Re-export all blockchain modules
    pub use btc;
    pub use bitcoin;
    pub use solana;
    pub use wallet;
    pub use types;
    
    // Common blockchain functions
    pub fn init() -> Result<(), Error> {
        // Initialize blockchain subsystem
        extern "Rust" {
            fn blockchain_init() -> Result<(), Error>;
        }
        blockchain_init()
    }
    
    pub fn cleanup() -> Result<(), Error> {
        // Cleanup blockchain subsystem
        extern "Rust" {
            fn blockchain_cleanup() -> Result<(), Error>;
        }
        blockchain_cleanup()
    }
    
    pub fn version() -> string {
        "0.3.50.0"
    }
}
```

### 7.3 `btc.ζ` - BTC Functions in Zeta

```zeta
//! BTC module - Bitcoin Core compatibility functions

module btc {
    // Address functions
    pub fn address_from_string(s: string) -> Result<Address, Error> {
        extern "Rust" {
            fn BTC_Address_from_string(s: *const u8, len: usize) -> Result<Address, Error>;
        }
        BTC_Address_from_string(s.as_ptr(), s.len())
    }
    
    pub fn address_to_string(addr: Address) -> string {
        extern "Rust" {
            fn BTC_Address_to_string(addr: Address) -> *const u8;
        }
        unsafe {
            let ptr = BTC_Address_to_string(addr);
            std::ffi::c_str_to_string(ptr)
        }
    }
    
    pub fn address_validate(s: string) -> bool {
        extern "Rust" {
            fn BTC_Address_validate(s: *const u8, len: usize) -> bool;
        }
        BTC_Address_validate(s.as_ptr(), s.len())
    }
    
    // Transaction functions
    pub fn transaction_create(inputs: []Input, outputs: []Output) -> Transaction {
        extern "Rust" {
            fn BTC_Transaction_create(inputs: *const Input, inputs_len: usize,
                                     outputs: *const Output, outputs_len: usize) -> Transaction;
        }
        BTC_Transaction_create(inputs.as_ptr(), inputs.len(),
                              outputs.as_ptr(), outputs.len())
    }
    
    pub fn transaction_sign(tx: mut Transaction, key: PrivateKey) -> Result<(), Error> {
        extern "Rust" {
            fn BTC_Transaction_sign(tx: *mut Transaction, key: *const PrivateKey) -> Result<(), Error>;
        }
        BTC_Transaction_sign(&mut tx, &key)
    }
    
    pub fn transaction_serialize(tx: Transaction) -> []u8 {
        extern "Rust" {
            fn BTC_Transaction_serialize(tx: *const Transaction, len: *mut usize) -> *const u8;
        }
        let mut len = 0;
        let ptr = BTC_Transaction_serialize(&tx, &mut len);
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
    
    // Wallet functions
    pub fn wallet_generate() -> Wallet {
        extern "Rust" {
            fn BTC_Wallet_generate() -> Wallet;
        }
        BTC_Wallet_generate()
    }
    
    pub fn wallet_from_mnemonic(words: string, passphrase: Option<string>) -> Result<Wallet, Error> {
        extern "Rust" {
            fn BTC_Wallet_from_mnemonic(words: *const u8, words_len: usize,
                                       passphrase: *const u8, passphrase_len: usize,
                                       has_passphrase: bool) -> Result<Wallet, Error>;
        }
        match passphrase {
            Some(pass) => BTC_Wallet_from_mnemonic(words.as_ptr(), words.len(),
                                                  pass.as_ptr(), pass.len(), true),
            None => BTC_Wallet_from_mnemonic(words.as_ptr(), words.len(),
                                            std::ptr::null(), 0, false),
        }
    }
}
```

### 7.4 `bitcoin.ζ` - Bitcoin Functions in Zeta

```zeta
//! Bitcoin module - BSV functions using nour library

module bitcoin {
    // Address functions
    pub fn address_from_string(s: string) -> Result<Address, Error> {
        extern "Rust" {
            fn Bitcoin_Address_from_string(s: *const u8, len: usize) -> Result<Address, Error>;
        }
        Bitcoin_Address_from_string(s.as_ptr(), s.len())
    }
    
    pub fn address_to_string(addr: Address) -> string {
        extern "Rust" {
            fn Bitcoin_Address_to_string(addr: Address) -> *const u8;
        }
        unsafe {
            let ptr = Bitcoin_Address_to_string(addr);
            std::ffi::c_str_to_string(ptr)
        }
    }
    
    pub fn address_validate(s: string) -> bool {
        extern "Rust" {
            fn Bitcoin_Address_validate(s: *const u8, len: usize) -> bool;
        }
        Bitcoin_Address_validate(s.as_ptr(), s.len())
    }
    
    pub fn address_from_pubkey(pubkey: []u8, network: string) -> Result<Address, Error> {
        extern "Rust" {
            fn Bitcoin_Address_from_pubkey(pubkey: *const u8, pubkey_len: usize,
                                          network: *const u8, network_len: usize) -> Result<Address, Error>;
        }
        Bitcoin_Address_from_pubkey(pubkey.as_ptr(), pubkey.len(),
                                   network.as_ptr(), network.len())
    }
    
    // Transaction functions
    pub fn transaction_create(inputs: []Input, outputs: []Output) -> Transaction {
        extern "Rust" {
            fn Bitcoin_Transaction_create(inputs: *const Input, inputs_len: usize,
                                         outputs: *const Output, outputs_len: usize) -> Transaction;
        }
        Bitcoin_Transaction_create(inputs.as_ptr(), inputs.len(),
                                  outputs.as_ptr(), outputs.len())
    }
    
    pub fn transaction_sign(tx: mut Transaction, key: PrivateKey) -> Result<(), Error> {
        extern "Rust" {
            fn Bitcoin_Transaction_sign(tx: *mut Transaction, key: *const PrivateKey) -> Result<(), Error>;
        }
        Bitcoin_Transaction_sign(&mut tx, &key)
    }
    
    pub fn transaction_serialize(tx: Transaction) -> []u8 {
        extern "Rust" {
            fn Bitcoin_Transaction_serialize(tx: *const Transaction, len: *mut usize) -> *const u8;
        }
        let mut len = 0;
        let ptr = Bitcoin_Transaction_serialize(&tx, &mut len);
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
    
    pub fn transaction_calculate_fee(tx: Transaction, fee_rate: u64) -> u64 {
        extern "Rust" {
            fn Bitcoin_Transaction_calculate_fee(tx: *const Transaction, fee_rate: u64) -> u64;
        }
        Bitcoin_Transaction_calculate_fee(&tx, fee_rate)
    }
    
    // Script functions
    pub fn script_from_hex(hex: string) -> Result<Script, Error> {
        extern "Rust" {
            fn Bitcoin_Script_from_hex(hex: *const u8, len: usize) -> Result<Script, Error>;
        }
        Bitcoin_Script_from_hex(hex.as_ptr(), hex.len())
    }
    
    pub fn script_to_hex(script: Script) -> string {
        extern "Rust" {
            fn Bitcoin_Script_to_hex(script: Script) -> *const u8;
        }
        unsafe {
            let ptr = Bitcoin_Script_to_hex(script);
            std::ffi::c_str_to_string(ptr)
        }
    }
    
    pub fn script_execute(script: Script, tx: Transaction) -> bool {
        extern "Rust" {
            fn Bitcoin_Script_execute(script: Script, tx: *const Transaction) -> bool;
        }
        Bitcoin_Script_execute(script, &tx)
    }
    
    // Wallet functions
    pub fn wallet_generate() -> Wallet {
        extern "Rust" {
            fn Bitcoin_Wallet_generate() -> Wallet;
        }
        Bitcoin_Wallet_generate()
    }
    
    pub fn wallet_from_mnemonic(words: string, passphrase: Option<string>) -> Result<Wallet, Error> {
        extern "Rust" {
            fn Bitcoin_Wallet_from_mnemonic(words: *const u8, words_len: usize,
                                           passphrase: *const u8, passphrase_len: usize,
                                           has_passphrase: bool) -> Result<Wallet, Error>;
        }
        match passphrase {
            Some(pass) => Bitcoin_Wallet_from_mnemonic(words.as_ptr(), words.len(),
                                                      pass.as_ptr(), pass.len(), true),
            None => Bitcoin_Wallet_from_mnemonic(words.as_ptr(), words.len(),
                                                std::ptr::null(), 0, false),
        }
    }
    
    pub fn wallet_get_address(wallet: Wallet, index: u32) -> Address {
        extern "Rust" {
            fn Bitcoin_Wallet_get_address(wallet: Wallet, index: u32) -> Address;
        }
        Bitcoin_Wallet_get_address(wallet, index)
    }
    
    // Network functions
    pub fn network_connect(node: string) -> Result<Connection, Error> {
        extern "Rust" {
            fn Bitcoin_Network_connect(node: *const u8, len: usize) -> Result<Connection, Error>;
        }
        Bitcoin_Network_connect(node.as_ptr(), node.len())
    }
    
    pub fn network_send_transaction(tx: Transaction) -> Result<TxId, Error> {
        extern "Rust" {
            fn Bitcoin_Network_send_transaction(tx: *const Transaction) -> Result<TxId, Error>;
        }
        Bitcoin_Network_send_transaction(&tx)
    }
    
    pub fn network_get_balance(address: Address) -> Result<u64, Error> {
        extern "Rust" {
            fn Bitcoin_Network_get_balance(address: Address) -> Result<u64, Error>;
        }
        Bitcoin_Network_get_balance(address)
    }
    
    // Mining functions
    pub fn mining_connect_pool(url: string, worker: string, password: string) -> Result<MiningSession, Error> {
        extern "Rust" {
            fn Bitcoin_Mining_connect_pool(url: *const u8, url_len: usize,
                                          worker: *const u8, worker_len: usize,
                                          password: *const u8, password_len: usize) -> Result<MiningSession, Error>;
        }
        Bitcoin_Mining_connect_pool(url.as_ptr(), url.len(),
                                   worker.as_ptr(), worker.len(),
                                   password.as_ptr(), password.len())
    }
    
    // Token functions
    pub fn token_deploy(name: string, symbol: string, supply: u64) -> Result<TokenId, Error> {
        extern "Rust" {
            fn Bitcoin_Token_deploy(name: *const u8, name_len: usize,
                                   symbol: *const u8, symbol_len: usize,
                                   supply: u64) -> Result<TokenId, Error>;
        }
        Bitcoin_Token_deploy(name.as_ptr(), name.len(),
                            symbol.as_ptr(), symbol.len(),
                            supply)
    }
    
    pub fn token_transfer(token_id: TokenId, to: Address, amount: u64) -> Result<TxId, Error> {
        extern "Rust" {
            fn Bitcoin_Token_transfer(token_id: TokenId, to: Address, amount: u64) -> Result<TxId, Error>;
        }
        Bitcoin_Token_transfer(token_id, to, amount)
    }
}
```

### 7.5 `solana.ζ` - Solana Functions in Zeta

```zeta
//! Solana module - Solana blockchain functions

module solana {
    // Address functions
    pub fn address_from_string(s: string) -> Result<Address, Error> {
        extern "Rust" {
            fn Solana_Address_from_string(s: *const u8, len: usize) -> Result<Address, Error>;
        }
        Solana_Address_from_string(s.as_ptr(), s.len())
    }
    
    pub fn address_to_string(addr: Address) -> string {
        extern "Rust" {
            fn Solana_Address_to_string(addr: Address) -> *const u8;
        }
        unsafe {
            let ptr = Solana_Address_to_string(addr);
            std::ffi::c_str_to_string(ptr)
        }
    }
    
    pub fn address_create_with_seed(base: Address, seed: string, program_id: Address) -> Result<Address, Error> {
        extern "Rust" {
            fn Solana_Address_create_with_seed(base: Address, seed: *const u8, seed_len: usize,
                                              program_id: Address) -> Result<Address, Error>;
        }
        Solana_Address_create_with_seed(base, seed.as_ptr(), seed.len(), program_id)
    }
    
    // Transaction functions
    pub fn transaction_create(instructions: []Instruction, signers: []Keypair) -> Transaction {
        extern "Rust" {
            fn Solana_Transaction_create(instructions: *const Instruction, instructions_len: usize,
                                        signers: *const Keypair, signers_len: usize) -> Transaction;
        }
        Solana_Transaction_create(instructions.as_ptr(), instructions.len(),
                                 signers.as_ptr(), signers.len())
    }
    
    pub fn transaction_sign(tx: mut Transaction, keypairs: []Keypair) -> Result<(), Error> {
        extern "Rust" {
            fn Solana_Transaction_sign(tx: *mut Transaction, keypairs: *const Keypair, keypairs_len: usize) -> Result<(), Error>;
        }
        Solana_Transaction_sign(&mut tx, keypairs.as_ptr(), keypairs.len())
    }
    
    pub fn transaction_serialize(tx: Transaction) -> []u8 {
        extern "Rust" {
            fn Solana_Transaction_serialize(tx: *const Transaction, len: *mut usize) -> *const u8;
        }
        let mut len = 0;
        let ptr = Solana_Transaction_serialize(&tx, &mut len);
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
    
    // Program functions
    pub fn program_create_account(space: u64, owner: Address) -> Instruction {
        extern "Rust" {
            fn Solana_Program_create_account(space: u64, owner: Address) -> Instruction;
        }
        Solana_Program_create_account(space, owner)
    }
    
    pub fn program_transfer_lamports(from: Address, to: Address, amount: u64) -> Instruction {
        extern "Rust" {
            fn Solana_Program_transfer_lamports(from: Address, to: Address, amount: u64) -> Instruction;
        }
        Solana_Program_transfer_lamports(from, to, amount)
    }
    
    // Wallet functions
    pub fn wallet_generate() -> Wallet {
        extern "Rust" {
            fn Solana_Wallet_generate() -> Wallet;
        }
        Solana_Wallet_generate()
    }
    
    pub fn wallet_from_mnemonic(words: string, passphrase: Option<string>) -> Result<Wallet, Error> {
        extern "Rust" {
            fn Solana_Wallet_from_mnemonic(words: *const u8, words_len: usize,
                                          passphrase: *const u8, passphrase_len: usize,
                                          has_passphrase: bool) -> Result<Wallet, Error>;
        }
        match passphrase {
            Some(pass) => Solana_Wallet_from_mnemonic(words.as_ptr(), words.len(),
                                                     pass.as_ptr(), pass.len(), true),
            None => Solana_Wallet_from_mnemonic(words.as_ptr(), words.len(),
                                               std::ptr::null(), 0, false),
        }
    }
    
    pub fn wallet_get_address(wallet: Wallet, index: u32) -> Address {
        extern "Rust" {
            fn Solana_Wallet_get_address(wallet: Wallet, index: u32) -> Address;
        }
        Solana_Wallet_get_address(wallet, index)
    }
    
    // Network functions
    pub fn network_connect(url: string) -> Result<Connection, Error> {
        extern "Rust" {
            fn Solana_Network_connect(url: *const u8, len: usize) -> Result<Connection, Error>;
        }
        Solana_Network_connect(url.as_ptr(), url.len())
    }
    
    pub fn network_send_transaction(tx: Transaction) -> Result<Signature, Error> {
        extern "Rust" {
            fn Solana_Network_send_transaction(tx: *const Transaction) -> Result<Signature, Error>;
        }
        Solana_Network_send_transaction(&tx)
    }
    
    pub fn network_get_balance(address: Address) -> Result<u64, Error> {
        extern "Rust" {
            fn Solana_Network_get_balance(address: Address) -> Result<u64, Error>;
        }
        Solana_Network_get_balance(address)
    }
    
    // Token functions
    pub fn token_create_mint(decimals: u8) -> Result<Mint, Error> {
        extern "Rust" {
            fn Solana_Token_create_mint(decimals: u8) -> Result<Mint, Error>;
        }
        Solana_Token_create_mint(decimals)
    }
    
    pub fn token_create_account(mint: Address, owner: Address) -> Result<Address, Error> {
        extern "Rust" {
            fn Solana_Token_create_account(mint: Address, owner: Address) -> Result<Address, Error>;
        }
        Solana_Token_create_account(mint, owner)
    }
    
    pub fn token_transfer(from: Address, to: Address, mint: Address, amount: u64, owner: Keypair) -> Result<Signature, Error> {
        extern "Rust" {
            fn Solana_Token_transfer(from: Address, to: Address, mint: Address,
                                    amount: u64, owner: Keypair) -> Result<Signature, Error>;
        }
        Solana_Token_transfer(from, to, mint, amount, owner)
    }
}
```

## 8. TEST STRUCTURE (`tests/blockchain/`)

### 8.1 Test Module Structure

```
tests/blockchain/
├── lib.rs                    # Test harness setup
├── common/                   # Common test utilities
│   ├── mod.rs
│   ├── fixtures.rs          # Test fixtures
│   ├── mocks.rs             # Mock implementations
│   └── utils.rs             # Test utilities
├── btc/                     # BTC module tests
│   ├── mod.rs
│   ├── address_tests.rs
│   ├── transaction_tests.rs
│   ├── script_tests.rs
│   └── wallet_tests.rs
├── bitcoin_sv/              # Bitcoin SV module tests
│   ├── mod.rs
│   ├── address_tests.rs
│   ├── transaction_tests.rs
│   ├── script_tests.rs
│   ├── wallet_tests.rs
│   ├── network_tests.rs
│   └── integration_tests.rs
├── solana/                  # Solana module tests
│   ├── mod.rs
│   ├── address_tests.rs
│   ├── transaction_tests.rs
│   ├── program_tests.rs
│   ├── wallet_tests.rs
│   └── network_tests.rs
├── wallet/                  # Wallet module tests
│   ├── mod.rs
│   ├── unified_tests.rs
│   ├── derivation_tests.rs
│   ├── storage_tests.rs
│   └── security_tests.rs
├── integration/             # Integration tests
│   ├── mod.rs
│   ├── cross_chain_tests.rs
│   ├── end_to_end_tests.rs
│   └── performance_tests.rs
└── property/               # Property-based tests
    ├── mod.rs
    ├── fuzzing_tests.rs
    ├── invariant_tests.rs
    └── edge_case_tests.rs
```

### 8.2 Example Test File (`tests/blockchain/bitcoin_sv/address_tests.rs`)

```rust
//! Bitcoin SV address tests

use crate::blockchain::bitcoin_sv::address::*;
use crate::blockchain::bitcoin_sv::error::BitcoinError;
use nour::address::Network;

#[test]
fn test_address_from_string_valid() {
    // Test valid BSV addresses
    let test_cases = vec![
        ("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa", Network::Mainnet), // Satoshi's address
        ("mipcBbFg9gMiCh81Kj8tqqdgoZub1ZJRfn", Network::Testnet), // Testnet address
    ];
    
    for (address_str, expected_network) in test_cases {
        let result = Bitcoin_Address_from_string(address_str);
        assert!(result.is_ok(), "Failed to parse address: {}", address_str);
        
        let address = result.unwrap();
        assert_eq!(address.network(), expected_network);
    }
}

#[test]
fn test_address_from_string_invalid() {
    // Test invalid addresses
    let invalid_addresses = vec![
        "", // Empty string
        "not-an-address", // Not Base58
        "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNb", // Invalid checksum
        "3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy", // P2SH address (different prefix)
    ];
    
    for address_str in invalid_addresses {
        let result = Bitcoin_Address_from_string(address_str);
        assert!(result.is_err(), "Should fail for invalid address: {}", address_str);
    }
}

#[test]
fn test_address_to_string_roundtrip() {
    // Test roundtrip conversion
    let test_addresses = vec![
        "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa",
        "mipcBbFg9gMiCh81Kj8tqqdgoZub1ZJRfn",
    ];
    
    for address_str in test_addresses {
        let address = Bitcoin_Address_from_string(address_str).unwrap();
        let converted = Bitcoin_Address_to_string(&address);
        assert_eq!(converted, address_str, "Roundtrip conversion failed");
    }
}

#[test]
fn test_address_validation() {
    assert!(Bitcoin_Address_validate("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"));
    assert!(!Bitcoin_Address_validate("invalid-address"));
    assert!(!Bitcoin_Address_validate(""));
}

#[test]
fn test_address_from_pubkey() {
    // Generate test public key
    use nour::crypto::{PrivateKey, PublicKey};
    let private_key = PrivateKey::new();
    let public_key = private_key.public_key();
    
    // Test mainnet address
    let address = Bitcoin_Address_from_pubkey(&public_key, Network::Mainnet);
    assert!(address.is_ok());
    
    let address = address.unwrap();
    assert_eq!(address.network(), Network::Mainnet);
    assert_eq!(address.address_type(), "p2pkh");
}

#[test]
fn test_address_conversion_to_common() {
    let bsv_address = Bitcoin_Address_from_string("1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa").unwrap();
    let common_address = to_common_address(&bsv_address);
    
    match common_address {
        crate::blockchain::types::Address::Bitcoin(s) => {
            assert_eq!(s, "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa");
        }
        _ => panic!("Expected Bitcoin address type"),
    }
}

#[test]
fn test_address_conversion_from_common() {
    let common_address = crate::blockchain::types::Address::Bitcoin(
        "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa".to_string()
    );
    
    let bsv_address = from_common_address(&common_address);
    assert!(bsv_address.is_ok());
    
    let bsv_address = bsv_address.unwrap();
    assert_eq!(bsv_address.network(), Network::Mainnet);
}
```

## 9. BUILD SYSTEM INTEGRATION

### 9.1 Cargo Build Configuration

#### **`.cargo/config.toml` Updates**
```toml
[build]
# Enable LTO for release builds
rustflags = ["-C", "link-arg=-fuse-ld=lld"]

[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 1
panic = "abort"

[profile.dev]
opt-level = 0
debug = true

[profile.test]
opt-level = 0
debug = true
```

#### **`build.rs` - Build Script**
```rust
//! Build script for blockchain features

use std::env;
use std::path::PathBuf;

fn main() {
    // Check for feature flags
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=Cargo.toml");
    
    // Set feature flags for conditional compilation
    if env::var("CARGO_FEATURE_BITCOIN_SV").is_ok() {
        println!("cargo:rustc-cfg=feature=\"bitcoin-sv\"");
    }
    
    if env::var("CARGO_FEATURE_SOLANA").is_ok() {
        println!("cargo:rustc-cfg=feature=\"solana\"");
    }
    
    // Generate version info
    let version = env::var("CARGO_PKG_VERSION").unwrap();
    println!("cargo:rustc-env=ZETA_VERSION={}", version);
    
    // Generate build timestamp
    let timestamp = chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC");
    println!("cargo:rustc-env=BUILD_TIMESTAMP={}", timestamp);
    
    // Generate git commit hash if available
    if let Ok(output) = std::process::Command::new("git")
        .args(&["rev-parse", "--short", "HEAD"])
        .output()
    {
        if output.status.success() {
            let commit_hash = String::from_utf8_lossy(&output.stdout).trim().to_string();
            println!("cargo:rustc-env=GIT_COMMIT_HASH={}", commit_hash);
        }
    }
}
```

### 9.2 Feature-specific Build Options

#### **BSV-specific Build Options**
```toml
# When building with bitcoin-sv feature
[features.bitcoin-sv.dependencies]
nour = { version = "1.0.0", features = ["async"] }
secp256k1 = { version = "0.31.1", features = ["global-context", "rand"] }

[features.bitcoin-sv.build-dependencies]
cc = "1.0"
```

#### **Solana-specific Build Options**
```toml
# When building with solana feature
[features.solana.dependencies]
solana-sdk = { version = "2.0.0", features = ["full"] }
solana-client = { version = "2.0.0", features = ["rpc", "rpc-transaction"] }
ed25519-dalek = { version = "2.1.1", features = ["serde", "rand"] }

[features.solana.build-dependencies]
solana-version = "2.0.0"
```

## 10. DEPLOYMENT CONFIGURATION

### 10.1 Runtime Configuration Files

#### **`config/blockchain.toml` - Runtime Configuration**
```toml
# Blockchain configuration file
[general]
# Enable/disable blockchain features
enable_bitcoin_sv = true
enable_solana = true
enable_networking = true

# Network settings
network = "testnet"  # mainnet, testnet, devnet, simulation
data_dir = "~/.zeta/blockchain"

# Performance settings
connection_timeout = 30
request_timeout = 60
retry_attempts = 3
max_connections = 10

# Logging
log_level = "info"
log_file = "blockchain.log"
enable_debug_logging = false

[bitcoin_sv]
# BSV-specific settings
p2p_enabled = true
p2p_port = 8333
rpc_enabled = false
rpc_port = 8332
max_mempool_size = 300  # MB

[solana]
# Solana-specific settings
rpc_url = "https://api.devnet.solana.com"
websocket_url = "wss://api.devnet.solana.com"
commitment = "confirmed"
skip_preflight = false

[security]
# Security settings
require_hardware_wallet = false
max_tx_value_without_confirmation = 1000000  # satoshis/lamports
enable_multisig = false
required_confirmations = 3
enable_tx_monitoring = true

[wallet]
# Wallet settings
encryption_enabled = true
auto_lock_timeout = 300  # seconds
backup_interval = 86400  # seconds (24 hours)
max_backup_count = 10
```

### 10.2 Environment Variables

```bash
# Blockchain environment variables
export ZETA_BLOCKCHAIN_ENABLED=1
export ZETA_BLOCKCHAIN_NETWORK=testnet
export ZETA_BLOCKCHAIN_DATA_DIR=~/.zeta/blockchain

# BSV-specific
export ZETA_BSV_P2P_ENABLED=1
export ZETA_BSV_RPC_ENABLED=0

# Solana-specific
export ZETA_SOLANA_RPC_URL=https://api.devnet.solana.com
export ZETA_SOLANA_COMMITMENT=confirmed

# Security
export ZETA_WALLET_ENCRYPTION_KEY=  # Set via secure input
export ZETA_REQUIRE_HARDWARE_WALLET=0

# Logging
export RUST_LOG=zeta_blockchain=info
export RUST_BACKTRACE=1
```

## 11. DOCUMENTATION STRUCTURE

### 11.1 API Documentation

```
docs/
├── api/                      # API documentation
│   ├── blockchain/          # Blockchain API
│   │   ├── overview.md
│   │   ├── btc.md          # BTC_ functions
│   │   ├── bitcoin.md      # Bitcoin_ functions
│   │   ├── solana.md       # Solana_ functions
│   │   └── wallet.md       # Wallet functions
│   ├── examples/           # Code examples
│   │   ├── basic_usage.md
│   │   ├── transaction_examples.md
│   │   ├── wallet_examples.md
│   │   └── token_examples.md
│   └── reference/          # Reference documentation
│       ├── error_codes.md
│       ├── types.md
│       └── constants.md
├── guides/                  # User guides
│   ├── getting_started.md
│   ├── wallet_management.md
│   ├── transaction_guide.md
│   └── security_best_practices.md
├── architecture/           # Architecture documentation
│   ├── design_decisions.md
│   ├── module_structure.md
│   └── security_architecture.md
└── development/           # Development documentation
    ├── building.md
    ├── testing.md
    ├── contributing.md
    └── release_process.md
```

### 11.2 Example Documentation File (`docs/api/blockchain/overview.md`)

```markdown
# Blockchain API Overview

## Introduction

The Zeta Blockchain API provides comprehensive support for multiple blockchain networks, including Bitcoin SV (via Father's `nour` library) and Solana. The API is designed with professional differentiation between blockchain ecosystems while maintaining a consistent developer experience.

## Key Features

- **Multi-chain Support**: BSV, Solana, and Bitcoin Core compatibility
- **Unified Wallet**: Single wallet supporting multiple blockchains
- **Security First**: Hardware wallet integration and encrypted storage
- **Performance**: Efficient cryptography and network operations
- **Extensibility**: Modular architecture for future blockchain additions

## Function Families

### BTC_ Functions
Legacy Bitcoin Core compatibility functions for applications requiring Bitcoin Core protocol support.

### Bitcoin_ Functions
Bitcoin SV implementation using Father's `nour` library, providing full BSV protocol support including Genesis rules and scaling features.

### Solana_ Functions
Solana blockchain integration using the official `solana-sdk`, supporting smart contracts, SPL tokens, and high-performance transactions.

## Getting Started

### Basic Usage Example

```zeta
import blockchain;

fn main() -> Result<(), Error> {
    // Initialize blockchain subsystem
    blockchain::init()?;
    
    // Create a BSV wallet
    let wallet = blockchain::bitcoin::wallet_generate();
    
    // Get wallet address
    let address = blockchain::bitcoin::wallet_get_address(wallet, 0);
    println!("Address: {}", address);
    
    // Create and send transaction
    let tx = blockchain::bitcoin::transaction_create(inputs, outputs);
    let txid = blockchain::bitcoin::network_send_transaction(tx)?;
    println!("Transaction sent: {}", txid);
    
    // Cleanup
    blockchain::cleanup()?;
    Ok(())
}
```

### Multi-chain Wallet Example

```zeta
import blockchain;

fn main() -> Result<(), Error> {
    // Create unified wallet
    let wallet = blockchain::wallet::new("My Wallet", None)?;
    
    // Get addresses for different blockchains
    let bsv_address = wallet.get_address(BlockchainType::BitcoinSV, 0)?;
    let solana_address = wallet.get_address(BlockchainType::Solana, 0)?;
    
    println!("BSV Address: {}", bsv_address);
    println!("Solana Address: {}", solana_address);
    
    // Get total balance across all chains
    let balances = wallet.get_total_balance()?;
    for (blockchain, balance) in balances {
        println!("{} balance: {}", blockchain, balance);
    }
    
    Ok(())
}
```

## Error Handling

All blockchain functions return `Result<T, Error>` where `Error` is a comprehensive error type that includes:

- **Blockchain-specific errors**: Network errors, transaction validation failures, etc.
- **Security errors**: Authentication failures, invalid signatures, etc.
- **Configuration errors**: Invalid settings, missing dependencies, etc.

## Security Considerations

1. **Private Key Storage**: Always use encrypted storage or hardware wallets
2. **Transaction Signing**: Implement multi-signature for high-value transactions
3. **Network Security**: Use TLS for all RPC communications
4. **Input Validation**: Sanitize all user inputs before processing
5. **Audit Logging**: Maintain comprehensive audit trails for compliance

## Performance Tips

1. **Batch Operations**: Use batch RPC calls when possible
2. **Connection Pooling**: Reuse network connections
3. **Caching**: Cache frequently accessed data (balances, transaction status)
4. **Async Operations**: Use async functions for network operations
5. **Memory Management**: Clear sensitive data from memory when done

## Next Steps

- [BTC Functions Reference](./btc.md)
- [Bitcoin Functions Reference](./bitcoin.md)
- [Solana Functions Reference](./solana.md)
- [Wallet Management Guide](../guides/wallet_management.md)
```

## 12. CONCLUSION

This module structure specification provides a comprehensive blueprint for implementing blockchain integration in Zeta v0.3.50+. The architecture is designed to be:

1. **Modular**: Clear separation between blockchain implementations
2. **Extensible**: Easy to add new blockchain support
3. **Secure**: Robust security architecture for key management
4. **Performant**: Efficient implementation leveraging Rust's strengths
5. **Maintainable**: Comprehensive testing and documentation

The specification follows Father's mandate to use the `nour` library as the primary BSV implementation while providing professional-grade support for multiple blockchain ecosystems. The unified wallet architecture ensures a consistent user experience while maintaining the security and isolation required for production use.

With this structure in place, the Zeta compiler will be positioned as a leading development platform for multi-chain blockchain applications, combining the scalability of BSV with the smart contract capabilities of Solana in a single, cohesive development environment.

---
*Module structure specification completed: 2026-04-02 10:41 GMT+1*
*Next step: Implementation according to the integration plan*
