//! Common type definitions for blockchain operations

use serde::{Serialize, Deserialize};

/// Network type for blockchain operations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Network {
    /// Bitcoin SV mainnet
    BsvMainnet,
    /// Bitcoin SV testnet
    BsvTestnet,
    /// Bitcoin SV scaling test network
    BsvStn,
    /// Solana mainnet
    SolanaMainnet,
    /// Solana testnet
    SolanaTestnet,
    /// Solana devnet
    SolanaDevnet,
}

impl Network {
    /// Get network as string
    pub fn as_str(&self) -> &'static str {
        match self {
            Network::BsvMainnet => "bsv-mainnet",
            Network::BsvTestnet => "bsv-testnet",
            Network::BsvStn => "bsv-stn",
            Network::SolanaMainnet => "solana-mainnet",
            Network::SolanaTestnet => "solana-testnet",
            Network::SolanaDevnet => "solana-devnet",
        }
    }
    
    /// Parse network from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "bsv-mainnet" => Some(Network::BsvMainnet),
            "bsv-testnet" => Some(Network::BsvTestnet),
            "bsv-stn" => Some(Network::BsvStn),
            "solana-mainnet" => Some(Network::SolanaMainnet),
            "solana-testnet" => Some(Network::SolanaTestnet),
            "solana-devnet" => Some(Network::SolanaDevnet),
            _ => None,
        }
    }
    
    /// Check if network is Bitcoin SV
    pub fn is_bsv(&self) -> bool {
        matches!(self, Network::BsvMainnet | Network::BsvTestnet | Network::BsvStn)
    }
    
    /// Check if network is Solana
    pub fn is_solana(&self) -> bool {
        matches!(self, Network::SolanaMainnet | Network::SolanaTestnet | Network::SolanaDevnet)
    }
}

/// Amount type for blockchain transactions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Amount {
    /// Amount in satoshis (for BSV) or lamports (for Solana)
    pub value: u64,
    /// Currency type
    pub currency: Currency,
}

impl Amount {
    /// Create a new amount
    pub fn new(value: u64, currency: Currency) -> Self {
        Self { value, currency }
    }
    
    /// Create BSV amount in satoshis
    pub fn bsv_satoshis(satoshis: u64) -> Self {
        Self {
            value: satoshis,
            currency: Currency::Bsv,
        }
    }
    
    /// Create Solana amount in lamports
    pub fn solana_lamports(lamports: u64) -> Self {
        Self {
            value: lamports,
            currency: Currency::Solana,
        }
    }
    
    /// Convert to human-readable string
    pub fn to_string(&self) -> String {
        match self.currency {
            Currency::Bsv => format!("{} satoshis", self.value),
            Currency::Solana => format!("{} lamports", self.value),
        }
    }
}

/// Currency type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Currency {
    /// Bitcoin SV
    Bsv,
    /// Solana
    Solana,
}

/// Transaction status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TransactionStatus {
    /// Transaction is pending
    Pending,
    /// Transaction is confirmed
    Confirmed,
    /// Transaction failed
    Failed,
    /// Transaction is unknown
    Unknown,
}

/// Address type for different blockchains
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Address {
    /// Bitcoin SV address
    Bsv(String),
    /// Solana address
    Solana(String),
}

impl Address {
    /// Get address as string
    pub fn as_str(&self) -> &str {
        match self {
            Address::Bsv(addr) => addr,
            Address::Solana(addr) => addr,
        }
    }
    
    /// Get network type for this address
    pub fn network(&self) -> Network {
        match self {
            Address::Bsv(_) => Network::BsvMainnet, // Default to mainnet
            Address::Solana(_) => Network::SolanaMainnet, // Default to mainnet
        }
    }
    
    /// Validate address format
    pub fn validate(&self) -> bool {
        match self {
            Address::Bsv(addr) => {
                // Basic BSV address validation (Base58, starts with 1 or 3)
                addr.len() >= 26 && addr.len() <= 35 && 
                (addr.starts_with('1') || addr.starts_with('3'))
            }
            Address::Solana(addr) => {
                // Basic Solana address validation (Base58, 32-44 chars)
                addr.len() >= 32 && addr.len() <= 44
            }
        }
    }
}

/// Transaction ID type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TransactionId(String);

impl TransactionId {
    /// Create new transaction ID
    pub fn new(id: String) -> Self {
        Self(id)
    }
    
    /// Get transaction ID as string
    pub fn as_str(&self) -> &str {
        &self.0
    }
    
    /// Get transaction ID as bytes
    pub fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }
}

/// Block hash type
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BlockHash(String);

impl BlockHash {
    /// Create new block hash
    pub fn new(hash: String) -> Self {
        Self(hash)
    }
    
    /// Get block hash as string
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Fee estimation for transactions
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct FeeEstimate {
    /// Fee per byte (satoshis/byte for BSV, lamports/byte for Solana)
    pub fee_per_byte: u64,
    /// Estimated total fee
    pub total_fee: u64,
    /// Priority level
    pub priority: Priority,
}

/// Transaction priority
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Priority {
    /// Low priority (cheaper, slower)
    Low,
    /// Medium priority
    Medium,
    /// High priority (more expensive, faster)
    High,
}

/// Key derivation path
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DerivationPath {
    /// Purpose field (44' for BIP44)
    pub purpose: u32,
    /// Coin type (236 for BSV, 501 for Solana)
    pub coin_type: u32,
    /// Account index
    pub account: u32,
    /// Change (0 for external, 1 for internal)
    pub change: u32,
    /// Address index
    pub index: u32,
}

impl DerivationPath {
    /// Create BSV derivation path
    pub fn bsv(account: u32, change: u32, index: u32) -> Self {
        Self {
            purpose: 44,
            coin_type: 236, // BSV coin type
            account,
            change,
            index,
        }
    }
    
    /// Create Solana derivation path
    pub fn solana(account: u32, change: u32, index: u32) -> Self {
        Self {
            purpose: 44,
            coin_type: 501, // Solana coin type
            account,
            change,
            index,
        }
    }
    
    /// Convert to string representation
    pub fn to_string(&self) -> String {
        format!("m/{}'/{}'/{}'/{}/{}", 
            self.purpose, 
            self.coin_type, 
            self.account, 
            self.change, 
            self.index)
    }
}