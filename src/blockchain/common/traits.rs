//! Common traits for blockchain operations

use crate::blockchain::common::types::*;
use crate::blockchain::common::error::BlockchainError;

/// Trait for blockchain address operations
pub trait AddressOps {
    /// Create address from public key
    fn from_pubkey(pubkey: &[u8], network: Network) -> Result<Address, BlockchainError>;
    
    /// Validate address format
    fn validate(address: &str) -> bool;
    
    /// Convert address to bytes
    fn to_bytes(address: &str) -> Result<Vec<u8>, BlockchainError>;
    
    /// Convert bytes to address
    fn from_bytes(bytes: &[u8], network: Network) -> Result<Address, BlockchainError>;
}

/// Trait for blockchain transaction operations
pub trait TransactionOps {
    /// Create new transaction
    fn new(
        inputs: Vec<TransactionInput>,
        outputs: Vec<TransactionOutput>,
        fee: Option<u64>,
    ) -> Result<Self, BlockchainError> where Self: Sized;
    
    /// Sign transaction
    fn sign(&mut self, private_key: &[u8]) -> Result<(), BlockchainError>;
    
    /// Verify transaction signature
    fn verify(&self) -> Result<bool, BlockchainError>;
    
    /// Serialize transaction to bytes
    fn to_bytes(&self) -> Result<Vec<u8>, BlockchainError>;
    
    /// Deserialize transaction from bytes
    fn from_bytes(bytes: &[u8]) -> Result<Self, BlockchainError> where Self: Sized;
    
    /// Get transaction ID
    fn id(&self) -> Result<TransactionId, BlockchainError>;
    
    /// Get transaction size in bytes
    fn size(&self) -> usize;
    
    /// Get transaction fee
    fn fee(&self) -> u64;
}

/// Trait for blockchain network operations
pub trait NetworkOps {
    /// Broadcast transaction to network
    fn broadcast_transaction(tx_bytes: &[u8]) -> Result<TransactionId, BlockchainError>;
    
    /// Get transaction status
    fn get_transaction_status(tx_id: &TransactionId) -> Result<TransactionStatus, BlockchainError>;
    
    /// Get balance for address
    fn get_balance(address: &Address) -> Result<Amount, BlockchainError>;
    
    /// Get current block height
    fn get_block_height() -> Result<u64, BlockchainError>;
    
    /// Get fee estimate
    fn estimate_fee(priority: Priority) -> Result<FeeEstimate, BlockchainError>;
    
    /// Get network info
    fn get_network_info() -> Result<NetworkInfo, BlockchainError>;
}

/// Trait for wallet operations
pub trait WalletOps {
    /// Create new wallet
    fn new(mnemonic: Option<&str>, password: Option<&str>) -> Result<Self, BlockchainError> where Self: Sized;
    
    /// Generate new mnemonic
    fn generate_mnemonic(word_count: usize) -> Result<String, BlockchainError>;
    
    /// Derive key from path
    fn derive_key(&self, path: &DerivationPath) -> Result<KeyPair, BlockchainError>;
    
    /// Get address for derivation path
    fn get_address(&self, path: &DerivationPath, network: Network) -> Result<Address, BlockchainError>;
    
    /// Sign message with key
    fn sign_message(&self, message: &[u8], path: &DerivationPath) -> Result<Vec<u8>, BlockchainError>;
    
    /// Verify message signature
    fn verify_message(&self, message: &[u8], signature: &[u8], address: &Address) -> Result<bool, BlockchainError>;
    
    /// Encrypt and store wallet
    fn save(&self, path: &str, password: &str) -> Result<(), BlockchainError>;
    
    /// Load and decrypt wallet
    fn load(path: &str, password: &str) -> Result<Self, BlockchainError> where Self: Sized;
}

/// Transaction input
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransactionInput {
    /// Previous transaction ID
    pub txid: TransactionId,
    /// Output index
    pub vout: u32,
    /// Script signature
    pub script_sig: Vec<u8>,
    /// Sequence number
    pub sequence: u32,
}

/// Transaction output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransactionOutput {
    /// Amount to send
    pub amount: Amount,
    /// Recipient address
    pub address: Address,
    /// Script public key
    pub script_pubkey: Vec<u8>,
}

/// Key pair for signing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KeyPair {
    /// Private key (encrypted in memory)
    pub private_key: Vec<u8>,
    /// Public key
    pub public_key: Vec<u8>,
    /// Chain code for derivation
    pub chain_code: Vec<u8>,
}

/// Network information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkInfo {
    /// Network name
    pub name: String,
    /// Current block height
    pub block_height: u64,
    /// Network difficulty
    pub difficulty: f64,
    /// Median fee rate
    pub median_fee: u64,
    /// Network version
    pub version: String,
    /// Is network synced
    pub synced: bool,
}

/// Blockchain client interface
pub trait BlockchainClient: AddressOps + TransactionOps + NetworkOps {
    /// Initialize client
    fn init(config: &BlockchainConfig) -> Result<(), BlockchainError>;
    
    /// Shutdown client
    fn shutdown() -> Result<(), BlockchainError>;
    
    /// Get client version
    fn version() -> &'static str;
    
    /// Check if client is connected
    fn is_connected() -> bool;
}

/// Unified blockchain operations
pub trait UnifiedBlockchain {
    /// Send transaction across chains
    fn send_transaction(
        &self,
        from: &Address,
        to: &Address,
        amount: Amount,
        fee: Option<u64>,
    ) -> Result<TransactionId, BlockchainError>;
    
    /// Get cross-chain balance
    fn get_balance(&self, address: &Address) -> Result<Amount, BlockchainError>;
    
    /// Convert between currencies
    fn convert_currency(
        &self,
        from: Currency,
        to: Currency,
        amount: u64,
    ) -> Result<u64, BlockchainError>;
    
    /// Bridge assets between chains
    fn bridge_assets(
        &self,
        from_chain: Network,
        to_chain: Network,
        amount: Amount,
        recipient: &Address,
    ) -> Result<TransactionId, BlockchainError>;
}