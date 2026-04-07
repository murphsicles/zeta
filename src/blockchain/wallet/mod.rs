//! Unified wallet module
//!
//! Implements BIP-39 wallet for both BSV and Solana with secure key management.

pub mod bip39;
pub mod encryption;
pub mod storage;

// Re-export wallet functionality
pub use bip39::*;
pub use encryption::*;
pub use storage::*;

use crate::blockchain::common::error::BlockchainError;
use crate::blockchain::common::config::BlockchainConfig;
use crate::blockchain::common::types::{DerivationPath, Network, Address, KeyPair};
use crate::blockchain::bsv::keys::BtcKeyPair;
use crate::blockchain::solana::address::SolanaAddress;

/// Main wallet structure
#[derive(Debug, Clone)]
pub struct Wallet {
    /// Encrypted master seed
    master_seed: Vec<u8>,
    /// Encryption salt
    salt: Vec<u8>,
    /// Wallet configuration
    config: WalletConfig,
    /// Derived keys cache
    key_cache: std::collections::HashMap<String, KeyPair>,
}

/// Wallet configuration
#[derive(Debug, Clone)]
pub struct WalletConfig {
    /// Encryption algorithm
    pub encryption_algorithm: String,
    /// Key derivation iterations
    pub key_derivation_iterations: u32,
    /// Auto-lock timeout (seconds)
    pub auto_lock_timeout: u64,
    /// Default derivation paths
    pub default_paths: DefaultPaths,
}

/// Default derivation paths
#[derive(Debug, Clone)]
pub struct DefaultPaths {
    /// BSV derivation path
    pub bsv: DerivationPath,
    /// Solana derivation path
    pub solana: DerivationPath,
}

impl Wallet {
    /// Create new wallet from mnemonic
    pub fn new(mnemonic: &str, password: &str, config: WalletConfig) -> Result<Self, BlockchainError> {
        log::debug!("Creating new wallet from mnemonic");
        
        // Generate seed from mnemonic
        let seed = bip39::mnemonic_to_seed(mnemonic, password)?;
        
        // Encrypt the seed
        let (encrypted_seed, salt) = encryption::encrypt_seed(&seed, password)?;
        
        Ok(Self {
            master_seed: encrypted_seed,
            salt,
            config,
            key_cache: std::collections::HashMap::new(),
        })
    }
    
    /// Generate new wallet with random mnemonic
    pub fn generate(word_count: usize, password: &str, config: WalletConfig) -> Result<(Self, String), BlockchainError> {
        log::debug!("Generating new wallet with {} words", word_count);
        
        // Generate mnemonic
        let mnemonic = bip39::generate_mnemonic(word_count)?;
        
        // Create wallet from mnemonic
        let wallet = Self::new(&mnemonic, password, config)?;
        
        Ok((wallet, mnemonic))
    }
    
    /// Derive key for specific path
    pub fn derive_key(&mut self, path: &DerivationPath, password: &str) -> Result<KeyPair, BlockchainError> {
        let cache_key = path.to_string();
        
        // Check cache first
        if let Some(key) = self.key_cache.get(&cache_key) {
            return Ok(key.clone());
        }
        
        // Decrypt master seed
        let seed = encryption::decrypt_seed(&self.master_seed, password, &self.salt)?;
        
        // Derive key from seed
        let key = bip39::derive_key_from_seed(&seed, path)?;
        
        // Cache the key
        self.key_cache.insert(cache_key, key.clone());
        
        Ok(key)
    }
    
    /// Get address for derivation path
    pub fn get_address(&mut self, path: &DerivationPath, network: Network, password: &str) -> Result<Address, BlockchainError> {
        // Derive the key
        let key = self.derive_key(path, password)?;
        
        // Create address based on network
        match network {
            Network::BsvMainnet | Network::BsvTestnet | Network::BsvStn => {
                // Create BSV address
                use crate::blockchain::bsv::address::Bitcoin_Address_from_pubkey;
                let btc_addr = Bitcoin_Address_from_pubkey(&key.public_key, network.as_str())?;
                Ok(Address::Bsv(btc_addr.to_string()))
            }
            Network::SolanaMainnet | Network::SolanaTestnet | Network::SolanaDevnet => {
                // Create Solana address
                use crate::blockchain::solana::address::Solana_Address_from_pubkey;
                let solana_addr = Solana_Address_from_pubkey(&key.public_key, network.as_str())?;
                Ok(Address::Solana(solana_addr.to_string()))
            }
        }
    }
    
    /// Sign message with key from derivation path
    pub fn sign_message(&mut self, message: &[u8], path: &DerivationPath, password: &str) -> Result<Vec<u8>, BlockchainError> {
        // Derive the key
        let key = self.derive_key(path, password)?;
        
        // Sign based on network (determined from coin type in path)
        if path.coin_type == 236 { // BSV
            use crate::blockchain::bsv::keys::Bitcoin_Key_sign;
            
            // Hash the message first
            use sha2::{Sha256, Digest};
            let hash = Sha256::digest(message);
            
            Bitcoin_Key_sign(&hash, &key.private_key)
        } else if path.coin_type == 501 { // Solana
            // Solana uses Ed25519
            use ed25519_dalek::{SigningKey, Signature};
            
            let signing_key = SigningKey::from_bytes(&key.private_key[..32].try_into()
                .map_err(|_| BlockchainError::crypto("Invalid private key length"))?);
            
            let signature = signing_key.sign(message);
            Ok(signature.to_bytes().to_vec())
        } else {
            Err(BlockchainError::key(format!("Unsupported coin type: {}", path.coin_type)))
        }
    }
    
    /// Save wallet to file
    pub fn save(&self, path: &str, password: &str) -> Result<(), BlockchainError> {
        storage::save_wallet(self, path, password)
    }
    
    /// Load wallet from file
    pub fn load(path: &str, password: &str) -> Result<Self, BlockchainError> {
        storage::load_wallet(path, password)
    }
    
    /// Change wallet password
    pub fn change_password(&mut self, old_password: &str, new_password: &str) -> Result<(), BlockchainError> {
        log::debug!("Changing wallet password");
        
        // Decrypt with old password
        let seed = encryption::decrypt_seed(&self.master_seed, old_password, &self.salt)?;
        
        // Re-encrypt with new password
        let (new_encrypted_seed, new_salt) = encryption::encrypt_seed(&seed, new_password)?;
        
        // Update wallet
        self.master_seed = new_encrypted_seed;
        self.salt = new_salt;
        
        // Clear key cache
        self.key_cache.clear();
        
        Ok(())
    }
    
    /// Lock wallet (clear sensitive data from memory)
    pub fn lock(&mut self) {
        log::debug!("Locking wallet");
        self.key_cache.clear();
        // Note: master_seed remains encrypted
    }
    
    /// Check if wallet is locked
    pub fn is_locked(&self) -> bool {
        self.key_cache.is_empty()
    }
}

/// Initialize wallet module
pub fn init(config: &BlockchainConfig) -> Result<(), BlockchainError> {
    log::debug!("Initializing wallet module");
    
    if !config.wallet.enabled {
        log::info!("Wallet module disabled in configuration");
        return Ok(());
    }
    
    // Initialize submodules
    bip39::init()?;
    encryption::init()?;
    storage::init(config)?;
    
    log::info!("Wallet module initialized successfully");
    Ok(())
}

/// Shutdown wallet module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::debug!("Shutting down wallet module");
    
    storage::shutdown()?;
    
    log::info!("Wallet module shutdown complete");
    Ok(())
}

/// Create default wallet configuration
pub fn default_config() -> WalletConfig {
    WalletConfig {
        encryption_algorithm: "AES-256-GCM".to_string(),
        key_derivation_iterations: 100000,
        auto_lock_timeout: 300,
        default_paths: DefaultPaths {
            bsv: DerivationPath::bsv(0, 0, 0),
            solana: DerivationPath::solana(0, 0, 0),
        },
    }
}