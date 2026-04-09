//! BIP-39 mnemonic and seed generation
//!
//! Implements BIP-39 standard for mnemonic phrase generation and seed derivation.

use crate::blockchain::common::error::BlockchainError;
use crate::blockchain::common::types::{DerivationPath, KeyPair};
use bip39::{Mnemonic, Language, Seed};
use hmac::Hmac;
use pbkdf2::pbkdf2;
use sha2::Sha512;

/// Generate BIP-39 mnemonic
/// 
/// # Arguments
/// * `word_count` - Number of words (12, 15, 18, 21, or 24)
/// 
/// # Returns
/// * `Ok(String)` - Mnemonic phrase
/// * `Err(WalletError)` - Invalid word count
pub fn generate_mnemonic(word_count: usize) -> Result<String, BlockchainError> {
    let entropy_bits = match word_count {
        12 => 128,
        15 => 160,
        18 => 192,
        21 => 224,
        24 => 256,
        _ => return Err(BlockchainError::wallet(
            format!("Invalid word count: {} (must be 12, 15, 18, 21, or 24)", word_count)
        )),
    };
    
    let mnemonic = Mnemonic::generate_in(Language::English, entropy_bits)
        .map_err(|e| BlockchainError::wallet(format!("Failed to generate mnemonic: {}", e)))?;
    
    Ok(mnemonic.to_string())
}

/// Convert mnemonic to seed
/// 
/// # Arguments
/// * `mnemonic` - BIP-39 mnemonic phrase
/// * `password` - Optional passphrase
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - 64-byte seed
/// * `Err(WalletError)` - Invalid mnemonic
pub fn mnemonic_to_seed(mnemonic: &str, password: &str) -> Result<Vec<u8>, BlockchainError> {
    let mnemonic_obj = Mnemonic::parse_in(Language::English, mnemonic)
        .map_err(|e| BlockchainError::wallet(format!("Invalid mnemonic: {}", e)))?;
    
    let seed = Seed::new(&mnemonic_obj, password);
    Ok(seed.as_bytes().to_vec())
}

/// Validate mnemonic phrase
/// 
/// # Arguments
/// * `mnemonic` - Mnemonic phrase to validate
/// 
/// # Returns
/// * `Ok(bool)` - True if mnemonic is valid
pub fn validate_mnemonic(mnemonic: &str) -> bool {
    Mnemonic::parse_in(Language::English, mnemonic).is_ok()
}

/// Derive key from seed using BIP-32
/// 
/// # Arguments
/// * `seed` - 64-byte seed
/// * `path` - Derivation path
/// 
/// # Returns
/// * `Ok(KeyPair)` - Derived key pair
/// * `Err(WalletError)` - Derivation failed
pub fn derive_key_from_seed(seed: &[u8], path: &DerivationPath) -> Result<KeyPair, BlockchainError> {
    if seed.len() != 64 {
        return Err(BlockchainError::wallet(
            format!("Invalid seed length: {} bytes (expected 64)", seed.len())
        ));
    }
    
    // BIP-32 master key derivation
    let master_key = derive_master_key(seed)?;
    
    // Derive child keys according to path
    let key = derive_child_key(&master_key, path)?;
    
    Ok(key)
}

/// Derive BIP-32 master key from seed
fn derive_master_key(seed: &[u8]) -> Result<MasterKey, BlockchainError> {
    // HMAC-SHA512 with "Bitcoin seed" as key
    let mut hmac = Hmac::<Sha512>::new_from_slice(b"Bitcoin seed")
        .map_err(|e| BlockchainError::crypto(format!("HMAC init failed: {}", e)))?;
    
    hmac.update(seed);
    let result = hmac.finalize().into_bytes();
    
    let (il, ir) = result.split_at(32);
    
    Ok(MasterKey {
        private_key: il.to_vec(),
        chain_code: ir.to_vec(),
    })
}

/// Derive child key from master key
fn derive_child_key(master_key: &MasterKey, path: &DerivationPath) -> Result<KeyPair, BlockchainError> {
    let mut current_key = master_key.clone();
    
    // Derive each level in the path
    let levels = [
        (path.purpose, true),   // hardened
        (path.coin_type, true), // hardened
        (path.account, true),   // hardened
        (path.change, false),   // not hardened
        (path.index, false),    // not hardened
    ];
    
    for (index, hardened) in levels.iter() {
        current_key = derive_child(&current_key, *index, *hardened)?;
    }
    
    // Get public key from private key
    let public_key = get_public_key(&current_key.private_key, true)?; // compressed
    
    Ok(KeyPair {
        private_key: current_key.private_key,
        public_key,
        chain_code: current_key.chain_code,
    })
}

/// Derive child key from parent
fn derive_child(parent: &MasterKey, index: u32, hardened: bool) -> Result<MasterKey, BlockchainError> {
    let mut hmac = Hmac::<Sha512>::new_from_slice(&parent.chain_code)
        .map_err(|e| BlockchainError::crypto(format!("HMAC init failed: {}", e)))?;
    
    if hardened {
        // Hardened derivation: 0x00 + private_key + index
        let mut data = vec![0x00];
        data.extend_from_slice(&parent.private_key);
        data.extend_from_slice(&index.to_be_bytes());
        hmac.update(&data);
    } else {
        // Normal derivation: public_key + index
        let public_key = get_public_key(&parent.private_key, true)?;
        let mut data = public_key;
        data.extend_from_slice(&index.to_be_bytes());
        hmac.update(&data);
    }
    
    let result = hmac.finalize().into_bytes();
    let (il, ir) = result.split_at(32);
    
    // Add il to parent private key (mod n)
    let child_private_key = add_private_keys(&parent.private_key, il)?;
    
    Ok(MasterKey {
        private_key: child_private_key,
        chain_code: ir.to_vec(),
    })
}

/// Get public key from private key
fn get_public_key(private_key: &[u8], compressed: bool) -> Result<Vec<u8>, BlockchainError> {
    use secp256k1::{Secp256k1, SecretKey, PublicKey};
    
    let secp = Secp256k1::new();
    let secret_key = SecretKey::from_slice(private_key)
        .map_err(|e| BlockchainError::crypto(format!("Invalid private key: {}", e)))?;
    
    let public_key = PublicKey::from_secret_key(&secp, &secret_key);
    
    Ok(if compressed {
        public_key.serialize().to_vec()
    } else {
        public_key.serialize_uncompressed().to_vec()
    })
}

/// Add two private keys (mod n)
fn add_private_keys(a: &[u8], b: &[u8]) -> Result<Vec<u8>, BlockchainError> {
    use secp256k1::{Secp256k1, SecretKey};
    use bitcoin_hashes::sha256;
    
    let secp = Secp256k1::new();
    
    let key_a = SecretKey::from_slice(a)
        .map_err(|e| BlockchainError::crypto(format!("Invalid private key A: {}", e)))?;
    
    let key_b = SecretKey::from_slice(b)
        .map_err(|e| BlockchainError::crypto(format!("Invalid private key B: {}", e)))?;
    
    // Add the keys
    let mut result = key_a.add_tweak(&key_b.into())
        .map_err(|e| BlockchainError::crypto(format!("Key addition failed: {}", e)))?;
    
    // Ensure result is not zero
    if result == SecretKey::from_slice(&[0; 32]).unwrap() {
        return Err(BlockchainError::crypto("Resulting private key is zero"));
    }
    
    Ok(result.secret_bytes().to_vec())
}

/// Master key structure for BIP-32
#[derive(Debug, Clone)]
struct MasterKey {
    /// Private key (32 bytes)
    private_key: Vec<u8>,
    /// Chain code (32 bytes)
    chain_code: Vec<u8>,
}

/// Initialize BIP-39 module
pub fn init() -> Result<(), BlockchainError> {
    log::debug!("Initializing BIP-39 module");
    Ok(())
}