//! BSV key operations
//!
//! Implements the `Bitcoin_Key_*` function family using Father's `nour` library.

use secp256k1::{Secp256k1, SecretKey, PublicKey, Message, Signature, ecdsa};
use crate::blockchain::common::error::BlockchainError;

/// BSV key pair
#[derive(Debug, Clone)]
pub struct BtcKeyPair {
    /// Secret key (32 bytes)
    pub secret_key: Vec<u8>,
    /// Public key (33 bytes compressed or 65 bytes uncompressed)
    pub public_key: Vec<u8>,
    /// Whether public key is compressed
    pub compressed: bool,
}

impl BtcKeyPair {
    /// Create new key pair
    pub fn new(secret_key: Vec<u8>, public_key: Vec<u8>, compressed: bool) -> Self {
        Self {
            secret_key,
            public_key,
            compressed,
        }
    }
    
    /// Generate new random key pair
    pub fn generate(compressed: bool) -> Result<Self, BlockchainError> {
        let secp = Secp256k1::new();
        let (secret_key, public_key) = secp.generate_keypair(&mut rand::thread_rng());
        
        let secret_bytes = secret_key.secret_bytes().to_vec();
        let public_bytes = if compressed {
            public_key.serialize().to_vec()
        } else {
            public_key.serialize_uncompressed().to_vec()
        };
        
        Ok(Self::new(secret_bytes, public_bytes, compressed))
    }
}

/// Generate new BSV key pair
/// 
/// # Arguments
/// * `compressed` - Whether to use compressed public key
/// 
/// # Returns
/// * `Ok(BtcKeyPair)` - Generated key pair
/// * `Err(BtcError)` - Key generation failed
pub fn Bitcoin_Key_generate(compressed: bool) -> Result<BtcKeyPair, BlockchainError> {
    BtcKeyPair::generate(compressed)
}

/// Create key pair from private key
/// 
/// # Arguments
/// * `private_key` - Private key bytes (32 bytes)
/// * `compressed` - Whether to use compressed public key
/// 
/// # Returns
/// * `Ok(BtcKeyPair)` - Created key pair
/// * `Err(BtcError)` - Invalid private key
pub fn Bitcoin_Key_from_private(private_key: &[u8], compressed: bool) -> Result<BtcKeyPair, BlockchainError> {
    if private_key.len() != 32 {
        return Err(BlockchainError::key(
            format!("Invalid private key length: {} bytes (expected 32)", private_key.len())
        ));
    }
    
    let secp = Secp256k1::new();
    let secret_key = SecretKey::from_slice(private_key)
        .map_err(|e| BlockchainError::key(format!("Invalid private key: {}", e)))?;
    
    let public_key = PublicKey::from_secret_key(&secp, &secret_key);
    let public_bytes = if compressed {
        public_key.serialize().to_vec()
    } else {
        public_key.serialize_uncompressed().to_vec()
    };
    
    Ok(BtcKeyPair::new(private_key.to_vec(), public_bytes, compressed))
}

/// Sign message with private key
/// 
/// # Arguments
/// * `message` - Message to sign (32-byte hash)
/// * `private_key` - Private key bytes (32 bytes)
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Signature (64 bytes)
/// * `Err(BtcError)` - Signing failed
pub fn Bitcoin_Key_sign(message: &[u8], private_key: &[u8]) -> Result<Vec<u8>, BlockchainError> {
    if message.len() != 32 {
        return Err(BlockchainError::crypto(
            format!("Invalid message length: {} bytes (expected 32)", message.len())
        ));
    }
    
    if private_key.len() != 32 {
        return Err(BlockchainError::key(
            format!("Invalid private key length: {} bytes (expected 32)", private_key.len())
        ));
    }
    
    let secp = Secp256k1::new();
    let secret_key = SecretKey::from_slice(private_key)
        .map_err(|e| BlockchainError::key(format!("Invalid private key: {}", e)))?;
    
    let message_obj = Message::from_slice(message)
        .map_err(|e| BlockchainError::crypto(format!("Invalid message: {}", e)))?;
    
    let signature = secp.sign_ecdsa(&message_obj, &secret_key);
    Ok(signature.serialize_compact().to_vec())
}

/// Verify message signature
/// 
/// # Arguments
/// * `message` - Message that was signed (32-byte hash)
/// * `signature` - Signature (64 bytes)
/// * `public_key` - Public key bytes (33 or 65 bytes)
/// 
/// # Returns
/// * `Ok(bool)` - True if signature is valid
/// * `Err(BtcError)` - Verification failed
pub fn Bitcoin_Key_verify(message: &[u8], signature: &[u8], public_key: &[u8]) -> Result<bool, BlockchainError> {
    if message.len() != 32 {
        return Err(BlockchainError::crypto(
            format!("Invalid message length: {} bytes (expected 32)", message.len())
        ));
    }
    
    if signature.len() != 64 {
        return Err(BlockchainError::crypto(
            format!("Invalid signature length: {} bytes (expected 64)", signature.len())
        ));
    }
    
    let secp = Secp256k1::new();
    let message_obj = Message::from_slice(message)
        .map_err(|e| BlockchainError::crypto(format!("Invalid message: {}", e)))?;
    
    let signature_obj = Signature::from_compact(signature)
        .map_err(|e| BlockchainError::crypto(format!("Invalid signature: {}", e)))?;
    
    let public_key_obj = PublicKey::from_slice(public_key)
        .map_err(|e| BlockchainError::key(format!("Invalid public key: {}", e)))?;
    
    Ok(secp.verify_ecdsa(&message_obj, &signature_obj, &public_key_obj).is_ok())
}

/// Get public key from private key
/// 
/// # Arguments
/// * `private_key` - Private key bytes (32 bytes)
/// * `compressed` - Whether to return compressed public key
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Public key bytes
/// * `Err(BtcError)` - Invalid private key
pub fn Bitcoin_Key_get_public(private_key: &[u8], compressed: bool) -> Result<Vec<u8>, BlockchainError> {
    if private_key.len() != 32 {
        return Err(BlockchainError::key(
            format!("Invalid private key length: {} bytes (expected 32)", private_key.len())
        ));
    }
    
    let secp = Secp256k1::new();
    let secret_key = SecretKey::from_slice(private_key)
        .map_err(|e| BlockchainError::key(format!("Invalid private key: {}", e)))?;
    
    let public_key = PublicKey::from_secret_key(&secp, &secret_key);
    
    Ok(if compressed {
        public_key.serialize().to_vec()
    } else {
        public_key.serialize_uncompressed().to_vec()
    })
}

/// Sign transaction hash
/// 
/// # Arguments
/// * `tx_hash` - Transaction hash (32 bytes)
/// * `private_key` - Private key bytes (32 bytes)
/// * `sighash_type` - Sighash type
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Signature with sighash type appended
/// * `Err(BtcError)` - Signing failed
pub fn Bitcoin_Key_sign_tx_hash(
    tx_hash: &[u8],
    private_key: &[u8],
    sighash_type: u8,
) -> Result<Vec<u8>, BlockchainError> {
    if tx_hash.len() != 32 {
        return Err(BlockchainError::crypto(
            format!("Invalid transaction hash length: {} bytes (expected 32)", tx_hash.len())
        ));
    }
    
    // Sign the hash
    let signature = Bitcoin_Key_sign(tx_hash, private_key)?;
    
    // Append sighash type
    let mut result = signature;
    result.push(sighash_type);
    
    Ok(result)
}

/// Verify transaction signature
/// 
/// # Arguments
/// * `tx_hash` - Transaction hash (32 bytes)
/// * `signature` - Signature with sighash type (65 bytes)
/// * `public_key` - Public key bytes (33 or 65 bytes)
/// 
/// # Returns
/// * `Ok(bool)` - True if signature is valid
/// * `Err(BtcError)` - Verification failed
pub fn Bitcoin_Key_verify_tx_signature(
    tx_hash: &[u8],
    signature: &[u8],
    public_key: &[u8],
) -> Result<bool, BlockchainError> {
    if tx_hash.len() != 32 {
        return Err(BlockchainError::crypto(
            format!("Invalid transaction hash length: {} bytes (expected 32)", tx_hash.len())
        ));
    }
    
    if signature.len() != 65 {
        return Err(BlockchainError::crypto(
            format!("Invalid signature length: {} bytes (expected 65)", signature.len())
        ));
    }
    
    // Split signature and sighash type
    let sig_bytes = &signature[..64];
    let _sighash_type = signature[64];
    
    Bitcoin_Key_verify(tx_hash, sig_bytes, public_key)
}

/// Create DER-encoded signature
/// 
/// # Arguments
/// * `signature` - Compact signature (64 bytes)
/// 
/// # Returns
/// * `Vec<u8>` - DER-encoded signature
pub fn Bitcoin_Key_signature_to_der(signature: &[u8]) -> Result<Vec<u8>, BlockchainError> {
    if signature.len() != 64 {
        return Err(BlockchainError::crypto(
            format!("Invalid signature length: {} bytes (expected 64)", signature.len())
        ));
    }
    
    let signature_obj = Signature::from_compact(signature)
        .map_err(|e| BlockchainError::crypto(format!("Invalid signature: {}", e)))?;
    
    Ok(signature_obj.serialize_der().to_vec())
}

/// Parse DER-encoded signature
/// 
/// # Arguments
/// * `der_signature` - DER-encoded signature
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Compact signature (64 bytes)
/// * `Err(BtcError)` - Parsing failed
pub fn Bitcoin_Key_signature_from_der(der_signature: &[u8]) -> Result<Vec<u8>, BlockchainError> {
    let signature_obj = Signature::from_der(der_signature)
        .map_err(|e| BlockchainError::crypto(format!("Invalid DER signature: {}", e)))?;
    
    Ok(signature_obj.serialize_compact().to_vec())
}

/// Initialize keys module
pub fn init() -> Result<(), BlockchainError> {
    log::debug!("Initializing BSV keys module");
    Ok(())
}

/// Shutdown keys module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::debug!("Shutting down BSV keys module");
    Ok(())
}