//! Wallet encryption module
//!
//! Implements secure encryption for wallet storage using AES-GCM and Argon2.

use crate::blockchain::common::error::BlockchainError;
use aes_gcm::{
    aead::{Aead, KeyInit, OsRng},
    Aes256Gcm, Nonce
};
use argon2::{
    password_hash::{
        rand_core::RngCore,
        SaltString, PasswordHasher, PasswordVerifier
    },
    Argon2, PasswordHash
};

/// Encrypt seed with password
/// 
/// # Arguments
/// * `seed` - 64-byte seed to encrypt
/// * `password` - Encryption password
/// 
/// # Returns
/// * `Ok((Vec<u8>, Vec<u8>))` - (encrypted_seed, salt)
/// * `Err(WalletError)` - Encryption failed
pub fn encrypt_seed(seed: &[u8], password: &str) -> Result<(Vec<u8>, Vec<u8>), BlockchainError> {
    if seed.len() != 64 {
        return Err(BlockchainError::crypto(
            format!("Invalid seed length: {} bytes (expected 64)", seed.len())
        ));
    }
    
    // Generate salt
    let salt = SaltString::generate(&mut OsRng);
    let salt_bytes = salt.as_bytes().to_vec();
    
    // Derive key from password using Argon2
    let argon2 = Argon2::default();
    let mut key = [0u8; 32]; // AES-256 key
    argon2.hash_password_into(password.as_bytes(), &salt_bytes, &mut key)
        .map_err(|e| BlockchainError::crypto(format!("Argon2 key derivation failed: {}", e)))?;
    
    // Generate random nonce
    let mut nonce_bytes = [0u8; 12];
    OsRng.fill_bytes(&mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);
    
    // Create cipher
    let cipher = Aes256Gcm::new_from_slice(&key)
        .map_err(|e| BlockchainError::crypto(format!("AES-GCM init failed: {}", e)))?;
    
    // Encrypt seed
    let ciphertext = cipher.encrypt(nonce, seed)
        .map_err(|e| BlockchainError::crypto(format!("Encryption failed: {}", e)))?;
    
    // Combine nonce and ciphertext
    let mut result = nonce_bytes.to_vec();
    result.extend_from_slice(&ciphertext);
    
    Ok((result, salt_bytes))
}

/// Decrypt seed with password
/// 
/// # Arguments
/// * `encrypted_seed` - Encrypted seed
/// * `password` - Decryption password
/// * `salt` - Salt used during encryption
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Decrypted 64-byte seed
/// * `Err(WalletError)` - Decryption failed
pub fn decrypt_seed(encrypted_seed: &[u8], password: &str, salt: &[u8]) -> Result<Vec<u8>, BlockchainError> {
    if encrypted_seed.len() < 12 {
        return Err(BlockchainError::crypto(
            format!("Encrypted seed too short: {} bytes (minimum 12)", encrypted_seed.len())
        ));
    }
    
    // Extract nonce (first 12 bytes)
    let nonce_bytes = &encrypted_seed[..12];
    let nonce = Nonce::from_slice(nonce_bytes);
    
    // Extract ciphertext (remaining bytes)
    let ciphertext = &encrypted_seed[12..];
    
    // Derive key from password using Argon2
    let argon2 = Argon2::default();
    let mut key = [0u8; 32]; // AES-256 key
    argon2.hash_password_into(password.as_bytes(), salt, &mut key)
        .map_err(|e| BlockchainError::crypto(format!("Argon2 key derivation failed: {}", e)))?;
    
    // Create cipher
    let cipher = Aes256Gcm::new_from_slice(&key)
        .map_err(|e| BlockchainError::crypto(format!("AES-GCM init failed: {}", e)))?;
    
    // Decrypt seed
    let seed = cipher.decrypt(nonce, ciphertext)
        .map_err(|e| BlockchainError::crypto(format!("Decryption failed: {}", e)))?;
    
    if seed.len() != 64 {
        return Err(BlockchainError::crypto(
            format!("Decrypted seed has wrong length: {} bytes (expected 64)", seed.len())
        ));
    }
    
    Ok(seed)
}

/// Hash password for verification
/// 
/// # Arguments
/// * `password` - Password to hash
/// 
/// # Returns
/// * `Ok(String)` - Password hash
/// * `Err(WalletError)` - Hashing failed
pub fn hash_password(password: &str) -> Result<String, BlockchainError> {
    let salt = SaltString::generate(&mut OsRng);
    let argon2 = Argon2::default();
    
    let password_hash = argon2.hash_password(password.as_bytes(), &salt)
        .map_err(|e| BlockchainError::crypto(format!("Password hashing failed: {}", e)))?;
    
    Ok(password_hash.to_string())
}

/// Verify password against hash
/// 
/// # Arguments
/// * `password` - Password to verify
/// * `password_hash` - Stored password hash
/// 
/// # Returns
/// * `Ok(bool)` - True if password matches
/// * `Err(WalletError)` - Verification failed
pub fn verify_password(password: &str, password_hash: &str) -> Result<bool, BlockchainError> {
    let parsed_hash = PasswordHash::new(password_hash)
        .map_err(|e| BlockchainError::crypto(format!("Invalid password hash: {}", e)))?;
    
    Ok(Argon2::default().verify_password(password.as_bytes(), &parsed_hash).is_ok())
}

/// Generate encryption key from password
/// 
/// # Arguments
/// * `password` - Password
/// * `salt` - Salt
/// 
/// # Returns
/// * `Ok([u8; 32])` - 32-byte encryption key
/// * `Err(WalletError)` - Key derivation failed
pub fn derive_key(password: &str, salt: &[u8]) -> Result<[u8; 32], BlockchainError> {
    let argon2 = Argon2::default();
    let mut key = [0u8; 32];
    
    argon2.hash_password_into(password.as_bytes(), salt, &mut key)
        .map_err(|e| BlockchainError::crypto(format!("Key derivation failed: {}", e)))?;
    
    Ok(key)
}

/// Encrypt data with key
/// 
/// # Arguments
/// * `data` - Data to encrypt
/// * `key` - 32-byte encryption key
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Encrypted data (nonce + ciphertext)
/// * `Err(WalletError)` - Encryption failed
pub fn encrypt_data(data: &[u8], key: &[u8; 32]) -> Result<Vec<u8>, BlockchainError> {
    if key.len() != 32 {
        return Err(BlockchainError::crypto(
            format!("Invalid key length: {} bytes (expected 32)", key.len())
        ));
    }
    
    // Generate random nonce
    let mut nonce_bytes = [0u8; 12];
    OsRng.fill_bytes(&mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);
    
    // Create cipher
    let cipher = Aes256Gcm::new_from_slice(key)
        .map_err(|e| BlockchainError::crypto(format!("AES-GCM init failed: {}", e)))?;
    
    // Encrypt data
    let ciphertext = cipher.encrypt(nonce, data)
        .map_err(|e| BlockchainError::crypto(format!("Encryption failed: {}", e)))?;
    
    // Combine nonce and ciphertext
    let mut result = nonce_bytes.to_vec();
    result.extend_from_slice(&ciphertext);
    
    Ok(result)
}

/// Decrypt data with key
/// 
/// # Arguments
/// * `encrypted_data` - Encrypted data (nonce + ciphertext)
/// * `key` - 32-byte encryption key
/// 
/// # Returns
/// * `Ok(Vec<u8>)` - Decrypted data
/// * `Err(WalletError)` - Decryption failed
pub fn decrypt_data(encrypted_data: &[u8], key: &[u8; 32]) -> Result<Vec<u8>, BlockchainError> {
    if encrypted_data.len() < 12 {
        return Err(BlockchainError::crypto(
            format!("Encrypted data too short: {} bytes (minimum 12)", encrypted_data.len())
        ));
    }
    
    if key.len() != 32 {
        return Err(BlockchainError::crypto(
            format!("Invalid key length: {} bytes (expected 32)", key.len())
        ));
    }
    
    // Extract nonce (first 12 bytes)
    let nonce_bytes = &encrypted_data[..12];
    let nonce = Nonce::from_slice(nonce_bytes);
    
    // Extract ciphertext (remaining bytes)
    let ciphertext = &encrypted_data[12..];
    
    // Create cipher
    let cipher = Aes256Gcm::new_from_slice(key)
        .map_err(|e| BlockchainError::crypto(format!("AES-GCM init failed: {}", e)))?;
    
    // Decrypt data
    cipher.decrypt(nonce, ciphertext)
        .map_err(|e| BlockchainError::crypto(format!("Decryption failed: {}", e)))
}

/// Initialize encryption module
pub fn init() -> Result<(), BlockchainError> {
    log::debug!("Initializing wallet encryption module");
    Ok(())
}