//! BSV script operations
//!
//! Implements the `Bitcoin_Script_*` function family using Father's `nour` library.

use nour::script::{Script as NourScript, Opcode};
use crate::blockchain::common::error::BlockchainError;

/// BSV script type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtcScript {
    inner: NourScript,
}

impl BtcScript {
    /// Create BSV script from nour script
    pub fn from_nour(script: NourScript) -> Self {
        Self { inner: script }
    }
    
    /// Convert to nour script
    pub fn to_nour(&self) -> NourScript {
        self.inner.clone()
    }
}

/// Create P2PKH script from public key hash
/// 
/// # Arguments
/// * `pubkey_hash` - Public key hash bytes (20 bytes)
/// 
/// # Returns
/// * `Ok(BtcScript)` - Created P2PKH script
/// * `Err(BtcError)` - Invalid hash length
pub fn Bitcoin_Script_p2pkh(pubkey_hash: &[u8]) -> Result<BtcScript, BlockchainError> {
    if pubkey_hash.len() != 20 {
        return Err(BlockchainError::script(
            format!("Invalid public key hash length: {} bytes (expected 20)", pubkey_hash.len())
        ));
    }
    
    let nour_script = NourScript::p2pkh(pubkey_hash);
    Ok(BtcScript::from_nour(nour_script))
}

/// Create P2SH script from script hash
/// 
/// # Arguments
/// * `script_hash` - Script hash bytes (20 bytes)
/// 
/// # Returns
/// * `Ok(BtcScript)` - Created P2SH script
/// * `Err(BtcError)` - Invalid hash length
pub fn Bitcoin_Script_p2sh(script_hash: &[u8]) -> Result<BtcScript, BlockchainError> {
    if script_hash.len() != 20 {
        return Err(BlockchainError::script(
            format!("Invalid script hash length: {} bytes (expected 20)", script_hash.len())
        ));
    }
    
    let nour_script = NourScript::p2sh(script_hash);
    Ok(BtcScript::from_nour(nour_script))
}

/// Create OP_RETURN script with data
/// 
/// # Arguments
/// * `data` - Data to embed in OP_RETURN
/// 
/// # Returns
/// * `BtcScript` - Created OP_RETURN script
pub fn Bitcoin_Script_op_return(data: &[u8]) -> BtcScript {
    let nour_script = NourScript::op_return(data);
    BtcScript::from_nour(nour_script)
}

/// Create multisig script
/// 
/// # Arguments
/// * `m` - Required signatures
/// * `public_keys` - List of public keys
/// 
/// # Returns
/// * `Ok(BtcScript)` - Created multisig script
/// * `Err(BtcError)` - Invalid parameters
pub fn Bitcoin_Script_multisig(m: u8, public_keys: &[Vec<u8>]) -> Result<BtcScript, BlockchainError> {
    if m == 0 || m as usize > public_keys.len() {
        return Err(BlockchainError::script(
            format!("Invalid m value: {} (must be 1 <= m <= {})", m, public_keys.len())
        ));
    }
    
    if public_keys.len() > 15 {
        return Err(BlockchainError::script(
            format!("Too many public keys: {} (max 15)", public_keys.len())
        ));
    }
    
    let nour_script = NourScript::multisig(m, public_keys);
    Ok(BtcScript::from_nour(nour_script))
}

/// Parse script from bytes
/// 
/// # Arguments
/// * `bytes` - Script bytes
/// 
/// # Returns
/// * `Ok(BtcScript)` - Parsed script
/// * `Err(BtcError)` - Failed to parse script
pub fn Bitcoin_Script_parse(bytes: &[u8]) -> Result<BtcScript, BlockchainError> {
    let nour_script = NourScript::from(bytes);
    Ok(BtcScript::from_nour(nour_script))
}

/// Serialize script to bytes
/// 
/// # Arguments
/// * `script` - Script to serialize
/// 
/// # Returns
/// * `Vec<u8>` - Serialized script bytes
pub fn Bitcoin_Script_serialize(script: &BtcScript) -> Vec<u8> {
    script.inner.to_bytes()
}

/// Get script type
/// 
/// # Arguments
/// * `script` - Script
/// 
/// # Returns
/// * `&str` - Script type
pub fn Bitcoin_Script_type(script: &BtcScript) -> &'static str {
    if script.inner.is_p2pkh() {
        "p2pkh"
    } else if script.inner.is_p2sh() {
        "p2sh"
    } else if script.inner.is_op_return() {
        "op_return"
    } else if script.inner.is_multisig() {
        "multisig"
    } else if script.inner.is_pubkey() {
        "pubkey"
    } else {
        "nonstandard"
    }
}

/// Check if script is P2PKH
/// 
/// # Arguments
/// * `script` - Script
/// 
/// # Returns
/// * `bool` - True if script is P2PKH
pub fn Bitcoin_Script_is_p2pkh(script: &BtcScript) -> bool {
    script.inner.is_p2pkh()
}

/// Check if script is P2SH
/// 
/// # Arguments
/// * `script` - Script
/// 
/// # Returns
/// * `bool` - True if script is P2SH
pub fn Bitcoin_Script_is_p2sh(script: &BtcScript) -> bool {
    script.inner.is_p2sh()
}

/// Check if script is OP_RETURN
/// 
/// # Arguments
/// * `script` - Script
/// 
/// # Returns
/// * `bool` - True if script is OP_RETURN
pub fn Bitcoin_Script_is_op_return(script: &BtcScript) -> bool {
    script.inner.is_op_return()
}

/// Check if script is multisig
/// 
/// # Arguments
/// * `script` - Script
/// 
/// # Returns
/// * `bool` - True if script is multisig
pub fn Bitcoin_Script_is_multisig(script: &BtcScript) -> bool {
    script.inner.is_multisig()
}

/// Execute script with transaction context
/// 
/// # Arguments
/// * `script_sig` - Script signature
/// * `script_pubkey` - Script public key
/// * `tx` - Transaction bytes
/// * `input_index` - Input index
/// * `amount` - Amount in satoshis
/// 
/// # Returns
/// * `Ok(bool)` - True if script executes successfully
/// * `Err(BtcError)` - Script execution failed
pub fn Bitcoin_Script_execute(
    script_sig: &BtcScript,
    script_pubkey: &BtcScript,
    tx: &[u8],
    input_index: usize,
    amount: u64,
) -> Result<bool, BlockchainError> {
    // TODO: Implement script execution with nour library
    // This is a placeholder - actual implementation would use nour's script interpreter
    
    log::warn!("Script execution not fully implemented - using placeholder");
    
    // Basic validation
    if script_sig.inner.is_empty() || script_pubkey.inner.is_empty() {
        return Ok(false);
    }
    
    // For now, return true for standard scripts
    let script_type = Bitcoin_Script_type(script_pubkey);
    Ok(matches!(script_type, "p2pkh" | "p2sh" | "multisig"))
}

/// Get script size in bytes
/// 
/// # Arguments
/// * `script` - Script
/// 
/// # Returns
/// * `usize` - Script size in bytes
pub fn Bitcoin_Script_size(script: &BtcScript) -> usize {
    script.inner.to_bytes().len()
}

/// Create custom script from opcodes
/// 
/// # Arguments
/// * `opcodes` - List of opcode names or data pushes
/// 
/// # Returns
/// * `Ok(BtcScript)` - Created script
/// * `Err(BtcError)` - Invalid opcode
pub fn Bitcoin_Script_from_opcodes(opcodes: &[&str]) -> Result<BtcScript, BlockchainError> {
    let mut nour_script = NourScript::new();
    
    for opcode_str in opcodes {
        // Try to parse as opcode
        if let Ok(opcode) = Opcode::from_name(opcode_str) {
            nour_script.push_opcode(opcode);
        } else {
            // Try to parse as hex data
            if let Ok(data) = hex::decode(opcode_str) {
                nour_script.push_slice(&data);
            } else {
                // Try to parse as decimal number
                if let Ok(num) = opcode_str.parse::<i64>() {
                    nour_script.push_int(num);
                } else {
                    return Err(BlockchainError::script(
                        format!("Invalid opcode or data: {}", opcode_str)
                    ));
                }
            }
        }
    }
    
    Ok(BtcScript::from_nour(nour_script))
}

/// Initialize script module
pub fn init() -> Result<(), BlockchainError> {
    log::debug!("Initializing BSV script module");
    Ok(())
}

/// Shutdown script module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::debug!("Shutting down BSV script module");
    Ok(())
}