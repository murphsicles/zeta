//! BSV address operations
//!
//! Implements the `Bitcoin_Address_*` function family using Father's `nour` library.

use nour::address::Address as NourAddress;
use nour::network::Network as NourNetwork;
use crate::blockchain::common::error::BlockchainError;
use crate::blockchain::common::types::{Address, Network};

/// BSV address type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BtcAddress {
    inner: NourAddress,
    network: Network,
}

impl BtcAddress {
    /// Create BSV address from nour address
    pub fn from_nour(addr: NourAddress, network: Network) -> Self {
        Self { inner: addr, network }
    }
    
    /// Convert to nour address
    pub fn to_nour(&self) -> NourAddress {
        self.inner.clone()
    }
    
    /// Get network
    pub fn network(&self) -> Network {
        self.network
    }
}

/// Convert string to BTC address
/// 
/// # Arguments
/// * `s` - Address string in Base58 format
/// 
/// # Returns
/// * `Ok(BtcAddress)` - Valid BTC address
/// * `Err(BtcError)` - Invalid address format
pub fn Bitcoin_Address_from_string(s: &str) -> Result<BtcAddress, BlockchainError> {
    let nour_addr = NourAddress::from_base58check(s)
        .map_err(|e| BlockchainError::address(format!("Invalid address format: {}", e)))?;
    
    // Determine network from address prefix
    let network = if s.starts_with('1') || s.starts_with('3') {
        Network::BsvMainnet
    } else if s.starts_with('m') || s.starts_with('n') || s.starts_with('2') {
        Network::BsvTestnet
    } else {
        return Err(BlockchainError::address("Unknown address network prefix"));
    };
    
    Ok(BtcAddress::from_nour(nour_addr, network))
}

/// Convert BTC address to string
/// 
/// # Arguments
/// * `addr` - BTC address to convert
/// 
/// # Returns
/// * `String` - Base58 encoded address string
pub fn Bitcoin_Address_to_string(addr: &BtcAddress) -> String {
    addr.inner.to_base58check()
}

/// Validate BTC address string
/// 
/// # Arguments
/// * `s` - Address string to validate
/// 
/// # Returns
/// * `bool` - True if address is valid
pub fn Bitcoin_Address_validate(s: &str) -> bool {
    NourAddress::from_base58check(s).is_ok()
}

/// Create BTC address from public key
/// 
/// # Arguments
/// * `pubkey` - Raw public key bytes (33 or 65 bytes)
/// * `network` - Network type ("mainnet" or "testnet")
/// 
/// # Returns
/// * `Ok(BtcAddress)` - Generated BTC address
/// * `Err(BtcError)` - Invalid public key or network
pub fn Bitcoin_Address_from_pubkey(pubkey: &[u8], network: &str) -> Result<BtcAddress, BlockchainError> {
    // Validate public key length
    if pubkey.len() != 33 && pubkey.len() != 65 {
        return Err(BlockchainError::address(
            format!("Invalid public key length: {} bytes (expected 33 or 65)", pubkey.len())
        ));
    }
    
    // Convert network string to nour network
    let nour_network = match network.to_lowercase().as_str() {
        "mainnet" => NourNetwork::Mainnet,
        "testnet" => NourNetwork::Testnet,
        "stn" => NourNetwork::STN,
        _ => return Err(BlockchainError::address(
            format!("Unknown network: {} (expected 'mainnet', 'testnet', or 'stn')", network)
        )),
    };
    
    // Convert network to our Network type
    let network_type = match nour_network {
        NourNetwork::Mainnet => Network::BsvMainnet,
        NourNetwork::Testnet => Network::BsvTestnet,
        NourNetwork::STN => Network::BsvStn,
    };
    
    // Create address from public key hash
    let nour_addr = NourAddress::from_pubkey_hash(
        nour::util::hash160(pubkey),
        nour_network
    );
    
    Ok(BtcAddress::from_nour(nour_addr, network_type))
}

/// Convert BTC address to script hash
/// 
/// # Arguments
/// * `addr` - BTC address to convert
/// 
/// # Returns
/// * `Vec<u8>` - Script hash bytes (20 bytes)
pub fn Bitcoin_Address_to_scripthash(addr: &BtcAddress) -> Vec<u8> {
    addr.inner.hash().to_vec()
}

/// Convert script hash to BTC address
/// 
/// # Arguments
/// * `hash` - Script hash bytes (20 bytes)
/// * `network` - Network type ("mainnet" or "testnet")
/// 
/// # Returns
/// * `Ok(BtcAddress)` - Generated BTC address
/// * `Err(BtcError)` - Invalid hash or network
pub fn Bitcoin_Address_from_scripthash(hash: &[u8], network: &str) -> Result<BtcAddress, BlockchainError> {
    if hash.len() != 20 {
        return Err(BlockchainError::address(
            format!("Invalid script hash length: {} bytes (expected 20)", hash.len())
        ));
    }
    
    // Convert network string to nour network
    let nour_network = match network.to_lowercase().as_str() {
        "mainnet" => NourNetwork::Mainnet,
        "testnet" => NourNetwork::Testnet,
        "stn" => NourNetwork::STN,
        _ => return Err(BlockchainError::address(
            format!("Unknown network: {} (expected 'mainnet', 'testnet', or 'stn')", network)
        )),
    };
    
    // Convert network to our Network type
    let network_type = match nour_network {
        NourNetwork::Mainnet => Network::BsvMainnet,
        NourNetwork::Testnet => Network::BsvTestnet,
        NourNetwork::STN => Network::BsvStn,
    };
    
    // Create address from script hash
    let mut hash_array = [0u8; 20];
    hash_array.copy_from_slice(hash);
    let nour_addr = NourAddress::from_script_hash(hash_array, nour_network);
    
    Ok(BtcAddress::from_nour(nour_addr, network_type))
}

/// Get address type
/// 
/// # Arguments
/// * `addr` - BTC address
/// 
/// # Returns
/// * `&str` - Address type ("p2pkh", "p2sh", or "unknown")
pub fn Bitcoin_Address_type(addr: &BtcAddress) -> &'static str {
    match addr.inner.version() {
        0x00 | 0x6f => "p2pkh",  // Mainnet/testnet P2PKH
        0x05 | 0xc4 => "p2sh",   // Mainnet/testnet P2SH
        _ => "unknown",
    }
}

/// Check if address is P2PKH
/// 
/// # Arguments
/// * `addr` - BTC address
/// 
/// # Returns
/// * `bool` - True if address is P2PKH
pub fn Bitcoin_Address_is_p2pkh(addr: &BtcAddress) -> bool {
    Bitcoin_Address_type(addr) == "p2pkh"
}

/// Check if address is P2SH
/// 
/// # Arguments
/// * `addr` - BTC address
/// 
/// # Returns
/// * `bool` - True if address is P2SH
pub fn Bitcoin_Address_is_p2sh(addr: &BtcAddress) -> bool {
    Bitcoin_Address_type(addr) == "p2sh"
}

/// Initialize address module
pub fn init() -> Result<(), BlockchainError> {
    log::debug!("Initializing BSV address module");
    Ok(())
}

/// Shutdown address module
pub fn shutdown() -> Result<(), BlockchainError> {
    log::debug!("Shutting down BSV address module");
    Ok(())
}