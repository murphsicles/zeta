//! Test blockchain integration

use zetac::blockchain;

fn main() {
    println!("Testing Zeta v0.3.50 Blockchain Integration");
    println!("===========================================\n");
    
    // Test 1: Check if blockchain module is available
    println!("Test 1: Module Availability");
    println!("Blockchain module available: {}", blockchain::is_available());
    println!("Version: {}", blockchain::version());
    
    // Test 2: Create default configuration
    println!("\nTest 2: Configuration");
    let config = blockchain::common::config::BlockchainConfig::default();
    println!("Default network: {:?}", config.network.default_network);
    println!("BSV enabled: {}", config.network.enable_bsv);
    println!("Solana enabled: {}", config.network.enable_solana);
    
    // Test 3: Test BSV address functions
    println!("\nTest 3: BSV Address Functions");
    let test_address = "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa";
    let is_valid = blockchain::bsv::address::Bitcoin_Address_validate(test_address);
    println!("Address '{}' valid: {}", test_address, is_valid);
    
    // Test 4: Test BIP-39 mnemonic generation
    println!("\nTest 4: BIP-39 Mnemonic");
    match blockchain::wallet::bip39::generate_mnemonic(12) {
        Ok(mnemonic) => {
            println!("Generated 12-word mnemonic: {}", mnemonic);
            let is_valid = blockchain::wallet::bip39::validate_mnemonic(&mnemonic);
            println!("Mnemonic valid: {}", is_valid);
        }
        Err(e) => println!("Failed to generate mnemonic: {}", e),
    }
    
    // Test 5: Test key generation
    println!("\nTest 5: Key Generation");
    match blockchain::bsv::keys::Bitcoin_Key_generate(true) {
        Ok(key_pair) => {
            println!("Generated BSV key pair");
            println!("Private key length: {} bytes", key_pair.secret_key.len());
            println!("Public key length: {} bytes", key_pair.public_key.len());
            println!("Compressed: {}", key_pair.compressed);
        }
        Err(e) => println!("Failed to generate key pair: {}", e),
    }
    
    // Test 6: Test derivation paths
    println!("\nTest 6: Derivation Paths");
    let bsv_path = blockchain::common::types::DerivationPath::bsv(0, 0, 0);
    let solana_path = blockchain::common::types::DerivationPath::solana(0, 0, 0);
    println!("BSV path: {}", bsv_path.to_string());
    println!("Solana path: {}", solana_path.to_string());
    
    println!("\nAll tests completed!");
}