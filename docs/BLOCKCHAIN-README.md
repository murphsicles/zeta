# Zeta Blockchain Integration v0.3.50

## Overview

Zeta v0.3.50 introduces comprehensive blockchain support for Bitcoin SV (using Father's `nour` library) and Solana. This enables Zeta to be used for blockchain application development with full support for transactions, smart contracts, and wallet management.

## Features

### Bitcoin SV Support
- **Address Management**: Create, validate, and convert BSV addresses
- **Transaction Processing**: Build, sign, and broadcast transactions
- **Script Engine**: Support for P2PKH, P2SH, OP_RETURN, and multisig scripts
- **Cryptography**: secp256k1 signing and verification
- **Network Integration**: RPC and P2P connectivity
- **Mining Support**: Teranode mining integration

### Solana Support
- **Address System**: Base58 addresses and Program Derived Addresses (PDAs)
- **Transaction Model**: Instruction-based transactions with Ed25519 signatures
- **Account Model**: Lamports balance, data storage, executable programs
- **Network Client**: RPC connectivity for mainnet, testnet, and devnet

### Unified Wallet
- **BIP-39 Mnemonics**: 12-24 word phrases in multiple languages
- **BIP-32 Derivation**: Hierarchical deterministic wallets
- **Cross-Chain**: Single wallet for both BSV and Solana
- **Security**: AES-256-GCM encryption with Argon2 password hashing
- **Auto-Lock**: Automatic locking for security

## Quick Start

### Installation

Add to your `Cargo.toml`:
```toml
[dependencies]
zetac = { version = "0.3.50", features = ["blockchain"] }
```

### Basic Usage

```rust
use zetac::blockchain;

// Initialize blockchain subsystem
let config = blockchain::common::config::BlockchainConfig::default();
blockchain::init(config)?;

// Generate BSV address
let btc_key = blockchain::bsv::keys::Bitcoin_Key_generate(true)?;
let btc_addr = blockchain::bsv::address::Bitcoin_Address_from_pubkey(
    &btc_key.public_key,
    "mainnet"
)?;

// Generate Solana address
let solana_addr = blockchain::solana::address::Solana_Address_from_pubkey(
    &btc_key.public_key,
    "mainnet"
)?;

// Create wallet
let wallet_config = blockchain::wallet::default_config();
let (wallet, mnemonic) = blockchain::wallet::Wallet::generate(
    12,
    "secure_password",
    wallet_config
)?;

// Derive BSV address from wallet
let bsv_path = blockchain::common::types::DerivationPath::bsv(0, 0, 0);
let bsv_address = wallet.get_address(&bsv_path, blockchain::common::types::Network::BsvMainnet, "secure_password")?;

// Shutdown
blockchain::shutdown()?;
```

## API Reference

### Function Families

#### BSV Functions (`Bitcoin_` prefix)
- `Bitcoin_Address_*` - Address operations
- `Bitcoin_Transaction_*` - Transaction operations
- `Bitcoin_Script_*` - Script operations
- `Bitcoin_Key_*` - Cryptographic operations
- `Bitcoin_Network_*` - Network operations
- `Bitcoin_Mining_*` - Mining operations

#### Solana Functions (`Solana_` prefix)
- `Solana_Address_*` - Address operations
- `Solana_Transaction_*` - Transaction operations
- `Solana_Account_*` - Account operations

### Common Types

```rust
use zetac::blockchain::common::types;

// Network type
let network = types::Network::BsvMainnet;

// Amount with currency
let amount = types::Amount::bsv_satoshis(100000000); // 1 BSV

// Derivation path
let path = types::DerivationPath::bsv(0, 0, 0); // m/44'/236'/0'/0/0
```

## Configuration

```rust
use zetac::blockchain::common::config::BlockchainConfig;

let config = BlockchainConfig {
    network: NetworkConfig {
        default_network: Network::BsvMainnet,
        enable_bsv: true,
        enable_solana: true,
        // ... other settings
    },
    wallet: WalletConfig {
        enabled: true,
        storage_path: "~/.zeta/wallet".to_string(),
        // ... other settings
    },
    // ... other configurations
};
```

## Security Notes

1. **Private Keys**: Always stored encrypted in memory
2. **Passwords**: Hashed with Argon2 before use
3. **Encryption**: AES-256-GCM for wallet storage
4. **Auto-Lock**: Wallets automatically lock after timeout
5. **Input Validation**: All inputs rigorously validated

## Examples

See `test_blockchain_integration.rs` for basic usage examples.

## Dependencies

- **BSV**: Father's `nour` library (local path)
- **Solana**: `solana-sdk`, `solana-client`, `solana-program` (optional)
- **Cryptography**: `bip39`, `ed25519-dalek`, `aes-gcm`, `argon2`

## Feature Flags

- `blockchain`: Enable all blockchain features
- `solana`: Enable Solana-specific features

## Architecture

The blockchain module follows a clean architecture:
- `common/`: Shared types, errors, traits, and configuration
- `bsv/`: Bitcoin SV implementation using `nour`
- `solana/`: Solana blockchain implementation
- `wallet/`: Unified wallet management

## Testing

Run the test suite:
```bash
cargo test --features blockchain
```

## Contributing

When contributing to the blockchain module:
1. Follow Father's mandate: Use `nour` library for BSV operations
2. Maintain API consistency with `Bitcoin_` and `Solana_` prefixes
3. Add comprehensive tests for new functionality
4. Document all public APIs

## License

MIT License - See main Zeta repository for details.

## Support

For issues and questions, please refer to the main Zeta documentation or create an issue in the repository.