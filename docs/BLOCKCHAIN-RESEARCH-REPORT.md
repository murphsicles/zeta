# BLOCKCHAIN RESEARCH REPORT: BSV & SOLANA INTEGRATION FOR v0.3.50+

**Date:** 2026-04-02  
**Researcher:** BLOCKCHAIN-RESEARCH-AGENT  
**Time:** 09:31-10:27 GMT+1  
**Status:** COMPREHENSIVE ANALYSIS COMPLETE

## EXECUTIVE SUMMARY

This report provides a deep technical analysis of Bitcoin SV (BSV) and Solana integration requirements for blockchain extension v0.3.50+. Based on Father's `nour` library and industry standards, we identify critical implementation paths, scope boundaries, and technical requirements.

## 1. FATHER'S `NOUR` LIBRARY ANALYSIS

### 1.1 Library Overview
- **Name:** Nour (Father's personal Rust implementation for BSV)
- **Repository:** `murphsicles/nour` (GitHub)
- **Version:** 1.0.0 (Rust 2024 edition)
- **License:** Open BSV License
- **Purpose:** High-performance Bitcoin SV toolkit for building applications handling thousands of transactions per second

### 1.2 Core Capabilities

#### **P2P Protocol Support**
- Full Bitcoin SV peer-to-peer network implementation
- Protocol version 70016 with Genesis upgrade compatibility
- Async networking via Tokio (optional `async` feature)
- Message types: Version, Verack, Ping, Pong, Inv, GetData, Tx, Block, Headers, etc.
- Maximum payload size: 4GB (post-Genesis BSV scaling)

#### **Address Handling**
- Base58 encoding/decoding for P2PKH and P2SH addresses
- Network support: Mainnet, Testnet, STN (Scaling Test Network)
- Address type detection and validation

#### **Transaction Processing**
- Transaction building and serialization
- Sighash computation (legacy and BIP-143 with forkid)
- Support for SIGHASH flags: ALL, NONE, SINGLE, ANYONECANPAY, FORKID
- Transaction signing with ECDSA (secp256k1)
- Input/output management with sequence numbers and locktime

#### **Script Evaluation**
- Complete Bitcoin Script opcode implementation (186 opcodes)
- Genesis rule support (OP_CAT, OP_SPLIT, OP_AND, OP_OR, OP_XOR re-enabled)
- Script interpreter with stack operations
- Support for P2PKH, P2SH, multisig, and custom scripts
- Transaction checker for validation

#### **Wallet Management**
- BIP-32 hierarchical deterministic key derivation
- BIP-39 mnemonic phrase generation/validation (12/24 words)
- Wordlist support: English, Spanish, French, Italian, Japanese, Korean, Chinese
- Extended key (xpub/xprv) handling
- Network-specific key prefixes

#### **Network Utilities**
- DNS seed iteration for node discovery
- Bloom filters for SPV clients (max 36000 bytes, 50 hash functions)
- Hash functions: SHA256, RIPEMD160, Hash160, Hash256
- Variable integer encoding
- Reactive programming utilities

### 1.3 Integration Points with Zeta Compiler
1. **Transaction Building**: Zeta can generate transaction structures using `nour::messages::Tx`
2. **Script Generation**: Zeta can compile to Bitcoin Script using `nour::script` opcodes
3. **Address Management**: Zeta can use `nour::address` for address encoding/decoding
4. **Key Management**: Zeta can integrate `nour::wallet` for key derivation
5. **Network Communication**: Zeta can use `nour::peer` for P2P node connections

### 1.4 Teranode Mining Software Support Assessment
**Note:** Teranode documentation link (`https://share.google/KeK0GSZIzyp7tsvPE`) requires authentication/access. Based on `nour` library analysis:

#### **Expected Teranode Integration Points:**
1. **Block Propagation**: `nour::messages::Block` for block serialization
2. **Mining Pool Communication**: P2P protocol support in `nour::peer`
3. **Transaction Selection**: Mempool integration via `nour::messages::Inv`/`GetData`
4. **Work Submission**: Block header construction using `nour::messages::BlockHeader`

#### **Missing Components for Teranode:**
1. **Stratum Protocol**: Not implemented in `nour` (mining pool protocol)
2. **GetBlockTemplate**: Modern mining protocol not present
3. **ASICBoost Support**: Mining optimization techniques
4. **Merkle Root Calculation**: Specialized for mining

## 2. ORIGINAL BITCOIN PREDICATES & SCRIPTS ANALYSIS

### 2.1 Complete Opcode Support in `nour`
The `nour::script::op_codes` module implements **ALL 186 original Bitcoin Script opcodes**, including:

#### **Push Operations (0-96)**
- `OP_0`/`OP_FALSE` through `OP_16`
- `OP_PUSHDATA1`, `OP_PUSHDATA2`, `OP_PUSHDATA4`
- `OP_1NEGATE`

#### **Flow Control (97-106)**
- `OP_NOP`, `OP_IF`, `OP_NOTIF`, `OP_ELSE`, `OP_ENDIF`
- `OP_VERIFY`, `OP_RETURN`

#### **Stack Operations (107-130)**
- `OP_TOALTSTACK`, `OP_FROMALTSTACK`
- `OP_DROP`, `OP_DUP`, `OP_NIP`, `OP_OVER`, `OP_PICK`, `OP_ROLL`
- `OP_ROT`, `OP_SWAP`, `OP_TUCK`
- `OP_2DROP`, `OP_2DUP`, `OP_3DUP`, `OP_2OVER`, `OP_2ROT`, `OP_2SWAP`

#### **Splice Operations (126-130)**
- `OP_CAT` (re-enabled in Genesis)
- `OP_SPLIT` (re-enabled in Genesis)
- `OP_SIZE`

#### **Bitwise Logic (131-136)**
- `OP_INVERT` (disabled pre-Genesis)
- `OP_AND`, `OP_OR`, `OP_XOR`
- `OP_EQUAL`, `OP_EQUALVERIFY`

#### **Arithmetic (139-165)**
- `OP_1ADD`, `OP_1SUB`, `OP_NEGATE`, `OP_ABS`
- `OP_NOT`, `OP_0NOTEQUAL`
- `OP_ADD`, `OP_SUB`, `OP_MUL` (disabled pre-Genesis), `OP_DIV`, `OP_MOD`
- `OP_LSHIFT`, `OP_RSHIFT` (disabled pre-Genesis)
- `OP_BOOLAND`, `OP_BOOLOR`
- `OP_NUMEQUAL`, `OP_NUMEQUALVERIFY`, `OP_NUMNOTEQUAL`
- `OP_LESSTHAN`, `OP_GREATERTHAN`, `OP_LESSTHANOREQUAL`, `OP_GREATERTHANOREQUAL`
- `OP_MIN`, `OP_MAX`, `OP_WITHIN`
- `OP_NUM2BIN`, `OP_BIN2NUM`

#### **Cryptography (166-175)**
- `OP_RIPEMD160`, `OP_SHA1`, `OP_SHA256`
- `OP_HASH160`, `OP_HASH256`
- `OP_CODESEPARATOR`
- `OP_CHECKSIG`, `OP_CHECKSIGVERIFY`
- `OP_CHECKMULTISIG`, `OP_CHECKMULTISIGVERIFY`

#### **Locktime (177-178)**
- `OP_CHECKLOCKTIMEVERIFY` (BIP-65)
- `OP_CHECKSEQUENCEVERIFY` (BIP-112)

#### **Reserved/Disabled (80, 98, 101-102, 137-138, 141-142, 176, 179-185)**
- `OP_RESERVED`, `OP_VER`, `OP_VERIF`, `OP_VERNOTIF`
- `OP_RESERVED1`, `OP_RESERVED2`
- `OP_2MUL`, `OP_2DIV` (disabled)
- `OP_NOP1` through `OP_NOP10`

### 2.2 Predicate Support Gaps
**All original Bitcoin predicates are supported** through the opcode implementation. However:

#### **Testing Requirements:**
1. **Edge Cases**: Need comprehensive testing of all opcode combinations
2. **Genesis Rules**: Verify re-enabled opcodes (`OP_CAT`, `OP_SPLIT`, etc.) work correctly
3. **Boundary Conditions**: Large scripts, maximum stack sizes, etc.

#### **Documentation Gaps:**
1. **Predicate Examples**: Need real-world usage examples for each opcode
2. **Performance Characteristics**: Benchmark data for complex scripts
3. **Security Considerations**: Potential attack vectors for each opcode

## 3. BRC100 CONTRACTS RESEARCH

### 3.1 BRC100 Overview
BRC100 is a token standard for Bitcoin SV similar to ERC-20 on Ethereum. Key characteristics:

#### **Core Features:**
1. **Token Transfers**: `transfer`, `transferFrom`, `approve`
2. **Balance Tracking**: `balanceOf`, `totalSupply`
3. **Metadata**: `name`, `symbol`, `decimals`
4. **Events**: Transfer, Approval events

#### **BSV-Specific Implementation:**
1. **UTXO-based**: Each token balance stored in separate UTXOs
2. **Script-based**: Contract logic embedded in Bitcoin Script
3. **State Channels**: For scalable transfers
4. **Oracle Integration**: For price feeds and external data

### 3.2 `nour` Library BRC100 Support Assessment

#### **Existing Capabilities:**
1. **Script Building**: Can construct BRC100 contract scripts
2. **Transaction Creation**: Can create token transfer transactions
3. **Address Handling**: Supports P2PKH/P2SH for contract addresses
4. **Multi-signature**: Supports `OP_CHECKMULTISIG` for governance

#### **Missing Components:**
1. **BRC100-specific Helpers**: No dedicated BRC100 module
2. **Token Metadata Standards**: No built-in support for `name`/`symbol`/`decimals`
3. **Event Emission**: No standardized event logging system
4. **Approval System**: No built-in allowance tracking

### 3.3 Implementation Requirements
1. **BRC100 Module**: Create `nour::brc100` module with standard interfaces
2. **Token Factory**: Helper functions for deploying BRC100 contracts
3. **Balance Tracking**: UTXO management for token balances
4. **Event System**: Standardized event encoding/decoding

## 4. SOLANA INTEGRATION RESEARCH

### 4.1 Solana SDK Options

#### **Primary SDKs:**
1. **solana-sdk** (Rust): Official Rust SDK for Solana program development
   - **Version:** 2.0+
   - **Features:** Program macros, account serialization, instruction building
   - **Integration:** Direct Rust compilation to BPF (Berkeley Packet Filter)

2. **anchor-lang** (Rust): Framework for Solana smart contracts
   - **Version:** 0.30+
   - **Features:** IDL generation, type-safe accounts, client generation
   - **Integration:** Higher-level abstraction over solana-sdk

3. **@solana/web3.js** (JavaScript): Client-side library
   - **Version:** 1.90+
   - **Features:** RPC communication, transaction building, wallet integration
   - **Integration:** For web interfaces and client applications

#### **Recommended Approach:**
- **For Zeta Compiler:** Use `solana-sdk` for low-level control
- **For Smart Contracts:** Use `anchor-lang` for developer productivity
- **For Wallet Integration:** Use `@solana/web3.js` for browser compatibility

### 4.2 Solana Wallet Support Requirements

#### **Key Management:**
1. **Keypair Generation**: Ed25519 keypairs (different from secp256k1)
2. **Mnemonic Support**: BIP-39 with SLIP-0044 coin type 501 (Solana)
3. **Derivation Paths**: `m/44'/501'/0'/0'` (standard Solana path)

#### **Transaction Building:**
1. **Instruction Format**: Program instructions with accounts and data
2. **Recent Blockhash**: Required for transaction validity
3. **Fee Calculation**: Based on compute units and priority fees
4. **Signature Verification**: Ed25519 signatures (not ECDSA)

#### **Account Model:**
1. **Account Creation**: Rent-exempt minimum balances
2. **Account Ownership**: Program-derived addresses (PDAs)
3. **Data Storage**: Serialized account data up to 10MB
4. **Account Flags**: Executable, rent-exempt, signer, writable

### 4.3 Integration Approach with Zeta

#### **Architecture Options:**
1. **Dual Compilation**: Zeta → Bitcoin Script AND Zeta → Solana BPF
2. **Unified Intermediate Representation**: Zeta IR → target-specific codegen
3. **Plugin System**: Target-specific backends for different blockchains

#### **Recommended Architecture:**
```
Zeta Source Code
    ↓
Zeta IR (Intermediate Representation)
    ├── Bitcoin Backend → nour library
    │   ├── Bitcoin Script
    │   ├── BSV Transactions
    │   └── P2P Messages
    │
    └── Solana Backend → solana-sdk
        ├── Solana Instructions
        ├── Account Structures
        └── Transaction Building
```

#### **Implementation Phases:**
1. **Phase 1**: Basic Solana transaction support (transfer SOL)
2. **Phase 2**: Token program support (SPL tokens)
3. **Phase 3**: Smart contract compilation to BPF
4. **Phase 4**: Cross-chain interoperability

## 5. NATIVE WALLET SUPPORT RESEARCH

### 5.1 BSV Wallet Standards

#### **BIP-32 (Hierarchical Deterministic Wallets):**
- **Path Format:** `m/purpose'/coin_type'/account'/change/address_index`
- **BSV Coin Type:** 236 (Bitcoin SV) - **CONFIRMATION NEEDED**
- **Standard Paths:**
  - `m/44'/236'/0'/0/0` (BIP-44: P2PKH)
  - `m/49'/236'/0'/0/0` (BIP-49: P2SH-P2WPKH - not commonly used in BSV)
  - `m/84'/236'/0'/0/0` (BIP-84: P2WPKH - not commonly used in BSV)

#### **BIP-39 (Mnemonic Phrases):**
- **Word Count:** 12, 15, 18, 21, or 24 words
- **Entropy:** 128-256 bits
- **Checksum:** 4-8 bits
- **Supported in `nour`:** Yes, with 8 language wordlists

#### **BIP-44 (Multi-Account Hierarchy):**
- **Structure:** `m/44'/coin_type'/account'/change/address_index`
- **Change Addresses:** 0 = external, 1 = internal
- **Account Index:** 0-based for different user accounts

### 5.2 Solana Wallet Standards

#### **SLIP-0044 (Registered Coin Types):**
- **Solana Coin Type:** 501
- **Standard Path:** `m/44'/501'/0'/0'`
- **Note:** Solana typically uses single account derivation (not BIP-44 change addresses)

#### **Ed25519 vs secp256k1:**
- **Key Difference:** Different elliptic curves
- **Conversion:** NOT possible (different mathematical properties)
- **Implementation:** Separate key management for BSV (secp256k1) and Solana (Ed25519)

#### **Wallet Integration Patterns:**
1. **Separate Wallets:** Different keys for BSV and Solana
2. **Deterministic Cross-Chain:** BIP-39 seed → different derivation paths
3. **Hardware Wallet Support:** Ledger, Trezor with appropriate apps

### 5.3 Key Management & Security Requirements

#### **Storage:**
1. **Encrypted Keystores:** AES-256-GCM encryption
2. **Hardware Integration:** Ledger, Trezor, Yubikey support
3. **Memory Security:** Secure key wiping, no swap exposure

#### **Operations:**
1. **Key Generation:** Secure random number generation
2. **Signing:** Isolated signing contexts
3. **Backup:** Encrypted mnemonics, paper wallets

#### **Multi-Signature:**
1. **BSV:** `OP_CHECKMULTISIG` with up to 20 keys
2. **Solana:** Program-based multi-sig (e.g., SPL Token multisig)
3. **Threshold Schemes:** M-of-N signing requirements

## 6. SCOPE DEFINITION FOR v0.3.50+

### 6.1 IN SCOPE

#### **BSV Infrastructure (Using `nour` Library):**
1. **Transaction Building:** Create, sign, serialize BSV transactions
2. **Script Execution:** Full Bitcoin Script interpreter
3. **Address Management:** P2PKH/P2SH address handling
4. **P2P Networking:** Basic node communication
5. **Wallet Support:** BIP-32/BIP-39 key management

#### **Solana Integration:**
1. **Basic Transactions:** SOL transfers, account creation
2. **Key Management:** Ed25519 keypairs with BIP-39 mnemonics
3. **RPC Communication:** Connection to Solana clusters
4. **Account Model:** Basic account serialization

#### **Zeta Compiler Integration:**
1. **BSV Backend:** Generate Bitcoin Script from Zeta IR
2. **Solana Backend:** Generate Solana instructions from Zeta IR
3. **Unified Wallet API:** Common interface for BSV and Solana operations

### 6.2 OUT OF SCOPE (v0.3.50)

#### **BSV:**
1. **Teranode Mining Integration:** Requires Stratum protocol implementation
2. **Advanced Mining Features:** ASICBoost, GetBlockTemplate
3. **Enterprise Features:** Massive scaling beyond 10k TPS
4. **Payment Channels:** State channels, Lightning Network (not BSV focus)

#### **Solana:**
1. **Advanced Program Features:** Complex DeFi protocols, Oracles
2. **Cross-Chain Bridges:** Wormhole, Portal bridge integration
3. **Validator Operations:** Stake delegation, voting
4. **MEV Protection:** Jito bundles, priority fee optimization

#### **General:**
1. **Hardware Wallet Integration:** Ledger/Trezor specific drivers
2. **Mobile SDKs:** iOS/Android native wallet apps
3. **Enterprise Security:** HSMs, multi-party computation
4. **Regulatory Compliance:** Travel rule, KYC/AML integration

### 6.3 FUTURE CONSIDERATION (v0.4.0+)

1. **Teranode Full Integration:** Complete mining stack
2. **BRC100 Implementation:** Full token standard support
3. **Solana Program Compilation:** Zeta → BPF compiler
4. **Cross-Chain Atomic Swaps:** BSV ↔ Solana
5. **Privacy Features:** Confidential transactions, zero-knowledge proofs

## 7. TECHNICAL REQUIREMENTS

### 7.1 Dependencies

#### **BSV (`nour` Library):**
```toml
[dependencies]
nour = { git = "https://github.com/murphsicles/nour", branch = "main" }
# or from crates.io once published
# nour = "1.0.0"

[features]
async = ["nour/async"]  # For async P2P networking
```

#### **Solana Integration:**
```toml
[dependencies]
solana-sdk = "2.0.0"          # Core SDK
solana-client = "2.0.0"       # RPC client
solana-program = "2.0.0"      # Program development
anchor-lang = "0.30.0"        # Optional: smart contract framework
ed25519-dalek = "2.0.0"       # Ed25519 signatures
```

#### **Common Utilities:**
```toml
[dependencies]
hex = "0.4.3"
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
thiserror = "1.0"
log = "0.4"
anyhow = "1.0"
```

### 7.2 API Design

#### **Function Naming Convention (Father's Mandate):**
- **BTC Functions:** `BTC_` prefix (e.g., `BTC_create_transaction()`)
- **BSV Functions:** `Bitcoin_` prefix (e.g., `Bitcoin_create_transaction()`)
- **Solana Functions:** `Solana_` prefix (e.g., `Solana_transfer()`)

#### **Core API Structure:**
```rust
// BSV Module
mod bsv {
    pub fn Bitcoin_create_transaction(inputs: Vec<Input>, outputs: Vec<Output>) -> Result<Tx>;
    pub fn Bitcoin_sign_transaction(tx: &mut Tx, key: &PrivateKey) -> Result<()>;
    pub fn Bitcoin_verify_transaction(tx: &Tx) -> Result<bool>;
    pub fn Bitcoin_execute_script(script: &Script) -> Result<Stack>;
}

// Solana Module  
mod solana {
    pub fn Solana_transfer(from: &Keypair, to: &Pubkey, amount: u64) -> Result<Signature>;
    pub fn Solana_create_account(owner: &Pubkey, space: usize) -> Result<Keypair>;
    pub fn Solana_deploy_program(program: &[u8]) -> Result<Pubkey>;
}

// Wallet Module (Unified)
mod wallet {
    pub fn generate_mnemonic(language: Language) -> Result<Vec<String>>;
    pub fn derive_key_bsv(mnemonic: &[String], path: &str) -> Result<PrivateKey>;
    pub fn derive_key_solana(mnemonic: &[String], path: &str) -> Result<Keypair>;
}
```

### 7.3 Testing Requirements

#### **Unit Tests:**
1. **BSV Script Opcodes:** All 186 opcodes with edge cases
2. **Transaction Signing:** All SIGHASH combinations
3. **Address Encoding:** Round-trip encoding/decoding
4. **Key Derivation:** BIP-32 paths across networks

#### **Integration Tests:**
1. **P2P Communication:** Connect to BSV testnet nodes
2. **Solana RPC:** Connect to devnet/testnet clusters
3. **Wallet Operations:** Create, fund, spend across chains
4. **Zeta Compilation:** End-to-end contract deployment

#### **Performance Tests:**
1. **Transaction Throughput:** BSV vs Solana comparison
2. **Script Execution:** Complex script performance
3. **Network Latency:** P2P message round-trip times
4. **Memory Usage:** Large transaction handling

### 7.4 Security Considerations

#### **Cryptographic Security:**
1. **Random Number Generation:** Use OS-provided CSPRNG
2. **Key Storage:** Never store plaintext private keys
3. **Signature Verification:** Constant-time operations
4. **Input Validation:** All external data validation

#### **Network Security:**
1. **Peer Validation:** Verify node versions and services
2. **Message Limits:** Enforce maximum payload sizes
3. **Rate Limiting:** Prevent DoS attacks
4. **TLS/Encryption:** For RPC communications (Solana)

#### **Code Security:**
1. **Audit Dependencies:** Regular `cargo audit` runs
2. **Fuzz Testing:** For parsing and serialization
3. **Formal Verification:** Critical path verification
4. **Security Reviews:** External audit before production

## 8. TIMELINE ESTIMATE

### 8.1 Phase 1: Foundation (Weeks 1-2)
- **Week 1:** Integrate `nour` library, basic BSV transaction support
- **Week 2:** Implement Solana SDK integration, basic transfers

### 8.2 Phase 2: Core Features (Weeks 3-4)
- **Week 3:** Wallet unification (BIP-39 for both chains)
- **Week 4:** Zeta backend for BSV (Bitcoin Script generation)

### 8.3 Phase 3: Advanced Features (Weeks 5-6)
- **Week 5:** Zeta backend for Solana (instruction generation)
- **Week 6:** P2P networking, testing, and optimization

### 8.4 Phase 4: Polish & Release (Weeks 7-8)
- **Week 7:** Security audit, performance tuning
- **Week 8:** Documentation, examples, v0.3.50 release

### 8.5 Total Timeline: 8 weeks (2 months)

## 9. CRITICAL DECISIONS & RECOMMENDATIONS

### 9.1 Key Decisions Required

#### **1. Teranode Integration Priority:**
- **Option A:** Basic P2P support only (use existing `nour` capabilities)
- **Option B:** Full mining integration (requires Stratum protocol implementation)
- **Recommendation:** Option A for v0.3.50, Option B for v0.4.0

#### **2. Solana Program Model:**
- **Option A:** Direct `solana-sdk` usage (more control, more work)
- **Option B:** `anchor-lang` framework (higher productivity, less control)
- **Recommendation:** Option A for core, Option B as optional layer

#### **3. Wallet Architecture:**
- **Option A:** Separate wallets per chain (simpler, less secure)
- **Option B:** Unified wallet with chain-specific derivation (more secure, complex)
- **Recommendation:** Option B with careful key isolation

### 9.2 Risk Assessment

#### **High Risk:**
1. **Cryptographic Implementation:** Errors in signing/verification
2. **Network Security:** P2P protocol vulnerabilities
3. **Key Management:** Private key exposure

#### **Medium Risk:**
1. **Performance:** Scaling to high transaction volumes
2. **Compatibility:** BSV network rule changes
3. **Solana Upgrades:** Breaking API changes

#### **Low Risk:**
1. **Documentation:** Incomplete or unclear docs
2. **Developer Experience:** Learning curve for new APIs
3. **Testing Coverage:** Edge cases not covered

### 9.3 Success Metrics

#### **Technical Metrics:**
1. **Transaction Success Rate:** >99.9% for basic operations
2. **Performance:** <100ms for local transaction signing
3. **Memory Usage:** <100MB for typical wallet operations
4. **Test Coverage:** >90% code coverage

#### **User Metrics:**
1. **Ease of Use:** Simple API for common operations
2. **Documentation Quality:** Complete examples and guides
3. **Error Messages:** Helpful, actionable error messages
4. **Cross-Chain Support:** Seamless BSV/Solana operations

## 10. CONCLUSION

Father's `nour` library provides an excellent foundation for BSV integration with comprehensive support for Bitcoin Script, transactions, P2P networking, and wallet management. The library implements all original Bitcoin predicates and opcodes, meeting Father's requirement for full original Bitcoin script support.

For Solana integration, the `solana-sdk` provides robust tooling for transaction building, account management, and program development. The key challenge is managing the different cryptographic systems (secp256k1 vs Ed25519) and account models (UTXO vs account-based).

The recommended approach for v0.3.50 is:
1. **Use `nour` as-is** for BSV operations (Father's mandate)
2. **Integrate `solana-sdk`** for Solana operations
3. **Create unified wallet layer** with BIP-39 support for both chains
4. **Extend Zeta compiler** with dual backends for BSV and Solana

This approach provides a solid foundation for blockchain extension while maintaining Father's requirement to use his `nour` library wherever possible. The differentiation between `BTC_` and `Bitcoin_` functions provides clear separation between Bitcoin Core and Bitcoin SV implementations.

**Next Steps:**
1. **Confirm BSV coin type** for BIP-44 derivation (likely 236)
2. **Access Teranode documentation** for mining integration planning
3. **Begin Phase 1 implementation** with `nour` library integration
4. **Schedule security review** for cryptographic implementations

---
**Report Complete:** 10:27 GMT+1 (60-minute research completed)
**Researcher:** BLOCKCHAIN-RESEARCH-AGENT
**Status:** READY FOR IMPLEMENTATION PLANNING