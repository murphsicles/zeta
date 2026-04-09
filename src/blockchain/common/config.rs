//! Configuration for blockchain operations

use serde::{Serialize, Deserialize};
use crate::blockchain::common::types::Network;

/// Main blockchain configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockchainConfig {
    /// Network configuration
    pub network: NetworkConfig,
    /// Wallet configuration
    pub wallet: WalletConfig,
    /// BSV-specific configuration
    pub bsv: BsvConfig,
    /// Solana-specific configuration
    pub solana: SolanaConfig,
    /// Security configuration
    pub security: SecurityConfig,
    /// Performance configuration
    pub performance: PerformanceConfig,
}

/// Network configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    /// Default network
    pub default_network: Network,
    /// Enable BSV network
    pub enable_bsv: bool,
    /// Enable Solana network
    pub enable_solana: bool,
    /// RPC endpoints
    pub rpc_endpoints: Vec<RpcEndpoint>,
    /// P2P nodes
    pub p2p_nodes: Vec<P2pNode>,
    /// Connection timeout (seconds)
    pub connection_timeout: u64,
    /// Request timeout (seconds)
    pub request_timeout: u64,
    /// Retry attempts
    pub retry_attempts: u32,
    /// Use TLS
    pub use_tls: bool,
}

/// RPC endpoint configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RpcEndpoint {
    /// Network
    pub network: Network,
    /// URL
    pub url: String,
    /// Username (optional)
    pub username: Option<String>,
    /// Password (optional)
    pub password: Option<String>,
    /// Priority (lower = higher priority)
    pub priority: u32,
}

/// P2P node configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct P2pNode {
    /// Network
    pub network: Network,
    /// Host
    pub host: String,
    /// Port
    pub port: u16,
    /// Protocol version
    pub protocol_version: u32,
    /// Services supported
    pub services: u64,
}

/// Wallet configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WalletConfig {
    /// Enable wallet
    pub enabled: bool,
    /// Wallet storage path
    pub storage_path: String,
    /// Default derivation path
    pub default_derivation_path: String,
    /// Encryption algorithm
    pub encryption_algorithm: String,
    /// Key derivation iterations
    pub key_derivation_iterations: u32,
    /// Auto-lock timeout (seconds, 0 = disabled)
    pub auto_lock_timeout: u64,
    /// Require password for spending
    pub require_password_for_spending: bool,
    /// Backup location
    pub backup_location: Option<String>,
    /// Backup frequency (days)
    pub backup_frequency: u32,
}

/// BSV-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BsvConfig {
    /// Use Father's nour library
    pub use_nour: bool,
    /// Nour library path (if local)
    pub nour_path: Option<String>,
    /// Default fee rate (satoshis per byte)
    pub default_fee_rate: u64,
    /// Minimum relay fee
    pub min_relay_fee: u64,
    /// Dust limit (satoshis)
    pub dust_limit: u64,
    /// Enable Teranode mining
    pub enable_teranode: bool,
    /// Teranode configuration
    pub teranode: TeranodeConfig,
    /// Enable BSV-21 tokens
    pub enable_bsv21: bool,
    /// Enable BRC-0100 wallet
    pub enable_brc0100: bool,
}

/// Teranode mining configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeranodeConfig {
    /// Enable mining
    pub enabled: bool,
    /// Mining address
    pub mining_address: String,
    /// Thread count
    pub thread_count: u32,
    /// Difficulty target
    pub difficulty_target: u32,
    /// Pool URL (optional)
    pub pool_url: Option<String>,
    /// Pool user (optional)
    pub pool_user: Option<String>,
    /// Pool password (optional)
    pub pool_password: Option<String>,
}

/// Solana-specific configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SolanaConfig {
    /// RPC endpoint
    pub rpc_endpoint: String,
    /// WebSocket endpoint
    pub websocket_endpoint: String,
    /// Commitment level
    pub commitment: String,
    /// Default fee (lamports)
    pub default_fee: u64,
    /// Priority fee (lamports)
    pub priority_fee: u64,
    /// Compute unit limit
    pub compute_unit_limit: u64,
    /// Compute unit price (micro-lamports)
    pub compute_unit_price: u64,
    /// Enable token program
    pub enable_token_program: bool,
    /// Enable stake program
    pub enable_stake_program: bool,
    /// Enable vote program
    pub enable_vote_program: bool,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    /// Enable hardware wallet support
    pub enable_hardware_wallet: bool,
    /// Hardware wallet type
    pub hardware_wallet_type: Option<String>,
    /// Require confirmation for large transactions
    pub require_confirmation_large_tx: bool,
    /// Large transaction threshold (satoshis/lamports)
    pub large_tx_threshold: u64,
    /// Enable transaction signing verification
    pub enable_tx_verification: bool,
    /// Enable address whitelist
    pub enable_address_whitelist: bool,
    /// Whitelisted addresses
    pub whitelisted_addresses: Vec<String>,
    /// Enable rate limiting
    pub enable_rate_limiting: bool,
    /// Rate limit requests per minute
    pub rate_limit_requests: u32,
    /// Enable audit logging
    pub enable_audit_logging: bool,
    /// Audit log path
    pub audit_log_path: String,
}

/// Performance configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceConfig {
    /// Cache size (MB)
    pub cache_size_mb: u64,
    /// Enable transaction caching
    pub enable_tx_caching: bool,
    /// Enable address caching
    pub enable_address_caching: bool,
    /// Enable block caching
    pub enable_block_caching: bool,
    /// Cache TTL (seconds)
    pub cache_ttl: u64,
    /// Connection pool size
    pub connection_pool_size: u32,
    /// Thread pool size
    pub thread_pool_size: u32,
    /// Batch request size
    pub batch_request_size: u32,
    /// Enable compression
    pub enable_compression: bool,
    /// Enable parallel processing
    pub enable_parallel_processing: bool,
}

impl Default for BlockchainConfig {
    fn default() -> Self {
        Self {
            network: NetworkConfig::default(),
            wallet: WalletConfig::default(),
            bsv: BsvConfig::default(),
            solana: SolanaConfig::default(),
            security: SecurityConfig::default(),
            performance: PerformanceConfig::default(),
        }
    }
}

impl Default for NetworkConfig {
    fn default() -> Self {
        Self {
            default_network: Network::BsvMainnet,
            enable_bsv: true,
            enable_solana: true,
            rpc_endpoints: vec![
                RpcEndpoint {
                    network: Network::BsvMainnet,
                    url: "https://api.whatsonchain.com/v1/bsv/main".to_string(),
                    username: None,
                    password: None,
                    priority: 1,
                },
                RpcEndpoint {
                    network: Network::SolanaMainnet,
                    url: "https://api.mainnet-beta.solana.com".to_string(),
                    username: None,
                    password: None,
                    priority: 1,
                },
            ],
            p2p_nodes: vec![],
            connection_timeout: 30,
            request_timeout: 60,
            retry_attempts: 3,
            use_tls: true,
        }
    }
}

impl Default for WalletConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            storage_path: "~/.zeta/wallet".to_string(),
            default_derivation_path: "m/44'/236'/0'/0/0".to_string(),
            encryption_algorithm: "AES-256-GCM".to_string(),
            key_derivation_iterations: 100000,
            auto_lock_timeout: 300, // 5 minutes
            require_password_for_spending: true,
            backup_location: None,
            backup_frequency: 7, // weekly
        }
    }
}

impl Default for BsvConfig {
    fn default() -> Self {
        Self {
            use_nour: true,
            nour_path: Some("./nour".to_string()),
            default_fee_rate: 1, // 1 satoshi per byte
            min_relay_fee: 1,
            dust_limit: 546,
            enable_teranode: false,
            teranode: TeranodeConfig::default(),
            enable_bsv21: true,
            enable_brc0100: true,
        }
    }
}

impl Default for TeranodeConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            mining_address: "".to_string(),
            thread_count: 4,
            difficulty_target: 1,
            pool_url: None,
            pool_user: None,
            pool_password: None,
        }
    }
}

impl Default for SolanaConfig {
    fn default() -> Self {
        Self {
            rpc_endpoint: "https://api.mainnet-beta.solana.com".to_string(),
            websocket_endpoint: "wss://api.mainnet-beta.solana.com".to_string(),
            commitment: "confirmed".to_string(),
            default_fee: 5000, // 0.000005 SOL
            priority_fee: 0,
            compute_unit_limit: 200000,
            compute_unit_price: 0,
            enable_token_program: true,
            enable_stake_program: false,
            enable_vote_program: false,
        }
    }
}

impl Default for SecurityConfig {
    fn default() -> Self {
        Self {
            enable_hardware_wallet: false,
            hardware_wallet_type: None,
            require_confirmation_large_tx: true,
            large_tx_threshold: 100000000, // 1 BSV or equivalent
            enable_tx_verification: true,
            enable_address_whitelist: false,
            whitelisted_addresses: vec![],
            enable_rate_limiting: true,
            rate_limit_requests: 60,
            enable_audit_logging: true,
            audit_log_path: "~/.zeta/audit.log".to_string(),
        }
    }
}

impl Default for PerformanceConfig {
    fn default() -> Self {
        Self {
            cache_size_mb: 100,
            enable_tx_caching: true,
            enable_address_caching: true,
            enable_block_caching: false,
            cache_ttl: 300, // 5 minutes
            connection_pool_size: 10,
            thread_pool_size: 4,
            batch_request_size: 50,
            enable_compression: true,
            enable_parallel_processing: true,
        }
    }
}