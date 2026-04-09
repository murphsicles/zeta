//! Data types for Teranode RPC communication

use serde::{Serialize, Deserialize};
use serde_json::Value;

/// RPC request structure
#[derive(Debug, Clone, Serialize)]
pub struct RpcRequest {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Method name
    pub method: String,
    /// Parameters
    pub params: Vec<Value>,
    /// Request ID
    pub id: u64,
}

impl RpcRequest {
    /// Create a new RPC request
    pub fn new(method: &str, params: Vec<Value>, id: u64) -> Self {
        Self {
            jsonrpc: "2.0".to_string(),
            method: method.to_string(),
            params,
            id,
        }
    }
}

/// RPC response structure
#[derive(Debug, Clone, Deserialize)]
pub struct RpcResponse<T> {
    /// JSON-RPC version
    pub jsonrpc: String,
    /// Result (if successful)
    pub result: Option<T>,
    /// Error (if failed)
    pub error: Option<RpcError>,
    /// Request ID
    pub id: u64,
}

/// RPC error structure
#[derive(Debug, Clone, Deserialize)]
pub struct RpcError {
    /// Error code
    pub code: i32,
    /// Error message
    pub message: String,
    /// Additional error data
    pub data: Option<Value>,
}

/// Mining candidate from Teranode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MiningCandidate {
    /// Block template version
    pub version: u32,
    /// Previous block hash
    pub prevhash: String,
    /// Coinbase transaction
    pub coinbase: String,
    /// Merkle root
    pub merkleroot: String,
    /// Block time
    pub time: u32,
    /// Bits (difficulty target)
    pub bits: String,
    /// Height
    pub height: u64,
    /// Target
    pub target: String,
    /// Transactions
    pub transactions: Vec<String>,
    /// Default witness commitment
    pub default_witness_commitment: Option<String>,
    /// Mining ID
    pub id: String,
    /// Rules
    pub rules: Vec<String>,
    /// Max time
    pub maxtime: Option<u32>,
    /// Min time
    pub mintime: Option<u32>,
    /// mutable
    pub mutable: Vec<String>,
    /// Nonce range
    pub noncerange: Option<String>,
    /// Capabilities
    pub capabilities: Vec<String>,
}

/// Mining solution to submit to Teranode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MiningSolution {
    /// Mining ID
    pub id: String,
    /// Nonce
    pub nonce: u32,
    /// Coinbase transaction
    pub coinbase: String,
    /// Time
    pub time: u32,
    /// Version
    pub version: u32,
    /// Extra nonce 1
    #[serde(rename = "extranonce1")]
    pub extra_nonce_1: Option<String>,
    /// Extra nonce 2
    #[serde(rename = "extranonce2")]
    pub extra_nonce_2: Option<String>,
    /// nTime
    pub ntime: Option<u32>,
    /// Merkle branches
    pub merkle_branch: Option<Vec<String>>,
}

/// Mining status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MiningStatus {
    /// Is mining active
    pub is_mining: bool,
    /// Hash rate (hashes per second)
    pub hash_rate: f64,
    /// Total hashes
    pub total_hashes: u64,
    /// Accepted shares
    pub accepted_shares: u64,
    /// Rejected shares
    pub rejected_shares: u64,
    /// Current difficulty
    pub difficulty: f64,
    /// Block height
    pub block_height: u64,
    /// Network hash rate
    pub network_hash_rate: f64,
    /// Pool connected
    pub pool_connected: bool,
    /// Last block time
    pub last_block_time: Option<u64>,
    /// Next difficulty adjustment
    pub next_difficulty_adjustment: Option<u64>,
}

/// Mining statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MiningStats {
    /// Uptime in seconds
    pub uptime_seconds: u64,
    /// Total blocks found
    pub total_blocks: u32,
    /// Total reward earned (in satoshis)
    pub total_reward: u64,
    /// Mining efficiency (hashes per watt)
    pub efficiency: f64,
    /// Hardware information
    pub hardware_info: String,
    /// CPU usage percentage
    pub cpu_usage: f64,
    /// Memory usage in MB
    pub memory_usage_mb: u64,
    /// Network usage in KB/s
    pub network_usage_kbps: f64,
    /// Power consumption in watts
    pub power_consumption_watts: f64,
}

/// Block information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BlockInfo {
    /// Block hash
    pub hash: String,
    /// Block height
    pub height: u64,
    /// Block time
    pub time: u64,
    /// Block size
    pub size: u32,
    /// Transaction count
    pub tx_count: u32,
    /// Difficulty
    pub difficulty: f64,
    /// Merkle root
    pub merkle_root: String,
    /// Nonce
    pub nonce: u32,
    /// Bits
    pub bits: String,
    /// Version
    pub version: u32,
    /// Previous block hash
    pub prev_hash: String,
    /// Next block hash
    pub next_hash: Option<String>,
    /// Reward
    pub reward: u64,
    /// Fees
    pub fees: u64,
}

/// Network information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkInfo {
    /// Network name
    pub network: String,
    /// Protocol version
    pub protocol_version: u32,
    /// Block count
    pub block_count: u64,
    /// Difficulty
    pub difficulty: f64,
    /// Hash rate
    pub hash_rate: f64,
    /// Connected peers
    pub connected_peers: u32,
    /// Relay fee (satoshis per byte)
    pub relay_fee: u64,
    /// Minimum relay fee (satoshis per byte)
    pub min_relay_fee: u64,
    /// Time offset
    pub time_offset: i64,
    /// Warnings
    pub warnings: String,
    /// Verification progress
    pub verification_progress: f64,
    /// Initial block download
    pub initial_block_download: bool,
}

/// Peer information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeerInfo {
    /// Peer ID
    pub id: u64,
    /// Address
    pub addr: String,
    /// Services
    pub services: String,
    /// Last send
    pub lastsend: u64,
    /// Last receive
    pub lastrecv: u64,
    /// Bytes sent
    pub bytessent: u64,
    /// Bytes received
    pub bytesrecv: u64,
    /// Connection time
    pub conntime: u64,
    /// Time offset
    pub timeoffset: i64,
    /// Ping time
    pub pingtime: f64,
    /// Minimum ping
    pub minping: f64,
    /// Version
    pub version: u32,
    /// Sub version
    pub subver: String,
    /// Inbound
    pub inbound: bool,
    /// Starting height
    pub startingheight: u64,
    /// Ban score
    pub banscore: u32,
    /// Synced headers
    pub synced_headers: u64,
    /// Synced blocks
    pub synced_blocks: u64,
    /// Whitelisted
    pub whitelisted: bool,
}