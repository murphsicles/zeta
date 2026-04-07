//! Configuration for Teranode RPC client

use serde::{Serialize, Deserialize};

/// Teranode RPC client configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TeranodeClientConfig {
    /// RPC server URL (e.g., http://localhost:18332)
    pub rpc_url: String,
    
    /// RPC username
    pub rpc_username: String,
    
    /// RPC password
    pub rpc_password: String,
    
    /// Connection timeout in seconds
    pub connection_timeout: u64,
    
    /// Request timeout in seconds
    pub request_timeout: u64,
    
    /// Maximum retry attempts
    pub max_retries: u32,
    
    /// Retry delay in milliseconds
    pub retry_delay_ms: u64,
    
    /// Enable connection pooling
    pub enable_connection_pool: bool,
    
    /// Connection pool size
    pub connection_pool_size: usize,
    
    /// Enable request batching
    pub enable_batching: bool,
    
    /// Batch size
    pub batch_size: usize,
    
    /// Enable compression
    pub enable_compression: bool,
    
    /// Enable keep-alive
    pub enable_keep_alive: bool,
    
    /// Keep-alive duration in seconds
    pub keep_alive_duration: u64,
    
    /// Enable metrics collection
    pub enable_metrics: bool,
    
    /// Metrics collection interval in seconds
    pub metrics_interval: u64,
}

impl Default for TeranodeClientConfig {
    fn default() -> Self {
        Self {
            rpc_url: "http://localhost:18332".to_string(),
            rpc_username: "".to_string(),
            rpc_password: "".to_string(),
            connection_timeout: 30,
            request_timeout: 60,
            max_retries: 3,
            retry_delay_ms: 1000,
            enable_connection_pool: true,
            connection_pool_size: 10,
            enable_batching: true,
            batch_size: 50,
            enable_compression: true,
            enable_keep_alive: true,
            keep_alive_duration: 30,
            enable_metrics: true,
            metrics_interval: 60,
        }
    }
}

impl TeranodeClientConfig {
    /// Create a new configuration with default values
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Create configuration from environment variables
    pub fn from_env() -> Result<Self, String> {
        let mut config = Self::default();
        
        if let Ok(url) = std::env::var("TERANODE_RPC_URL") {
            config.rpc_url = url;
        }
        
        if let Ok(username) = std::env::var("TERANODE_RPC_USERNAME") {
            config.rpc_username = username;
        }
        
        if let Ok(password) = std::env::var("TERANODE_RPC_PASSWORD") {
            config.rpc_password = password;
        }
        
        Ok(config)
    }
    
    /// Validate configuration
    pub fn validate(&self) -> Result<(), String> {
        if self.rpc_url.is_empty() {
            return Err("RPC URL cannot be empty".to_string());
        }
        
        if self.connection_timeout == 0 {
            return Err("Connection timeout must be greater than 0".to_string());
        }
        
        if self.request_timeout == 0 {
            return Err("Request timeout must be greater than 0".to_string());
        }
        
        if self.connection_pool_size == 0 {
            return Err("Connection pool size must be greater than 0".to_string());
        }
        
        if self.batch_size == 0 {
            return Err("Batch size must be greater than 0".to_string());
        }
        
        Ok(())
    }
    
    /// Get RPC URL
    pub fn rpc_url(&self) -> &str {
        &self.rpc_url
    }
    
    /// Get RPC username
    pub fn rpc_username(&self) -> &str {
        &self.rpc_username
    }
    
    /// Get RPC password
    pub fn rpc_password(&self) -> &str {
        &self.rpc_password
    }
}