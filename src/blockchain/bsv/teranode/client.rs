//! Teranode RPC client implementation

use std::sync::Arc;
use std::time::Duration;
use reqwest::{Client, ClientBuilder, Response};
use reqwest::header::{HeaderMap, HeaderValue, AUTHORIZATION, CONTENT_TYPE};
use serde::{Serialize, de::DeserializeOwned};
use serde_json::{Value, json};
use tokio::sync::Mutex;
use base64;

use crate::blockchain::bsv::teranode::error::TeranodeError;
use crate::blockchain::bsv::teranode::config::TeranodeClientConfig;
use crate::blockchain::bsv::teranode::types::*;

/// Teranode RPC client
#[derive(Clone)]
pub struct TeranodeClient {
    /// HTTP client
    client: Client,
    /// Configuration
    config: TeranodeClientConfig,
    /// Request counter for IDs
    request_counter: Arc<Mutex<u64>>,
    /// Base URL
    base_url: String,
}

impl TeranodeClient {
    /// Create a new Teranode RPC client
    pub fn new(config: TeranodeClientConfig) -> Result<Self, TeranodeError> {
        config.validate()
            .map_err(|e| TeranodeError::Config(e))?;
        
        // Build HTTP client with configuration
        let mut headers = HeaderMap::new();
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));
        
        // Add basic auth if credentials provided
        if !config.rpc_username.is_empty() && !config.rpc_password.is_empty() {
            let auth = format!("{}:{}", config.rpc_username, config.rpc_password);
            let encoded = base64::engine::general_purpose::STANDARD.encode(auth);
            headers.insert(
                AUTHORIZATION,
                HeaderValue::from_str(&format!("Basic {}", encoded))
                    .map_err(|e| TeranodeError::Config(format!("Invalid auth header: {}", e)))?,
            );
        }
        
        let client_builder = ClientBuilder::new()
            .timeout(Duration::from_secs(config.request_timeout))
            .connect_timeout(Duration::from_secs(config.connection_timeout))
            .default_headers(headers);
        
        let client = if config.enable_connection_pool {
            client_builder
                .pool_max_idle_per_host(config.connection_pool_size)
                .build()
                .map_err(|e| TeranodeError::Config(format!("Failed to build HTTP client: {}", e)))?
        } else {
            client_builder
                .build()
                .map_err(|e| TeranodeError::Config(format!("Failed to build HTTP client: {}", e)))?
        };
        
        let base_url = config.rpc_url.clone();
        Ok(Self {
            client,
            config,
            request_counter: Arc::new(Mutex::new(0)),
            base_url,
        })
    }
    
    /// Create a new Teranode RPC client with default configuration
    pub fn new_default() -> Result<Self, TeranodeError> {
        Self::new(TeranodeClientConfig::default())
    }
    
    /// Create a new Teranode RPC client from environment variables
    pub fn from_env() -> Result<Self, TeranodeError> {
        let config = TeranodeClientConfig::from_env()
            .map_err(|e| TeranodeError::Config(e))?;
        Self::new(config)
    }
    
    /// Make an RPC call
    pub async fn call<T: DeserializeOwned>(
        &self,
        method: &str,
        params: Vec<Value>,
    ) -> Result<T, TeranodeError> {
        self.call_with_retry(method, params, 0).await
    }
    
    /// Make an RPC call with retry logic
    async fn call_with_retry<T: DeserializeOwned>(
        &self,
        method: &str,
        params: Vec<Value>,
        attempt: u32,
    ) -> Result<T, TeranodeError> {
        // Generate request ID
        let mut counter = self.request_counter.lock().await;
        let request_id = *counter;
        *counter = request_id.wrapping_add(1);
        drop(counter);
        
        // Create request
        let request = RpcRequest::new(method, params, request_id);
        
        // Make HTTP request
        let response = self.client
            .post(&self.base_url)
            .json(&request)
            .send()
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    TeranodeError::Timeout(self.config.request_timeout)
                } else {
                    TeranodeError::Network(e)
                }
            })?;
        
        // Check response status
        if !response.status().is_success() {
            return match response.status().as_u16() {
                401 | 403 => Err(TeranodeError::Auth("Authentication failed".to_string())),
                _ => {
                    let status = response.status();
                    let text = response.text().await.unwrap_or_default();
                    Err(TeranodeError::Other(format!("HTTP {}: {}", status, text)))
                }
            };
        }
        
        // Parse response
        let rpc_response: RpcResponse<T> = response
            .json()
            .await
            .map_err(|e| TeranodeError::Json(serde_json::Error::custom(e.to_string())))?;
        
        // Check for RPC error
        if let Some(error) = rpc_response.error {
            // Check if we should retry
            if attempt < self.config.max_retries {
                // Some RPC errors are retryable (e.g., temporary failures)
                if Self::is_retryable_error(&error) {
                    tokio::time::sleep(Duration::from_millis(self.config.retry_delay_ms)).await;
                    return self.call_with_retry(method, params, attempt + 1).await;
                }
            }
            
            return Err(TeranodeError::rpc(error.code, &error.message));
        }
        
        // Extract result
        rpc_response.result.ok_or_else(|| {
            TeranodeError::InvalidResponse("Response missing result field".to_string())
        })
    }
    
    /// Check if an RPC error is retryable
    fn is_retryable_error(error: &RpcError) -> bool {
        // RPC_IN_WARMUP (-28), RPC_MISC_ERROR (-1) for temporary issues
        matches!(error.code, -28 | -1)
    }
    
    /// Test connection to Teranode node
    pub async fn test_connection(&self) -> Result<bool, TeranodeError> {
        match self.get_blockchain_info().await {
            Ok(info) => {
                log::debug!("Connected to Teranode: {}", info.network);
                Ok(true)
            }
            Err(e) => {
                log::warn!("Failed to connect to Teranode: {}", e);
                Ok(false)
            }
        }
    }
    
    /// Get blockchain information
    pub async fn get_blockchain_info(&self) -> Result<NetworkInfo, TeranodeError> {
        self.call("getblockchaininfo", vec![]).await
    }
    
    /// Get mining candidate (block template)
    pub async fn get_mining_candidate(&self) -> Result<MiningCandidate, TeranodeError> {
        self.call("getminingcandidate", vec![]).await
    }
    
    /// Submit mining solution
    pub async fn submit_mining_solution(
        &self,
        solution: &MiningSolution,
    ) -> Result<bool, TeranodeError> {
        let params = vec![
            json!(solution.id),
            json!(solution.nonce),
            json!(solution.coinbase),
            json!(solution.time),
            json!(solution.version),
        ];
        
        // Add optional fields if present
        let mut params_value = params;
        if let Some(extra_nonce_1) = &solution.extra_nonce_1 {
            params_value.push(json!(extra_nonce_1));
            if let Some(extra_nonce_2) = &solution.extra_nonce_2 {
                params_value.push(json!(extra_nonce_2));
                if let Some(ntime) = solution.ntime {
                    params_value.push(json!(ntime));
                    if let Some(merkle_branch) = &solution.merkle_branch {
                        params_value.push(json!(merkle_branch));
                    }
                }
            }
        }
        
        self.call("submitminingsolution", params_value).await
    }
    
    /// Get mining status
    pub async fn get_mining_status(&self) -> Result<MiningStatus, TeranodeError> {
        // Note: This might need to be implemented differently depending on Teranode API
        // For now, we'll combine multiple RPC calls
        let blockchain_info = self.get_blockchain_info().await?;
        let network_hash_rate = blockchain_info.hash_rate;
        let block_height = blockchain_info.block_count;
        let difficulty = blockchain_info.difficulty;
        
        // Get peer info to check connectivity
        let peers: Vec<PeerInfo> = self.call("getpeerinfo", vec![]).await.unwrap_or_default();
        let pool_connected = !peers.is_empty();
        
        // Get mining info if available
        let mining_info_result: Result<Value, _> = self.call("getmininginfo", vec![]).await;
        
        let (is_mining, hash_rate, total_hashes, accepted_shares, rejected_shares) = 
            match mining_info_result {
                Ok(info) => {
                    let is_mining = info.get("generate").and_then(|v| v.as_bool()).unwrap_or(false);
                    let hash_rate = info.get("hashespersec").and_then(|v| v.as_f64()).unwrap_or(0.0);
                    let total_hashes = info.get("totalhashes").and_then(|v| v.as_u64()).unwrap_or(0);
                    let accepted_shares = info.get("accepted").and_then(|v| v.as_u64()).unwrap_or(0);
                    let rejected_shares = info.get("rejected").and_then(|v| v.as_u64()).unwrap_or(0);
                    (is_mining, hash_rate, total_hashes, accepted_shares, rejected_shares)
                }
                Err(_) => (false, 0.0, 0, 0, 0),
            };
        
        Ok(MiningStatus {
            is_mining,
            hash_rate,
            total_hashes,
            accepted_shares,
            rejected_shares,
            difficulty,
            block_height,
            network_hash_rate,
            pool_connected,
            last_block_time: None,
            next_difficulty_adjustment: None,
        })
    }
    
    /// Get block information
    pub async fn get_block_info(&self, hash: &str) -> Result<BlockInfo, TeranodeError> {
        let params = vec![json!(hash)];
        self.call("getblock", params).await
    }
    
    /// Get peer information
    pub async fn get_peer_info(&self) -> Result<Vec<PeerInfo>, TeranodeError> {
        self.call("getpeerinfo", vec![]).await
    }
    
    /// Get network hash rate
    pub async fn get_network_hash_rate(&self) -> Result<f64, TeranodeError> {
        let info = self.get_blockchain_info().await?;
        Ok(info.hash_rate)
    }
    
    /// Get current block height
    pub async fn get_block_height(&self) -> Result<u64, TeranodeError> {
        let info = self.get_blockchain_info().await?;
        Ok(info.block_count)
    }
    
    /// Get difficulty
    pub async fn get_difficulty(&self) -> Result<f64, TeranodeError> {
        let info = self.get_blockchain_info().await?;
        Ok(info.difficulty)
    }
    
    /// Validate address
    pub async fn validate_address(&self, address: &str) -> Result<bool, TeranodeError> {
        let params = vec![json!(address)];
        let result: Value = self.call("validateaddress", params).await?;
        Ok(result.get("isvalid").and_then(|v| v.as_bool()).unwrap_or(false))
    }
    
    /// Get transaction by ID
    pub async fn get_transaction(&self, txid: &str) -> Result<Value, TeranodeError> {
        let params = vec![json!(txid)];
        self.call("getrawtransaction", params).await
    }
    
    /// Send raw transaction
    pub async fn send_raw_transaction(&self, hex: &str) -> Result<String, TeranodeError> {
        let params = vec![json!(hex)];
        self.call("sendrawtransaction", params).await
    }
    
    /// Estimate fee rate
    pub async fn estimate_fee_rate(&self, blocks: u32) -> Result<f64, TeranodeError> {
        let params = vec![json!(blocks)];
        let result: Value = self.call("estimatesmartfee", params).await?;
        result.get("feerate")
            .and_then(|v| v.as_f64())
            .ok_or_else(|| TeranodeError::InvalidResponse("Missing feerate in response".to_string()))
    }
    
    /// Get mempool information
    pub async fn get_mempool_info(&self) -> Result<Value, TeranodeError> {
        self.call("getmempoolinfo", vec![]).await
    }
    
    /// Batch multiple RPC calls
    pub async fn batch_call<T: DeserializeOwned>(
        &self,
        calls: Vec<(&str, Vec<Value>)>,
    ) -> Result<Vec<Result<T, TeranodeError>>, TeranodeError> {
        if !self.config.enable_batching {
            return Err(TeranodeError::Config("Batching not enabled".to_string()));
        }
        
        if calls.len() > self.config.batch_size {
            return Err(TeranodeError::Config(format!(
                "Batch size {} exceeds maximum {}", 
                calls.len(), 
                self.config.batch_size
            )));
        }
        
        // Generate request IDs
        let mut counter = self.request_counter.lock().await;
        let mut requests = Vec::new();
        for (method, params) in calls {
            let request_id = *counter;
            *counter = request_id.wrapping_add(1);
            requests.push(RpcRequest::new(method, params, request_id));
        }
        drop(counter);
        
        // Make batch request
        let response = self.client
            .post(&self.base_url)
            .json(&requests)
            .send()
            .await
            .map_err(TeranodeError::Network)?;
        
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            return Err(TeranodeError::Other(format!("HTTP {}: {}", status, text)));
        }
        
        // Parse batch response
        let responses: Vec<RpcResponse<T>> = response
            .json()
            .await
            .map_err(|e| TeranodeError::Json(serde_json::Error::custom(e.to_string())))?;
        
        // Convert to results
        let mut results = Vec::new();
        for response in responses {
            if let Some(error) = response.error {
                results.push(Err(TeranodeError::rpc(error.code, &error.message)));
            } else if let Some(result) = response.result {
                results.push(Ok(result));
            } else {
                results.push(Err(TeranodeError::InvalidResponse(
                    "Response missing result field".to_string()
                )));
            }
        }
        
        Ok(results)
    }
}