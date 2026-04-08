//! Integration tests for Teranode mining software

#[cfg(test)]
mod tests {
    use crate::blockchain::bsv::teranode::{
        TeranodeClient, 
        TeranodeClientConfig,
        TeranodeError,
    };
    
    /// Test Teranode client creation
    #[test]
    fn test_client_creation() {
        let config = TeranodeClientConfig::default();
        let client = TeranodeClient::new(config);
        
        // Client should be created successfully with default config
        assert!(client.is_ok());
    }
    
    /// Test configuration validation
    #[test]
    fn test_config_validation() {
        let mut config = TeranodeClientConfig::default();
        
        // Valid config should pass
        assert!(config.validate().is_ok());
        
        // Empty URL should fail
        config.rpc_url = "".to_string();
        assert!(config.validate().is_err());
        
        // Reset and test zero timeout
        config = TeranodeClientConfig::default();
        config.connection_timeout = 0;
        assert!(config.validate().is_err());
        
        // Reset and test zero request timeout
        config = TeranodeClientConfig::default();
        config.request_timeout = 0;
        assert!(config.validate().is_err());
        
        // Reset and test zero connection pool size
        config = TeranodeClientConfig::default();
        config.connection_pool_size = 0;
        assert!(config.validate().is_err());
        
        // Reset and test zero batch size
        config = TeranodeClientConfig::default();
        config.batch_size = 0;
        assert!(config.validate().is_err());
    }
    
    /// Test error types
    #[test]
    fn test_error_types() {
        // Test RPC error creation
        let rpc_error = TeranodeError::rpc(-1, "Test error");
        
        match rpc_error {
            TeranodeError::Rpc { code, message } => {
                assert_eq!(code, -1);
                assert_eq!(message, "Test error");
            }
            _ => panic!("Expected Rpc error"),
        }
        
        // Test network error detection
        let config_error = TeranodeError::Config("Test".to_string());
        assert!(!config_error.is_network_error());
        assert!(!config_error.is_auth_error());
        assert!(!config_error.is_timeout_error());
    }
    
    /// Test configuration from environment
    #[test]
    fn test_config_from_env() {
        // Save original environment variables
        let original_url = std::env::var("TERANODE_RPC_URL").ok();
        let original_username = std::env::var("TERANODE_RPC_USERNAME").ok();
        let original_password = std::env::var("TERANODE_RPC_PASSWORD").ok();
        
        // Set test environment variables
        std::env::set_var("TERANODE_RPC_URL", "http://test:18332");
        std::env::set_var("TERANODE_RPC_USERNAME", "testuser");
        std::env::set_var("TERANODE_RPC_PASSWORD", "testpass");
        
        let config = TeranodeClientConfig::from_env();
        assert!(config.is_ok());
        
        let config = config.unwrap();
        assert_eq!(config.rpc_url, "http://test:18332");
        assert_eq!(config.rpc_username, "testuser");
        assert_eq!(config.rpc_password, "testpass");
        
        // Restore original environment variables
        if let Some(url) = original_url {
            std::env::set_var("TERANODE_RPC_URL", url);
        } else {
            std::env::remove_var("TERANODE_RPC_URL");
        }
        
        if let Some(username) = original_username {
            std::env::set_var("TERANODE_RPC_USERNAME", username);
        } else {
            std::env::remove_var("TERANODE_RPC_USERNAME");
        }
        
        if let Some(password) = original_password {
            std::env::set_var("TERANODE_RPC_PASSWORD", password);
        } else {
            std::env::remove_var("TERANODE_RPC_PASSWORD");
        }
    }
    
    /// Test mining types serialization
    #[test]
    fn test_mining_types_serialization() {
        use serde_json;
        use crate::blockchain::bsv::teranode::types::{MiningCandidate, MiningSolution};
        
        // Test MiningCandidate serialization
        let candidate = MiningCandidate {
            version: 1,
            prevhash: "0000000000000000000000000000000000000000000000000000000000000000".to_string(),
            coinbase: "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff00ffffffff0100000000000000000000000000".to_string(),
            merkleroot: "0000000000000000000000000000000000000000000000000000000000000000".to_string(),
            time: 1234567890,
            bits: "1d00ffff".to_string(),
            height: 1,
            target: "00000000ffff0000000000000000000000000000000000000000000000000000".to_string(),
            transactions: vec![],
            default_witness_commitment: None,
            id: "test-id".to_string(),
            rules: vec![],
            maxtime: None,
            mintime: None,
            mutable: vec![],
            noncerange: None,
            capabilities: vec![],
        };
        
        let serialized = serde_json::to_string(&candidate);
        assert!(serialized.is_ok());
        
        // Test MiningSolution serialization
        let solution = MiningSolution {
            id: "test-id".to_string(),
            nonce: 123456,
            coinbase: "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff00ffffffff0100000000000000000000000000".to_string(),
            time: 1234567890,
            version: 1,
            extra_nonce_1: None,
            extra_nonce_2: None,
            ntime: None,
            merkle_branch: None,
        };
        
        let serialized = serde_json::to_string(&solution);
        assert!(serialized.is_ok());
    }
}