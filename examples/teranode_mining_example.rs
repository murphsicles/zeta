//! Example of Teranode mining integration with Zeta v0.3.50+
//!
//! This example demonstrates how to use the Teranode RPC client
//! for Bitcoin SV mining operations.

use zetac::blockchain::bsv::teranode::{
    TeranodeClient,
    TeranodeClientConfig,
    MiningCandidate,
    MiningSolution,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Teranode Mining Integration Example ===\n");
    
    // 1. Create Teranode client configuration
    println!("1. Creating Teranode client configuration...");
    let config = TeranodeClientConfig {
        rpc_url: "http://localhost:18332".to_string(),
        rpc_username: "your_username".to_string(),
        rpc_password: "your_password".to_string(),
        ..Default::default()
    };
    
    // 2. Create Teranode client
    println!("2. Creating Teranode client...");
    let client = TeranodeClient::new(config)?;
    println!("   Client created successfully!");
    
    // 3. Test connection
    println!("3. Testing connection to Teranode node...");
    let connected = client.test_connection().await?;
    if connected {
        println!("   Connected successfully!");
    } else {
        println!("   Connection failed!");
        return Ok(());
    }
    
    // 4. Get blockchain information
    println!("4. Getting blockchain information...");
    let blockchain_info = client.get_blockchain_info().await?;
    println!("   Network: {}", blockchain_info.network);
    println!("   Block count: {}", blockchain_info.block_count);
    println!("   Difficulty: {:.2}", blockchain_info.difficulty);
    println!("   Hash rate: {:.2} EH/s", blockchain_info.hash_rate / 1e18);
    
    // 5. Get mining candidate (block template)
    println!("5. Getting mining candidate...");
    match client.get_mining_candidate().await {
        Ok(candidate) => {
            println!("   Mining candidate received!");
            println!("   ID: {}", candidate.id);
            println!("   Height: {}", candidate.height);
            println!("   Previous hash: {}", candidate.prevhash);
            println!("   Bits: {}", candidate.bits);
            
            // 6. Create and submit a mining solution (example)
            println!("6. Creating example mining solution...");
            let solution = MiningSolution {
                id: candidate.id.clone(),
                nonce: 123456,
                coinbase: candidate.coinbase,
                time: candidate.time,
                version: candidate.version,
                extra_nonce_1: None,
                extra_nonce_2: None,
                ntime: None,
                merkle_branch: None,
            };
            
            println!("   Solution created (example - not actually mining)");
            println!("   Note: Actual mining would involve finding a valid nonce");
            
            // 7. Get mining status
            println!("7. Getting mining status...");
            let status = client.get_mining_status().await?;
            println!("   Mining active: {}", status.is_mining);
            println!("   Hash rate: {:.2} H/s", status.hash_rate);
            println!("   Total hashes: {}", status.total_hashes);
            println!("   Difficulty: {:.2}", status.difficulty);
            println!("   Pool connected: {}", status.pool_connected);
        }
        Err(e) => {
            println!("   Failed to get mining candidate: {}", e);
            println!("   This is expected if Teranode doesn't support getminingcandidate");
        }
    }
    
    // 8. Get peer information
    println!("8. Getting peer information...");
    match client.get_peer_info().await {
        Ok(peers) => {
            println!("   Connected peers: {}", peers.len());
            for (i, peer) in peers.iter().take(3).enumerate() {
                println!("   Peer {}: {} (v{})", i + 1, peer.addr, peer.version);
            }
            if peers.len() > 3 {
                println!("   ... and {} more", peers.len() - 3);
            }
        }
        Err(e) => {
            println!("   Failed to get peer info: {}", e);
        }
    }
    
    // 9. Demonstrate batch calls
    println!("9. Demonstrating batch calls...");
    let calls = vec![
        ("getblockcount", vec![]),
        ("getdifficulty", vec![]),
        ("getnetworkhashps", vec![]),
    ];
    
    match client.batch_call::<serde_json::Value>(calls).await {
        Ok(results) => {
            println!("   Batch call completed with {} results", results.len());
            for (i, result) in results.iter().enumerate() {
                match result {
                    Ok(value) => println!("   Result {}: {:?}", i + 1, value),
                    Err(e) => println!("   Result {} error: {}", i + 1, e),
                }
            }
        }
        Err(e) => {
            println!("   Batch call failed: {}", e);
        }
    }
    
    println!("\n=== Example Complete ===");
    println!("\nSummary:");
    println!("- Teranode RPC client successfully implemented");
    println!("- Supports Bitcoin-compatible HTTP/JSON-RPC protocol");
    println!("- Implements critical mining commands:");
    println!("  • getminingcandidate - Get block template");
    println!("  • submitminingsolution - Submit mined block");
    println!("- Includes error handling, retry logic, and connection pooling");
    println!("- Ready for integration with Zeta v0.3.50+ mining workflow");
    
    Ok(())
}