//! Zeta Language Server Protocol server
//!
//! This binary implements the LSP server for the Zeta programming language.
//! It communicates via stdin/stdout using the JSON-RPC protocol.

use std::env;
use zetac::lsp::LspServer;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Simple logging to stderr
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .format_timestamp(None)
        .format_level(false)
        .format_target(false)
        .init();
    
    // Check for version flag
    let args: Vec<String> = env::args().collect();
    if args.len() > 1 && (args[1] == "--version" || args[1] == "-v") {
        println!("zeta-lsp v0.3.44");
        return Ok(());
    }
    
    if args.len() > 1 && (args[1] == "--help" || args[1] == "-h") {
        println!("Zeta Language Server Protocol server");
        println!();
        println!("Usage: zeta-lsp [OPTIONS]");
        println!();
        println!("Options:");
        println!("  -h, --help     Print this help message");
        println!("  -v, --version  Print version information");
        println!();
        println!("The server communicates via stdin/stdout using JSON-RPC.");
        return Ok(());
    }
    
    // Create and run the LSP server
    let server = LspServer::new();
    
    match server.run() {
        Ok(()) => {
            log::info!("LSP server exited successfully");
            Ok(())
        }
        Err(e) => {
            log::error!("LSP server error: {}", e);
            Err(Box::new(e))
        }
    }
}