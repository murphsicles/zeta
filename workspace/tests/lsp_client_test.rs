// Simple test client for Zeta LSP server
// This demonstrates how an editor would communicate with the LSP server

use std::io::{self, Write};
use std::process::{Command, Stdio};
use std::thread;
use std::time::Duration;

fn send_lsp_message(process: &mut std::process::Child, method: &str, params: serde_json::Value, id: u64) -> io::Result<()> {
    let message = serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        "method": method,
        "params": params
    });
    
    let message_str = serde_json::to_string(&message).unwrap();
    let request = format!("Content-Length: {}\r\n\r\n{}", message_str.len(), message_str);
    
    if let Some(stdin) = &mut process.stdin {
        stdin.write_all(request.as_bytes())?;
        stdin.flush()?;
    }
    
    Ok(())
}

fn main() -> io::Result<()> {
    println!("=== Zeta LSP Server Test ===\n");
    
    // Start the LSP server
    println!("Starting LSP server...");
    let mut server = Command::new("cargo")
        .args(["run", "--bin", "lsp_server"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    
    // Give server time to start
    thread::sleep(Duration::from_millis(100));
    
    println!("Sending initialize request...");
    
    // Send initialize request
    let initialize_params = serde_json::json!({
        "processId": std::process::id(),
        "rootUri": "file:///test/project",
        "capabilities": {}
    });
    
    send_lsp_message(&mut server, "initialize", initialize_params, 1)?;
    
    // Give server time to respond
    thread::sleep(Duration::from_millis(50));
    
    // Read response
    let mut output = String::new();
    if let Some(stdout) = &mut server.stdout {
        io::read_to_string(stdout, &mut output)?;
    }
    
    println!("Server response: {}", output);
    
    // Send a document open notification
    println!("\nSending document open notification...");
    
    let document_params = serde_json::json!({
        "textDocument": {
            "uri": "file:///test/project/main.z",
            "languageId": "zeta",
            "version": 1,
            "text": r#"fn main() {
    let x = 42;
    println!("Hello, Zeta!");
    
    struct Point {
        x: i64,
        y: i64,
    }
    
    let p = Point { x: 10, y: 20 };
    println!("Point: ({}, {})", p.x, p.y);
}"#
        }
    });
    
    send_lsp_message(&mut server, "textDocument/didOpen", document_params, 0)?;
    
    // Send hover request
    println!("\nSending hover request...");
    
    let hover_params = serde_json::json!({
        "textDocument": {
            "uri": "file:///test/project/main.z"
        },
        "position": {
            "line": 0,
            "character": 3
        }
    });
    
    send_lsp_message(&mut server, "textDocument/hover", hover_params, 2)?;
    
    // Send completion request
    println!("\nSending completion request...");
    
    let completion_params = serde_json::json!({
        "textDocument": {
            "uri": "file:///test/project/main.z"
        },
        "position": {
            "line": 1,
            "character": 8
        }
    });
    
    send_lsp_message(&mut server, "textDocument/completion", completion_params, 3)?;
    
    // Read responses
    thread::sleep(Duration::from_millis(100));
    
    output.clear();
    if let Some(stdout) = &mut server.stdout {
        io::read_to_string(stdout, &mut output)?;
    }
    
    println!("\nServer responses:\n{}", output);
    
    // Send shutdown
    println!("\nSending shutdown request...");
    send_lsp_message(&mut server, "shutdown", serde_json::Value::Null, 4)?;
    
    // Send exit
    thread::sleep(Duration::from_millis(50));
    send_lsp_message(&mut server, "exit", serde_json::Value::Null, 0)?;
    
    // Wait for server to exit
    let _ = server.wait();
    
    println!("\n=== Test Complete ===");
    println!("This demonstrates the LSP server foundation.");
    println!("In a real implementation, the server would:");
    println!("1. Parse Zeta code accurately");
    println!("2. Provide real type information");
    println!("3. Support goto definition, find references, etc.");
    println!("4. Handle multiple documents");
    println!("5. Provide real-time diagnostics");
    
    Ok(())
}