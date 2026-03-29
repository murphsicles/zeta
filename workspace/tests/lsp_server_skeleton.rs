// LSP (Language Server Protocol) Server Skeleton for Zeta
// This demonstrates the foundation for IDE tooling

use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Write};
use std::process;

#[derive(Debug, serde::Deserialize)]
struct LspMessage {
    jsonrpc: String,
    id: Option<u64>,
    method: Option<String>,
    params: Option<serde_json::Value>,
}

#[derive(Debug, serde::Serialize)]
struct LspResponse {
    jsonrpc: String,
    id: u64,
    result: serde_json::Value,
}

#[derive(Debug, serde::Serialize)]
struct LspError {
    code: i32,
    message: String,
}

#[derive(Debug, serde::Serialize)]
struct LspErrorResponse {
    jsonrpc: String,
    id: u64,
    error: LspError,
}

#[derive(Debug)]
struct Position {
    line: u32,
    character: u32,
}

#[derive(Debug)]
struct Range {
    start: Position,
    end: Position,
}

#[derive(Debug)]
struct Location {
    uri: String,
    range: Range,
}

#[derive(Debug)]
struct SymbolInformation {
    name: String,
    kind: i32,
    location: Location,
}

struct ZetaLspServer {
    documents: HashMap<String, String>,
    symbols: HashMap<String, Vec<SymbolInformation>>,
}

impl ZetaLspServer {
    fn new() -> Self {
        Self {
            documents: HashMap::new(),
            symbols: HashMap::new(),
        }
    }
    
    fn handle_initialize(&self, id: u64) -> LspResponse {
        println!("[LSP] Initializing server...");
        
        let capabilities = serde_json::json!({
            "textDocumentSync": 1, // Full sync
            "hoverProvider": true,
            "completionProvider": {
                "triggerCharacters": [".", ":"]
            },
            "definitionProvider": true,
            "referencesProvider": true,
            "documentSymbolProvider": true,
        });
        
        LspResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: serde_json::json!({
                "capabilities": capabilities,
                "serverInfo": {
                    "name": "Zeta Language Server",
                    "version": "0.1.0"
                }
            }),
        }
    }
    
    fn handle_text_document_did_open(&mut self, params: serde_json::Value) {
        if let Some(uri) = params.get("textDocument").and_then(|td| td.get("uri")).and_then(|u| u.as_str()) {
            if let Some(text) = params.get("textDocument").and_then(|td| td.get("text")).and_then(|t| t.as_str()) {
                println!("[LSP] Document opened: {}", uri);
                self.documents.insert(uri.to_string(), text.to_string());
                self.update_symbols(uri, text);
            }
        }
    }
    
    fn handle_text_document_did_change(&mut self, params: serde_json::Value) {
        if let Some(uri) = params.get("textDocument").and_then(|td| td.get("uri")).and_then(|u| u.as_str()) {
            if let Some(changes) = params.get("contentChanges").and_then(|cc| cc.as_array()) {
                if let Some(change) = changes.first() {
                    if let Some(text) = change.get("text").and_then(|t| t.as_str()) {
                        println!("[LSP] Document changed: {}", uri);
                        self.documents.insert(uri.to_string(), text.to_string());
                        self.update_symbols(uri, text);
                    }
                }
            }
        }
    }
    
    fn update_symbols(&mut self, uri: &str, text: &str) {
        println!("[LSP] Updating symbols for: {}", uri);
        
        let mut symbols = Vec::new();
        
        // Simple symbol extraction (would use real parser in production)
        for (line_num, line) in text.lines().enumerate() {
            if line.trim().starts_with("fn ") {
                let name_start = line.find("fn ").unwrap() + 3;
                let name_end = line[name_start..].find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(line.len() - name_start);
                let name = line[name_start..name_start + name_end].to_string();
                
                symbols.push(SymbolInformation {
                    name,
                    kind: 12, // Function
                    location: Location {
                        uri: uri.to_string(),
                        range: Range {
                            start: Position { line: line_num as u32, character: 0 },
                            end: Position { line: line_num as u32, character: line.len() as u32 },
                        },
                    },
                });
            } else if line.trim().starts_with("struct ") {
                let name_start = line.find("struct ").unwrap() + 7;
                let name_end = line[name_start..].find(|c: char| !c.is_alphanumeric() && c != '_').unwrap_or(line.len() - name_start);
                let name = line[name_start..name_start + name_end].to_string();
                
                symbols.push(SymbolInformation {
                    name,
                    kind: 23, // Struct
                    location: Location {
                        uri: uri.to_string(),
                        range: Range {
                            start: Position { line: line_num as u32, character: 0 },
                            end: Position { line: line_num as u32, character: line.len() as u32 },
                        },
                    },
                });
            }
        }
        
        self.symbols.insert(uri.to_string(), symbols);
        println!("[LSP] Found {} symbols", self.symbols[uri].len());
    }
    
    fn handle_text_document_hover(&self, params: serde_json::Value) -> LspResponse {
        let id = params.get("id").and_then(|id| id.as_u64()).unwrap_or(0);
        
        // Simple hover response
        let result = serde_json::json!({
            "contents": {
                "kind": "markdown",
                "value": "**Zeta Language**\n\nThis is a placeholder hover response.\n\nIn a real implementation, this would show:\n- Type information\n- Documentation\n- Source location"
            }
        });
        
        LspResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result,
        }
    }
    
    fn handle_text_document_completion(&self, params: serde_json::Value) -> LspResponse {
        let id = params.get("id").and_then(|id| id.as_u64()).unwrap_or(0);
        
        // Simple completion list
        let items = vec![
            serde_json::json!({
                "label": "fn",
                "kind": 14, // Keyword
                "detail": "Function definition",
                "documentation": "Define a new function"
            }),
            serde_json::json!({
                "label": "let",
                "kind": 14,
                "detail": "Variable declaration",
                "documentation": "Declare a new variable"
            }),
            serde_json::json!({
                "label": "println!",
                "kind": 3, // Function
                "detail": "Macro to print to stdout",
                "documentation": "Prints to standard output with formatting"
            }),
        ];
        
        LspResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: serde_json::json!({
                "isIncomplete": false,
                "items": items
            }),
        }
    }
    
    fn handle_shutdown(&self, id: u64) -> LspResponse {
        println!("[LSP] Shutting down...");
        
        LspResponse {
            jsonrpc: "2.0".to_string(),
            id,
            result: serde_json::Value::Null,
        }
    }
    
    fn run(&mut self) -> io::Result<()> {
        println!("[LSP] Zeta Language Server starting...");
        
        let stdin = io::stdin();
        let mut stdin_reader = BufReader::new(stdin.lock());
        let mut stdout = io::stdout();
        
        loop {
            let mut content_length = 0;
            let mut line = String::new();
            
            // Read headers
            loop {
                line.clear();
                stdin_reader.read_line(&mut line)?;
                
                if line == "\r\n" || line == "\n" {
                    break;
                }
                
                if line.starts_with("Content-Length: ") {
                    if let Ok(len) = line["Content-Length: ".len()..].trim().parse::<usize>() {
                        content_length = len;
                    }
                }
            }
            
            // Read message body
            let mut body = vec![0; content_length];
            stdin_reader.read_exact(&mut body)?;
            let body_str = String::from_utf8(body).unwrap();
            
            // Parse message
            let message: LspMessage = match serde_json::from_str(&body_str) {
                Ok(msg) => msg,
                Err(e) => {
                    eprintln!("[LSP] Failed to parse message: {}", e);
                    continue;
                }
            };
            
            println!("[LSP] Received message: {:?}", message.method);
            
            // Handle message
            let response = match message.method.as_deref() {
                Some("initialize") => {
                    if let Some(id) = message.id {
                        let response = self.handle_initialize(id);
                        Some(response)
                    } else {
                        None
                    }
                }
                Some("textDocument/didOpen") => {
                    if let Some(params) = message.params {
                        self.handle_text_document_did_open(params);
                    }
                    None
                }
                Some("textDocument/didChange") => {
                    if let Some(params) = message.params {
                        self.handle_text_document_did_change(params);
                    }
                    None
                }
                Some("textDocument/hover") => {
                    if let Some(params) = message.params {
                        Some(self.handle_text_document_hover(params))
                    } else {
                        None
                    }
                }
                Some("textDocument/completion") => {
                    if let Some(params) = message.params {
                        Some(self.handle_text_document_completion(params))
                    } else {
                        None
                    }
                }
                Some("shutdown") => {
                    if let Some(id) = message.id {
                        Some(self.handle_shutdown(id))
                    } else {
                        None
                    }
                }
                Some("exit") => {
                    println!("[LSP] Exit requested");
                    process::exit(0);
                }
                _ => {
                    println!("[LSP] Unhandled method: {:?}", message.method);
                    None
                }
            };
            
            // Send response if needed
            if let Some(response) = response {
                let response_json = serde_json::to_string(&response)?;
                let response_msg = format!("Content-Length: {}\r\n\r\n{}", response_json.len(), response_json);
                stdout.write_all(response_msg.as_bytes())?;
                stdout.flush()?;
            }
        }
    }
}

fn main() -> io::Result<()> {
    let mut server = ZetaLspServer::new();
    server.run()
}