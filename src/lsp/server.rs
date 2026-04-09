//! LSP server implementation

use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::sync::{Arc, Mutex};

use crate::lsp::protocol::{LspMessage, Request, Response, Notification};
use crate::lsp::handlers;
use crate::lsp::capabilities::ServerCapabilities;
use crate::lsp::LspResult;

/// LSP server state
#[derive(Debug)]
pub struct ServerState {
    /// Open documents with their content
    pub documents: HashMap<String, String>,
    /// Server capabilities
    pub capabilities: ServerCapabilities,
    /// Workspace root path
    pub root_path: Option<String>,
}

impl ServerState {
    /// Create new server state
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            capabilities: ServerCapabilities::default(),
            root_path: None,
        }
    }
}

/// Main LSP server
pub struct LspServer {
    /// Server state (shared across requests)
    state: Arc<Mutex<ServerState>>,
}

impl LspServer {
    /// Create a new LSP server
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(ServerState::new())),
        }
    }

    /// Run the LSP server on stdin/stdout
    pub fn run(&self) -> LspResult<()> {
        log::info!("Starting Zeta LSP server");
        
        let stdin = std::io::stdin();
        let stdout = std::io::stdout();
        
        let reader = BufReader::new(stdin);
        
        for line in reader.lines() {
            let line = line.map_err(|e| {
                crate::lsp::LspError::Protocol(format!("Failed to read line: {}", e))
            })?;
            
            if line.is_empty() {
                continue;
            }
            
            // Parse the LSP message
            let message: LspMessage = serde_json::from_str(&line).map_err(|e| {
                crate::lsp::LspError::Protocol(format!("Failed to parse JSON: {}", e))
            })?;
            
            // Handle the message
            let response = self.handle_message(message)?;
            
            // Send response if any
            if let Some(response) = response {
                let json = serde_json::to_string(&response).map_err(|e| {
                    crate::lsp::LspError::Protocol(format!("Failed to serialize response: {}", e))
                })?;
                
                let mut writer = stdout.lock();
                writeln!(writer, "Content-Length: {}\r\n\r\n{}", json.len(), json)
                    .map_err(|e| {
                        crate::lsp::LspError::Protocol(format!("Failed to write response: {}", e))
                    })?;
                writer.flush().map_err(|e| {
                    crate::lsp::LspError::Protocol(format!("Failed to flush output: {}", e))
                })?;
            }
        }
        
        Ok(())
    }
    
    /// Handle an LSP message
    fn handle_message(&self, message: LspMessage) -> LspResult<Option<Response>> {
        match message {
            LspMessage::Request(request) => self.handle_request(request),
            LspMessage::Notification(notification) => {
                self.handle_notification(notification)?;
                Ok(None)
            }
            LspMessage::Response(_) => {
                // We don't send requests, so we shouldn't receive responses
                log::warn!("Received unexpected response");
                Ok(None)
            }
        }
    }
    
    /// Handle an LSP request
    fn handle_request(&self, request: Request) -> LspResult<Option<Response>> {
        let state = self.state.lock().map_err(|e| {
            crate::lsp::LspError::Internal(format!("Failed to lock state: {}", e))
        })?;
        
        match request.method.as_str() {
            "initialize" => handlers::handle_initialize(request, &state),
            "textDocument/completion" => handlers::handle_completion(request, &state),
            "textDocument/hover" => handlers::handle_hover(request, &state),
            "textDocument/definition" => handlers::handle_definition(request, &state),
            "textDocument/references" => handlers::handle_references(request, &state),
            "shutdown" => handlers::handle_shutdown(request, &state),
            _ => {
                log::warn!("Unhandled request method: {}", request.method);
                Ok(Some(Response::error(
                    request.id,
                    crate::lsp::LspError::NotImplemented(format!(
                        "Method {} not implemented",
                        request.method
                    )),
                )))
            }
        }
    }
    
    /// Handle an LSP notification
    fn handle_notification(&self, notification: Notification) -> LspResult<()> {
        let mut state = self.state.lock().map_err(|e| {
            crate::lsp::LspError::Internal(format!("Failed to lock state: {}", e))
        })?;
        
        match notification.method.as_str() {
            "initialized" => handlers::handle_initialized(&mut state),
            "textDocument/didOpen" => handlers::handle_did_open(notification, &mut state),
            "textDocument/didChange" => handlers::handle_did_change(notification, &mut state),
            "textDocument/didClose" => handlers::handle_did_close(notification, &mut state),
            "exit" => handlers::handle_exit(&mut state),
            _ => {
                log::warn!("Unhandled notification method: {}", notification.method);
                Ok(())
            }
        }
    }
}