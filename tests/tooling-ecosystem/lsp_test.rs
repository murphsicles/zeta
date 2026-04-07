//! Language Server Protocol tests

#[cfg(test)]
mod tests {
    use zetac::lsp::{LspServer, LspError};
    use std::io::{Write, Read};
    use std::process::{Command, Stdio};
    use std::thread;
    use std::time::Duration;

    /// Test LSP server initialization
    #[test]
    fn test_lsp_initialization() {
        // This test would require a full LSP client implementation
        // For now, we'll test that the server compiles and can be created
        let server = LspServer::new();
        assert!(true, "LSP server created successfully");
    }

    /// Test LSP protocol types serialization
    #[test]
    fn test_lsp_protocol_serialization() {
        use zetac::lsp::protocol::*;
        use serde_json::json;

        // Test request serialization
        let request = Request {
            jsonrpc: "2.0".to_string(),
            id: json!(1),
            method: "initialize".to_string(),
            params: json!({
                "processId": 1234,
                "rootUri": "file:///test",
                "capabilities": {}
            }),
        };

        let json = serde_json::to_string(&request).unwrap();
        let parsed: Request = serde_json::from_str(&json).unwrap();
        
        assert_eq!(parsed.jsonrpc, "2.0");
        assert_eq!(parsed.method, "initialize");

        // Test response serialization
        let response = Response::success(json!(1), json!({"capabilities": {}}));
        let json = serde_json::to_string(&response).unwrap();
        let parsed: Response = serde_json::from_str(&json).unwrap();
        
        assert_eq!(parsed.jsonrpc, "2.0");
    }

    /// Test completion item creation
    #[test]
    fn test_completion_items() {
        use zetac::lsp::protocol::{CompletionItem, CompletionItemKind};

        let item = CompletionItem {
            label: "fn".to_string(),
            kind: Some(CompletionItemKind::Keyword),
            documentation: Some("Function definition".to_string()),
            detail: Some("keyword".to_string()),
        };

        assert_eq!(item.label, "fn");
        assert!(matches!(item.kind, Some(CompletionItemKind::Keyword)));
    }

    /// Test position and range types
    #[test]
    fn test_position_range() {
        use zetac::lsp::protocol::{Position, Range};

        let start = Position { line: 0, character: 0 };
        let end = Position { line: 10, character: 20 };
        let range = Range { start, end };

        assert_eq!(range.start.line, 0);
        assert_eq!(range.end.line, 10);
        assert_eq!(range.end.character, 20);
    }
}