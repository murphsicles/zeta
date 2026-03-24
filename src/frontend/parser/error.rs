//! Error reporting utilities for the Zeta parser

use nom::error::{ErrorKind, ParseError};

/// Calculate line and column number from position in source
pub fn line_col(input: &str, position: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    
    for (i, c) in input.char_indices() {
        if i >= position {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    
    (line, col)
}

/// Get a snippet of source code around a position
pub fn get_context_snippet(input: &str, position: usize, context_lines: usize) -> String {
    let lines: Vec<&str> = input.lines().collect();
    let (line_num, _) = line_col(input, position);
    
    let start_line = line_num.saturating_sub(context_lines);
    let end_line = (line_num + context_lines).min(lines.len());
    
    let mut result = String::new();
    for i in start_line..end_line {
        let line_content = lines.get(i - 1).unwrap_or(&"");
        result.push_str(&format!("{:4}: {}\n", i, line_content));
        
        // Add caret indicator for the error line
        if i == line_num {
            let (_, col) = line_col(input, position);
            // Adjust for 1-based column and line number display
            let indent = format!("{:4}: ", i).len();
            result.push_str(&" ".repeat(indent + col - 1));
            result.push_str("^\n");
        }
    }
    
    result
}

/// Common error messages for Zeta parser
pub mod messages {
    pub const UNEXPECTED_TOKEN: &str = "Unexpected token";
    pub const EXPECTED_IDENTIFIER: &str = "Expected identifier";
    pub const EXPECTED_EXPRESSION: &str = "Expected expression";
    pub const EXPECTED_TYPE: &str = "Expected type";
    pub const UNCLOSED_STRING: &str = "Unclosed string literal";
    pub const UNCLOSED_COMMENT: &str = "Unclosed block comment";
    pub const INVALID_NUMBER: &str = "Invalid numeric literal";
    pub const MISSING_SEMICOLON: &str = "Missing semicolon";
    pub const UNEXPECTED_EOF: &str = "Unexpected end of input";
    
    /// Get suggestion for common errors
    pub fn get_suggestion(error: &str, context: &str) -> Option<&'static str> {
        match error {
            UNCLOSED_STRING => Some("Did you forget a closing quote?"),
            UNCLOSED_COMMENT => Some("Did you forget to close the comment with '*/'?"),
            EXPECTED_IDENTIFIER if context.contains("let") => Some("Variable names must start with a letter or underscore"),
            EXPECTED_EXPRESSION if context.contains("=") => Some("Right side of assignment must be an expression"),
            _ => None,
        }
    }
}

/// Enhanced error type for Zeta parser
#[derive(Debug, Clone)]
pub struct ZetaError<'a> {
    pub input: &'a str,
    pub kind: ErrorKind,
    pub message: Option<String>,
    pub suggestion: Option<String>,
}

impl<'a> ParseError<&'a str> for ZetaError<'a> {
    fn from_error_kind(input: &'a str, kind: ErrorKind) -> Self {
        ZetaError {
            input,
            kind,
            message: None,
            suggestion: None,
        }
    }

    fn append(_input: &'a str, _kind: ErrorKind, other: Self) -> Self {
        other
    }
}

impl<'a> ZetaError<'a> {
    /// Create a new error with a custom message
    pub fn with_message(mut self, message: &str) -> Self {
        self.message = Some(message.to_string());
        self
    }
    
    /// Add a suggestion to the error
    pub fn with_suggestion(mut self, suggestion: &str) -> Self {
        self.suggestion = Some(suggestion.to_string());
        self
    }
    
    /// Format the error with line/column information
    pub fn format(&self) -> String {
        let (line, col) = line_col(self.input, 0);
        let position = self.input.len();
        let remaining_input = &self.input[..position.min(self.input.len())];
        
        let mut result = format!("Error at line {}, column {}:\n", line, col);
        
        // Add context snippet
        let context = get_context_snippet(self.input, 0, 2);
        if !context.is_empty() {
            result.push_str("\n");
            result.push_str(&context);
            result.push_str("\n");
        }
        
        // Add error kind
        result.push_str(&format!("Error: {:?}", self.kind));
        
        // Add custom message if available
        if let Some(msg) = &self.message {
            result.push_str(&format!(" - {}", msg));
        }
        
        // Add suggestion if available
        if let Some(sugg) = &self.suggestion {
            result.push_str(&format!("\nSuggestion: {}", sugg));
        }
        
        // Show what was parsed successfully
        if !remaining_input.is_empty() {
            let parsed_len = 20.min(remaining_input.len());
            let parsed = &remaining_input[..parsed_len];
            result.push_str(&format!("\nParsed: {:?}", parsed));
            if parsed_len < remaining_input.len() {
                result.push_str("...");
            }
        }
        
        result
    }
}

/// Helper to create a Zeta error
pub fn make_error<'a>(input: &'a str, kind: ErrorKind, message: &str) -> nom::Err<ZetaError<'a>> {
    nom::Err::Error(ZetaError::from_error_kind(input, kind).with_message(message))
}

/// Helper to create a Zeta error with suggestion
pub fn make_error_with_suggestion<'a>(input: &'a str, kind: ErrorKind, message: &str, suggestion: &str) -> nom::Err<ZetaError<'a>> {
    nom::Err::Error(
        ZetaError::from_error_kind(input, kind)
            .with_message(message)
            .with_suggestion(suggestion)
    )
}