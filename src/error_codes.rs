//! Zeta Error Codes Registry
//!
//! Structured error codes for LLM-assisted development and consistent diagnostics.
//! Format: EXXXX where:
//!   E = Error
//!   First digit = Category
//!   Next 3 digits = Specific error
//!
//! Categories:
//!   E1XXX - Parse Errors
//!   E2XXX - Type System Errors
//!   E3XXX - Semantic/Symbol Errors
//!   E4XXX - Code Generation Errors
//!   E5XXX - Runtime/Linking Errors
//!   E6XXX - Module/Import Errors
//!   E7XXX - Borrow Checker Errors
//!   E8XXX - Optimization Errors
//!   E9XXX - Tooling/Configuration Errors

use std::collections::HashMap;
use std::fmt;

/// Error code with description and category
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ErrorCode {
    pub code: String,
    pub category: ErrorCategory,
    pub description: String,
    pub example: Option<String>,
    pub suggestion: Option<String>,
}

/// Error categories matching Rust's general structure
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ErrorCategory {
    Parse,
    Type,
    Semantic,
    Codegen,
    Runtime,
    Module,
    Borrow,
    Optimization,
    Tooling,
}

impl fmt::Display for ErrorCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorCategory::Parse => write!(f, "parse"),
            ErrorCategory::Type => write!(f, "type"),
            ErrorCategory::Semantic => write!(f, "semantic"),
            ErrorCategory::Codegen => write!(f, "codegen"),
            ErrorCategory::Runtime => write!(f, "runtime"),
            ErrorCategory::Module => write!(f, "module"),
            ErrorCategory::Borrow => write!(f, "borrow"),
            ErrorCategory::Optimization => write!(f, "optimization"),
            ErrorCategory::Tooling => write!(f, "tooling"),
        }
    }
}

/// Registry of all Zeta error codes
pub struct ErrorCodeRegistry {
    codes: HashMap<String, ErrorCode>,
}

impl Default for ErrorCodeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl ErrorCodeRegistry {
    /// Create new registry with all defined error codes
    pub fn new() -> Self {
        let mut codes = HashMap::new();

        // E1XXX - Parse Errors
        codes.insert(
            "E1001".to_string(),
            ErrorCode {
                code: "E1001".to_string(),
                category: ErrorCategory::Parse,
                description: "Unexpected token".to_string(),
                example: Some("Expected `;` but found `}`".to_string()),
                suggestion: Some(
                    "Check for missing semicolon or incorrect brace placement".to_string(),
                ),
            },
        );

        codes.insert(
            "E1002".to_string(),
            ErrorCode {
                code: "E1002".to_string(),
                category: ErrorCategory::Parse,
                description: "Unterminated string literal".to_string(),
                example: Some("`let s = \"hello world;`".to_string()),
                suggestion: Some("Add closing quote: `let s = \"hello world\";`".to_string()),
            },
        );

        codes.insert(
            "E1003".to_string(),
            ErrorCode {
                code: "E1003".to_string(),
                category: ErrorCategory::Parse,
                description: "Missing closing brace".to_string(),
                example: Some("`fn foo() {` without closing `}`".to_string()),
                suggestion: Some("Add closing brace `}`".to_string()),
            },
        );

        // E2XXX - Type System Errors
        codes.insert(
            "E2001".to_string(),
            ErrorCode {
                code: "E2001".to_string(),
                category: ErrorCategory::Type,
                description: "Type mismatch".to_string(),
                example: Some("Expected `i32` but found `i64`".to_string()),
                suggestion: Some("Add type cast or change variable type".to_string()),
            },
        );

        codes.insert(
            "E2002".to_string(),
            ErrorCode {
                code: "E2002".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined type".to_string(),
                example: Some("`let x: UnknownType = 10;`".to_string()),
                suggestion: Some("Define the type or import it from a module".to_string()),
            },
        );

        codes.insert(
            "E2003".to_string(),
            ErrorCode {
                code: "E2003".to_string(),
                category: ErrorCategory::Type,
                description: "Cannot infer type".to_string(),
                example: Some("`let x = ;` (missing initializer)".to_string()),
                suggestion: Some("Add type annotation or initial value".to_string()),
            },
        );

        codes.insert(
            "E2004".to_string(),
            ErrorCode {
                code: "E2004".to_string(),
                category: ErrorCategory::Type,
                description: "Type arguments mismatch".to_string(),
                example: Some(
                    "`Vec::<i32>::new()` with wrong number of type arguments".to_string(),
                ),
                suggestion: Some("Check number and order of type arguments".to_string()),
            },
        );

        codes.insert(
            "E2005".to_string(),
            ErrorCode {
                code: "E2005".to_string(),
                category: ErrorCategory::Type,
                description: "Method not found for type".to_string(),
                example: Some("`x.foo()` where `foo` is not defined for type of `x`".to_string()),
                suggestion: Some(
                    "Check method name or implement the method for the type".to_string(),
                ),
            },
        );

        // E3XXX - Semantic/Symbol Errors
        codes.insert(
            "E3001".to_string(),
            ErrorCode {
                code: "E3001".to_string(),
                category: ErrorCategory::Semantic,
                description: "Undefined variable".to_string(),
                example: Some("`println!(x)` where `x` is not defined".to_string()),
                suggestion: Some("Define the variable or check for typos".to_string()),
            },
        );

        codes.insert(
            "E3002".to_string(),
            ErrorCode {
                code: "E3002".to_string(),
                category: ErrorCategory::Semantic,
                description: "Duplicate definition".to_string(),
                example: Some("`fn foo() {}` defined twice".to_string()),
                suggestion: Some("Remove duplicate definition or rename".to_string()),
            },
        );

        // E4XXX - Code Generation Errors
        codes.insert(
            "E4001".to_string(),
            ErrorCode {
                code: "E4001".to_string(),
                category: ErrorCategory::Codegen,
                description: "Missing function implementation".to_string(),
                example: Some("`Point::new()` referenced but not defined".to_string()),
                suggestion: Some("Implement the function or check for typos".to_string()),
            },
        );

        codes.insert(
            "E4002".to_string(),
            ErrorCode {
                code: "E4002".to_string(),
                category: ErrorCategory::Codegen,
                description: "LLVM IR generation failed".to_string(),
                example: Some("Internal compiler error during code generation".to_string()),
                suggestion: Some("Report as bug with minimal reproduction".to_string()),
            },
        );

        // E5XXX - Runtime/Linking Errors
        codes.insert(
            "E5001".to_string(),
            ErrorCode {
                code: "E5001".to_string(),
                category: ErrorCategory::Runtime,
                description: "Division by zero".to_string(),
                example: Some("`let x = 10 / 0;`".to_string()),
                suggestion: Some("Add zero check before division".to_string()),
            },
        );

        codes.insert(
            "E5002".to_string(),
            ErrorCode {
                code: "E5002".to_string(),
                category: ErrorCategory::Runtime,
                description: "Array index out of bounds".to_string(),
                example: Some("`arr[10]` where array has only 5 elements".to_string()),
                suggestion: Some("Check array bounds before access".to_string()),
            },
        );

        // E6XXX - Module/Import Errors
        codes.insert(
            "E6001".to_string(),
            ErrorCode {
                code: "E6001".to_string(),
                category: ErrorCategory::Module,
                description: "Module not found".to_string(),
                example: Some("`use nonexistent::module;`".to_string()),
                suggestion: Some("Check module path or create the module".to_string()),
            },
        );

        codes.insert(
            "E6002".to_string(),
            ErrorCode {
                code: "E6002".to_string(),
                category: ErrorCategory::Module,
                description: "Import conflict".to_string(),
                example: Some("`use foo::bar; use baz::bar;`".to_string()),
                suggestion: Some("Rename imports or use qualified names".to_string()),
            },
        );

        // E7XXX - Borrow Checker Errors (placeholder for future)
        codes.insert(
            "E7001".to_string(),
            ErrorCode {
                code: "E7001".to_string(),
                category: ErrorCategory::Borrow,
                description: "Borrow checker violation".to_string(),
                example: Some("Multiple mutable borrows of same variable".to_string()),
                suggestion: Some("Restructure code to follow borrowing rules".to_string()),
            },
        );

        // E8XXX - Optimization Errors (placeholder for future)
        codes.insert(
            "E8001".to_string(),
            ErrorCode {
                code: "E8001".to_string(),
                category: ErrorCategory::Optimization,
                description: "Optimization failed".to_string(),
                example: Some("Internal compiler error during optimization".to_string()),
                suggestion: Some("Report as bug with minimal reproduction".to_string()),
            },
        );

        // E9XXX - Tooling/Configuration Errors
        codes.insert(
            "E9001".to_string(),
            ErrorCode {
                code: "E9001".to_string(),
                category: ErrorCategory::Tooling,
                description: "Invalid compiler flag".to_string(),
                example: Some("`--invalid-flag` passed to compiler".to_string()),
                suggestion: Some("Check compiler documentation for valid flags".to_string()),
            },
        );

        codes.insert(
            "E9002".to_string(),
            ErrorCode {
                code: "E9002".to_string(),
                category: ErrorCategory::Tooling,
                description: "File not found".to_string(),
                example: Some("`zetac nonexistent.z`".to_string()),
                suggestion: Some("Check file path and permissions".to_string()),
            },
        );

        Self { codes }
    }

    /// Get error code by code string
    pub fn get(&self, code: &str) -> Option<&ErrorCode> {
        self.codes.get(code)
    }

    /// Get all error codes
    pub fn all_codes(&self) -> Vec<&ErrorCode> {
        self.codes.values().collect()
    }

    /// Get error codes by category
    pub fn by_category(&self, category: ErrorCategory) -> Vec<&ErrorCode> {
        self.codes
            .values()
            .filter(|ec| ec.category == category)
            .collect()
    }

    /// Format error message with code
    pub fn format_error(&self, code: &str, context: &str) -> String {
        if let Some(error_code) = self.get(code) {
            format!(
                "error[{}]: {} ({})\nContext: {}\n{}",
                error_code.code,
                error_code.description,
                error_code.category,
                context,
                error_code.suggestion.as_deref().unwrap_or("")
            )
        } else {
            format!("error[{}]: Unknown error code\nContext: {}", code, context)
        }
    }
}

lazy_static::lazy_static! {
    pub static ref ERROR_CODES: ErrorCodeRegistry = ErrorCodeRegistry::new();
}

/// Helper function to format error with code
pub fn format_with_code(code: &str, context: &str) -> String {
    ERROR_CODES.format_error(code, context)
}

/// Diagnostic struct for error reporting (simplified Rust version)
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub code: String,
    pub message: String,
    pub span: Option<(usize, usize)>, // (line, column) - simplified
    pub help: Option<String>,
    pub note: Option<String>,
}

impl Diagnostic {
    pub fn new(code: &str, message: String) -> Self {
        Self {
            code: code.to_string(),
            message,
            span: None,
            help: None,
            note: None,
        }
    }

    pub fn with_span(mut self, line: usize, column: usize) -> Self {
        self.span = Some((line, column));
        self
    }

    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }

    pub fn format(&self) -> String {
        let mut output = format!("error[{}]: {}", self.code, self.message);

        if let Some((line, column)) = self.span {
            output.push_str(&format!("\n  --> at line {}, column {}", line, column));
        }

        if let Some(help) = &self.help {
            output.push_str(&format!("\n  help: {}", help));
        }

        if let Some(note) = &self.note {
            output.push_str(&format!("\n  note: {}", note));
        }

        output
    }
}

/// Common error codes for easy reference
pub mod common {
    // Parse errors
    pub const UNEXPECTED_TOKEN: &str = "E1001";
    pub const UNTERMINATED_STRING: &str = "E1002";
    pub const MISSING_CLOSING_BRACE: &str = "E1003";

    // Type errors
    pub const TYPE_MISMATCH: &str = "E2001";
    pub const UNDEFINED_TYPE: &str = "E2002";
    pub const CANNOT_INFER_TYPE: &str = "E2003";
    pub const TYPE_ARGS_MISMATCH: &str = "E2004";
    pub const METHOD_NOT_FOUND: &str = "E2005";

    // Semantic errors
    pub const UNDEFINED_VARIABLE: &str = "E3001";
    pub const DUPLICATE_DEFINITION: &str = "E3002";

    // Codegen errors
    pub const MISSING_FUNCTION: &str = "E4001";
    pub const LLVM_GENERATION_FAILED: &str = "E4002";

    // Runtime errors
    pub const DIVISION_BY_ZERO: &str = "E5001";
    pub const ARRAY_INDEX_OOB: &str = "E5002";

    // Module errors
    pub const MODULE_NOT_FOUND: &str = "E6001";
    pub const IMPORT_CONFLICT: &str = "E6002";

    // Tooling errors
    pub const INVALID_COMPILER_FLAG: &str = "E9001";
    pub const FILE_NOT_FOUND: &str = "E9002";
}
