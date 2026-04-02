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

// Re-export diagnostics for backward compatibility
pub use crate::diagnostics::{Diagnostic, Severity, SourceLocation, SourceSpan};

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

        // W1XXX - Parse Warnings
        codes.insert(
            "W1001".to_string(),
            ErrorCode {
                code: "W1001".to_string(),
                category: ErrorCategory::Parse,
                description: "Unused import".to_string(),
                example: Some("`use std::collections::HashMap;` never used".to_string()),
                suggestion: Some("Remove unused import or use the imported item".to_string()),
            },
        );

        codes.insert(
            "W1002".to_string(),
            ErrorCode {
                code: "W1002".to_string(),
                category: ErrorCategory::Parse,
                description: "Unnecessary parentheses".to_string(),
                example: Some("`let x = (10 + 5);`".to_string()),
                suggestion: Some("Remove unnecessary parentheses".to_string()),
            },
        );

        // W2XXX - Type Warnings
        codes.insert(
            "W2001".to_string(),
            ErrorCode {
                code: "W2001".to_string(),
                category: ErrorCategory::Type,
                description: "Type could be inferred".to_string(),
                example: Some("`let x: i32 = 10;` where type could be inferred".to_string()),
                suggestion: Some("Remove explicit type annotation".to_string()),
            },
        );

        codes.insert(
            "W2002".to_string(),
            ErrorCode {
                code: "W2002".to_string(),
                category: ErrorCategory::Type,
                description: "Redundant type cast".to_string(),
                example: Some("`let x = 10 as i32;` where 10 is already i32".to_string()),
                suggestion: Some("Remove redundant type cast".to_string()),
            },
        );

        // W3XXX - Semantic Warnings
        codes.insert(
            "W3001".to_string(),
            ErrorCode {
                code: "W3001".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unused variable".to_string(),
                example: Some("`let x = 10;` never used".to_string()),
                suggestion: Some("Use the variable or prefix with underscore".to_string()),
            },
        );

        codes.insert(
            "W3002".to_string(),
            ErrorCode {
                code: "W3002".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unused function parameter".to_string(),
                example: Some("`fn foo(x: i32) {}` where x is never used".to_string()),
                suggestion: Some("Use the parameter or prefix with underscore".to_string()),
            },
        );

        codes.insert(
            "W3003".to_string(),
            ErrorCode {
                code: "W3003".to_string(),
                category: ErrorCategory::Semantic,
                description: "Dead code".to_string(),
                example: Some("Code after `return` statement".to_string()),
                suggestion: Some("Remove dead code or restructure logic".to_string()),
            },
        );

        codes.insert(
            "W3004".to_string(),
            ErrorCode {
                code: "W3004".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unreachable pattern".to_string(),
                example: Some("`match x { 1 => ..., 2 => ..., 1 => ... }`".to_string()),
                suggestion: Some("Remove unreachable pattern".to_string()),
            },
        );

        // W4XXX - Code Generation Warnings
        codes.insert(
            "W4001".to_string(),
            ErrorCode {
                code: "W4001".to_string(),
                category: ErrorCategory::Codegen,
                description: "Inefficient code pattern".to_string(),
                example: Some("`String::new() + \"text\"` instead of `\"text\".to_string()`".to_string()),
                suggestion: Some("Use more efficient alternative".to_string()),
            },
        );

        // W5XXX - Runtime Warnings
        codes.insert(
            "W5001".to_string(),
            ErrorCode {
                code: "W5001".to_string(),
                category: ErrorCategory::Runtime,
                description: "Possible division by zero".to_string(),
                example: Some("`x / y` where y might be zero".to_string()),
                suggestion: Some("Add zero check before division".to_string()),
            },
        );

        codes.insert(
            "W5002".to_string(),
            ErrorCode {
                code: "W5002".to_string(),
                category: ErrorCategory::Runtime,
                description: "Possible array index out of bounds".to_string(),
                example: Some("`arr[i]` where i might be >= arr.len()".to_string()),
                suggestion: Some("Add bounds check before access".to_string()),
            },
        );

        // W8XXX - Optimization Warnings
        codes.insert(
            "W8001".to_string(),
            ErrorCode {
                code: "W8001".to_string(),
                category: ErrorCategory::Optimization,
                description: "Missed optimization opportunity".to_string(),
                example: Some("`x * 2` could be `x << 1`".to_string()),
                suggestion: Some("Use more efficient operation".to_string()),
            },
        );

        codes.insert(
            "W8002".to_string(),
            ErrorCode {
                code: "W8002".to_string(),
                category: ErrorCategory::Optimization,
                description: "Inefficient loop".to_string(),
                example: Some("Loop with expensive operation in invariant position".to_string()),
                suggestion: Some("Move invariant computation outside loop".to_string()),
            },
        );

        // W9XXX - Tooling Warnings
        codes.insert(
            "W9001".to_string(),
            ErrorCode {
                code: "W9001".to_string(),
                category: ErrorCategory::Tooling,
                description: "Deprecated feature used".to_string(),
                example: Some("Using deprecated function or syntax".to_string()),
                suggestion: Some("Use recommended alternative".to_string()),
            },
        );

        codes.insert(
            "W9002".to_string(),
            ErrorCode {
                code: "W9002".to_string(),
                category: ErrorCategory::Tooling,
                description: "Missing documentation".to_string(),
                example: Some("Public function without doc comment".to_string()),
                suggestion: Some("Add documentation comment".to_string()),
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

// Note: The main Diagnostic struct is now in the diagnostics module
// This provides backward compatibility wrappers

/// Create a diagnostic from an error code
pub fn diagnostic_from_code(code: &str, message: String, span: Option<SourceSpan>) -> Diagnostic {
    let mut diag = Diagnostic::error(code, message);
    if let Some(span) = span {
        diag = diag.with_span(span);
    }

    // Add suggestions from error code registry if available
    if let Some(error_code) = ERROR_CODES.get(code) {
        if let Some(suggestion) = &error_code.suggestion {
            diag = diag.with_suggestion(suggestion.clone());
        }
    }

    diag
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

    // Warning codes
    pub const UNUSED_IMPORT: &str = "W1001";
    pub const UNNECESSARY_PARENTHESES: &str = "W1002";
    pub const TYPE_COULD_BE_INFERRED: &str = "W2001";
    pub const REDUNDANT_TYPE_CAST: &str = "W2002";
    pub const UNUSED_VARIABLE: &str = "W3001";
    pub const UNUSED_PARAMETER: &str = "W3002";
    pub const DEAD_CODE: &str = "W3003";
    pub const UNREACHABLE_PATTERN: &str = "W3004";
    pub const INEFFICIENT_CODE_PATTERN: &str = "W4001";
    pub const POSSIBLE_DIVISION_BY_ZERO: &str = "W5001";
    pub const POSSIBLE_ARRAY_INDEX_OOB: &str = "W5002";
    pub const MISSED_OPTIMIZATION: &str = "W8001";
    pub const INEFFICIENT_LOOP: &str = "W8002";
    pub const DEPRECATED_FEATURE: &str = "W9001";
    pub const MISSING_DOCUMENTATION: &str = "W9002";
}
