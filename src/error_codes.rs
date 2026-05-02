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
        codes.insert(
            "E1001".to_string(),
            ErrorCode {
                code: "E1001".to_string(),
                category: ErrorCategory::Parse,
                description: "General parse error".to_string(),
                example: None,
                suggestion: Some("Check for syntax errors near the reported location.".to_string()),
            },
        );

        codes.insert(
            "E1002".to_string(),
            ErrorCode {
                code: "E1002".to_string(),
                category: ErrorCategory::Parse,
                description: "Unexpected token".to_string(),
                example: None,
                suggestion: Some(
                    "Check for missing delimiters or incorrect operators.".to_string(),
                ),
            },
        );

        codes.insert(
            "E1003".to_string(),
            ErrorCode {
                code: "E1003".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected identifier".to_string(),
                example: None,
                suggestion: Some("Provide a valid name for the declaration.".to_string()),
            },
        );

        codes.insert(
            "E1004".to_string(),
            ErrorCode {
                code: "E1004".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected type".to_string(),
                example: None,
                suggestion: Some("Add a type after `:` or in angle brackets.".to_string()),
            },
        );

        codes.insert(
            "E1005".to_string(),
            ErrorCode {
                code: "E1005".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected expression".to_string(),
                example: None,
                suggestion: Some("Provide a value or expression.".to_string()),
            },
        );

        codes.insert(
            "E1006".to_string(),
            ErrorCode {
                code: "E1006".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected semicolon".to_string(),
                example: None,
                suggestion: Some("Add `;` at the end of the statement.".to_string()),
            },
        );

        codes.insert(
            "E1007".to_string(),
            ErrorCode {
                code: "E1007".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected `{`".to_string(),
                example: None,
                suggestion: Some("Add `{` to begin the block.".to_string()),
            },
        );

        codes.insert(
            "E1008".to_string(),
            ErrorCode {
                code: "E1008".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected `(` or `<`".to_string(),
                example: None,
                suggestion: Some("Add `(...)` or `::<...>` after the function name.".to_string()),
            },
        );

        codes.insert(
            "E1009".to_string(),
            ErrorCode {
                code: "E1009".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected `)`".to_string(),
                example: None,
                suggestion: Some("Add closing `)` to match the opening `(`.".to_string()),
            },
        );

        codes.insert(
            "E1010".to_string(),
            ErrorCode {
                code: "E1010".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected `}`".to_string(),
                example: None,
                suggestion: Some("Add closing `}` to match the opening `{`.".to_string()),
            },
        );

        codes.insert(
            "E1011".to_string(),
            ErrorCode {
                code: "E1011".to_string(),
                category: ErrorCategory::Parse,
                description: "Expected `]`".to_string(),
                example: None,
                suggestion: Some("Add closing `]` to match the opening `[`.".to_string()),
            },
        );

        codes.insert(
            "E1012".to_string(),
            ErrorCode {
                code: "E1012".to_string(),
                category: ErrorCategory::Parse,
                description: "Unmatched delimiter".to_string(),
                example: None,
                suggestion: Some("Check brace/bracket/paren nesting.".to_string()),
            },
        );

        codes.insert(
            "E1013".to_string(),
            ErrorCode {
                code: "E1013".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid integer literal".to_string(),
                example: None,
                suggestion: Some("Use a valid integer within range.".to_string()),
            },
        );

        codes.insert(
            "E1014".to_string(),
            ErrorCode {
                code: "E1014".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid float literal".to_string(),
                example: None,
                suggestion: Some("Use valid float syntax (e.g. `3.14` or `1e5`).".to_string()),
            },
        );

        codes.insert(
            "E1015".to_string(),
            ErrorCode {
                code: "E1015".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid escape sequence".to_string(),
                example: None,
                suggestion: Some("Valid escapes: `\n`, `\t`, `\r`, `\\`, `\"`.".to_string()),
            },
        );

        codes.insert(
            "E1016".to_string(),
            ErrorCode {
                code: "E1016".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid string literal".to_string(),
                example: None,
                suggestion: Some("Add closing quote to the string.".to_string()),
            },
        );

        codes.insert(
            "E1017".to_string(),
            ErrorCode {
                code: "E1017".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid character literal".to_string(),
                example: None,
                suggestion: Some("Use single quotes with one character: `'a'`.".to_string()),
            },
        );

        codes.insert(
            "E1018".to_string(),
            ErrorCode {
                code: "E1018".to_string(),
                category: ErrorCategory::Parse,
                description: "Unknown operator".to_string(),
                example: None,
                suggestion: Some(
                    "Use a valid operator: `+`, `-`, `*`, `/`, `==`, etc.".to_string(),
                ),
            },
        );

        codes.insert(
            "E1019".to_string(),
            ErrorCode {
                code: "E1019".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid attribute syntax".to_string(),
                example: None,
                suggestion: Some("Use `#[attribute]` or `#![attribute]` syntax.".to_string()),
            },
        );

        codes.insert(
            "E1020".to_string(),
            ErrorCode {
                code: "E1020".to_string(),
                category: ErrorCategory::Parse,
                description: "Unknown attribute".to_string(),
                example: None,
                suggestion: Some("Check attribute name for typos.".to_string()),
            },
        );

        codes.insert(
            "E1021".to_string(),
            ErrorCode {
                code: "E1021".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid pattern".to_string(),
                example: None,
                suggestion: Some(
                    "Use valid pattern syntax: literal, variable, wildcard `_`, or struct pattern."
                        .to_string(),
                ),
            },
        );

        codes.insert(
            "E1022".to_string(),
            ErrorCode {
                code: "E1022".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid where clause".to_string(),
                example: None,
                suggestion: Some("Add trait bounds after the colon: `where T: Clone`.".to_string()),
            },
        );

        codes.insert(
            "E1023".to_string(),
            ErrorCode {
                code: "E1023".to_string(),
                category: ErrorCategory::Parse,
                description: "Invalid generic syntax".to_string(),
                example: None,
                suggestion: Some("Use `<T, U>` syntax.".to_string()),
            },
        );

        codes.insert(
            "E1024".to_string(),
            ErrorCode {
                code: "E1024".to_string(),
                category: ErrorCategory::Parse,
                description: "Reserved keyword as identifier".to_string(),
                example: None,
                suggestion: Some("Rename to a non-keyword identifier.".to_string()),
            },
        );

        codes.insert(
            "E1025".to_string(),
            ErrorCode {
                code: "E1025".to_string(),
                category: ErrorCategory::Parse,
                description: "Doc comment on invalid item".to_string(),
                example: None,
                suggestion: Some("Move the doc comment to a valid item or use `//`.".to_string()),
            },
        );

        codes.insert(
            "E2001".to_string(),
            ErrorCode {
                code: "E2001".to_string(),
                category: ErrorCategory::Type,
                description: "Typecheck failed".to_string(),
                example: None,
                suggestion: Some(
                    "Check the types of all expressions in the reported area.".to_string(),
                ),
            },
        );

        codes.insert(
            "E2002".to_string(),
            ErrorCode {
                code: "E2002".to_string(),
                category: ErrorCategory::Type,
                description: "Type mismatch".to_string(),
                example: None,
                suggestion: Some("Convert between types using `as` or a constructor.".to_string()),
            },
        );

        codes.insert(
            "E2003".to_string(),
            ErrorCode {
                code: "E2003".to_string(),
                category: ErrorCategory::Type,
                description: "Const evaluation error".to_string(),
                example: None,
                suggestion: Some(
                    "Simplify the expression or use only const-compatible operations.".to_string(),
                ),
            },
        );

        codes.insert(
            "E2004".to_string(),
            ErrorCode {
                code: "E2004".to_string(),
                category: ErrorCategory::Type,
                description: "Cannot infer type".to_string(),
                example: None,
                suggestion: Some("Add an explicit type annotation.".to_string()),
            },
        );

        codes.insert(
            "E2005".to_string(),
            ErrorCode {
                code: "E2005".to_string(),
                category: ErrorCategory::Type,
                description: "Missing type annotation".to_string(),
                example: None,
                suggestion: Some("Add `: TypeName` after the variable or parameter.".to_string()),
            },
        );

        codes.insert(
            "E2006".to_string(),
            ErrorCode {
                code: "E2006".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined function".to_string(),
                example: None,
                suggestion: Some("Check the function name for typos or define it.".to_string()),
            },
        );

        codes.insert(
            "E2007".to_string(),
            ErrorCode {
                code: "E2007".to_string(),
                category: ErrorCategory::Type,
                description: "Wrong argument count".to_string(),
                example: None,
                suggestion: Some("Check function signature for expected parameters.".to_string()),
            },
        );

        codes.insert(
            "E2008".to_string(),
            ErrorCode {
                code: "E2008".to_string(),
                category: ErrorCategory::Type,
                description: "Argument type mismatch".to_string(),
                example: None,
                suggestion: Some("Pass a value of the correct type.".to_string()),
            },
        );

        codes.insert(
            "E2009".to_string(),
            ErrorCode {
                code: "E2009".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined variable".to_string(),
                example: None,
                suggestion: Some("Check spelling or declare the variable.".to_string()),
            },
        );

        codes.insert(
            "E2010".to_string(),
            ErrorCode {
                code: "E2010".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined type".to_string(),
                example: None,
                suggestion: Some("Define the type or import it from a module.".to_string()),
            },
        );

        codes.insert(
            "E2011".to_string(),
            ErrorCode {
                code: "E2011".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined concept".to_string(),
                example: None,
                suggestion: Some("Define the concept or check for typos.".to_string()),
            },
        );

        codes.insert(
            "E2012".to_string(),
            ErrorCode {
                code: "E2012".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined field".to_string(),
                example: None,
                suggestion: Some("Check field name spelling.".to_string()),
            },
        );

        codes.insert(
            "E2013".to_string(),
            ErrorCode {
                code: "E2013".to_string(),
                category: ErrorCategory::Type,
                description: "Undefined method".to_string(),
                example: None,
                suggestion: Some("Implement the method or check the method name.".to_string()),
            },
        );

        codes.insert(
            "E2014".to_string(),
            ErrorCode {
                code: "E2014".to_string(),
                category: ErrorCategory::Type,
                description: "Cannot infer return type".to_string(),
                example: None,
                suggestion: Some("Add explicit return type annotation.".to_string()),
            },
        );

        codes.insert(
            "E2015".to_string(),
            ErrorCode {
                code: "E2015".to_string(),
                category: ErrorCategory::Type,
                description: "Return type mismatch".to_string(),
                example: None,
                suggestion: Some("Return a value of the correct type.".to_string()),
            },
        );

        codes.insert(
            "E2016".to_string(),
            ErrorCode {
                code: "E2016".to_string(),
                category: ErrorCategory::Type,
                description: "Concept not satisfied".to_string(),
                example: None,
                suggestion: Some("Implement the concept for this type.".to_string()),
            },
        );

        codes.insert(
            "E2017".to_string(),
            ErrorCode {
                code: "E2017".to_string(),
                category: ErrorCategory::Type,
                description: "Missing concept implementation".to_string(),
                example: None,
                suggestion: Some("Implement all required methods for the concept.".to_string()),
            },
        );

        codes.insert(
            "E2018".to_string(),
            ErrorCode {
                code: "E2018".to_string(),
                category: ErrorCategory::Type,
                description: "Duplicate concept implementation".to_string(),
                example: None,
                suggestion: Some("Remove the duplicate implementation.".to_string()),
            },
        );

        codes.insert(
            "E2019".to_string(),
            ErrorCode {
                code: "E2019".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid operator for type".to_string(),
                example: None,
                suggestion: Some("Check if the types support this operation.".to_string()),
            },
        );

        codes.insert(
            "E2020".to_string(),
            ErrorCode {
                code: "E2020".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid unary operator".to_string(),
                example: None,
                suggestion: Some("Check if the type supports this unary operation.".to_string()),
            },
        );

        codes.insert(
            "E2021".to_string(),
            ErrorCode {
                code: "E2021".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid comparison".to_string(),
                example: None,
                suggestion: Some("Check if the types implement comparison.".to_string()),
            },
        );

        codes.insert(
            "E2022".to_string(),
            ErrorCode {
                code: "E2022".to_string(),
                category: ErrorCategory::Type,
                description: "Recursive type".to_string(),
                example: None,
                suggestion: Some("Use indirection (Box/pointer) for recursive types.".to_string()),
            },
        );

        codes.insert(
            "E2023".to_string(),
            ErrorCode {
                code: "E2023".to_string(),
                category: ErrorCategory::Type,
                description: "Recursive type alias".to_string(),
                example: None,
                suggestion: Some("Break the alias cycle with indirection.".to_string()),
            },
        );

        codes.insert(
            "E2024".to_string(),
            ErrorCode {
                code: "E2024".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid generic parameter count".to_string(),
                example: None,
                suggestion: Some("Check the type definition for expected parameters.".to_string()),
            },
        );

        codes.insert(
            "E2025".to_string(),
            ErrorCode {
                code: "E2025".to_string(),
                category: ErrorCategory::Type,
                description: "Generic parameter kind mismatch".to_string(),
                example: None,
                suggestion: Some("Check the definition of the generic parameter.".to_string()),
            },
        );

        codes.insert(
            "E2026".to_string(),
            ErrorCode {
                code: "E2026".to_string(),
                category: ErrorCategory::Type,
                description: "Generic bound not satisfied".to_string(),
                example: None,
                suggestion: Some("Use a type that implements the required concepts.".to_string()),
            },
        );

        codes.insert(
            "E2027".to_string(),
            ErrorCode {
                code: "E2027".to_string(),
                category: ErrorCategory::Type,
                description: "Cycle in generic resolution".to_string(),
                example: None,
                suggestion: Some("Simplify the generic constraints.".to_string()),
            },
        );

        codes.insert(
            "E2028".to_string(),
            ErrorCode {
                code: "E2028".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid enum variant".to_string(),
                example: None,
                suggestion: Some("Check variant name spelling.".to_string()),
            },
        );

        codes.insert(
            "E2029".to_string(),
            ErrorCode {
                code: "E2029".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid struct constructor".to_string(),
                example: None,
                suggestion: Some(
                    "Use the struct literal syntax `Name { field: val }`.".to_string(),
                ),
            },
        );

        codes.insert(
            "E2030".to_string(),
            ErrorCode {
                code: "E2030".to_string(),
                category: ErrorCategory::Type,
                description: "Mismatched struct fields".to_string(),
                example: None,
                suggestion: Some("Check field names and types.".to_string()),
            },
        );

        codes.insert(
            "E2031".to_string(),
            ErrorCode {
                code: "E2031".to_string(),
                category: ErrorCategory::Type,
                description: "Missing struct field".to_string(),
                example: None,
                suggestion: Some("Provide a value for the missing field.".to_string()),
            },
        );

        codes.insert(
            "E2032".to_string(),
            ErrorCode {
                code: "E2032".to_string(),
                category: ErrorCategory::Type,
                description: "Extra struct field".to_string(),
                example: None,
                suggestion: Some("Remove the unknown field.".to_string()),
            },
        );

        codes.insert(
            "E2033".to_string(),
            ErrorCode {
                code: "E2033".to_string(),
                category: ErrorCategory::Type,
                description: "Cannot apply unary operator".to_string(),
                example: None,
                suggestion: Some("This operator requires a numeric or boolean type.".to_string()),
            },
        );

        codes.insert(
            "E2034".to_string(),
            ErrorCode {
                code: "E2034".to_string(),
                category: ErrorCategory::Type,
                description: "Array index must be integer".to_string(),
                example: None,
                suggestion: Some("Use an integer value for array indexing.".to_string()),
            },
        );

        codes.insert(
            "E2035".to_string(),
            ErrorCode {
                code: "E2035".to_string(),
                category: ErrorCategory::Type,
                description: "Array index out of bounds".to_string(),
                example: None,
                suggestion: Some("Use an index within the array bounds.".to_string()),
            },
        );

        codes.insert(
            "E2036".to_string(),
            ErrorCode {
                code: "E2036".to_string(),
                category: ErrorCategory::Type,
                description: "Invalid array size".to_string(),
                example: None,
                suggestion: Some("Provide a valid positive array size.".to_string()),
            },
        );

        codes.insert(
            "E2037".to_string(),
            ErrorCode {
                code: "E2037".to_string(),
                category: ErrorCategory::Type,
                description: "Unsized type in array".to_string(),
                example: None,
                suggestion: Some("Use a sized type or a slice instead.".to_string()),
            },
        );

        codes.insert(
            "E2038".to_string(),
            ErrorCode {
                code: "E2038".to_string(),
                category: ErrorCategory::Type,
                description: "Mismatched types in binary operation".to_string(),
                example: None,
                suggestion: Some("Both operands should be the same type.".to_string()),
            },
        );

        codes.insert(
            "E2039".to_string(),
            ErrorCode {
                code: "E2039".to_string(),
                category: ErrorCategory::Type,
                description: "If expression type mismatch".to_string(),
                example: None,
                suggestion: Some("Make all branches return consistent types.".to_string()),
            },
        );

        codes.insert(
            "E2040".to_string(),
            ErrorCode {
                code: "E2040".to_string(),
                category: ErrorCategory::Type,
                description: "Match arm type mismatch".to_string(),
                example: None,
                suggestion: Some("Make all match arms return consistent types.".to_string()),
            },
        );

        codes.insert(
            "E3001".to_string(),
            ErrorCode {
                code: "E3001".to_string(),
                category: ErrorCategory::Semantic,
                description: "Macro expansion error".to_string(),
                example: None,
                suggestion: Some("Check the macro definition and invocation syntax.".to_string()),
            },
        );

        codes.insert(
            "E3002".to_string(),
            ErrorCode {
                code: "E3002".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unsupported derive".to_string(),
                example: None,
                suggestion: Some(
                    "Remove the derive attribute or implement the trait manually.".to_string(),
                ),
            },
        );

        codes.insert(
            "E3003".to_string(),
            ErrorCode {
                code: "E3003".to_string(),
                category: ErrorCategory::Semantic,
                description: "Undefined module".to_string(),
                example: None,
                suggestion: Some("Check the module path or create the module file.".to_string()),
            },
        );

        codes.insert(
            "E3004".to_string(),
            ErrorCode {
                code: "E3004".to_string(),
                category: ErrorCategory::Semantic,
                description: "Undefined name".to_string(),
                example: None,
                suggestion: Some("Check the spelling or define the name before use.".to_string()),
            },
        );

        codes.insert(
            "E3005".to_string(),
            ErrorCode {
                code: "E3005".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unused variable".to_string(),
                example: None,
                suggestion: Some("Use the variable or prefix with `_` to suppress.".to_string()),
            },
        );

        codes.insert(
            "E3006".to_string(),
            ErrorCode {
                code: "E3006".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unused parameter".to_string(),
                example: None,
                suggestion: Some("Use the parameter or prefix with `_`.".to_string()),
            },
        );

        codes.insert(
            "E3007".to_string(),
            ErrorCode {
                code: "E3007".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unused function".to_string(),
                example: None,
                suggestion: Some("Remove the function or add a call site.".to_string()),
            },
        );

        codes.insert(
            "E3008".to_string(),
            ErrorCode {
                code: "E3008".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unused type".to_string(),
                example: None,
                suggestion: Some("Remove the unused type definition.".to_string()),
            },
        );

        codes.insert(
            "E3009".to_string(),
            ErrorCode {
                code: "E3009".to_string(),
                category: ErrorCategory::Semantic,
                description: "Variable shadowed".to_string(),
                example: None,
                suggestion: Some("Use a different name or add `mut` for reassignment.".to_string()),
            },
        );

        codes.insert(
            "E3010".to_string(),
            ErrorCode {
                code: "E3010".to_string(),
                category: ErrorCategory::Semantic,
                description: "Unreachable expression".to_string(),
                example: None,
                suggestion: Some("Remove the unreachable code.".to_string()),
            },
        );

        codes.insert(
            "E3011".to_string(),
            ErrorCode {
                code: "E3011".to_string(),
                category: ErrorCategory::Semantic,
                description: "Dead code in conditional".to_string(),
                example: None,
                suggestion: Some("Remove the dead branch or correct the logic.".to_string()),
            },
        );

        codes.insert(
            "E3012".to_string(),
            ErrorCode {
                code: "E3012".to_string(),
                category: ErrorCategory::Semantic,
                description: "Divergent function".to_string(),
                example: None,
                suggestion: Some("Add a return expression to all code paths.".to_string()),
            },
        );

        codes.insert(
            "E3013".to_string(),
            ErrorCode {
                code: "E3013".to_string(),
                category: ErrorCategory::Semantic,
                description: "Missing return value".to_string(),
                example: None,
                suggestion: Some("Add a return expression at the end of the function.".to_string()),
            },
        );

        codes.insert(
            "E3014".to_string(),
            ErrorCode {
                code: "E3014".to_string(),
                category: ErrorCategory::Semantic,
                description: "Invalid visibility".to_string(),
                example: None,
                suggestion: Some("Use `pub`, `pub(crate)`, or `pub(super)`.".to_string()),
            },
        );

        codes.insert(
            "E3015".to_string(),
            ErrorCode {
                code: "E3015".to_string(),
                category: ErrorCategory::Semantic,
                description: "Private field access".to_string(),
                example: None,
                suggestion: Some(
                    "Make the field public or access it from within the module.".to_string(),
                ),
            },
        );

        codes.insert(
            "E3016".to_string(),
            ErrorCode {
                code: "E3016".to_string(),
                category: ErrorCategory::Semantic,
                description: "Private function call".to_string(),
                example: None,
                suggestion: Some(
                    "Make the function public or call from within the module.".to_string(),
                ),
            },
        );

        codes.insert(
            "E3017".to_string(),
            ErrorCode {
                code: "E3017".to_string(),
                category: ErrorCategory::Semantic,
                description: "Conflicting function definition".to_string(),
                example: None,
                suggestion: Some("Rename one of the functions.".to_string()),
            },
        );

        codes.insert(
            "E3018".to_string(),
            ErrorCode {
                code: "E3018".to_string(),
                category: ErrorCategory::Semantic,
                description: "Conflicting type definition".to_string(),
                example: None,
                suggestion: Some("Rename one of the types.".to_string()),
            },
        );

        codes.insert(
            "E3019".to_string(),
            ErrorCode {
                code: "E3019".to_string(),
                category: ErrorCategory::Semantic,
                description: "Invalid re-export".to_string(),
                example: None,
                suggestion: Some("Make the item public or remove the re-export.".to_string()),
            },
        );

        codes.insert(
            "E3020".to_string(),
            ErrorCode {
                code: "E3020".to_string(),
                category: ErrorCategory::Semantic,
                description: "Invalid documentation syntax".to_string(),
                example: None,
                suggestion: Some("Fix the markdown syntax.".to_string()),
            },
        );

        codes.insert(
            "E4001".to_string(),
            ErrorCode {
                code: "E4001".to_string(),
                category: ErrorCategory::Codegen,
                description: "No main function".to_string(),
                example: None,
                suggestion: Some(
                    "Define `fn main() -> i64 { ... }` as the entry point.".to_string(),
                ),
            },
        );

        codes.insert(
            "E4002".to_string(),
            ErrorCode {
                code: "E4002".to_string(),
                category: ErrorCategory::Codegen,
                description: "Code generation failed".to_string(),
                example: None,
                suggestion: Some("This is an internal compiler error.".to_string()),
            },
        );

        codes.insert(
            "E4003".to_string(),
            ErrorCode {
                code: "E4003".to_string(),
                category: ErrorCategory::Codegen,
                description: "LLVM verification error".to_string(),
                example: None,
                suggestion: Some("This is an internal compiler error.".to_string()),
            },
        );

        codes.insert(
            "E4004".to_string(),
            ErrorCode {
                code: "E4004".to_string(),
                category: ErrorCategory::Codegen,
                description: "Unsupported feature".to_string(),
                example: None,
                suggestion: Some("Feature is not yet implemented in the backend.".to_string()),
            },
        );

        codes.insert(
            "E4005".to_string(),
            ErrorCode {
                code: "E4005".to_string(),
                category: ErrorCategory::Codegen,
                description: "Unsupported type in codegen".to_string(),
                example: None,
                suggestion: Some(
                    "This type cannot be used in the current compilation target.".to_string(),
                ),
            },
        );

        codes.insert(
            "E4006".to_string(),
            ErrorCode {
                code: "E4006".to_string(),
                category: ErrorCategory::Codegen,
                description: "Unsupported operation in codegen".to_string(),
                example: None,
                suggestion: Some("This operation is not yet supported by the backend.".to_string()),
            },
        );

        codes.insert(
            "E4007".to_string(),
            ErrorCode {
                code: "E4007".to_string(),
                category: ErrorCategory::Codegen,
                description: "Unsupported intrinsic".to_string(),
                example: None,
                suggestion: Some("The intrinsic function is not recognized.".to_string()),
            },
        );

        codes.insert(
            "E4008".to_string(),
            ErrorCode {
                code: "E4008".to_string(),
                category: ErrorCategory::Codegen,
                description: "Inline assembly error".to_string(),
                example: None,
                suggestion: Some("Check assembly syntax or target support.".to_string()),
            },
        );

        codes.insert(
            "E4009".to_string(),
            ErrorCode {
                code: "E4009".to_string(),
                category: ErrorCategory::Codegen,
                description: "Target feature not available".to_string(),
                example: None,
                suggestion: Some(
                    "Compile for a target that supports the required feature.".to_string(),
                ),
            },
        );

        codes.insert(
            "E4010".to_string(),
            ErrorCode {
                code: "E4010".to_string(),
                category: ErrorCategory::Codegen,
                description: "AOT object file failed".to_string(),
                example: None,
                suggestion: Some("Check disk space and output path permissions.".to_string()),
            },
        );

        codes.insert(
            "E4011".to_string(),
            ErrorCode {
                code: "E4011".to_string(),
                category: ErrorCategory::Codegen,
                description: "JIT compilation error".to_string(),
                example: None,
                suggestion: Some("Check for unsupported target features.".to_string()),
            },
        );

        codes.insert(
            "E4012".to_string(),
            ErrorCode {
                code: "E4012".to_string(),
                category: ErrorCategory::Codegen,
                description: "Function too large".to_string(),
                example: None,
                suggestion: Some("Refactor the function into smaller functions.".to_string()),
            },
        );

        codes.insert(
            "E4013".to_string(),
            ErrorCode {
                code: "E4013".to_string(),
                category: ErrorCategory::Codegen,
                description: "Inlining limit exceeded".to_string(),
                example: None,
                suggestion: Some("Refactor to reduce function call depth.".to_string()),
            },
        );

        codes.insert(
            "E4014".to_string(),
            ErrorCode {
                code: "E4014".to_string(),
                category: ErrorCategory::Codegen,
                description: "Constant too large".to_string(),
                example: None,
                suggestion: Some("Reduce the size of the constant.".to_string()),
            },
        );

        codes.insert(
            "E4015".to_string(),
            ErrorCode {
                code: "E4015".to_string(),
                category: ErrorCategory::Codegen,
                description: "Linkage conflict".to_string(),
                example: None,
                suggestion: Some("Check for duplicate function definitions.".to_string()),
            },
        );

        codes.insert(
            "E5001".to_string(),
            ErrorCode {
                code: "E5001".to_string(),
                category: ErrorCategory::Runtime,
                description: "Linker error".to_string(),
                example: None,
                suggestion: Some("Check linker flags and library paths.".to_string()),
            },
        );

        codes.insert(
            "E5002".to_string(),
            ErrorCode {
                code: "E5002".to_string(),
                category: ErrorCategory::Runtime,
                description: "Undefined symbol".to_string(),
                example: None,
                suggestion: Some("Ensure the function is defined and linked.".to_string()),
            },
        );

        codes.insert(
            "E5003".to_string(),
            ErrorCode {
                code: "E5003".to_string(),
                category: ErrorCategory::Runtime,
                description: "Duplicate symbol".to_string(),
                example: None,
                suggestion: Some("Remove the duplicate definition.".to_string()),
            },
        );

        codes.insert(
            "E5004".to_string(),
            ErrorCode {
                code: "E5004".to_string(),
                category: ErrorCategory::Runtime,
                description: "Runtime function not found".to_string(),
                example: None,
                suggestion: Some("Ensure the runtime library is linked correctly.".to_string()),
            },
        );

        codes.insert(
            "E5005".to_string(),
            ErrorCode {
                code: "E5005".to_string(),
                category: ErrorCategory::Runtime,
                description: "Invalid linker flag".to_string(),
                example: None,
                suggestion: Some("Check the linker flag syntax.".to_string()),
            },
        );

        codes.insert(
            "E5006".to_string(),
            ErrorCode {
                code: "E5006".to_string(),
                category: ErrorCategory::Runtime,
                description: "Object file error".to_string(),
                example: None,
                suggestion: Some("Rebuild the object file.".to_string()),
            },
        );

        codes.insert(
            "E5007".to_string(),
            ErrorCode {
                code: "E5007".to_string(),
                category: ErrorCategory::Runtime,
                description: "Runtime assertion failed".to_string(),
                example: None,
                suggestion: Some("Fix the condition being asserted.".to_string()),
            },
        );

        codes.insert(
            "E5008".to_string(),
            ErrorCode {
                code: "E5008".to_string(),
                category: ErrorCategory::Runtime,
                description: "Runtime panic".to_string(),
                example: None,
                suggestion: Some(
                    "Check for unwrap on None/Err or out-of-bounds access.".to_string(),
                ),
            },
        );

        codes.insert(
            "E5009".to_string(),
            ErrorCode {
                code: "E5009".to_string(),
                category: ErrorCategory::Runtime,
                description: "Stack overflow".to_string(),
                example: None,
                suggestion: Some("Refactor to use iteration or increase stack size.".to_string()),
            },
        );

        codes.insert(
            "E5010".to_string(),
            ErrorCode {
                code: "E5010".to_string(),
                category: ErrorCategory::Runtime,
                description: "Out of memory".to_string(),
                example: None,
                suggestion: Some("Reduce memory usage or increase available memory.".to_string()),
            },
        );

        codes.insert(
            "E5011".to_string(),
            ErrorCode {
                code: "E5011".to_string(),
                category: ErrorCategory::Runtime,
                description: "Null pointer dereference".to_string(),
                example: None,
                suggestion: Some("Check for null pointers before dereferencing.".to_string()),
            },
        );

        codes.insert(
            "E5012".to_string(),
            ErrorCode {
                code: "E5012".to_string(),
                category: ErrorCategory::Runtime,
                description: "Arithmetic overflow".to_string(),
                example: None,
                suggestion: Some("Use checked or wrapping arithmetic operations.".to_string()),
            },
        );

        codes.insert(
            "E5013".to_string(),
            ErrorCode {
                code: "E5013".to_string(),
                category: ErrorCategory::Runtime,
                description: "Division by zero".to_string(),
                example: None,
                suggestion: Some("Add a zero check before division.".to_string()),
            },
        );

        codes.insert(
            "E5014".to_string(),
            ErrorCode {
                code: "E5014".to_string(),
                category: ErrorCategory::Runtime,
                description: "Array index out of bounds".to_string(),
                example: None,
                suggestion: Some("Check index bounds before access.".to_string()),
            },
        );

        codes.insert(
            "E5015".to_string(),
            ErrorCode {
                code: "E5015".to_string(),
                category: ErrorCategory::Runtime,
                description: "Unwrap on None or Err".to_string(),
                example: None,
                suggestion: Some("Handle the None/Err case with `match` or `if let`.".to_string()),
            },
        );

        codes.insert(
            "E6001".to_string(),
            ErrorCode {
                code: "E6001".to_string(),
                category: ErrorCategory::Module,
                description: "Module not found".to_string(),
                example: None,
                suggestion: Some("Create the module file or fix the module path.".to_string()),
            },
        );

        codes.insert(
            "E6002".to_string(),
            ErrorCode {
                code: "E6002".to_string(),
                category: ErrorCategory::Module,
                description: "Failed to load module".to_string(),
                example: None,
                suggestion: Some("Check file permissions and path.".to_string()),
            },
        );

        codes.insert(
            "E6003".to_string(),
            ErrorCode {
                code: "E6003".to_string(),
                category: ErrorCategory::Module,
                description: "Circular module dependency".to_string(),
                example: None,
                suggestion: Some("Break the cycle by restructuring modules.".to_string()),
            },
        );

        codes.insert(
            "E6004".to_string(),
            ErrorCode {
                code: "E6004".to_string(),
                category: ErrorCategory::Module,
                description: "Name conflict".to_string(),
                example: None,
                suggestion: Some("Rename one of the conflicting items.".to_string()),
            },
        );

        codes.insert(
            "E6005".to_string(),
            ErrorCode {
                code: "E6005".to_string(),
                category: ErrorCategory::Module,
                description: "Invalid re-export".to_string(),
                example: None,
                suggestion: Some("Make the item public first.".to_string()),
            },
        );

        codes.insert(
            "E6006".to_string(),
            ErrorCode {
                code: "E6006".to_string(),
                category: ErrorCategory::Module,
                description: "Unused import".to_string(),
                example: None,
                suggestion: Some("Remove the unused import.".to_string()),
            },
        );

        codes.insert(
            "E6007".to_string(),
            ErrorCode {
                code: "E6007".to_string(),
                category: ErrorCategory::Module,
                description: "Module outside crate root".to_string(),
                example: None,
                suggestion: Some("Move the file into the crate root.".to_string()),
            },
        );

        codes.insert(
            "E6008".to_string(),
            ErrorCode {
                code: "E6008".to_string(),
                category: ErrorCategory::Module,
                description: "Invalid module declaration".to_string(),
                example: None,
                suggestion: Some("Create `name.z` or `name/mod.z`.".to_string()),
            },
        );

        codes.insert(
            "E6009".to_string(),
            ErrorCode {
                code: "E6009".to_string(),
                category: ErrorCategory::Module,
                description: "Module parse error".to_string(),
                example: None,
                suggestion: Some("Fix the syntax errors in the module file.".to_string()),
            },
        );

        codes.insert(
            "E6010".to_string(),
            ErrorCode {
                code: "E6010".to_string(),
                category: ErrorCategory::Module,
                description: "Conflicting module paths".to_string(),
                example: None,
                suggestion: Some("Use a single canonical path.".to_string()),
            },
        );

        codes.insert(
            "E6011".to_string(),
            ErrorCode {
                code: "E6011".to_string(),
                category: ErrorCategory::Module,
                description: "Recursive module alias".to_string(),
                example: None,
                suggestion: Some("Break the alias cycle.".to_string()),
            },
        );

        codes.insert(
            "E6012".to_string(),
            ErrorCode {
                code: "E6012".to_string(),
                category: ErrorCategory::Module,
                description: "Invalid module visibility".to_string(),
                example: None,
                suggestion: Some("Use `pub` or `pub(crate)` for modules.".to_string()),
            },
        );

        codes.insert(
            "E6013".to_string(),
            ErrorCode {
                code: "E6013".to_string(),
                category: ErrorCategory::Module,
                description: "Wildcard import conflict".to_string(),
                example: None,
                suggestion: Some("Qualify the name or remove one of the imports.".to_string()),
            },
        );

        codes.insert(
            "E6014".to_string(),
            ErrorCode {
                code: "E6014".to_string(),
                category: ErrorCategory::Module,
                description: "Missing module name".to_string(),
                example: None,
                suggestion: Some("Provide a name for the module.".to_string()),
            },
        );

        codes.insert(
            "E6015".to_string(),
            ErrorCode {
                code: "E6015".to_string(),
                category: ErrorCategory::Module,
                description: "Module in expression context".to_string(),
                example: None,
                suggestion: Some("Module declarations must be at file or block level.".to_string()),
            },
        );

        codes.insert(
            "E7001".to_string(),
            ErrorCode {
                code: "E7001".to_string(),
                category: ErrorCategory::Borrow,
                description: "Borrow checker error".to_string(),
                example: None,
                suggestion: Some("Follow ownership and borrowing rules.".to_string()),
            },
        );

        codes.insert(
            "E7002".to_string(),
            ErrorCode {
                code: "E7002".to_string(),
                category: ErrorCategory::Borrow,
                description: "Use after move".to_string(),
                example: None,
                suggestion: Some("Clone the value or restructure to avoid the move.".to_string()),
            },
        );

        codes.insert(
            "E7003".to_string(),
            ErrorCode {
                code: "E7003".to_string(),
                category: ErrorCategory::Borrow,
                description: "Borrow of moved value".to_string(),
                example: None,
                suggestion: Some("Ensure the value outlives the reference.".to_string()),
            },
        );

        codes.insert(
            "E7004".to_string(),
            ErrorCode {
                code: "E7004".to_string(),
                category: ErrorCategory::Borrow,
                description: "Multiple mutable borrows".to_string(),
                example: None,
                suggestion: Some("Restructure to have one mutable borrow at a time.".to_string()),
            },
        );

        codes.insert(
            "E7005".to_string(),
            ErrorCode {
                code: "E7005".to_string(),
                category: ErrorCategory::Borrow,
                description: "Mutable and immutable borrow conflict".to_string(),
                example: None,
                suggestion: Some(
                    "Restructure to separate mutable and immutable access.".to_string(),
                ),
            },
        );

        codes.insert(
            "E7006".to_string(),
            ErrorCode {
                code: "E7006".to_string(),
                category: ErrorCategory::Borrow,
                description: "Returning reference to local".to_string(),
                example: None,
                suggestion: Some(
                    "Return the value directly or use a static/owned reference.".to_string(),
                ),
            },
        );

        codes.insert(
            "E7007".to_string(),
            ErrorCode {
                code: "E7007".to_string(),
                category: ErrorCategory::Borrow,
                description: "Dangling reference".to_string(),
                example: None,
                suggestion: Some("Ensure the referenced value lives long enough.".to_string()),
            },
        );

        codes.insert(
            "E7008".to_string(),
            ErrorCode {
                code: "E7008".to_string(),
                category: ErrorCategory::Borrow,
                description: "Assign to borrowed value".to_string(),
                example: None,
                suggestion: Some("Modify the value after the borrow ends.".to_string()),
            },
        );

        codes.insert(
            "E7009".to_string(),
            ErrorCode {
                code: "E7009".to_string(),
                category: ErrorCategory::Borrow,
                description: "Reborrow extends beyond original".to_string(),
                example: None,
                suggestion: Some("Restructure to keep the original borrow in scope.".to_string()),
            },
        );

        codes.insert(
            "E7010".to_string(),
            ErrorCode {
                code: "E7010".to_string(),
                category: ErrorCategory::Borrow,
                description: "Borrow across mutation boundary".to_string(),
                example: None,
                suggestion: Some("Restructure to keep borrows and mutations separate.".to_string()),
            },
        );

        codes.insert(
            "E7011".to_string(),
            ErrorCode {
                code: "E7011".to_string(),
                category: ErrorCategory::Borrow,
                description: "Cannot move out of borrow".to_string(),
                example: None,
                suggestion: Some("Clone the value or use `&mut` to swap.".to_string()),
            },
        );

        codes.insert(
            "E7012".to_string(),
            ErrorCode {
                code: "E7012".to_string(),
                category: ErrorCategory::Borrow,
                description: "Cannot move from mutable reference".to_string(),
                example: None,
                suggestion: Some("Use `std::mem::replace` or `std::mem::take`.".to_string()),
            },
        );

        codes.insert(
            "E7013".to_string(),
            ErrorCode {
                code: "E7013".to_string(),
                category: ErrorCategory::Borrow,
                description: "Partial move conflict".to_string(),
                example: None,
                suggestion: Some("Clone the borrowed field or restructure.".to_string()),
            },
        );

        codes.insert(
            "E7014".to_string(),
            ErrorCode {
                code: "E7014".to_string(),
                category: ErrorCategory::Borrow,
                description: "Invalid ownership transfer".to_string(),
                example: None,
                suggestion: Some("Use a reference instead of moving.".to_string()),
            },
        );

        codes.insert(
            "E7015".to_string(),
            ErrorCode {
                code: "E7015".to_string(),
                category: ErrorCategory::Borrow,
                description: "Lifetime mismatch".to_string(),
                example: None,
                suggestion: Some("Ensure borrowed value lives long enough.".to_string()),
            },
        );

        codes.insert(
            "E7016".to_string(),
            ErrorCode {
                code: "E7016".to_string(),
                category: ErrorCategory::Borrow,
                description: "Missing lifetime annotation".to_string(),
                example: None,
                suggestion: Some("Add lifetime annotations to function signature.".to_string()),
            },
        );

        codes.insert(
            "E7017".to_string(),
            ErrorCode {
                code: "E7017".to_string(),
                category: ErrorCategory::Borrow,
                description: "Lifetime elision conflict".to_string(),
                example: None,
                suggestion: Some("Add explicit lifetime annotations.".to_string()),
            },
        );

        codes.insert(
            "E7018".to_string(),
            ErrorCode {
                code: "E7018".to_string(),
                category: ErrorCategory::Borrow,
                description: "Closure capture conflict".to_string(),
                example: None,
                suggestion: Some("Use `move` keyword or adjust capture mode.".to_string()),
            },
        );

        codes.insert(
            "E7019".to_string(),
            ErrorCode {
                code: "E7019".to_string(),
                category: ErrorCategory::Borrow,
                description: "Mutability mismatch".to_string(),
                example: None,
                suggestion: Some(
                    "Pass a mutable reference or change the function signature.".to_string(),
                ),
            },
        );

        codes.insert(
            "E7020".to_string(),
            ErrorCode {
                code: "E7020".to_string(),
                category: ErrorCategory::Borrow,
                description: "Move in loop iteration".to_string(),
                example: None,
                suggestion: Some(
                    "Clone or restructure to avoid the move across iterations.".to_string(),
                ),
            },
        );

        codes.insert(
            "E8001".to_string(),
            ErrorCode {
                code: "E8001".to_string(),
                category: ErrorCategory::Optimization,
                description: "Const evaluation limit".to_string(),
                example: None,
                suggestion: Some("Simplify the const expression.".to_string()),
            },
        );

        codes.insert(
            "E8002".to_string(),
            ErrorCode {
                code: "E8002".to_string(),
                category: ErrorCategory::Optimization,
                description: "Const evaluation blocked".to_string(),
                example: None,
                suggestion: Some("Only const-compatible operations are allowed.".to_string()),
            },
        );

        codes.insert(
            "E8003".to_string(),
            ErrorCode {
                code: "E8003".to_string(),
                category: ErrorCategory::Optimization,
                description: "Unsupported const operation".to_string(),
                example: None,
                suggestion: Some("Use only const fn calls in const contexts.".to_string()),
            },
        );

        codes.insert(
            "E8004".to_string(),
            ErrorCode {
                code: "E8004".to_string(),
                category: ErrorCategory::Optimization,
                description: "Mutable reference in const".to_string(),
                example: None,
                suggestion: Some("Use const-compatible patterns.".to_string()),
            },
        );

        codes.insert(
            "E8005".to_string(),
            ErrorCode {
                code: "E8005".to_string(),
                category: ErrorCategory::Optimization,
                description: "Const fn call outside const".to_string(),
                example: None,
                suggestion: Some(
                    "Declare the function as non-const or call from const context.".to_string(),
                ),
            },
        );

        codes.insert(
            "E8006".to_string(),
            ErrorCode {
                code: "E8006".to_string(),
                category: ErrorCategory::Optimization,
                description: "Inlining limit exceeded".to_string(),
                example: None,
                suggestion: Some("Reduce function size or mark `#[inline(never)]`.".to_string()),
            },
        );

        codes.insert(
            "E8007".to_string(),
            ErrorCode {
                code: "E8007".to_string(),
                category: ErrorCategory::Optimization,
                description: "Loop unrolling limit".to_string(),
                example: None,
                suggestion: Some("Reduce loop body size or iteration count.".to_string()),
            },
        );

        codes.insert(
            "E8008".to_string(),
            ErrorCode {
                code: "E8008".to_string(),
                category: ErrorCategory::Optimization,
                description: "Tail call optimization failed".to_string(),
                example: None,
                suggestion: Some(
                    "Restructure the recursive call to be the last expression.".to_string(),
                ),
            },
        );

        codes.insert(
            "E8009".to_string(),
            ErrorCode {
                code: "E8009".to_string(),
                category: ErrorCategory::Optimization,
                description: "Constant propagation limit".to_string(),
                example: None,
                suggestion: Some("Simplify the expression.".to_string()),
            },
        );

        codes.insert(
            "E8010".to_string(),
            ErrorCode {
                code: "E8010".to_string(),
                category: ErrorCategory::Optimization,
                description: "MIR optimization error".to_string(),
                example: None,
                suggestion: Some("This is an internal compiler error.".to_string()),
            },
        );

        codes.insert(
            "E9001".to_string(),
            ErrorCode {
                code: "E9001".to_string(),
                category: ErrorCategory::Tooling,
                description: "Invalid compiler flag".to_string(),
                example: None,
                suggestion: Some("Check `--help` for valid flags.".to_string()),
            },
        );

        codes.insert(
            "E9002".to_string(),
            ErrorCode {
                code: "E9002".to_string(),
                category: ErrorCategory::Tooling,
                description: "File not found".to_string(),
                example: None,
                suggestion: Some("Check the file path and try again.".to_string()),
            },
        );

        codes.insert(
            "E9003".to_string(),
            ErrorCode {
                code: "E9003".to_string(),
                category: ErrorCategory::Tooling,
                description: "Output path invalid".to_string(),
                example: None,
                suggestion: Some("Check the output path.".to_string()),
            },
        );

        codes.insert(
            "E9004".to_string(),
            ErrorCode {
                code: "E9004".to_string(),
                category: ErrorCategory::Tooling,
                description: "Internal compiler error".to_string(),
                example: None,
                suggestion: Some("Please report this bug with a minimal reproduction.".to_string()),
            },
        );

        codes.insert(
            "E9005".to_string(),
            ErrorCode {
                code: "E9005".to_string(),
                category: ErrorCategory::Tooling,
                description: "LSP initialization error".to_string(),
                example: None,
                suggestion: Some("Check LSP configuration.".to_string()),
            },
        );

        codes.insert(
            "E9006".to_string(),
            ErrorCode {
                code: "E9006".to_string(),
                category: ErrorCategory::Tooling,
                description: "LSP document error".to_string(),
                example: None,
                suggestion: Some("Fix syntax errors in the document.".to_string()),
            },
        );

        codes.insert(
            "E9007".to_string(),
            ErrorCode {
                code: "E9007".to_string(),
                category: ErrorCategory::Tooling,
                description: "LSP completion error".to_string(),
                example: None,
                suggestion: Some("This is an internal tooling error.".to_string()),
            },
        );

        codes.insert(
            "E9008".to_string(),
            ErrorCode {
                code: "E9008".to_string(),
                category: ErrorCategory::Tooling,
                description: "Configuration error".to_string(),
                example: None,
                suggestion: Some("Fix the configuration file syntax.".to_string()),
            },
        );

        codes.insert(
            "E9009".to_string(),
            ErrorCode {
                code: "E9009".to_string(),
                category: ErrorCategory::Tooling,
                description: "Dependency resolution error".to_string(),
                example: None,
                suggestion: Some("Check dependency declarations.".to_string()),
            },
        );

        codes.insert(
            "E9010".to_string(),
            ErrorCode {
                code: "E9010".to_string(),
                category: ErrorCategory::Tooling,
                description: "Network error".to_string(),
                example: None,
                suggestion: Some("Check network connectivity.".to_string()),
            },
        );

        codes.insert(
            "E9011".to_string(),
            ErrorCode {
                code: "E9011".to_string(),
                category: ErrorCategory::Tooling,
                description: "Build cache corruption".to_string(),
                example: None,
                suggestion: Some("Rebuild without cache by cleaning.".to_string()),
            },
        );

        codes.insert(
            "E9012".to_string(),
            ErrorCode {
                code: "E9012".to_string(),
                category: ErrorCategory::Tooling,
                description: "Test runner error".to_string(),
                example: None,
                suggestion: Some("Check test configuration.".to_string()),
            },
        );

        codes.insert(
            "E9013".to_string(),
            ErrorCode {
                code: "E9013".to_string(),
                category: ErrorCategory::Tooling,
                description: "Benchmark error".to_string(),
                example: None,
                suggestion: Some("Check benchmark configuration.".to_string()),
            },
        );

        codes.insert(
            "E9014".to_string(),
            ErrorCode {
                code: "E9014".to_string(),
                category: ErrorCategory::Tooling,
                description: "REPL initialization error".to_string(),
                example: None,
                suggestion: Some("Check terminal capabilities.".to_string()),
            },
        );

        codes.insert(
            "E9015".to_string(),
            ErrorCode {
                code: "E9015".to_string(),
                category: ErrorCategory::Tooling,
                description: "Resource limit exceeded".to_string(),
                example: None,
                suggestion: Some("Reduce input size or increase memory limit.".to_string()),
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
    if let Some(error_code) = ERROR_CODES.get(code)
        && let Some(suggestion) = &error_code.suggestion
    {
        diag = diag.with_suggestion(suggestion.clone());
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
