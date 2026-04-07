//! Unified Type Checking Interface
//!
//! Provides a unified facade for type checking that can route to
//! either the old or new type system implementations.
//!
//! This is the first step in resolving the architecture conflict
//! between old and new type systems.

use crate::frontend::ast::AstNode;
use crate::middle::types::{Substitution, Type, UnifyError};

/// Unified type checking result
#[derive(Debug)]
pub enum TypeCheckResult {
    /// Type checking succeeded with substitution
    Success(Substitution),
    /// Type checking failed with errors
    Failure(Vec<UnifyError>),
    /// Fallback to old system needed
    Fallback,
}

/// Unified type checking trait
pub trait UnifiedTypeCheck {
    /// Perform type checking on AST nodes
    fn typecheck_unified(&mut self, asts: &[AstNode]) -> TypeCheckResult;

    /// Convert string type to algebraic Type
    fn parse_type_string(&self, s: &str) -> Result<Type, String>;

    /// Convert algebraic Type to string representation
    fn type_to_string(&self, ty: &Type) -> String;
}

/// Type checking strategy
#[derive(Debug, Clone, Copy)]
pub enum TypeCheckStrategy {
    /// Use new type system (algebraic types, unification)
    NewSystem,
    /// Use old type system (string-based, simple)
    OldSystem,
    /// Auto-select based on capabilities
    Auto,
}

/// Unified type checker that routes to appropriate implementation
pub struct UnifiedTypeChecker {
    strategy: TypeCheckStrategy,
    // We'll add implementation-specific state here as needed
}

impl UnifiedTypeChecker {
    /// Create a new unified type checker with specified strategy
    pub fn new(strategy: TypeCheckStrategy) -> Self {
        Self { strategy }
    }

    /// Create with auto-selection strategy (default)
    pub fn auto() -> Self {
        Self::new(TypeCheckStrategy::Auto)
    }
}

impl UnifiedTypeCheck for UnifiedTypeChecker {
    fn typecheck_unified(&mut self, asts: &[AstNode]) -> TypeCheckResult {
        match self.strategy {
            TypeCheckStrategy::NewSystem => {
                // Try new system first
                // TODO: Integrate with new_resolver::InferContext
                TypeCheckResult::Fallback
            }
            TypeCheckStrategy::OldSystem => {
                // Use old system
                // TODO: Integrate with old typecheck logic
                TypeCheckResult::Fallback
            }
            TypeCheckStrategy::Auto => {
                // Auto-select: try new, fallback to old
                // This mimics current behavior in typecheck.rs
                TypeCheckResult::Fallback
            }
        }
    }

    fn parse_type_string(&self, s: &str) -> Result<Type, String> {
        // Use Type's built-in parser
        Ok(Type::from_string(s))
    }

    fn type_to_string(&self, ty: &Type) -> String {
        // Use Type's Display implementation or custom formatting
        format!("{:?}", ty) // Simple debug for now
    }
}

/// Integration with existing Resolver
impl UnifiedTypeCheck for super::resolver::Resolver {
    fn typecheck_unified(&mut self, asts: &[AstNode]) -> TypeCheckResult {
        // Use the existing hybrid approach from typecheck.rs
        use super::typecheck_new::NewTypeCheck;

        match self.typecheck_new(asts) {
            Ok(sub) => TypeCheckResult::Success(sub),
            Err(errors) => {
                // Check if we should fallback
                let has_type_mismatch = errors
                    .iter()
                    .any(|e| matches!(e, UnifyError::Mismatch(_, _)));

                if has_type_mismatch {
                    TypeCheckResult::Failure(errors)
                } else {
                    TypeCheckResult::Fallback
                }
            }
        }
    }

    fn parse_type_string(&self, s: &str) -> Result<Type, String> {
        use super::typecheck_new::NewTypeCheck;
        Ok(self.string_to_type(s))
    }

    fn type_to_string(&self, ty: &Type) -> String {
        use super::typecheck_new::NewTypeCheck;
        NewTypeCheck::type_to_string(self, ty)
    }
}
