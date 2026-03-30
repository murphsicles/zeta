//! Shared type context for integration bridge
//!
//! Provides shared type context structures and utilities for
//! coordinating between parser, type checker, and codegen.

use std::sync::{Arc, Mutex};

use crate::middle::types::lifetime::{Lifetime, LifetimeVar};
use crate::middle::types::{Type, TypeVar};

/// Shared type context for thread-safe access
#[derive(Debug, Clone)]
pub struct SharedTypeContext {
    inner: Arc<Mutex<TypeContext>>,
}

impl Default for SharedTypeContext {
    fn default() -> Self {
        Self::new()
    }
}

impl SharedTypeContext {
    pub fn new() -> Self {
        Self {
            inner: Arc::new(Mutex::new(TypeContext::new())),
        }
    }

    pub fn lock(&self) -> std::sync::MutexGuard<'_, TypeContext> {
        self.inner.lock().unwrap()
    }
}

/// Stack of type contexts for nested scopes
#[derive(Debug, Clone)]
pub struct TypeContextStack {
    contexts: Vec<TypeContext>,
}

impl Default for TypeContextStack {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeContextStack {
    pub fn new() -> Self {
        Self {
            contexts: vec![TypeContext::new()],
        }
    }

    pub fn push(&mut self, context: TypeContext) {
        self.contexts.push(context);
    }

    pub fn pop(&mut self) -> Option<TypeContext> {
        if self.contexts.len() > 1 {
            self.contexts.pop()
        } else {
            None
        }
    }

    pub fn current(&self) -> &TypeContext {
        self.contexts.last().unwrap()
    }

    pub fn current_mut(&mut self) -> &mut TypeContext {
        self.contexts.last_mut().unwrap()
    }
}

/// RAII guard for entering/exiting a scope
pub struct ScopeGuard<'a> {
    #[allow(dead_code)]
    stack: &'a mut TypeContextStack,
    #[allow(dead_code)]
    context: TypeContext,
}

impl<'a> ScopeGuard<'a> {
    pub fn new(stack: &'a mut TypeContextStack) -> Self {
        let context = stack.current().enter_scope();
        Self { stack, context }
    }
}

impl<'a> Drop for ScopeGuard<'a> {
    fn drop(&mut self) {
        // Scope exited, context will be dropped
    }
}

/// Error conversion utilities
pub mod error_conversion {
    use crate::integration::IntegrationError;

    pub fn type_error_to_integration(error: String) -> IntegrationError {
        IntegrationError::TypeError(error)
    }

    pub fn parse_error_to_integration(error: String) -> IntegrationError {
        IntegrationError::ParseError(error)
    }
}

/// Type variable utilities
pub mod type_var_utils {
    use super::*;

    pub fn fresh_type_var() -> Type {
        Type::Variable(TypeVar::fresh())
    }

    pub fn fresh_lifetime_var() -> Lifetime {
        Lifetime::Variable(LifetimeVar::fresh())
    }
}

// Re-export TypeContext from generic_integration for convenience
pub use super::generic_integration::TypeContext;
