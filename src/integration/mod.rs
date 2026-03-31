//! Integration bridge between parser, type checker, and codegen
//!
//! This module provides coordination for generic type support across
//! all compilation phases.

// Note: Temporarily simplified to avoid compilation conflicts
// Full integration will be enabled once type system is stable

// Integration module is now enabled but optional
// Use cfg feature flag to control inclusion
#[cfg(feature = "integration")]
pub mod coordination;
#[cfg(feature = "integration")]
pub mod generic_integration;
#[cfg(feature = "integration")]
pub mod type_context;

// Re-export main types (conditionally)
#[cfg(feature = "integration")]
pub use generic_integration::{
    ConcreteFunction, Constraint, ConstraintSet, GenericFunction, GenericIntegration, GenericParam,
    IntegrationError, TypeContext, WhereClause, conversion,
};

#[cfg(feature = "integration")]
pub use type_context::{
    ScopeGuard, SharedTypeContext, TypeContextStack, error_conversion, type_var_utils,
};

#[cfg(feature = "integration")]
pub use coordination::{ComponentStatus, CoordinationManager, CoordinationMessage, protocols};

// Provide dummy implementations when integration feature is disabled
#[cfg(not(feature = "integration"))]
pub struct GenericIntegration {
    // Dummy implementation
}

#[cfg(not(feature = "integration"))]
impl GenericIntegration {
    pub fn new() -> Self {
        Self {}
    }

    pub fn has_errors(&self) -> bool {
        false
    }
}

#[cfg(not(feature = "integration"))]
impl Default for GenericIntegration {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(not(feature = "integration"))]
pub struct CoordinationManager {
    // Dummy implementation
}

#[cfg(not(feature = "integration"))]
impl CoordinationManager {
    pub fn new() -> Self {
        Self {}
    }
}

#[cfg(not(feature = "integration"))]
impl Default for CoordinationManager {
    fn default() -> Self {
        Self::new()
    }
}
