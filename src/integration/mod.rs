//! Integration bridge between parser, type checker, and codegen
//!
//! This module provides coordination for generic type support across
//! all compilation phases.

// Note: Temporarily simplified to avoid compilation conflicts
// Full integration will be enabled once type system is stable

// Temporarily disabled due to compilation errors
pub mod generic_integration;
pub mod type_context;
pub mod coordination;

// Re-export main types (temporarily empty)
pub use generic_integration::{
    GenericIntegration, GenericParam, TypeContext, Constraint, ConstraintSet,
    GenericFunction, ConcreteFunction, IntegrationError, WhereClause,
    conversion,
};

pub use type_context::{
    SharedTypeContext, TypeContextStack, ScopeGuard,
    error_conversion, type_var_utils,
};

pub use coordination::{
    CoordinationManager, CoordinationMessage, ComponentStatus,
    protocols,
};