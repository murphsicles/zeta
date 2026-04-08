//! Macro system for Zeta
//!
//! This module provides a comprehensive macro system including:
//! 1. Declarative macros (macro_rules! style)
//! 2. Macro expansion with hygiene
//! 3. Integration with CTFE
//! 4. Bridge to frontend macro system

pub mod expander;
pub mod registry;
pub mod hygiene;
pub mod integration;

// Re-export for convenience
pub use expander::MacroExpander;
pub use registry::MacroRegistry;
pub use hygiene::HygieneContext;
pub use integration::{MacroSystemBridge, MacroExpansionPass};