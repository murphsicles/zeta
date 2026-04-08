//! Macro expansion system for Zeta.
//!
//! This module provides macro expansion capabilities including:
//! - Declarative macros (macro_rules! style)
//! - Macro expansion and hygiene
//! - Macro registry and lookup

pub mod expander;
pub mod registry;
pub mod hygiene;

// Re-export main types
pub use expander::MacroExpander;
pub use registry::MacroRegistry;
pub use hygiene::HygieneContext;