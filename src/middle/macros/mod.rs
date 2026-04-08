//! Macro system for Zeta
//!
//! This module provides a comprehensive macro system including:
//! 1. Declarative macros (macro_rules! style)
//! 2. Macro expansion with hygiene
//! 3. Integration with CTFE

pub mod expander;
pub mod registry;
pub mod hygiene;

// Re-export for convenience
pub use expander::MacroExpander;
pub use registry::MacroRegistry;
pub use hygiene::HygieneContext;