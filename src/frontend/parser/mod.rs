// src/frontend/parser/mod.rs
// pub mod array_syntax;  // Temporarily disabled due to compilation errors
pub mod expr;
pub mod identity_type;
pub mod location;
#[allow(clippy::module_inception)]
pub mod parser;
pub mod pattern;
pub mod stmt;
pub mod top_level;
