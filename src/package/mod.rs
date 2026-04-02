//! Package management system for Zeta
//! 
//! This module provides functionality for:
//! - Crate manifest parsing (Cargo.toml style)
//! - Dependency resolution and version management
//! - Package discovery and installation
//! - Workspace and multi-crate project support

pub mod manifest;
pub mod dependency;
pub mod resolver;
pub mod workspace;
pub mod zorb_integration;

// Re-export public API
pub use manifest::Manifest;
pub use dependency::{Dependency, VersionReq};
pub use resolver::DependencyResolver;
pub use workspace::Workspace;
pub use zorb_integration::{ZorbClient, PackageInfo};