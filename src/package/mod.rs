//! Package management system for Zeta
//! 
//! This module provides functionality for:
//! - Crate manifest parsing (Cargo.toml style)
//! - Dependency resolution and version management
//! - Package discovery and installation
//! - Workspace and multi-crate project support
//! - Enhanced features: dependency graph visualization, vulnerability scanning, package signing

pub mod manifest;
pub mod dependency;
pub mod resolver;
pub mod workspace;
pub mod zorb_integration;
pub mod enhanced;

// Re-export public API
pub use manifest::Manifest;
pub use dependency::{Dependency, VersionReq};
pub use resolver::DependencyResolver;
pub use workspace::Workspace;
pub use zorb_integration::{ZorbClient, PackageInfo};
pub use enhanced::{
    DependencyGraph, DependencyNode, Vulnerability, SeverityLevel,
    SignatureStatus, VulnerabilityDatabase, SignatureVerifier, PackageSigner,
};