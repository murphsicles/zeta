//! Package management system for Zeta
//!
//! This module provides functionality for:
//! - Crate manifest parsing (Cargo.toml style)
//! - Dependency resolution and version management
//! - Package discovery and installation
//! - Workspace and multi-crate project support
//! - Enhanced features: dependency graph visualization, vulnerability scanning, package signing

pub mod dependency;
pub mod enhanced;
pub mod manifest;
pub mod resolver;
pub mod workspace;
pub mod zorb_integration;

// Re-export public API
pub use dependency::{Dependency, VersionReq};
pub use enhanced::{
    DependencyGraph, DependencyNode, PackageSigner, SeverityLevel, SignatureStatus,
    SignatureVerifier, Vulnerability, VulnerabilityDatabase,
};
pub use manifest::Manifest;
pub use resolver::DependencyResolver;
pub use workspace::Workspace;
pub use zorb_integration::{PackageInfo, ZorbClient};
