//! Basic tests for the package ecosystem
//!
//! These tests verify the core functionality of:
//! - Manifest parsing
//! - Dependency resolution
//! - Version requirement matching

use zetac::package::manifest::Manifest;
use zetac::package::dependency::{Version, VersionReq};
use zetac::package::resolver::{PackageRegistry, PackageMetadata};

#[test]
fn test_manifest_parsing() {
    let toml_content = r#"
[package]
name = "test-crate"
version = "0.1.0"
authors = ["Test Author <test@example.com>"]
description = "A test crate"
license = "MIT"
edition = "2024"

[dependencies]
serde = "1.0"
tokio = { version = "1.0", features = ["full"] }

[dev-dependencies]
proptest = "1.0"

[features]
default = ["serde"]
extra = ["tokio/fs"]
"#;
    
    let manifest = Manifest::parse(toml_content).expect("Failed to parse manifest");
    
    assert_eq!(manifest.package.name, "test-crate");
    assert_eq!(manifest.package.version, "0.1.0");
    assert_eq!(manifest.package.authors.len(), 1);
    assert_eq!(manifest.package.authors[0], "Test Author <test@example.com>");
    assert_eq!(manifest.package.description, Some("A test crate".to_string()));
    assert_eq!(manifest.package.license, Some("MIT".to_string()));
    assert_eq!(manifest.package.edition, "2024");
    
    assert!(manifest.dependencies.contains_key("serde"));
    assert!(manifest.dependencies.contains_key("tokio"));
    assert!(manifest.dev_dependencies.contains_key("proptest"));
    
    assert!(manifest.features.contains_key("default"));
    assert!(manifest.features.contains_key("extra"));
}

#[test]
fn test_version_parsing() {
    // Test basic version parsing
    let v1 = Version::parse("1.2.3").expect("Failed to parse version");
    assert_eq!(v1.major, 1);
    assert_eq!(v1.minor, 2);
    assert_eq!(v1.patch, 3);
    assert_eq!(v1.pre, None);
    assert_eq!(v1.build, None);
    
    // Test with pre-release
    let v2 = Version::parse("2.0.0-beta.1").expect("Failed to parse version");
    assert_eq!(v2.major, 2);
    assert_eq!(v2.minor, 0);
    assert_eq!(v2.patch, 0);
    assert_eq!(v2.pre, Some("beta.1".to_string()));
    assert_eq!(v2.build, None);
    
    // Test with build metadata
    let v3 = Version::parse("3.1.4+20130313144700").expect("Failed to parse version");
    assert_eq!(v3.major, 3);
    assert_eq!(v3.minor, 1);
    assert_eq!(v3.patch, 4);
    assert_eq!(v3.pre, None);
    assert_eq!(v3.build, Some("20130313144700".to_string()));
    
    // Test with both pre-release and build metadata
    let v4 = Version::parse("4.0.0-alpha.2+exp.sha.5114f85").expect("Failed to parse version");
    assert_eq!(v4.major, 4);
    assert_eq!(v4.minor, 0);
    assert_eq!(v4.patch, 0);
    assert_eq!(v4.pre, Some("alpha.2".to_string()));
    assert_eq!(v4.build, Some("exp.sha.5114f85".to_string()));
}

#[test]
fn test_version_requirement_matching() {
    // Test exact version matching
    let req1 = VersionReq::parse("1.2.3").expect("Failed to parse requirement");
    let v1 = Version::parse("1.2.3").expect("Failed to parse version");
    let v2 = Version::parse("1.2.4").expect("Failed to parse version");
    assert!(req1.matches(&v1));
    assert!(!req1.matches(&v2));
    
    // Test compatible version matching (^)
    let req2 = VersionReq::parse("^1.2.3").expect("Failed to parse requirement");
    let v3 = Version::parse("1.2.3").expect("Failed to parse version");
    let v4 = Version::parse("1.3.0").expect("Failed to parse version");
    let v5 = Version::parse("2.0.0").expect("Failed to parse version");
    assert!(req2.matches(&v3));
    assert!(req2.matches(&v4));
    assert!(!req2.matches(&v5));
    
    // Test approximate version matching (~)
    let req3 = VersionReq::parse("~1.2.3").expect("Failed to parse requirement");
    let v6 = Version::parse("1.2.3").expect("Failed to parse version");
    let v7 = Version::parse("1.2.9").expect("Failed to parse version");
    let v8 = Version::parse("1.3.0").expect("Failed to parse version");
    assert!(req3.matches(&v6));
    assert!(req3.matches(&v7));
    assert!(!req3.matches(&v8));
    
    // Test greater than or equal
    let req4 = VersionReq::parse(">=1.2.3").expect("Failed to parse requirement");
    let v9 = Version::parse("1.2.3").expect("Failed to parse version");
    let v10 = Version::parse("2.0.0").expect("Failed to parse version");
    let v11 = Version::parse("1.2.2").expect("Failed to parse version");
    assert!(req4.matches(&v9));
    assert!(req4.matches(&v10));
    assert!(!req4.matches(&v11));
}

#[test]
fn test_dependency_resolution() {
    // Create a simple registry
    let mut registry = PackageRegistry::new();
    
    // Add a package with multiple versions
    let metadata = PackageMetadata {
        dependencies: std::collections::HashMap::new(),
        features: std::collections::HashMap::new(),
    };
    
    registry.add_package("test-package", Version::parse("1.0.0").unwrap(), metadata.clone());
    registry.add_package("test-package", Version::parse("1.1.0").unwrap(), metadata.clone());
    registry.add_package("test-package", Version::parse("2.0.0").unwrap(), metadata);
    
    // Test finding best match
    let req1 = VersionReq::parse("^1.0.0").unwrap();
    let best1 = registry.find_best_match("test-package", &req1).unwrap();
    assert_eq!(best1.to_string(), "1.1.0");
    
    let req2 = VersionReq::parse(">=1.0.0 <2.0.0").unwrap();
    let best2 = registry.find_best_match("test-package", &req2).unwrap();
    assert_eq!(best2.to_string(), "1.1.0");
    
    let req3 = VersionReq::parse("2.0.0").unwrap();
    let best3 = registry.find_best_match("test-package", &req3).unwrap();
    assert_eq!(best3.to_string(), "2.0.0");
}

#[test]
fn test_manifest_serialization() {
    let mut manifest = Manifest::default_for_crate("serialization-test");
    manifest.package.description = Some("Test serialization".to_string());
    manifest.package.authors = vec!["Author One".to_string(), "Author Two".to_string()];
    manifest.add_dependency("serde", "1.0");
    manifest.add_dependency("tokio", "^1.0");
    
    let toml = manifest.to_toml().expect("Failed to serialize manifest");
    
    // Parse it back and verify
    let parsed = Manifest::parse(&toml).expect("Failed to parse serialized manifest");
    
    assert_eq!(parsed.package.name, "serialization-test");
    assert_eq!(parsed.package.description, Some("Test serialization".to_string()));
    assert_eq!(parsed.package.authors.len(), 2);
    assert!(parsed.dependencies.contains_key("serde"));
    assert!(parsed.dependencies.contains_key("tokio"));
}

#[test]
fn test_version_ordering() {
    let v1 = Version::parse("1.0.0").unwrap();
    let v2 = Version::parse("1.0.1").unwrap();
    let v3 = Version::parse("1.1.0").unwrap();
    let v4 = Version::parse("2.0.0").unwrap();
    let v5 = Version::parse("1.0.0-alpha").unwrap();
    let v6 = Version::parse("1.0.0-beta").unwrap();
    
    assert!(v1 < v2);
    assert!(v2 < v3);
    assert!(v3 < v4);
    assert!(v5 < v1); // Pre-release versions are less than release versions
    assert!(v5 < v6); // alpha < beta
    assert!(v6 < v1); // beta < release
}

#[test]
fn test_workspace_manifest() {
    let toml_content = r#"
[package]
name = "workspace-root"
version = "0.1.0"
edition = "2024"

[workspace]
members = ["crate-a", "crate-b"]
exclude = ["old-crate"]
default-members = ["crate-a"]

[workspace.dependencies]
common-dep = "1.0"
"#;
    
    let manifest = Manifest::parse(toml_content).expect("Failed to parse workspace manifest");
    
    assert_eq!(manifest.package.name, "workspace-root");
    assert_eq!(manifest.workspace.members, vec!["crate-a", "crate-b"]);
    assert_eq!(manifest.workspace.exclude, vec!["old-crate"]);
    assert_eq!(manifest.workspace.default_members, vec!["crate-a"]);
    assert!(manifest.workspace.dependencies.contains_key("common-dep"));
}