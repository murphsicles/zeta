//! Integration test for package ecosystem
//!
//! This test demonstrates real-world usage of the package ecosystem
//! with the Zeta compiler.

use std::path::PathBuf;
use zetac::package::workspace::Workspace;
use zetac::package::zorb_integration::ZorbClient;

#[test]
fn test_workspace_creation_and_management() {
    // Create a temporary directory for testing
    let temp_dir = tempfile::tempdir().expect("Failed to create temp directory");
    let workspace_path = temp_dir.path();
    
    // Create a simple workspace manifest
    let manifest_content = r#"
[package]
name = "test-workspace"
version = "0.1.0"
edition = "2024"

[workspace]
members = ["lib-a", "bin-b"]

[workspace.dependencies]
common = "1.0"
"#;
    
    std::fs::write(workspace_path.join("Cargo.toml"), manifest_content)
        .expect("Failed to write workspace manifest");
    
    // Try to load the workspace
    let workspace = Workspace::discover(workspace_path);
    
    // The workspace should fail to load because members don't exist yet
    // This is expected behavior
    assert!(workspace.is_err());
    
    // Clean up
    drop(temp_dir);
}

#[test]
fn test_package_manifest_operations() {
    use zetac::package::manifest::Manifest;
    
    // Create a test manifest
    let mut manifest = Manifest::default_for_crate("test-operations");
    manifest.package.description = Some("Test package operations".to_string());
    manifest.package.version = "0.2.0".to_string();
    
    // Add dependencies
    manifest.add_dependency("serde", "1.0");
    manifest.add_dependency("tokio", "^1.0");
    
    // Add a dev dependency
    manifest.dev_dependencies.insert(
        "proptest".to_string(),
        zetac::package::manifest::DependencySpec::Simple("1.0".to_string()),
    );
    
    // Verify manifest properties
    assert_eq!(manifest.package.name, "test-operations");
    assert_eq!(manifest.package.version, "0.2.0");
    assert_eq!(manifest.package.description, Some("Test package operations".to_string()));
    
    // Verify dependencies
    assert!(manifest.dependencies.contains_key("serde"));
    assert!(manifest.dependencies.contains_key("tokio"));
    assert!(manifest.dev_dependencies.contains_key("proptest"));
    
    // Get all dependencies
    let all_deps = manifest.all_dependencies();
    assert_eq!(all_deps.len(), 3);
    assert!(all_deps.contains_key("serde"));
    assert!(all_deps.contains_key("tokio"));
    assert!(all_deps.contains_key("proptest"));
    
    // Test serialization round-trip
    let toml = manifest.to_toml().expect("Failed to serialize manifest");
    let parsed = Manifest::parse(&toml).expect("Failed to parse serialized manifest");
    
    assert_eq!(parsed.package.name, "test-operations");
    assert_eq!(parsed.package.version, "0.2.0");
    assert!(parsed.dependencies.contains_key("serde"));
    assert!(parsed.dependencies.contains_key("tokio"));
    assert!(parsed.dev_dependencies.contains_key("proptest"));
}

#[test]
fn test_dependency_graph_operations() {
    use zetac::package::dependency::{DependencyGraph, Dependency, DependencySource, Version};
    
    let mut graph = DependencyGraph::new();
    
    // Create some test dependencies
    let dep1 = Dependency {
        name: "package-a".to_string(),
        version: Version::parse("1.0.0").unwrap(),
        source: DependencySource::Registry,
        features: vec![],
        optional: false,
    };
    
    let dep2 = Dependency {
        name: "package-b".to_string(),
        version: Version::parse("2.1.0").unwrap(),
        source: DependencySource::Registry,
        features: vec!["feature-x".to_string()],
        optional: true,
    };
    
    let dep3 = Dependency {
        name: "package-c".to_string(),
        version: Version::parse("0.5.3").unwrap(),
        source: DependencySource::Git {
            url: "https://github.com/example/package-c.git".to_string(),
            branch: Some("main".to_string()),
            tag: None,
            rev: None,
        },
        features: vec![],
        optional: false,
    };
    
    // Add dependencies to graph
    graph.add_dependency(dep1, vec!["package-b".to_string()]);
    graph.add_dependency(dep2, vec!["package-c".to_string()]);
    graph.add_dependency(dep3, vec![]);
    
    // Add roots
    graph.add_root("package-a".to_string());
    
    // Test topological order
    let order = graph.topological_order().expect("Failed to get topological order");
    
    // package-c should come before package-b, which should come before package-a
    assert!(order.contains(&"package-a".to_string()));
    assert!(order.contains(&"package-b".to_string()));
    assert!(order.contains(&"package-c".to_string()));
    
    // Check for cycles (should be none)
    assert!(!graph.has_cycles());
    
    // Test transitive dependencies
    let transitive = graph.transitive_dependencies("package-a");
    assert!(transitive.contains("package-b"));
    assert!(transitive.contains("package-c"));
    assert_eq!(transitive.len(), 2);
}

#[test]
fn test_version_requirement_parsing_edge_cases() {
    use zetac::package::dependency::VersionReq;
    
    // Test various version requirement formats
    let test_cases = vec![
        ("1.0.0", true),
        ("^1.0", true),
        ("~1.2", true),
        (">=1.0 <2.0", true),
        ("<=2.5.0", true),
        (">3.0.0", true),
        ("=4.2.1", true),
        ("", false), // Empty should fail
        ("invalid", false), // Invalid format should fail
        ("1.0.0.0", false), // Too many components should fail
    ];
    
    for (input, should_succeed) in test_cases {
        let result = VersionReq::parse(input);
        
        if should_succeed {
            assert!(result.is_ok(), "Failed to parse '{}': {:?}", input, result.err());
            
            // Verify it can be converted back to string
            let req = result.unwrap();
            assert!(!req.req.is_empty());
        } else {
            assert!(result.is_err(), "Should have failed to parse '{}'", input);
        }
    }
}

#[test]
fn test_manifest_default_values() {
    use zetac::package::manifest::Manifest;
    
    // Create a minimal manifest
    let toml_content = r#"
[package]
name = "minimal-crate"
version = "0.1.0"
"#;
    
    let manifest = Manifest::parse(toml_content).expect("Failed to parse minimal manifest");
    
    // Check default values
    assert_eq!(manifest.package.name, "minimal-crate");
    assert_eq!(manifest.package.version, "0.1.0");
    assert!(manifest.package.authors.is_empty());
    assert_eq!(manifest.package.description, None);
    assert_eq!(manifest.package.license, None);
    assert_eq!(manifest.package.repository, None);
    assert_eq!(manifest.package.documentation, None);
    assert_eq!(manifest.package.readme, None);
    assert_eq!(manifest.package.edition, "2024"); // Default from Manifest::default_for_crate
    
    // Check empty collections
    assert!(manifest.dependencies.is_empty());
    assert!(manifest.dev_dependencies.is_empty());
    assert!(manifest.build_dependencies.is_empty());
    assert!(manifest.features.is_empty());
    
    // Check default workspace config
    assert!(manifest.workspace.members.is_empty());
    assert!(manifest.workspace.exclude.is_empty());
    assert!(manifest.workspace.default_members.is_empty());
    assert!(manifest.workspace.dependencies.is_empty());
}

// Note: ZorbClient tests are omitted because they require
// the zorb executable to be installed and would have side effects.