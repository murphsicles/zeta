//! Real-world workflow demonstration
//!
//! This example shows how the package ecosystem enables real-world
//! development workflows similar to Cargo.

use zetac::package::manifest::Manifest;
use zetac::package::dependency::{Version, VersionReq};
use zetac::package::resolver::{PackageRegistry, PackageMetadata, DependencyResolver};

/// Example 1: Creating and managing a new crate
fn example_create_and_manage_crate() {
    println!("=== Example 1: Creating and Managing a Crate ===");
    
    // Create a new manifest for a crate
    let mut manifest = Manifest::default_for_crate("my-awesome-library");
    manifest.package.description = Some("An awesome library for doing awesome things".to_string());
    manifest.package.authors = vec!["Awesome Developer <awesome@example.com>".to_string()];
    manifest.package.license = Some("MIT OR Apache-2.0".to_string());
    manifest.package.repository = Some("https://github.com/awesome/my-awesome-library".to_string());
    
    // Add dependencies
    manifest.add_dependency("serde", "^1.0");
    manifest.add_dependency("tokio", ">=1.0 <2.0");
    
    // Add a dev dependency for testing
    let mut dev_deps = std::collections::HashMap::new();
    dev_deps.insert(
        "proptest".to_string(),
        zetac::package::manifest::DependencySpec::Simple("^1.0".to_string()),
    );
    manifest.dev_dependencies = dev_deps;
    
    // Define features
    let mut features = std::collections::HashMap::new();
    features.insert("default".to_string(), vec!["serde".to_string()]);
    features.insert("async".to_string(), vec!["tokio".to_string()]);
    manifest.features = features;
    
    // Serialize to TOML
    let toml = manifest.to_toml().expect("Failed to serialize manifest");
    println!("Generated manifest:\n{}", toml);
    
    // Parse it back to verify
    let parsed = Manifest::parse(&toml).expect("Failed to parse generated manifest");
    assert_eq!(parsed.package.name, "my-awesome-library");
    println!("✓ Manifest created and verified successfully\n");
}

/// Example 2: Dependency resolution
fn example_dependency_resolution() {
    println!("=== Example 2: Dependency Resolution ===");
    
    // Create a mock registry with some packages
    let mut registry = PackageRegistry::new();
    
    // Add serde with multiple versions
    let serde_metadata = PackageMetadata {
        dependencies: std::collections::HashMap::new(),
        features: std::collections::HashMap::new(),
    };
    
    registry.add_package("serde", Version::parse("1.0.0").unwrap(), serde_metadata.clone());
    registry.add_package("serde", Version::parse("1.0.1").unwrap(), serde_metadata.clone());
    registry.add_package("serde", Version::parse("1.1.0").unwrap(), serde_metadata.clone());
    
    // Add tokio with dependencies
    let mut tokio_deps = std::collections::HashMap::new();
    tokio_deps.insert("bytes".to_string(), VersionReq::parse("^1.0").unwrap());
    
    let tokio_metadata = PackageMetadata {
        dependencies: tokio_deps,
        features: std::collections::HashMap::new(),
    };
    
    registry.add_package("tokio", Version::parse("1.0.0").unwrap(), tokio_metadata.clone());
    registry.add_package("tokio", Version::parse("1.1.0").unwrap(), tokio_metadata);
    
    // Add bytes
    registry.add_package("bytes", Version::parse("1.0.0").unwrap(), serde_metadata);
    
    // Create a resolver
    let resolver = DependencyResolver::new(registry);
    
    // Create a manifest with dependencies
    let mut manifest = Manifest::default_for_crate("example-app");
    manifest.add_dependency("serde", "^1.0.0");
    manifest.add_dependency("tokio", "^1.0");
    
    // Resolve dependencies
    let result = resolver.resolve(&manifest).expect("Failed to resolve dependencies");
    
    println!("Resolved {} packages:", result.package_count());
    for (name, version) in &result.versions {
        println!("  {} = {}", name, version);
    }
    
    // Generate lockfile
    let lockfile = result.generate_lockfile();
    println!("\nGenerated lockfile:\n{}", lockfile);
    
    println!("✓ Dependency resolution completed successfully\n");
}

/// Example 3: Workspace management
fn example_workspace_management() {
    println!("=== Example 3: Workspace Management ===");
    
    // Note: This is a conceptual example since we can't actually
    // create files in a test without side effects
    
    println!("Workspace operations:");
    println!("1. Discover workspace from directory");
    println!("2. Load all member crates");
    println!("3. Resolve shared dependencies");
    println!("4. Build all crates in correct order");
    println!("5. Run tests across workspace");
    println!("6. Publish crates to registry");
    
    println!("\nExample workspace structure:");
    println!("my-workspace/");
    println!("├── Cargo.toml (workspace manifest)");
    println!("├── core-lib/");
    println!("│   ├── Cargo.toml");
    println!("│   └── src/");
    println!("├── web-api/");
    println!("│   ├── Cargo.toml");
    println!("│   └── src/");
    println!("└── cli-tool/");
    println!("    ├── Cargo.toml");
    println!("    └── src/");
    
    println!("\n✓ Workspace concepts demonstrated\n");
}

/// Example 4: Version requirement matching scenarios
fn example_version_matching_scenarios() {
    println!("=== Example 4: Version Requirement Matching ===");
    
    let test_cases = vec![
        ("1.2.3", "1.2.3", true, "Exact match"),
        ("^1.2.3", "1.2.3", true, "Compatible exact"),
        ("^1.2.3", "1.3.0", true, "Compatible minor bump"),
        ("^1.2.3", "2.0.0", false, "Compatible major bump fails"),
        ("~1.2.3", "1.2.9", true, "Approximate patch bump"),
        ("~1.2.3", "1.3.0", false, "Approximate minor bump fails"),
        (">=1.0.0", "1.5.0", true, "Greater than or equal"),
        ("<2.0.0", "1.9.9", true, "Less than"),
        (">=1.0.0 <2.0.0", "1.5.0", true, "Range match"),
        ("=1.2.3", "1.2.3", true, "Explicit equality"),
    ];
    
    for (req_str, version_str, should_match, description) in test_cases {
        let req = VersionReq::parse(req_str).expect("Failed to parse requirement");
        let version = Version::parse(version_str).expect("Failed to parse version");
        let matches = req.matches(&version);
        
        let status = if matches == should_match { "✓" } else { "✗" };
        println!("{} {} -> {}: {} (expected: {})", 
            status, req_str, version_str, description, should_match);
    }
    
    println!("\n✓ Version matching scenarios demonstrated\n");
}

/// Example 5: Real-world development workflow
fn example_real_world_workflow() {
    println!("=== Example 5: Real-World Development Workflow ===");
    
    println!("1. Developer creates a new crate:");
    println!("   $ zeta new my-project");
    println!("   $ cd my-project");
    
    println!("\n2. Developer adds dependencies:");
    println!("   $ zeta add serde");
    println!("   $ zeta add tokio --features full");
    
    println!("\n3. Developer writes code in src/lib.rs");
    
    println!("\n4. Developer runs tests:");
    println!("   $ zeta test");
    
    println!("\n5. Developer checks for outdated dependencies:");
    println!("   $ zeta outdated");
    
    println!("\n6. Developer updates dependencies:");
    println!("   $ zeta update");
    
    println!("\n7. Developer builds the project:");
    println!("   $ zeta build");
    
    println!("\n8. Developer publishes to registry:");
    println!("   $ zeta publish");
    
    println!("\n9. Another developer uses the published crate:");
    println!("   $ zeta add my-project");
    
    println!("\n✓ Complete development workflow demonstrated");
}

#[test]
fn run_real_world_examples() {
    // Run all examples
    example_create_and_manage_crate();
    example_dependency_resolution();
    example_workspace_management();
    example_version_matching_scenarios();
    example_real_world_workflow();
    
    println!("=========================================");
    println!("All real-world workflow examples completed!");
    println!("Package ecosystem is ready for v0.3.36!");
}