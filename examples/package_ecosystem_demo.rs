//! Demonstration of the Zeta Package Ecosystem for v0.3.36
//!
//! This example shows how to use the new package ecosystem features:
//! 1. Manifest parsing and creation
//! 2. Dependency resolution
//! 3. Workspace management
//! 4. Real-world workflows

use zetac::package::manifest::Manifest;
use zetac::package::dependency::{Version, VersionReq};
use zetac::package::resolver::{PackageRegistry, PackageMetadata, DependencyResolver};

fn main() {
    println!("=== Zeta Package Ecosystem Demo (v0.3.36) ===\n");
    
    demo_manifest_parsing();
    demo_dependency_management();
    demo_version_requirements();
    demo_real_world_workflow();
    
    println!("=== Demo Complete ===");
}

fn demo_manifest_parsing() {
    println!("1. Manifest Parsing and Creation");
    println!("=".repeat(40));
    
    // Create a manifest programmatically
    let mut manifest = Manifest::default_for_crate("demo-library");
    manifest.package.description = Some("A demonstration library for Zeta package ecosystem".to_string());
    manifest.package.authors = vec!["Zeta Developer <dev@zeta-lang.org>".to_string()];
    manifest.package.version = "0.1.0".to_string();
    manifest.package.license = Some("MIT OR Apache-2.0".to_string());
    manifest.package.repository = Some("https://github.com/zeta-lang/demo-library".to_string());
    
    // Add dependencies
    manifest.add_dependency("serde", "^1.0");
    manifest.add_dependency("tokio", ">=1.0 <2.0");
    
    // Add features
    let mut features = std::collections::HashMap::new();
    features.insert("default".to_string(), vec!["serde".to_string()]);
    features.insert("async".to_string(), vec!["tokio".to_string()]);
    manifest.features = features;
    
    // Convert to TOML
    let toml = manifest.to_toml().expect("Failed to serialize manifest");
    println!("Generated manifest TOML:\n{}", toml);
    
    // Parse it back
    let parsed = Manifest::parse(&toml).expect("Failed to parse manifest");
    println!("\nParsed back successfully!");
    println!("- Package name: {}", parsed.package.name);
    println!("- Version: {}", parsed.package.version);
    println!("- Dependencies: {}", parsed.dependencies.len());
    println!("- Features: {}", parsed.features.len());
    println!();
}

fn demo_dependency_management() {
    println!("2. Dependency Management");
    println!("=".repeat(40));
    
    // Create a simple registry
    let mut registry = PackageRegistry::new();
    
    // Add some packages
    let metadata = PackageMetadata {
        dependencies: std::collections::HashMap::new(),
        features: std::collections::HashMap::new(),
    };
    
    registry.add_package("serde", Version::parse("1.0.0").unwrap(), metadata.clone());
    registry.add_package("serde", Version::parse("1.0.1").unwrap(), metadata.clone());
    registry.add_package("serde", Version::parse("1.1.0").unwrap(), metadata.clone());
    registry.add_package("tokio", Version::parse("1.0.0").unwrap(), metadata.clone());
    registry.add_package("tokio", Version::parse("1.1.0").unwrap(), metadata);
    
    // Create a resolver
    let resolver = DependencyResolver::new(registry);
    
    // Create a manifest with dependencies
    let mut manifest = Manifest::default_for_crate("demo-app");
    manifest.add_dependency("serde", "^1.0.0");
    manifest.add_dependency("tokio", "^1.0");
    
    println!("Resolving dependencies for demo-app...");
    
    // Try to resolve (this will fail because we don't have actual dependency metadata)
    // but it demonstrates the API
    match resolver.resolve(&manifest) {
        Ok(result) => {
            println!("Successfully resolved {} packages", result.package_count());
            println!("Generated lockfile would contain {} entries", result.versions.len());
        }
        Err(e) => {
            println!("Resolution failed (expected for demo): {}", e);
            println!("This is expected because we're using a minimal demo registry.");
        }
    }
    println!();
}

fn demo_version_requirements() {
    println!("3. Version Requirement Matching");
    println!("=".repeat(40));
    
    let test_cases = vec![
        ("1.2.3", "1.2.3", true, "Exact version match"),
        ("^1.2.3", "1.3.0", true, "Compatible version (^)"),
        ("^1.2.3", "2.0.0", false, "Major version bump not compatible (^)"),
        ("~1.2.3", "1.2.9", true, "Approximate version (~)"),
        ("~1.2.3", "1.3.0", false, "Minor bump not approximate (~)"),
        (">=1.0.0", "1.5.0", true, "Greater than or equal"),
        ("<2.0.0", "1.9.9", true, "Less than"),
    ];
    
    for (req_str, version_str, should_match, description) in test_cases {
        let req = VersionReq::parse(req_str).expect("Failed to parse requirement");
        let version = Version::parse(version_str).expect("Failed to parse version");
        let matches = req.matches(&version);
        
        let status = if matches == should_match { "✓" } else { "✗" };
        println!("{} {} -> {}: {}", status, req_str, version_str, description);
    }
    println!();
}

fn demo_real_world_workflow() {
    println!("4. Real-World Development Workflow");
    println!("=".repeat(40));
    
    println!("Typical workflow with Zeta Package Manager:");
    println!();
    println!("1. Create a new project:");
    println!("   $ zeta new my-project");
    println!("   $ cd my-project");
    println!();
    println!("2. Add dependencies:");
    println!("   $ zeta add serde");
    println!("   $ zeta add tokio --features full");
    println!();
    println!("3. Write your code in src/");
    println!();
    println!("4. Build and test:");
    println!("   $ zeta build");
    println!("   $ zeta test");
    println!();
    println!("5. Manage dependencies:");
    println!("   $ zeta outdated    # Check for updates");
    println!("   $ zeta update      # Update dependencies");
    println!();
    println!("6. Publish to registry:");
    println!("   $ zeta publish");
    println!();
    println!("7. Use in other projects:");
    println!("   $ zeta add my-project");
    println!();
    
    println!("The package ecosystem enables:");
    println!("- 📦 Crate management with Cargo.toml-style manifests");
    println!("- 🔗 Dependency resolution with semantic versioning");
    println!("- 🏢 Workspace support for multi-crate projects");
    println!("- 🔄 Version locking for reproducible builds");
    println!("- 📚 Registry integration via Zorb");
    println!("- 🚀 Real-world development workflows");
}