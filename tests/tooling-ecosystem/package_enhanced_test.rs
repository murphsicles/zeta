//! Enhanced package manager tests

#[cfg(test)]
mod tests {
    use zetac::package::enhanced::*;
    use zetac::package::{Manifest, Dependency, DependencyResolver};
    use std::collections::HashMap;

    /// Test dependency graph creation
    #[test]
    fn test_dependency_graph() {
        // Create a simple manifest
        let mut manifest = Manifest {
            package: zetac::package::manifest::PackageInfo {
                name: "my_project".to_string(),
                version: "0.1.0".to_string(),
                authors: vec!["Test Author".to_string()],
                description: "Test project".to_string(),
                license: Some("MIT".to_string()),
                ..Default::default()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            features: HashMap::new(),
            workspace: None,
        };

        // Add some dependencies
        manifest.dependencies.insert(
            "zeta-std".to_string(),
            Dependency::Version("^1.0".to_string()),
        );
        manifest.dependencies.insert(
            "serde".to_string(),
            Dependency::Version("^1.0".to_string()),
        );

        // Create a mock resolver
        let resolver = DependencyResolver::new();

        // Create dependency graph
        let graph = DependencyGraph::from_manifest(&manifest, &resolver);
        
        assert_eq!(graph.root().name, "my_project");
        assert_eq!(graph.root().version, "0.1.0");
        assert_eq!(graph.root().dependencies.len(), 2);
    }

    /// Test vulnerability database
    #[test]
    fn test_vulnerability_database() {
        let mut db = VulnerabilityDatabase::new();
        
        // Add a vulnerability
        let vuln = Vulnerability {
            id: "CVE-2021-12345".to_string(),
            severity: SeverityLevel::High,
            description: "Test vulnerability".to_string(),
            affected_versions: "<=1.2.3".to_string(),
            fix_version: Some("1.2.4".to_string()),
            references: vec!["https://example.com".to_string()],
        };
        
        db.add_vulnerability("vulnerable_crate".to_string(), vuln);
        
        // Check for vulnerabilities
        let vulns = db.check_package("vulnerable_crate", "1.2.3");
        assert_eq!(vulns.len(), 1);
        assert_eq!(vulns[0].id, "CVE-2021-12345");
        
        // Check non-vulnerable package
        let vulns = db.check_package("safe_crate", "1.0.0");
        assert!(vulns.is_empty());
    }

    /// Test signature verifier
    #[test]
    fn test_signature_verifier() {
        let verifier = SignatureVerifier::new();
        
        // Test verification (simulated)
        let status = verifier.verify_package("zeta-std", "1.0.0");
        assert_eq!(status, SignatureStatus::Valid);
        
        let status = verifier.verify_package("untrusted-crate", "1.0.0");
        assert_eq!(status, SignatureStatus::Invalid);
        
        let status = verifier.verify_package("unsigned-crate", "1.0.0");
        assert_eq!(status, SignatureStatus::Unsigned);
    }

    /// Test Graphviz DOT generation
    #[test]
    fn test_graphviz_generation() {
        let mut manifest = Manifest {
            package: zetac::package::manifest::PackageInfo {
                name: "test_project".to_string(),
                version: "0.1.0".to_string(),
                ..Default::default()
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            features: HashMap::new(),
            workspace: None,
        };

        manifest.dependencies.insert(
            "dep1".to_string(),
            Dependency::Version("^1.0".to_string()),
        );

        let resolver = DependencyResolver::new();
        let graph = DependencyGraph::from_manifest(&manifest, &resolver);
        
        let dot = graph.to_dot();
        
        // Check that DOT contains expected elements
        assert!(dot.contains("digraph dependencies"));
        assert!(dot.contains("test_project"));
        assert!(dot.contains("dep1"));
        assert!(dot.contains("->"));
    }

    /// Test package signer
    #[test]
    fn test_package_signer() {
        let signer = PackageSigner::new(
            "test_signer".to_string(),
            b"private_key_data".to_vec(),
        );
        
        assert_eq!(signer.signer(), "test_signer");
        
        // Test signing (simulated)
        let signature = signer.sign_package(std::path::Path::new("test.pkg")).unwrap();
        assert!(signature.starts_with(b"SIGNATURE:"));
        assert!(signature.contains(b"test_signer"));
    }
}
