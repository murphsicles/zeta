//! Enhanced package manager features
//!
//! This module provides advanced package management features:
//! - Dependency graph visualization
//! - Vulnerability scanning
//! - Package signing and verification
//! - Advanced dependency resolution

use std::collections::{HashMap, HashSet};
use std::path::Path;
use serde::{Deserialize, Serialize};

use super::{Manifest, DependencyResolver};
use crate::package::manifest::WorkspaceConfig;

/// Dependency graph node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyNode {
    /// Package name
    pub name: String,
    /// Package version
    pub version: String,
    /// Dependencies
    pub dependencies: Vec<String>,
    /// Reverse dependencies (packages that depend on this one)
    pub reverse_dependencies: Vec<String>,
    /// Vulnerability information
    pub vulnerabilities: Vec<Vulnerability>,
    /// Package signature status
    pub signature_status: SignatureStatus,
}

/// Vulnerability information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Vulnerability {
    /// Vulnerability ID (e.g., CVE-2021-12345)
    pub id: String,
    /// Severity level
    pub severity: SeverityLevel,
    /// Description
    pub description: String,
    /// Affected versions
    pub affected_versions: String,
    /// Fix version (if available)
    pub fix_version: Option<String>,
    /// References
    pub references: Vec<String>,
}

/// Severity level
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum SeverityLevel {
    /// Critical severity
    Critical,
    /// High severity
    High,
    /// Medium severity
    Medium,
    /// Low severity
    Low,
    /// Informational
    Info,
}

/// Signature status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum SignatureStatus {
    /// Package is signed and signature is valid
    Valid,
    /// Package is signed but signature is invalid
    Invalid,
    /// Package is not signed
    Unsigned,
    /// Signature verification failed
    VerificationFailed,
    /// Signature status unknown
    Unknown,
}

/// Dependency graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyGraph {
    /// Nodes by package name
    nodes: HashMap<String, DependencyNode>,
    /// Root package
    root: String,
}

impl DependencyGraph {
    /// Create a new dependency graph from a manifest
    pub fn from_manifest(manifest: &Manifest, resolver: &DependencyResolver) -> Self {
        let mut nodes = HashMap::new();
        let root = manifest.package.name.clone();
        
        // Add root node
        nodes.insert(root.clone(), DependencyNode {
            name: manifest.package.name.clone(),
            version: manifest.package.version.clone(),
            dependencies: manifest.dependencies.keys().cloned().collect(),
            reverse_dependencies: Vec::new(),
            vulnerabilities: Vec::new(),
            signature_status: SignatureStatus::Unknown,
        });
        
        // Build graph recursively
        Self::build_graph(&root, manifest, resolver, &mut nodes);
        
        Self { nodes, root }
    }
    
    /// Build graph recursively
    fn build_graph(
        current: &str,
        manifest: &Manifest,
        resolver: &DependencyResolver,
        nodes: &mut HashMap<String, DependencyNode>,
    ) {
        for (dep_name, _) in &manifest.dependencies {
            if !nodes.contains_key(dep_name) {
                // Try to resolve the dependency
                // Note: In a real implementation, this would call resolver.resolve_dependency
                // For now, we'll create a dummy manifest
                let dep_manifest = Manifest {
                    package: crate::package::manifest::Package {
                        name: dep_name.clone(),
                        version: "1.0.0".to_string(),
                        authors: vec!["Author".to_string()],
                        description: Some("Dependency".to_string()),
                        ..Default::default()
                    },
                    dependencies: std::collections::HashMap::new(),
                    dev_dependencies: std::collections::HashMap::new(),
                    build_dependencies: std::collections::HashMap::new(),
                    features: std::collections::HashMap::new(),
                    workspace: WorkspaceConfig::default(),
                };
                
                let node = DependencyNode {
                    name: dep_manifest.package.name.clone(),
                    version: dep_manifest.package.version.clone(),
                    dependencies: dep_manifest.dependencies.keys().cloned().collect(),
                    reverse_dependencies: vec![current.to_string()],
                    vulnerabilities: Vec::new(),
                    signature_status: SignatureStatus::Unknown,
                };
                
                nodes.insert(dep_name.clone(), node);
                
                // Recursively build graph for this dependency
                Self::build_graph(dep_name, &dep_manifest, resolver, nodes);
            } else {
                // Update reverse dependencies
                if let Some(node) = nodes.get_mut(dep_name) {
                    if !node.reverse_dependencies.contains(&current.to_string()) {
                        node.reverse_dependencies.push(current.to_string());
                    }
                }
            }
        }
    }
    
    /// Get node by name
    pub fn get_node(&self, name: &str) -> Option<&DependencyNode> {
        self.nodes.get(name)
    }
    
    /// Get all nodes
    pub fn nodes(&self) -> &HashMap<String, DependencyNode> {
        &self.nodes
    }
    
    /// Get root node
    pub fn root(&self) -> &DependencyNode {
        self.nodes.get(&self.root).unwrap()
    }
    
    /// Generate Graphviz DOT format for visualization
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("digraph dependencies {\n");
        dot.push_str("  rankdir=LR;\n");
        dot.push_str("  node [shape=box, style=filled, fillcolor=lightblue];\n\n");
        
        // Add nodes
        for (name, node) in &self.nodes {
            let fillcolor = match node.signature_status {
                SignatureStatus::Valid => "lightgreen",
                SignatureStatus::Invalid => "lightcoral",
                SignatureStatus::Unsigned => "lightyellow",
                SignatureStatus::VerificationFailed => "orange",
                SignatureStatus::Unknown => "lightblue",
            };
            
            let vuln_color = if !node.vulnerabilities.is_empty() {
                let max_severity = node.vulnerabilities.iter()
                    .map(|v| v.severity)
                    .max()
                    .unwrap_or(SeverityLevel::Info);
                
                match max_severity {
                    SeverityLevel::Critical => "red",
                    SeverityLevel::High => "darkorange",
                    SeverityLevel::Medium => "yellow",
                    SeverityLevel::Low => "lightyellow",
                    SeverityLevel::Info => "white",
                }
            } else {
                "white"
            };
            
            dot.push_str(&format!(
                "  \"{}\" [label=\"{}\\n{}\\n{}\", fillcolor=\"{}\", color=\"{}\"];\n",
                name,
                node.name,
                node.version,
                if node.vulnerabilities.is_empty() {
                    "No vulns".to_string()
                } else {
                    format!("{} vulns", node.vulnerabilities.len())
                },
                fillcolor,
                vuln_color
            ));
        }
        
        dot.push_str("\n");
        
        // Add edges
        for (name, node) in &self.nodes {
            for dep in &node.dependencies {
                if self.nodes.contains_key(dep) {
                    dot.push_str(&format!("  \"{}\" -> \"{}\";\n", name, dep));
                }
            }
        }
        
        dot.push_str("}\n");
        dot
    }
    
    /// Check for vulnerabilities
    pub fn check_vulnerabilities(&mut self, vulnerability_db: &VulnerabilityDatabase) {
        for (_, node) in self.nodes.iter_mut() {
            node.vulnerabilities = vulnerability_db.check_package(&node.name, &node.version);
        }
    }
    
    /// Get all vulnerabilities in the graph
    pub fn get_all_vulnerabilities(&self) -> Vec<(&DependencyNode, &Vulnerability)> {
        let mut vulns = Vec::new();
        
        for node in self.nodes.values() {
            for vuln in &node.vulnerabilities {
                vulns.push((node, vuln));
            }
        }
        
        vulns.sort_by(|(_, a), (_, b)| b.severity.cmp(&a.severity));
        vulns
    }
    
    /// Check signature status for all packages
    pub fn check_signatures(&mut self, signature_verifier: &SignatureVerifier) {
        for (_, node) in self.nodes.iter_mut() {
            node.signature_status = signature_verifier.verify_package(&node.name, &node.version);
        }
    }
}

/// Vulnerability database
pub struct VulnerabilityDatabase {
    /// Vulnerabilities by package name
    vulnerabilities: HashMap<String, Vec<Vulnerability>>,
}

impl VulnerabilityDatabase {
    /// Create a new vulnerability database
    pub fn new() -> Self {
        Self {
            vulnerabilities: HashMap::new(),
        }
    }
    
    /// Add a vulnerability
    pub fn add_vulnerability(&mut self, package_name: String, vulnerability: Vulnerability) {
        self.vulnerabilities
            .entry(package_name)
            .or_insert_with(Vec::new)
            .push(vulnerability);
    }
    
    /// Check package for vulnerabilities
    pub fn check_package(&self, package_name: &str, version: &str) -> Vec<Vulnerability> {
        if let Some(vulns) = self.vulnerabilities.get(package_name) {
            vulns.iter()
                .filter(|v| Self::version_matches(version, &v.affected_versions))
                .cloned()
                .collect()
        } else {
            Vec::new()
        }
    }
    
    /// Check if version matches affected versions range
    fn version_matches(version: &str, affected_versions: &str) -> bool {
        // Simple version matching - in real implementation would use semver
        affected_versions.contains(version)
    }
    
    /// Load from file
    pub fn load_from_file(path: &Path) -> Result<Self, std::io::Error> {
        let content = std::fs::read_to_string(path)?;
        let vulnerabilities: HashMap<String, Vec<Vulnerability>> = serde_json::from_str(&content)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        
        Ok(Self { vulnerabilities })
    }
    
    /// Save to file
    pub fn save_to_file(&self, path: &Path) -> Result<(), std::io::Error> {
        let content = serde_json::to_string_pretty(&self.vulnerabilities)?;
        std::fs::write(path, content)
    }
}

/// Signature verifier
pub struct SignatureVerifier {
    /// Public keys for verification
    public_keys: HashMap<String, Vec<u8>>,
    /// Trusted signers
    trusted_signers: HashSet<String>,
}

impl SignatureVerifier {
    /// Create a new signature verifier
    pub fn new() -> Self {
        Self {
            public_keys: HashMap::new(),
            trusted_signers: HashSet::new(),
        }
    }
    
    /// Add a public key
    pub fn add_public_key(&mut self, signer: String, public_key: Vec<u8>) {
        self.public_keys.insert(signer.clone(), public_key);
        self.trusted_signers.insert(signer);
    }
    
    /// Verify package signature
    pub fn verify_package(&self, package_name: &str, version: &str) -> SignatureStatus {
        // In a real implementation, this would:
        // 1. Download package signature
        // 2. Verify signature with public key
        // 3. Check if signer is trusted
        
        // For now, simulate verification
        if package_name == "zeta-std" {
            SignatureStatus::Valid
        } else if package_name.starts_with("untrusted-") {
            SignatureStatus::Invalid
        } else if package_name.starts_with("unsigned-") {
            SignatureStatus::Unsigned
        } else {
            SignatureStatus::Unknown
        }
    }
    
    /// Check if signer is trusted
    pub fn is_trusted_signer(&self, signer: &str) -> bool {
        self.trusted_signers.contains(signer)
    }
}

/// Package signing utility
pub struct PackageSigner {
    /// Private key for signing
    private_key: Vec<u8>,
    /// Signer identifier
    signer: String,
}

impl PackageSigner {
    /// Create a new package signer
    pub fn new(signer: String, private_key: Vec<u8>) -> Self {
        Self { private_key, signer }
    }
    
    /// Sign a package
    pub fn sign_package(&self, package_path: &Path) -> Result<Vec<u8>, String> {
        // In a real implementation, this would:
        // 1. Hash the package contents
        // 2. Sign the hash with private key
        // 3. Return the signature
        
        // For now, return a dummy signature
        let mut signature = Vec::new();
        signature.extend_from_slice(b"SIGNATURE:");
        signature.extend_from_slice(&self.signer.as_bytes());
        signature.extend_from_slice(b":");
        signature.extend_from_slice(&self.private_key[..8]); // First 8 bytes
        
        Ok(signature)
    }
    
    /// Get signer identifier
    pub fn signer(&self) -> &str {
        &self.signer
    }
}