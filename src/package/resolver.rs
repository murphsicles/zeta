//! Dependency resolution algorithms
//!
//! This module provides algorithms for resolving version constraints
//! and building dependency graphs.

use std::collections::HashMap;
use crate::package::dependency::{Dependency, DependencyGraph, DependencySource, Version, VersionReq};
use crate::package::manifest::{Manifest, DependencySpec};

/// Registry for available package versions
pub struct PackageRegistry {
    /// Available packages and their versions
    packages: HashMap<String, Vec<Version>>,
    
    /// Package metadata (dependencies for each version)
    metadata: HashMap<String, HashMap<Version, PackageMetadata>>,
}

/// Package metadata for a specific version
#[derive(Clone)]
pub struct PackageMetadata {
    /// Dependencies of this package version
    pub dependencies: HashMap<String, VersionReq>,
    
    /// Features available in this package
    pub features: HashMap<String, Vec<String>>,
}

/// Dependency resolver
pub struct DependencyResolver {
    /// Package registry
    registry: PackageRegistry,
    
    /// Locked versions (for reproducible builds)
    locked_versions: HashMap<String, Version>,
}

/// Resolution result
pub struct ResolutionResult {
    /// Resolved dependency graph
    pub graph: DependencyGraph,
    
    /// Selected versions for each package
    pub versions: HashMap<String, Version>,
    
    /// Any warnings during resolution
    pub warnings: Vec<String>,
}

impl PackageRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            packages: HashMap::new(),
            metadata: HashMap::new(),
        }
    }
    
    /// Add a package version to the registry
    pub fn add_package(&mut self, name: &str, version: Version, metadata: PackageMetadata) {
        self.packages
            .entry(name.to_string())
            .or_insert_with(Vec::new)
            .push(version.clone());
        
        self.metadata
            .entry(name.to_string())
            .or_insert_with(HashMap::new)
            .insert(version, metadata);
    }
    
    /// Get available versions for a package
    pub fn get_versions(&self, name: &str) -> Option<&Vec<Version>> {
        self.packages.get(name)
    }
    
    /// Get metadata for a specific package version
    pub fn get_metadata(&self, name: &str, version: &Version) -> Option<&PackageMetadata> {
        self.metadata.get(name)?.get(version)
    }
    
    /// Find the latest version matching a requirement
    pub fn find_best_match(&self, name: &str, requirement: &VersionReq) -> Option<Version> {
        let versions = self.packages.get(name)?;
        
        versions
            .iter()
            .filter(|v| requirement.matches(v))
            .max()
            .cloned()
    }
}

impl DependencyResolver {
    /// Create a new resolver with the given registry
    pub fn new(registry: PackageRegistry) -> Self {
        Self {
            registry,
            locked_versions: HashMap::new(),
        }
    }
    
    /// Resolve dependencies for a manifest
    pub fn resolve(&self, manifest: &Manifest) -> Result<ResolutionResult, String> {
        let mut graph = DependencyGraph::new();
        let mut versions = HashMap::new();
        let mut warnings = Vec::new();
        
        // Add root package
        let root_name = manifest.package.name.clone();
        let root_version = Version::parse(&manifest.package.version)
            .map_err(|e| format!("Invalid root package version: {}", e))?;
        
        versions.insert(root_name.clone(), root_version.clone());
        graph.add_root(root_name.clone());
        
        // Process all dependencies
        let all_deps = manifest.all_dependencies();
        
        for (dep_name, dep_spec) in all_deps {
            self.resolve_dependency(
                &dep_name,
                dep_spec,
                &mut graph,
                &mut versions,
                &mut warnings,
            )?;
        }
        
        // Check for cycles
        if graph.has_cycles() {
            return Err("Cyclic dependencies detected".to_string());
        }
        
        Ok(ResolutionResult {
            graph,
            versions,
            warnings,
        })
    }
    
    /// Resolve a single dependency
    fn resolve_dependency(
        &self,
        name: &str,
        spec: &DependencySpec,
        graph: &mut DependencyGraph,
        versions: &mut HashMap<String, Version>,
        warnings: &mut Vec<String>,
    ) -> Result<(), String> {
        // Parse version requirement
        let version_req = VersionReq::parse(spec.version_req())
            .map_err(|e| format!("Invalid version requirement for {}: {}", name, e))?;
        
        // Check if we already have a version for this package
        if let Some(existing_version) = versions.get(name) {
            // Verify the existing version satisfies the new requirement
            if !version_req.matches(existing_version) {
                return Err(format!(
                    "Version conflict for {}: required {}, but {} is already selected",
                    name, version_req, existing_version
                ));
            }
            return Ok(());
        }
        
        // Find the best matching version
        let selected_version = self.registry
            .find_best_match(name, &version_req)
            .ok_or_else(|| format!("No version of {} matches {}", name, version_req))?;
        
        versions.insert(name.to_string(), selected_version.clone());
        
        // Get metadata for the selected version
        let metadata = self.registry
            .get_metadata(name, &selected_version)
            .ok_or_else(|| format!("No metadata for {} version {}", name, selected_version))?;
        
        // Create dependency node
        let dependency = Dependency {
            name: name.to_string(),
            version: selected_version.clone(),
            source: self.determine_source(spec),
            features: self.extract_features(spec),
            optional: self.is_optional(spec),
        };
        
        // Add to graph with its dependencies
        let deps: Vec<String> = metadata.dependencies.keys().cloned().collect();
        graph.add_dependency(dependency, deps.clone());
        
        // Recursively resolve dependencies
        for (dep_name, dep_req) in &metadata.dependencies {
            self.resolve_dependency(
                dep_name,
                &DependencySpec::Simple(dep_req.req.clone()),
                graph,
                versions,
                warnings,
            )?;
        }
        
        Ok(())
    }
    
    /// Determine the source of a dependency
    fn determine_source(&self, spec: &DependencySpec) -> DependencySource {
        match spec {
            DependencySpec::Detailed(details) => {
                if let Some(git) = &details.git {
                    DependencySource::Git {
                        url: git.clone(),
                        branch: details.branch.clone(),
                        tag: details.tag.clone(),
                        rev: details.rev.clone(),
                    }
                } else if let Some(path) = &details.path {
                    DependencySource::Path(path.clone())
                } else {
                    DependencySource::Registry
                }
            }
            _ => DependencySource::Registry,
        }
    }
    
    /// Extract features from a dependency specification
    fn extract_features(&self, spec: &DependencySpec) -> Vec<String> {
        match spec {
            DependencySpec::Detailed(details) => details.features.clone(),
            _ => Vec::new(),
        }
    }
    
    /// Check if a dependency is optional
    fn is_optional(&self, spec: &DependencySpec) -> bool {
        match spec {
            DependencySpec::Detailed(details) => details.optional,
            _ => false,
        }
    }
    
    /// Lock versions for reproducible builds
    pub fn lock_versions(&mut self, result: &ResolutionResult) {
        self.locked_versions = result.versions.clone();
    }
    
    /// Get locked versions
    pub fn get_locked_versions(&self) -> &HashMap<String, Version> {
        &self.locked_versions
    }
}

impl ResolutionResult {
    /// Generate a lock file (similar to Cargo.lock)
    pub fn generate_lockfile(&self) -> String {
        let mut lockfile = String::new();
        
        lockfile.push_str("# This file is automatically generated by Zeta Package Manager\n");
        lockfile.push_str("# It is not intended for manual editing.\n\n");
        lockfile.push_str("[[package]]\n");
        
        // Sort packages for deterministic output
        let mut packages: Vec<(&String, &Version)> = self.versions.iter().collect();
        packages.sort_by(|a, b| a.0.cmp(b.0));
        
        for (name, version) in packages {
            lockfile.push_str(&format!("name = \"{}\"\n", name));
            lockfile.push_str(&format!("version = \"{}\"\n", version));
            
            // Add dependencies if any
            if let Some(node) = self.graph.nodes.get(name) {
                if !node.dependencies.is_empty() {
                    lockfile.push_str("dependencies = [\n");
                    for dep in &node.dependencies {
                        if let Some(dep_version) = self.versions.get(dep) {
                            lockfile.push_str(&format!("  \"{} {}\",\n", dep, dep_version));
                        }
                    }
                    lockfile.push_str("]\n");
                }
            }
            
            lockfile.push_str("\n");
        }
        
        lockfile
    }
    
    /// Check if all dependencies are satisfied
    pub fn is_satisfied(&self) -> bool {
        // Check that all dependencies in the graph have versions
        for node_name in self.graph.nodes.keys() {
            if !self.versions.contains_key(node_name) {
                return false;
            }
        }
        true
    }
    
    /// Get the number of resolved packages
    pub fn package_count(&self) -> usize {
        self.versions.len()
    }
}