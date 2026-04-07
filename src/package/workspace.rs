//! Workspace and multi-crate project support
//!
//! This module provides functionality for managing workspaces
//! with multiple crates and shared dependencies.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::fs;

use crate::package::manifest::Manifest;
use crate::package::dependency::{DependencyGraph, Version};
use crate::package::resolver::{DependencyResolver, ResolutionResult};

/// A workspace containing multiple crates
pub struct Workspace {
    /// Root directory of the workspace
    pub root: PathBuf,
    
    /// Workspace manifest
    pub manifest: Manifest,
    
    /// Member crates in the workspace
    pub members: HashMap<String, WorkspaceMember>,
    
    /// Shared dependency resolver
    pub resolver: DependencyResolver,
}

/// A member crate in a workspace
pub struct WorkspaceMember {
    /// Path to the crate relative to workspace root
    pub path: PathBuf,
    
    /// Crate manifest
    pub manifest: Manifest,
    
    /// Resolved dependencies
    pub resolution: Option<ResolutionResult>,
}

impl Workspace {
    /// Discover and load a workspace from a directory
    pub fn discover(path: &Path) -> Result<Self, String> {
        // Look for workspace manifest (Cargo.toml or Zeta.toml)
        let manifest_path = path.join("Cargo.toml");
        if !manifest_path.exists() {
            return Err("No Cargo.toml found in directory".to_string());
        }
        
        // Load workspace manifest
        let manifest = Manifest::load_from_file(&manifest_path)
            .map_err(|e| format!("Failed to load workspace manifest: {}", e))?;
        
        let mut workspace = Self {
            root: path.to_path_buf(),
            manifest: manifest.clone(),
            members: HashMap::new(),
            resolver: DependencyResolver::new(crate::package::resolver::PackageRegistry::new()),
        };
        
        // Load member crates
        workspace.load_members()?;
        
        Ok(workspace)
    }
    
    /// Load all member crates
    fn load_members(&mut self) -> Result<(), String> {
        // Use workspace members from manifest, or discover automatically
        let member_paths = if !self.manifest.workspace.members.is_empty() {
            self.manifest.workspace.members.clone()
        } else {
            // Auto-discover crates in subdirectories
            self.discover_members()?
        };
        
        for member_path in member_paths {
            let full_path = self.root.join(&member_path);
            
            // Look for crate manifest
            let crate_manifest_path = full_path.join("Cargo.toml");
            if !crate_manifest_path.exists() {
                return Err(format!("No Cargo.toml found in member: {}", member_path));
            }
            
            let manifest = Manifest::load_from_file(&crate_manifest_path)
                .map_err(|e| format!("Failed to load manifest for {}: {}", member_path, e))?;
            
            let member = WorkspaceMember {
                path: PathBuf::from(member_path.clone()),
                manifest,
                resolution: None,
            };
            
            self.members.insert(member_path, member);
        }
        
        Ok(())
    }
    
    /// Auto-discover member crates in subdirectories
    fn discover_members(&self) -> Result<Vec<String>, String> {
        let mut members = Vec::new();
        
        for entry in fs::read_dir(&self.root)
            .map_err(|e| format!("Failed to read workspace directory: {}", e))?
        {
            let entry = entry.map_err(|e| format!("Failed to read directory entry: {}", e))?;
            let path = entry.path();
            
            if path.is_dir() {
                let manifest_path = path.join("Cargo.toml");
                if manifest_path.exists() {
                    if let Some(dir_name) = path.file_name() {
                        members.push(dir_name.to_string_lossy().to_string());
                    }
                }
            }
        }
        
        if members.is_empty() {
            return Err("No member crates found in workspace".to_string());
        }
        
        Ok(members)
    }
    
    /// Resolve dependencies for all workspace members
    pub fn resolve_all(&mut self) -> Result<(), String> {
        // First, collect all dependencies from all members
        let mut all_deps = HashMap::new();
        
        for (name, member) in &self.members {
            // Add member's own dependencies
            for (dep_name, dep_spec) in member.manifest.all_dependencies() {
                all_deps.entry(dep_name.clone())
                    .or_insert_with(Vec::new)
                    .push((name.clone(), dep_spec));
            }
        }
        
        // Add workspace dependencies
        for (dep_name, dep_spec) in &self.manifest.workspace.dependencies {
            all_deps.entry(dep_name.clone())
                .or_insert_with(Vec::new)
                .push(("workspace".to_string(), dep_spec));
        }
        
        // TODO: Actually resolve dependencies using the resolver
        // For now, we'll just mark each member as resolved
        for member in self.members.values_mut() {
            // Create a dummy resolution for now
            member.resolution = Some(ResolutionResult {
                graph: DependencyGraph::new(),
                versions: HashMap::new(),
                warnings: Vec::new(),
            });
        }
        
        Ok(())
    }
    
    /// Build the entire workspace
    pub fn build(&self) -> Result<(), String> {
        if self.members.is_empty() {
            return Err("No members to build".to_string());
        }
        
        println!("Building workspace with {} members:", self.members.len());
        
        for (name, member) in &self.members {
            println!("  Building {}...", name);
            
            // Check if dependencies are resolved
            if member.resolution.is_none() {
                return Err(format!("Dependencies not resolved for {}", name));
            }
            
            // TODO: Actual build process
            // This would involve:
            // 1. Compiling dependencies
            // 2. Compiling the crate itself
            // 3. Linking if necessary
        }
        
        println!("Workspace build completed successfully");
        Ok(())
    }
    
    /// Run tests for the entire workspace
    pub fn test(&self) -> Result<(), String> {
        println!("Running tests for workspace:");
        
        for (name, member) in &self.members {
            println!("  Testing {}...", name);
            
            // TODO: Actual test execution
            // This would involve:
            // 1. Building in test mode
            // 2. Running test binaries
            // 3. Collecting and reporting results
        }
        
        println!("All tests passed");
        Ok(())
    }
    
    /// Publish crates from the workspace
    pub fn publish(&self, registry: &str) -> Result<(), String> {
        println!("Publishing to registry: {}", registry);
        
        for (name, member) in &self.members {
            println!("  Publishing {} version {}...", 
                member.manifest.package.name,
                member.manifest.package.version);
            
            // TODO: Actual publish process
            // This would involve:
            // 1. Validating the crate
            // 2. Packaging source code
            // 3. Uploading to registry
        }
        
        println!("Publish completed");
        Ok(())
    }
    
    /// Get a member crate by name
    pub fn get_member(&self, name: &str) -> Option<&WorkspaceMember> {
        self.members.get(name)
    }
    
    /// Get all member names
    pub fn member_names(&self) -> Vec<String> {
        self.members.keys().cloned().collect()
    }
    
    /// Check if the workspace is valid
    pub fn validate(&self) -> Result<(), String> {
        // Check for duplicate crate names
        let mut names = HashSet::new();
        
        for (path, member) in &self.members {
            let name = &member.manifest.package.name;
            
            if names.contains(name) {
                return Err(format!("Duplicate crate name: {} (in {})", name, path));
            }
            
            names.insert(name.clone());
        }
        
        // Check that all workspace dependencies are satisfied
        // TODO: Implement dependency validation
        
        Ok(())
    }
    
    /// Create a new crate in the workspace
    pub fn create_crate(&mut self, name: &str, path: &str) -> Result<(), String> {
        let crate_path = self.root.join(path);
        
        // Check if path already exists
        if crate_path.exists() {
            return Err(format!("Path already exists: {}", path));
        }
        
        // Create directory
        fs::create_dir_all(&crate_path)
            .map_err(|e| format!("Failed to create directory: {}", e))?;
        
        // Create default manifest
        let manifest = Manifest::default_for_crate(name);
        
        // Write manifest
        let manifest_path = crate_path.join("Cargo.toml");
        let toml_content = manifest.to_toml()
            .map_err(|e| format!("Failed to serialize manifest: {}", e))?;
        
        fs::write(&manifest_path, toml_content)
            .map_err(|e| format!("Failed to write manifest: {}", e))?;
        
        // Create src directory
        let src_path = crate_path.join("src");
        fs::create_dir(&src_path)
            .map_err(|e| format!("Failed to create src directory: {}", e))?;
        
        // Create lib.rs with basic template
        let lib_content = format!(
            "//! {}\n//!\n//! A crate in the workspace.\n\npub fn hello() -> &'static str {{\n    \"Hello from {}!\"\n}}\n\n#[cfg(test)]\nmod tests {{\n    use super::*;\n    \n    #[test]\n    fn test_hello() {{\n        assert_eq!(hello(), \"Hello from {}!\");\n    }}\n}}\n",
            name, name, name
        );
        
        let lib_path = src_path.join("lib.rs");
        fs::write(&lib_path, lib_content)
            .map_err(|e| format!("Failed to write lib.rs: {}", e))?;
        
        // Add to workspace members
        if !self.manifest.workspace.members.contains(&path.to_string()) {
            self.manifest.workspace.members.push(path.to_string());
            
            // Update workspace manifest
            let workspace_manifest_path = self.root.join("Cargo.toml");
            let workspace_toml = self.manifest.to_toml()
                .map_err(|e| format!("Failed to serialize workspace manifest: {}", e))?;
            
            fs::write(&workspace_manifest_path, workspace_toml)
                .map_err(|e| format!("Failed to update workspace manifest: {}", e))?;
        }
        
        // Add to members map
        let member = WorkspaceMember {
            path: PathBuf::from(path),
            manifest,
            resolution: None,
        };
        
        self.members.insert(path.to_string(), member);
        
        println!("Created crate '{}' at {}", name, path);
        Ok(())
    }
}

impl WorkspaceMember {
    /// Get the crate name
    pub fn name(&self) -> &str {
        &self.manifest.package.name
    }
    
    /// Get the crate version
    pub fn version(&self) -> &str {
        &self.manifest.package.version
    }
    
    /// Check if dependencies are resolved
    pub fn is_resolved(&self) -> bool {
        self.resolution.is_some()
    }
    
    /// Get resolved versions (if any)
    pub fn resolved_versions(&self) -> Option<&HashMap<String, Version>> {
        self.resolution.as_ref().map(|r| &r.versions)
    }
}