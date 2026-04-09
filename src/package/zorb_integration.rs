//! Zorb package manager integration
//!
//! This module provides integration with the Zorb package manager
//! for package discovery, installation, and management.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::fs;
use std::process::Command;

use crate::package::manifest::Manifest;
use crate::package::dependency::Version;
use crate::package::resolver::{PackageRegistry, PackageMetadata};

/// Zorb package manager client
pub struct ZorbClient {
    /// Zorb executable path
    zorb_path: PathBuf,
    
    /// Cache directory for downloaded packages
    cache_dir: PathBuf,
    
    /// Local registry index
    registry_index: HashMap<String, Vec<PackageInfo>>,
}

/// Package information from Zorb registry
pub struct PackageInfo {
    /// Package name
    pub name: String,
    
    /// Latest version
    pub latest_version: Version,
    
    /// All available versions
    pub versions: Vec<Version>,
    
    /// Package description
    pub description: Option<String>,
    
    /// Repository URL
    pub repository: Option<String>,
    
    /// Documentation URL
    pub documentation: Option<String>,
    
    /// Download count
    pub downloads: u64,
}

impl ZorbClient {
    /// Create a new Zorb client
    pub fn new() -> Result<Self, String> {
        // Try to find zorb executable
        let zorb_path = Self::find_zorb_executable()
            .ok_or_else(|| "Zorb executable not found in PATH".to_string())?;
        
        // Create cache directory
        let cache_dir = dirs::cache_dir()
            .ok_or_else(|| "Could not determine cache directory".to_string())?
            .join("zeta")
            .join("zorb");
        
        fs::create_dir_all(&cache_dir)
            .map_err(|e| format!("Failed to create cache directory: {}", e))?;
        
        Ok(Self {
            zorb_path,
            cache_dir,
            registry_index: HashMap::new(),
        })
    }
    
    /// Find zorb executable in PATH
    fn find_zorb_executable() -> Option<PathBuf> {
        if cfg!(target_os = "windows") {
            which::which("zorb.exe").ok()
        } else {
            which::which("zorb").ok()
        }
    }
    
    /// Update the local registry index
    pub fn update_index(&mut self) -> Result<(), String> {
        println!("Updating Zorb registry index...");
        
        // Run zorb update command
        let output = Command::new(&self.zorb_path)
            .arg("update")
            .output()
            .map_err(|e| format!("Failed to run zorb update: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb update failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        // Load index from cache
        self.load_index()?;
        
        println!("Registry index updated successfully");
        Ok(())
    }
    
    /// Load registry index from cache
    fn load_index(&mut self) -> Result<(), String> {
        let index_path = self.cache_dir.join("index.json");
        
        if !index_path.exists() {
            // Index doesn't exist yet, will be created on first update
            return Ok(());
        }
        
        let index_content = fs::read_to_string(&index_path)
            .map_err(|e| format!("Failed to read index: {}", e))?;
        
        // TODO: Parse actual index format
        // For now, we'll create a dummy index
        self.registry_index = HashMap::new();
        
        Ok(())
    }
    
    /// Search for packages in the registry
    pub fn search(&self, query: &str) -> Result<Vec<PackageInfo>, String> {
        let mut results = Vec::new();
        
        // Simple substring search in package names
        for (name, packages) in &self.registry_index {
            if name.contains(query) {
                if let Some(latest) = packages.first() {
                    results.push(latest.clone());
                }
            }
        }
        
        // If index is empty, try to search using zorb command
        if results.is_empty() {
            return self.search_via_command(query);
        }
        
        results.sort_by(|a, b| b.downloads.cmp(&a.downloads));
        Ok(results)
    }
    
    /// Search using zorb command-line tool
    fn search_via_command(&self, query: &str) -> Result<Vec<PackageInfo>, String> {
        let output = Command::new(&self.zorb_path)
            .arg("search")
            .arg(query)
            .output()
            .map_err(|e| format!("Failed to run zorb search: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb search failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        let output_str = String::from_utf8_lossy(&output.stdout);
        
        // Parse command output
        let mut results = Vec::new();
        
        for line in output_str.lines() {
            if line.trim().is_empty() || line.starts_with("NAME") {
                continue;
            }
            
            // Parse line like: "zeta-serde   0.1.0   Serialization library for Zeta"
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                let name = parts[0].to_string();
                let version_str = parts[1];
                
                if let Ok(version) = Version::parse(version_str) {
                    let description = if parts.len() > 2 {
                        Some(parts[2..].join(" "))
                    } else {
                        None
                    };
                    
                    results.push(PackageInfo {
                        name,
                        latest_version: version.clone(),
                        versions: vec![version],
                        description,
                        repository: None,
                        documentation: None,
                        downloads: 0,
                    });
                }
            }
        }
        
        Ok(results)
    }
    
    /// Install a package
    pub fn install(&self, name: &str, version: Option<&Version>) -> Result<PathBuf, String> {
        println!("Installing package: {}...", name);
        
        let mut command = Command::new(&self.zorb_path);
        command.arg("install").arg(name);
        
        if let Some(ver) = version {
            command.arg("--version").arg(ver.to_string());
        }
        
        let output = command
            .output()
            .map_err(|e| format!("Failed to run zorb install: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb install failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        // Determine installation path
        let install_dir = self.cache_dir
            .join("packages")
            .join(name)
            .join(version.map(|v| v.to_string()).unwrap_or_else(|| "latest".to_string()));
        
        if !install_dir.exists() {
            return Err(format!("Package installed but not found at: {}", 
                install_dir.display()));
        }
        
        println!("Package installed successfully at: {}", install_dir.display());
        Ok(install_dir)
    }
    
    /// Add a dependency to a manifest
    pub fn add_dependency(
        &self,
        manifest_path: &Path,
        name: &str,
        version_req: &str,
    ) -> Result<(), String> {
        // Load existing manifest
        let mut manifest = Manifest::load_from_file(manifest_path)
            .map_err(|e| format!("Failed to load manifest: {}", e))?;
        
        // Add dependency
        manifest.add_dependency(name, version_req);
        
        // Save updated manifest
        let toml_content = manifest.to_toml()
            .map_err(|e| format!("Failed to serialize manifest: {}", e))?;
        
        fs::write(manifest_path, toml_content)
            .map_err(|e| format!("Failed to write manifest: {}", e))?;
        
        println!("Added dependency: {} {}", name, version_req);
        Ok(())
    }
    
    /// Remove a dependency from a manifest
    pub fn remove_dependency(&self, manifest_path: &Path, name: &str) -> Result<(), String> {
        // Load existing manifest
        let mut manifest = Manifest::load_from_file(manifest_path)
            .map_err(|e| format!("Failed to load manifest: {}", e))?;
        
        // Remove dependency from all sections
        manifest.dependencies.remove(name);
        manifest.dev_dependencies.remove(name);
        manifest.build_dependencies.remove(name);
        
        // Save updated manifest
        let toml_content = manifest.to_toml()
            .map_err(|e| format!("Failed to serialize manifest: {}", e))?;
        
        fs::write(manifest_path, toml_content)
            .map_err(|e| format!("Failed to write manifest: {}", e))?;
        
        println!("Removed dependency: {}", name);
        Ok(())
    }
    
    /// Build a package with its dependencies
    pub fn build_package(&self, manifest_path: &Path) -> Result<(), String> {
        println!("Building package...");
        
        let output = Command::new(&self.zorb_path)
            .arg("build")
            .arg("--manifest-path")
            .arg(manifest_path)
            .output()
            .map_err(|e| format!("Failed to run zorb build: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb build failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        println!("Build completed successfully");
        Ok(())
    }
    
    /// Publish a package to the registry
    pub fn publish(&self, manifest_path: &Path) -> Result<(), String> {
        println!("Publishing package...");
        
        let output = Command::new(&self.zorb_path)
            .arg("publish")
            .arg("--manifest-path")
            .arg(manifest_path)
            .output()
            .map_err(|e| format!("Failed to run zorb publish: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb publish failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        println!("Package published successfully");
        Ok(())
    }
    
    /// Generate documentation for a package
    pub fn generate_docs(&self, manifest_path: &Path) -> Result<(), String> {
        println!("Generating documentation...");
        
        let output = Command::new(&self.zorb_path)
            .arg("doc")
            .arg("--manifest-path")
            .arg(manifest_path)
            .arg("--open")
            .output()
            .map_err(|e| format!("Failed to run zorb doc: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb doc failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        println!("Documentation generated successfully");
        Ok(())
    }
    
    /// Run tests for a package
    pub fn run_tests(&self, manifest_path: &Path) -> Result<(), String> {
        println!("Running tests...");
        
        let output = Command::new(&self.zorb_path)
            .arg("test")
            .arg("--manifest-path")
            .arg(manifest_path)
            .output()
            .map_err(|e| format!("Failed to run zorb test: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb test failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        println!("Tests completed successfully");
        Ok(())
    }
    
    /// Check for outdated dependencies
    pub fn check_outdated(&self, manifest_path: &Path) -> Result<Vec<OutdatedDependency>, String> {
        println!("Checking for outdated dependencies...");
        
        let output = Command::new(&self.zorb_path)
            .arg("outdated")
            .arg("--manifest-path")
            .arg(manifest_path)
            .output()
            .map_err(|e| format!("Failed to run zorb outdated: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb outdated failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        // Parse output
        let output_str = String::from_utf8_lossy(&output.stdout);
        let mut outdated = Vec::new();
        
        for line in output_str.lines().skip(1) { // Skip header
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 4 {
                let name = parts[0].to_string();
                let current = parts[1].to_string();
                let latest = parts[3].to_string();
                
                outdated.push(OutdatedDependency {
                    name,
                    current_version: current,
                    latest_version: latest,
                });
            }
        }
        
        Ok(outdated)
    }
    
    /// Update dependencies to latest versions
    pub fn update_dependencies(&self, manifest_path: &Path) -> Result<(), String> {
        println!("Updating dependencies...");
        
        let output = Command::new(&self.zorb_path)
            .arg("update")
            .arg("--manifest-path")
            .arg(manifest_path)
            .output()
            .map_err(|e| format!("Failed to run zorb update: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("zorb update failed: {}", 
                String::from_utf8_lossy(&output.stderr)));
        }
        
        println!("Dependencies updated successfully");
        Ok(())
    }
    
    /// Convert Zorb registry to PackageRegistry
    pub fn to_package_registry(&self) -> PackageRegistry {
        let mut registry = PackageRegistry::new();
        
        for (name, packages) in &self.registry_index {
            for package in packages {
                // Create metadata for each version
                let metadata = PackageMetadata {
                    dependencies: HashMap::new(), // TODO: Load actual dependencies
                    features: HashMap::new(),     // TODO: Load actual features
                };
                
                // Add all versions
                for version in &package.versions {
                    registry.add_package(name, version.clone(), metadata.clone());
                }
            }
        }
        
        registry
    }
}

/// Information about an outdated dependency
pub struct OutdatedDependency {
    /// Dependency name
    pub name: String,
    
    /// Current version
    pub current_version: String,
    
    /// Latest available version
    pub latest_version: String,
}

impl PackageInfo {
    /// Create a new package info
    pub fn new(name: &str, version: Version) -> Self {
        Self {
            name: name.to_string(),
            latest_version: version.clone(),
            versions: vec![version],
            description: None,
            repository: None,
            documentation: None,
            downloads: 0,
        }
    }
}

impl Clone for PackageInfo {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            latest_version: self.latest_version.clone(),
            versions: self.versions.clone(),
            description: self.description.clone(),
            repository: self.repository.clone(),
            documentation: self.documentation.clone(),
            downloads: self.downloads,
        }
    }
}