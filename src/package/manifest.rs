//! Crate manifest parsing and representation
//!
//! This module handles parsing of Cargo.toml-style manifest files
//! and provides structures to represent crate metadata.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::fs;

/// Represents a crate manifest (similar to Cargo.toml)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    /// Package metadata section
    pub package: Package,
    
    /// Dependencies section
    #[serde(default)]
    pub dependencies: HashMap<String, DependencySpec>,
    
    /// Dev dependencies section
    #[serde(rename = "dev-dependencies", default)]
    pub dev_dependencies: HashMap<String, DependencySpec>,
    
    /// Build dependencies section
    #[serde(rename = "build-dependencies", default)]
    pub build_dependencies: HashMap<String, DependencySpec>,
    
    /// Features section
    #[serde(default)]
    pub features: HashMap<String, Vec<String>>,
    
    /// Workspace configuration
    #[serde(default)]
    pub workspace: WorkspaceConfig,
}

/// Package metadata
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Package {
    /// Package name
    pub name: String,
    
    /// Package version (semver)
    pub version: String,
    
    /// Package authors
    #[serde(default)]
    pub authors: Vec<String>,
    
    /// Package description
    #[serde(default)]
    pub description: Option<String>,
    
    /// License
    #[serde(default)]
    pub license: Option<String>,
    
    /// Repository URL
    #[serde(default)]
    pub repository: Option<String>,
    
    /// Documentation URL
    #[serde(default)]
    pub documentation: Option<String>,
    
    /// Readme file
    #[serde(default)]
    pub readme: Option<String>,
    
    /// Edition (e.g., "2024")
    #[serde(default)]
    pub edition: String,
}

/// Dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencySpec {
    /// Simple version requirement
    Simple(String),
    
    /// Detailed dependency specification
    Detailed(DependencyDetails),
}

/// Detailed dependency information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyDetails {
    /// Version requirement
    pub version: String,
    
    /// Optional Git repository
    #[serde(default)]
    pub git: Option<String>,
    
    /// Optional Git branch
    #[serde(default)]
    pub branch: Option<String>,
    
    /// Optional Git tag
    #[serde(default)]
    pub tag: Option<String>,
    
    /// Optional Git revision
    #[serde(default)]
    pub rev: Option<String>,
    
    /// Optional path to local dependency
    #[serde(default)]
    pub path: Option<String>,
    
    /// Optional features to enable
    #[serde(default)]
    pub features: Vec<String>,
    
    /// Whether this is an optional dependency
    #[serde(default)]
    pub optional: bool,
    
    /// Default features to disable
    #[serde(rename = "default-features", default)]
    pub default_features: bool,
}

/// Workspace configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    /// Members of the workspace
    #[serde(default)]
    pub members: Vec<String>,
    
    /// Excluded members from the workspace
    #[serde(default)]
    pub exclude: Vec<String>,
    
    /// Default members
    #[serde(rename = "default-members", default)]
    pub default_members: Vec<String>,
    
    /// Workspace dependencies
    #[serde(default)]
    pub dependencies: HashMap<String, DependencySpec>,
}

impl Manifest {
    /// Parse a manifest from a TOML string
    pub fn parse(toml_content: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(toml_content)
    }
    
    /// Load a manifest from a file
    pub fn load_from_file(path: &Path) -> Result<Self, Box<dyn std::error::Error>> {
        let content = fs::read_to_string(path)?;
        Self::parse(&content).map_err(|e| e.into())
    }
    
    /// Create a default manifest for a new crate
    pub fn default_for_crate(name: &str) -> Self {
        Self {
            package: Package {
                name: name.to_string(),
                version: "0.1.0".to_string(),
                authors: vec![],
                description: None,
                license: None,
                repository: None,
                documentation: None,
                readme: None,
                edition: "2024".to_string(),
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            build_dependencies: HashMap::new(),
            features: HashMap::new(),
            workspace: WorkspaceConfig::default(),
        }
    }
    
    /// Add a dependency to the manifest
    pub fn add_dependency(&mut self, name: &str, version_req: &str) {
        self.dependencies.insert(
            name.to_string(),
            DependencySpec::Simple(version_req.to_string()),
        );
    }
    
    /// Add a detailed dependency to the manifest
    pub fn add_detailed_dependency(&mut self, name: &str, details: DependencyDetails) {
        self.dependencies.insert(
            name.to_string(),
            DependencySpec::Detailed(details),
        );
    }
    
    /// Get all dependencies (including dev and build dependencies)
    pub fn all_dependencies(&self) -> HashMap<String, &DependencySpec> {
        let mut all = HashMap::new();
        
        for (name, spec) in &self.dependencies {
            all.insert(name.clone(), spec);
        }
        
        for (name, spec) in &self.dev_dependencies {
            all.insert(name.clone(), spec);
        }
        
        for (name, spec) in &self.build_dependencies {
            all.insert(name.clone(), spec);
        }
        
        all
    }
    
    /// Convert to TOML string
    pub fn to_toml(&self) -> Result<String, toml::ser::Error> {
        toml::to_string(self)
    }
}

impl DependencySpec {
    /// Get the version requirement
    pub fn version_req(&self) -> &str {
        match self {
            DependencySpec::Simple(version) => version,
            DependencySpec::Detailed(details) => &details.version,
        }
    }
    
    /// Check if this is a Git dependency
    pub fn is_git(&self) -> bool {
        match self {
            DependencySpec::Detailed(details) => details.git.is_some(),
            _ => false,
        }
    }
    
    /// Check if this is a local path dependency
    pub fn is_path(&self) -> bool {
        match self {
            DependencySpec::Detailed(details) => details.path.is_some(),
            _ => false,
        }
    }
}