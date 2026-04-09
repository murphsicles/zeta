//! Dependency management and version resolution
//!
//! This module provides structures and algorithms for:
//! - Version requirement parsing and validation
//! - Dependency graph building
//! - Version constraint resolution

use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::fmt;

/// A version requirement (e.g., "1.2.3", "^1.0", "~1.2", ">=1.0 <2.0")
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VersionReq {
    /// The parsed requirement string
    pub req: String,
    
    /// Optional comparator (e.g., "^", "~", ">=", etc.)
    pub comparator: Option<Comparator>,
    
    /// Version components
    pub version: Version,
}

/// Version comparator
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comparator {
    /// Exact version (=)
    Exact,
    /// Compatible with (^)
    Compatible,
    /// Approximately equivalent to (~)
    Approximate,
    /// Greater than (>)
    Greater,
    /// Greater than or equal to (>=)
    GreaterOrEqual,
    /// Less than (<)
    Less,
    /// Less than or equal to (<=)
    LessOrEqual,
}

/// Semantic version
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Version {
    /// Major version
    pub major: u64,
    
    /// Minor version
    pub minor: u64,
    
    /// Patch version
    pub patch: u64,
    
    /// Pre-release identifier
    pub pre: Option<String>,
    
    /// Build metadata
    pub build: Option<String>,
}

/// A resolved dependency with its version
#[derive(Debug, Clone)]
pub struct Dependency {
    /// Dependency name
    pub name: String,
    
    /// Resolved version
    pub version: Version,
    
    /// Source of the dependency (crates.io, git, path)
    pub source: DependencySource,
    
    /// Features to enable
    pub features: Vec<String>,
    
    /// Whether this is optional
    pub optional: bool,
}

/// Dependency source
#[derive(Debug, Clone)]
pub enum DependencySource {
    /// From crates.io registry
    Registry,
    
    /// From Git repository
    Git {
        /// Repository URL
        url: String,
        /// Optional branch
        branch: Option<String>,
        /// Optional tag
        tag: Option<String>,
        /// Optional revision
        rev: Option<String>,
    },
    
    /// Local path
    Path(String),
}

/// Dependency graph node
#[derive(Debug, Clone)]
pub struct DependencyNode {
    /// The dependency
    pub dependency: Dependency,
    
    /// Direct dependencies of this node
    pub dependencies: Vec<String>,
}

/// Dependency graph
#[derive(Debug, Clone)]
pub struct DependencyGraph {
    /// Nodes in the graph, keyed by package name
    pub nodes: HashMap<String, DependencyNode>,
    
    /// Root dependencies
    pub roots: Vec<String>,
}

impl VersionReq {
    /// Parse a version requirement string
    pub fn parse(req: &str) -> Result<Self, String> {
        let req = req.trim();
        
        if req.is_empty() {
            return Err("Empty version requirement".to_string());
        }
        
        // Parse comparator if present
        let (comparator, version_str) = if req.starts_with('^') {
            (Some(Comparator::Compatible), &req[1..])
        } else if req.starts_with('~') {
            (Some(Comparator::Approximate), &req[1..])
        } else if req.starts_with('>') {
            if req.starts_with(">=") {
                (Some(Comparator::GreaterOrEqual), &req[2..])
            } else {
                (Some(Comparator::Greater), &req[1..])
            }
        } else if req.starts_with('<') {
            if req.starts_with("<=") {
                (Some(Comparator::LessOrEqual), &req[2..])
            } else {
                (Some(Comparator::Less), &req[1..])
            }
        } else if req.starts_with('=') {
            (Some(Comparator::Exact), &req[1..])
        } else {
            (None, req)
        };
        
        // Parse version
        let version = Version::parse(version_str)?;
        
        Ok(Self {
            req: req.to_string(),
            comparator,
            version,
        })
    }
    
    /// Check if a version satisfies this requirement
    pub fn matches(&self, version: &Version) -> bool {
        match self.comparator {
            None => version == &self.version,
            Some(Comparator::Exact) => version == &self.version,
            Some(Comparator::Compatible) => {
                // ^1.2.3 means >=1.2.3 <2.0.0
                version.major == self.version.major
                    && version >= &self.version
                    && version.major == self.version.major
            }
            Some(Comparator::Approximate) => {
                // ~1.2.3 means >=1.2.3 <1.3.0
                version.major == self.version.major
                    && version.minor == self.version.minor
                    && version >= &self.version
            }
            Some(Comparator::Greater) => version > &self.version,
            Some(Comparator::GreaterOrEqual) => version >= &self.version,
            Some(Comparator::Less) => version < &self.version,
            Some(Comparator::LessOrEqual) => version <= &self.version,
        }
    }
}

impl Version {
    /// Parse a semantic version string
    pub fn parse(version: &str) -> Result<Self, String> {
        let version = version.trim();
        
        if version.is_empty() {
            return Err("Empty version string".to_string());
        }
        
        // Split version and pre-release/build metadata
        let mut parts = version.splitn(2, '-');
        let version_part = parts.next().unwrap();
        let pre_part = parts.next();
        
        let mut pre = None;
        let mut build = None;
        
        if let Some(pre_str) = pre_part {
            // Split pre-release and build metadata
            let mut pre_parts = pre_str.splitn(2, '+');
            pre = Some(pre_parts.next().unwrap().to_string());
            build = pre_parts.next().map(|s| s.to_string());
        }
        
        // Parse version components
        let components: Vec<&str> = version_part.split('.').collect();
        
        if components.is_empty() || components.len() > 3 {
            return Err(format!("Invalid version format: {}", version));
        }
        
        let major = components[0]
            .parse()
            .map_err(|_| format!("Invalid major version: {}", components[0]))?;
        
        let minor = if components.len() > 1 {
            components[1]
                .parse()
                .map_err(|_| format!("Invalid minor version: {}", components[1]))?
        } else {
            0
        };
        
        let patch = if components.len() > 2 {
            components[2]
                .parse()
                .map_err(|_| format!("Invalid patch version: {}", components[2]))?
        } else {
            0
        };
        
        Ok(Self {
            major,
            minor,
            patch,
            pre,
            build,
        })
    }
    
    /// Create a new version
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
            pre: None,
            build: None,
        }
    }
    
    /// Convert to string
    pub fn to_string(&self) -> String {
        let mut result = format!("{}.{}.{}", self.major, self.minor, self.patch);
        
        if let Some(pre) = &self.pre {
            result.push_str(&format!("-{}", pre));
        }
        
        if let Some(build) = &self.build {
            result.push_str(&format!("+{}", build));
        }
        
        result
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.major.cmp(&other.major) {
            Ordering::Equal => match self.minor.cmp(&other.minor) {
                Ordering::Equal => match self.patch.cmp(&other.patch) {
                    Ordering::Equal => {
                        // Versions with pre-release identifiers have lower precedence
                        match (&self.pre, &other.pre) {
                            (None, None) => Ordering::Equal,
                            (Some(_), None) => Ordering::Less,
                            (None, Some(_)) => Ordering::Greater,
                            (Some(a), Some(b)) => a.cmp(b),
                        }
                    }
                    patch_ord => patch_ord,
                },
                minor_ord => minor_ord,
            },
            major_ord => major_ord,
        }
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl fmt::Display for VersionReq {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.req)
    }
}

impl DependencyGraph {
    /// Create a new empty dependency graph
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            roots: Vec::new(),
        }
    }
    
    /// Add a dependency to the graph
    pub fn add_dependency(&mut self, dependency: Dependency, deps: Vec<String>) {
        let node = DependencyNode {
            dependency,
            dependencies: deps,
        };
        
        self.nodes.insert(node.dependency.name.clone(), node);
    }
    
    /// Add a root dependency
    pub fn add_root(&mut self, name: String) {
        if !self.roots.contains(&name) {
            self.roots.push(name);
        }
    }
    
    /// Get topological order of dependencies
    pub fn topological_order(&self) -> Result<Vec<String>, String> {
        let mut visited = HashSet::new();
        let mut temp_visited = HashSet::new();
        let mut order = Vec::new();
        
        for root in &self.roots {
            self.visit(root, &mut visited, &mut temp_visited, &mut order)?;
        }
        
        Ok(order)
    }
    
    /// Visit a node for topological sort
    fn visit(
        &self,
        name: &str,
        visited: &mut HashSet<String>,
        temp_visited: &mut HashSet<String>,
        order: &mut Vec<String>,
    ) -> Result<(), String> {
        if visited.contains(name) {
            return Ok(());
        }
        
        if temp_visited.contains(name) {
            return Err(format!("Cyclic dependency detected involving {}", name));
        }
        
        temp_visited.insert(name.to_string());
        
        if let Some(node) = self.nodes.get(name) {
            for dep in &node.dependencies {
                self.visit(dep, visited, temp_visited, order)?;
            }
        }
        
        temp_visited.remove(name);
        visited.insert(name.to_string());
        order.push(name.to_string());
        
        Ok(())
    }
    
    /// Check if the graph has any cycles
    pub fn has_cycles(&self) -> bool {
        self.topological_order().is_err()
    }
    
    /// Get all transitive dependencies of a package
    pub fn transitive_dependencies(&self, package: &str) -> HashSet<String> {
        let mut deps = HashSet::new();
        let mut stack = vec![package.to_string()];
        
        while let Some(current) = stack.pop() {
            if let Some(node) = self.nodes.get(&current) {
                for dep in &node.dependencies {
                    if deps.insert(dep.clone()) {
                        stack.push(dep.clone());
                    }
                }
            }
        }
        
        deps
    }
}