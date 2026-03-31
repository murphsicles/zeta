//! Module resolution for Zorb module system
//! Handles `use` statements and module file discovery

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::frontend::ast::AstNode;
use crate::frontend::parser::top_level::parse_zeta;

/// Module cache entry
#[derive(Debug, Clone)]
pub struct Module {
    /// Module name (e.g., "option", "result")
    pub name: String,
    /// Full path to module file
    pub path: PathBuf,
    /// AST nodes from the module
    pub asts: Vec<AstNode>,
    /// Public items exported from this module
    pub exports: HashMap<String, AstNode>,
}

/// Module resolver that loads and caches modules
pub struct ModuleResolver {
    /// Root directory for module search (usually workspace root)
    root_dir: PathBuf,
    /// Cache of loaded modules
    modules: HashMap<String, Module>,
    /// Current import scope
    imports: HashMap<String, String>, // name -> full path
}

impl ModuleResolver {
    pub fn new(root_dir: impl AsRef<Path>) -> Self {
        Self {
            root_dir: root_dir.as_ref().to_path_buf(),
            modules: HashMap::new(),
            imports: HashMap::new(),
        }
    }

    /// Resolve a use statement path to a module file
    /// For `use a::b::c::Item;`, we resolve `a::b::c` to a module file
    pub fn resolve_use_path(&mut self, path: &[String]) -> Result<PathBuf, String> {
        // For `use a::b::c::Item;`, path is ["a", "b", "c", "Item"]
        // We need to resolve ["a", "b", "c"] to a module file
        if path.is_empty() {
            return Err("Empty path".to_string());
        }

        // Check for Zeta compiler imports: `use zeta::frontend::ast::AstNode;`
        // These are for self-compilation and don't correspond to .z files
        if !path.is_empty() && path[0] == "zeta" {
            // For self-compilation, we need to handle zeta:: imports specially
            // These refer to the compiler's own data structures, not .z files
            // We'll create a virtual module path for these
            let module_name = path.join("_");
            let virtual_path = PathBuf::from(format!("zeta_virtual/{}", module_name));
            return Ok(virtual_path);
        }

        // Check for standard library imports: `use zorb::std::option::Option;`
        if path.len() >= 2 && path[0] == "zorb" && path[1] == "std" {
            // This is a standard library import
            // For `use zorb::std::option::Option`, path is ["zorb", "std", "option", "Option"]
            // We need to resolve ["zorb", "std", "option"] to zorb/std/option.z

            let module_path = if path.len() > 2 {
                &path[..path.len() - 1] // Remove the last component (the item name)
            } else {
                path // Keep all if only 2 components
            };

            let mut stdlib_path = PathBuf::from("zorb/std");

            // Add module path components (skip "zorb" and "std" which we already handled)
            for component in module_path.iter().skip(2) {
                stdlib_path.push(component);
            }

            // Try with .z extension
            let mut z_path = stdlib_path.clone();
            z_path.set_extension("z");

            if z_path.exists() {
                return Ok(z_path);
            }

            // Try as directory with mod.z
            let mut mod_path = stdlib_path.clone();
            mod_path.push("mod.z");

            if mod_path.exists() {
                return Ok(mod_path);
            }

            return Err(format!(
                "Standard library module not found: {}",
                path.join("::")
            ));
        }

        // Try to resolve the path without the last component
        let module_path = if path.len() > 1 {
            &path[..path.len() - 1]
        } else {
            // Single component like `use Item;` - look in current directory
            &[]
        };

        let mut file_path = self.root_dir.clone();

        for component in module_path {
            file_path.push(component);
        }

        // Try with .z extension
        let mut z_path = file_path.clone();
        z_path.set_extension("z");

        if z_path.exists() {
            return Ok(z_path);
        }

        // Try as directory with mod.z
        let mut mod_path = file_path.clone();
        mod_path.push("mod.z");

        if mod_path.exists() {
            return Ok(mod_path);
        }

        // If no file found, try the full path (for backward compatibility)
        if !module_path.is_empty() {
            let mut full_path = self.root_dir.clone();
            for component in path {
                full_path.push(component);
            }
            let mut full_z_path = full_path.clone();
            full_z_path.set_extension("z");

            if full_z_path.exists() {
                return Ok(full_z_path);
            }
        }

        Err(format!("Module not found: {}", path.join("::")))
    }

    /// Load a module from file
    pub fn load_module(&mut self, path: &Path) -> Result<&Module, String> {
        let path_str = path.to_string_lossy().to_string();
        println!("[MODULE RESOLVER] Loading module from: {}", path.display());

        // Check cache first
        if self.modules.contains_key(&path_str) {
            println!("[MODULE RESOLVER] Module already cached");
            return Ok(self.modules.get(&path_str).unwrap());
        }

        // Read and parse file
        let content = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read module file {}: {}", path.display(), e))?;

        println!(
            "[MODULE RESOLVER] Read {} bytes from {}",
            content.len(),
            path.display()
        );

        let (remaining, asts) = parse_zeta(&content)
            .map_err(|e| format!("Failed to parse module {}: {:?}", path.display(), e))?;

        println!(
            "[MODULE RESOLVER] Parsed {} ASTs from {}",
            asts.len(),
            path.display()
        );

        if !remaining.is_empty() {
            return Err(format!(
                "Incomplete parse in module {}: {}",
                path.display(),
                remaining
            ));
        }

        // Extract exports (public items)
        let mut exports = HashMap::new();
        for ast in &asts {
            match ast {
                AstNode::EnumDef { name, pub_, .. } => {
                    if *pub_ {
                        println!("[MODULE RESOLVER] Found public enum export: {}", name);
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        println!(
                            "[MODULE RESOLVER] Found private enum (not exported): {}",
                            name
                        );
                    }
                }
                AstNode::StructDef { name, pub_, .. } => {
                    if *pub_ {
                        println!("[MODULE RESOLVER] Found public struct export: {}", name);
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        println!(
                            "[MODULE RESOLVER] Found private struct (not exported): {}",
                            name
                        );
                    }
                }
                AstNode::FuncDef { name, pub_, .. } => {
                    if *pub_ {
                        println!("[MODULE RESOLVER] Found public func export: {}", name);
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        println!(
                            "[MODULE RESOLVER] Found private func (not exported): {}",
                            name
                        );
                    }
                }
                AstNode::TypeAlias { name, pub_, .. } => {
                    if *pub_ {
                        println!("[MODULE RESOLVER] Found public type alias export: {}", name);
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        println!(
                            "[MODULE RESOLVER] Found private type alias (not exported): {}",
                            name
                        );
                    }
                }
                AstNode::ConstDef { name, pub_, .. } => {
                    if *pub_ {
                        println!("[MODULE RESOLVER] Found public const export: {}", name);
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        println!(
                            "[MODULE RESOLVER] Found private const (not exported): {}",
                            name
                        );
                    }
                }
                _ => {}
            }
        }

        // Get module name from file path
        let name = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        let module = Module {
            name: name.clone(),
            path: path.to_path_buf(),
            asts,
            exports,
        };

        self.modules.insert(path_str.clone(), module);
        println!("[MODULE RESOLVER] Module loaded successfully: {}", name);
        Ok(self.modules.get(&path_str).unwrap())
    }

    /// Process a use statement and add imports to scope
    /// Returns the module's ASTs that need to be registered
    pub fn process_use_statement(&mut self, path: &[String]) -> Result<Vec<AstNode>, String> {
        // Resolve path to file
        let module_path = self.resolve_use_path(path)?;

        // Load the module
        let module = self.load_module(&module_path)?.clone();

        // Add all exports to import scope
        for name in module.exports.keys() {
            let full_path = path.join("::") + "::" + name;
            self.imports.insert(name.clone(), full_path);
        }

        // Return the module's ASTs so they can be registered
        Ok(module.asts)
    }

    /// Look up an item in the import scope
    pub fn lookup(&self, name: &str) -> Option<&str> {
        self.imports.get(name).map(|s| s.as_str())
    }

    /// Get all loaded modules (for debugging)
    pub fn get_modules(&self) -> &HashMap<String, Module> {
        &self.modules
    }

    /// Get all imports (for debugging)
    pub fn get_imports(&self) -> &HashMap<String, String> {
        &self.imports
    }
}
