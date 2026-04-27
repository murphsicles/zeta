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

        // Check for Zeta imports: `use zeta::frontend::ast::AstNode;`
        // These can be for self-compilation (virtual modules) or regular crate imports
        if !path.is_empty() && path[0] == "zeta" {
            // First check if this is a self-compilation import (zeta crate importing itself)
            // For self-compilation, we create virtual modules
            // For regular crate imports, we need to resolve to actual .z files
            
            // Get the module path without the item name
            let module_path = if path.len() > 1 {
                &path[..path.len() - 1] // Remove the last component (the item name)
            } else {
                path // Keep all if only 1 component (shouldn't happen for valid imports)
            };
            
            // Check if this is a self-compilation pattern (zeta importing its own modules)
            // These are typically internal compiler modules
            let is_self_compilation = match &module_path[..] {
                [a, b, c] if a == "zeta" && b == "frontend" && c == "ast" => true,
                [a, b, c, d] if a == "zeta" && b == "frontend" && c == "parser" && d == "top_level" => true,
                [a, b, c, d] if a == "zeta" && b == "middle" && c == "resolver" && d == "resolver" => true,
                [a, b, c, d] if a == "zeta" && b == "middle" && c == "mir" && d == "mir" => true,
                [a, b, c] if a == "zeta" && b == "middle" && c == "specialization" => true,
                [a, b, c, d] if a == "zeta" && b == "backend" && c == "codegen" && d == "codegen" => true,
                [a, b, c, d] if a == "zeta" && b == "runtime" && c == "actor" && d == "channel" => true,
                [a, b, c, d] if a == "zeta" && b == "runtime" && c == "actor" && d == "scheduler" => true,
                _ => false,
            };
            
            if is_self_compilation {
                // For self-compilation, create virtual module
                let module_name = module_path.join("::");
                let virtual_path = PathBuf::from(format!("zeta_virtual/{}", module_name));
                return Ok(virtual_path);
            } else {
                // For regular zeta crate imports, treat like crate:: imports
                // but relative to zeta_src/ directory
                ;
                
                // Skip "zeta" prefix and remove item name
                let module_path = if path.len() > 1 {
                    &path[1..path.len() - 1] // Skip "zeta" and remove the last component
                } else {
                    &path[1..] // Skip "zeta" only
                };
                
                let mut zeta_path = PathBuf::from("zeta_src");
                
                for component in module_path {
                    zeta_path.push(component);
                }
                
                // Try with .z extension first
                let mut z_path = zeta_path.clone();
                z_path.set_extension("z");
                
                if z_path.exists() {
                    return Ok(z_path);
                }
                
                // Try as directory with mod.z
                let mut mod_path = zeta_path.clone();
                mod_path.push("mod.z");
                
                if mod_path.exists() {
                    return Ok(mod_path);
                }
                
                // If not found, create a virtual module for common zeta crate items
                let module_name = module_path.join("::");
                let virtual_path = PathBuf::from(format!("zeta_virtual/{}", module_name));
                return Ok(virtual_path);
            }
        }

        // Check for crate-relative imports: `use crate::middle::mir::Mir;`
        if !path.is_empty() && path[0] == "crate" {
            ;
            // For crate-relative imports, we need to resolve relative to zeta_src/
            // For `use crate::middle::mir::Mir;`, path is ["crate", "middle", "mir", "Mir"]
            // We need to resolve ["middle", "mir"] to zeta_src/middle/mir.z or zeta_src/middle/mir/mod.z

            let module_path = if path.len() > 1 {
                &path[1..path.len() - 1] // Skip "crate" and remove the last component (the item name)
            } else {
                &path[1..] // Skip "crate" only (shouldn't happen for valid crate:: imports)
            };

            ;

            let mut crate_path = PathBuf::from("zeta_src");

            for component in module_path {
                crate_path.push(component);
            }

            // Try with .z extension first (e.g., zeta_src/middle/mir.z)
            let mut z_path = crate_path.clone();
            z_path.set_extension("z");

            if z_path.exists() {
                return Ok(z_path);
            }

            // Try as directory with mod.z (e.g., zeta_src/middle/mir/mod.z)
            let mut mod_path = crate_path.clone();
            mod_path.push("mod.z");

            if mod_path.exists() {
                // If we found a mod.z file, check if there's a .z file with the same name as the directory
                // For example: zeta_src/middle/mir/mod.z exists, check for zeta_src/middle/mir/mir.z
                if let Some(dir_name) = crate_path.file_name().and_then(|n| n.to_str()) {
                    let mut submodule_path = crate_path.clone();
                    submodule_path.push(format!("{}.z", dir_name));

                    if submodule_path.exists() {
                        ;
                        return Ok(submodule_path);
                    }
                }
                return Ok(mod_path);
            }

            return Err(format!("Crate module not found: {}", path.join("::")));
        }

        // Check for Rust standard library imports: `use std::collections::HashMap;`
        if !path.is_empty() && path[0] == "std" {
            // For Rust std imports, we need to resolve to stub_types/std/
            // For `use std::collections::HashMap;`, path is ["std", "collections", "HashMap"]
            // We need to resolve ["std", "collections"] to stub_types/std/collections/

            let module_path = if path.len() > 1 {
                &path[..path.len() - 1] // Remove the last component (the item name)
            } else {
                path // Keep all if only 1 component
            };

            let mut std_path = PathBuf::from("stub_types");

            for component in module_path {
                std_path.push(component);
            }

            // Try with .z extension (we'll create Zeta stub files)
            let mut z_path = std_path.clone();
            z_path.set_extension("z");

            if z_path.exists() {
                return Ok(z_path);
            }

            // For std:: imports, we don't use mod.z structure
            // We create single .z files like stub_types/std/collections.z

            // If not found, create a minimal stub
            ;
            return self.create_std_stub(module_path);
        }

        // Check for external crate imports via zorb:: prefix: `use zorb::reqwest::blocking::Client;`, `use zorb::serde::Deserialize;`
        if path.len() >= 2
            && path[0] == "zorb"
            && (path[1] == "reqwest" || path[1] == "serde" || path[1] == "serde_json")
        {
            // For external crate imports via zorb::, we need to resolve to stub_types/external/
            // For `use zorb::reqwest::blocking::Client;`, path is ["zorb", "reqwest", "blocking", "Client"]
            // We need to resolve ["reqwest", "blocking"] to stub_types/external/reqwest/blocking/
            // (skip the "zorb" prefix)

            let module_path = if path.len() > 2 {
                &path[1..path.len() - 1] // Skip "zorb" and remove the last component (the item name)
            } else if path.len() > 1 {
                &path[1..] // Skip "zorb" only
            } else {
                path // Shouldn't happen for valid zorb:: imports
            };

            let mut ext_path = PathBuf::from("stub_types/external");

            for component in module_path {
                ext_path.push(component);
            }

            // Try with .z extension (we'll create Zeta stub files)
            let mut z_path = ext_path.clone();
            z_path.set_extension("z");

            if z_path.exists() {
                return Ok(z_path);
            }

            // For external crate imports, we don't use mod.z structure
            // We create single .z files like stub_types/external/reqwest.z

            // If not found, create a minimal stub
            ;
            return self.create_external_stub(module_path);
        }

        // Check for external crate imports without zorb:: prefix (backward compatibility)
        // `use reqwest::blocking::Client;`, `use serde::Deserialize;`
        if !path.is_empty()
            && (path[0] == "reqwest" || path[0] == "serde" || path[0] == "serde_json")
        {
            // For backward compatibility with existing code that uses Rust-style imports
            // For `use reqwest::blocking::Client;`, path is ["reqwest", "blocking", "Client"]
            // We need to resolve ["reqwest", "blocking"] to stub_types/external/reqwest/blocking/

            let module_path = if path.len() > 1 {
                &path[..path.len() - 1] // Remove the last component (the item name)
            } else {
                path // Keep all if only 1 component
            };

            let mut ext_path = PathBuf::from("stub_types/external");

            for component in module_path {
                ext_path.push(component);
            }

            // Try with .z extension (we'll create Zeta stub files)
            let mut z_path = ext_path.clone();
            z_path.set_extension("z");

            if z_path.exists() {
                return Ok(z_path);
            }

            // For external crate imports, we don't use mod.z structure
            // We create single .z files like stub_types/external/reqwest.z

            // If not found, create a minimal stub
            ;
            return self.create_external_stub(module_path);
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

        // Check for zorb package manager imports: `use zorb::package::Package;`, `use zorb::manifest::Manifest;`
        if path.len() >= 2 && path[0] == "zorb" {
            // Check if this is a package manager module (not std or external crate)
            let is_package_manager_module = match path[1].as_str() {
                "package" | "manifest" | "registry" | "dependency" | "lockfile" | "config" => true,
                _ => false,
            };
            
            if is_package_manager_module {
                ;
                
                // For zorb package manager imports, we need to resolve to zorb/ directory
                // For `use zorb::package::Package;`, path is ["zorb", "package", "Package"]
                // We need to resolve ["zorb", "package"] to zorb/package.z or zorb/package/mod.z
                
                let module_path = if path.len() > 2 {
                    &path[..path.len() - 1] // Remove the last component (the item name)
                } else {
                    path // Keep all if only 2 components
                };
                
                let mut zorb_path = PathBuf::from("zorb");
                
                // Add all components (including "zorb" prefix)
                for component in module_path {
                    zorb_path.push(component);
                }
                
                // Try with .z extension
                let mut z_path = zorb_path.clone();
                z_path.set_extension("z");
                
                if z_path.exists() {
                    return Ok(z_path);
                }
                
                // Try as directory with mod.z
                let mut mod_path = zorb_path.clone();
                mod_path.push("mod.z");
                
                if mod_path.exists() {
                    return Ok(mod_path);
                }
                
                // If not found, create a virtual module for common package manager items
                return self.create_zorb_package_stub(&module_path);
            }
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
        ;

        // Check cache first
        if self.modules.contains_key(&path_str) {
            ;
            return Ok(self.modules.get(&path_str).unwrap());
        }

        // Check if this is a virtual module for zeta:: imports
        if path_str.starts_with("zeta_virtual/") {
            ;
            return self.load_virtual_module(&path_str);
        }

        // Check if this is a std module (handle specially for PrimeZeta)
        if path_str.contains("stub_types\\std") || path_str.contains("stub_types/std") {
            ;
            return self.load_std_module();
        }

        // Read and parse file
        let content = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read module file {}: {}", path.display(), e))?;

        ;

        let (remaining, asts) = parse_zeta(&content)
            .map_err(|e| format!("Failed to parse module {}: {:?}", path.display(), e))?;

        ;

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
                        ;
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        ;
                    }
                }
                AstNode::StructDef { name, pub_, .. } => {
                    if *pub_ {
                        ;
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        ;
                    }
                }
                AstNode::FuncDef { name, pub_, .. } => {
                    if *pub_ {
                        ;
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        ;
                    }
                }
                AstNode::TypeAlias { name, pub_, .. } => {
                    if *pub_ {
                        ;
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        ;
                    }
                }
                AstNode::ConstDef { name, pub_, comptime_, .. } => {
                    if *pub_ {
                        ;
                        exports.insert(name.clone(), ast.clone());
                    } else {
                        ;
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
        ;
        Ok(self.modules.get(&path_str).unwrap())
    }

    /// Load a virtual module for std:: imports (PrimeZeta compatibility)
    fn load_std_module(&mut self) -> Result<&Module, String> {
        let path_str = "std_virtual/std";

        // Check cache first
        if self.modules.contains_key(path_str) {
            ;
            return Ok(self.modules.get(path_str).unwrap());
        }

        // Create a virtual module with PrimeZeta functions
        let mut exports = HashMap::new();
        let mut asts = Vec::new();

        // Create malloc function
        let malloc_func = AstNode::FuncDef {
            name: "malloc".to_string(),
            generics: vec![],
            lifetimes: vec![],
            params: vec![("size".to_string(), "i64".to_string())],
            ret: "i64".to_string(),
            body: vec![],
            attrs: vec![],
            ret_expr: None,
            single_line: false,
            doc: "Allocate memory".to_string(),
            pub_: true,
            async_: false,
            const_: false,
            comptime_: false,
            where_clauses: vec![],
        };
        asts.push(malloc_func.clone());
        exports.insert("malloc".to_string(), malloc_func);

        // Create free function
        let free_func = AstNode::FuncDef {
            name: "free".to_string(),
            generics: vec![],
            lifetimes: vec![],
            params: vec![("ptr".to_string(), "i64".to_string())],
            ret: "()".to_string(),
            body: vec![],
            attrs: vec![],
            ret_expr: None,
            single_line: false,
            doc: "Free memory".to_string(),
            pub_: true,
            async_: false,
            const_: false,
            comptime_: false,
            where_clauses: vec![],
        };
        asts.push(free_func.clone());
        exports.insert("free".to_string(), free_func);

        // Create print function
        let print_func = AstNode::FuncDef {
            name: "print".to_string(),
            generics: vec![],
            lifetimes: vec![],
            params: vec![("msg".to_string(), "i64".to_string())],
            ret: "()".to_string(),
            body: vec![],
            attrs: vec![],
            ret_expr: None,
            single_line: false,
            doc: "Print message".to_string(),
            pub_: true,
            async_: false,
            const_: false,
            comptime_: false,
            where_clauses: vec![],
        };
        asts.push(print_func.clone());
        exports.insert("print".to_string(), print_func);

        // Create println function
        let println_func = AstNode::FuncDef {
            name: "println".to_string(),
            generics: vec![],
            lifetimes: vec![],
            params: vec![("msg".to_string(), "i64".to_string())],
            ret: "()".to_string(),
            body: vec![],
            attrs: vec![],
            ret_expr: None,
            single_line: false,
            doc: "Print message with newline".to_string(),
            pub_: true,
            async_: false,
            const_: false,
            comptime_: false,
            where_clauses: vec![],
        };
        asts.push(println_func.clone());
        exports.insert("println".to_string(), println_func);

        // Create args function
        let args_func = AstNode::FuncDef {
            name: "args".to_string(),
            generics: vec![],
            lifetimes: vec![],
            params: vec![],
            ret: "i64".to_string(),
            body: vec![],
            attrs: vec![],
            ret_expr: None,
            single_line: false,
            doc: "Get command line arguments".to_string(),
            pub_: true,
            async_: false,
            const_: false,
            comptime_: false,
            where_clauses: vec![],
        };
        asts.push(args_func.clone());
        exports.insert("args".to_string(), args_func);

        // Create the module
        let module = Module {
            name: "std".to_string(),
            path: PathBuf::from("stub_types/std.z"),
            asts,
            exports,
        };

        self.modules.insert(path_str.to_string(), module);
        ;
        Ok(self.modules.get(path_str).unwrap())
    }

    /// Load a virtual module for zeta:: imports (self-compilation)
    fn load_virtual_module(&mut self, path_str: &str) -> Result<&Module, String> {
        // Extract the module path from the virtual path
        // path_str format: "zeta_virtual/frontend_ast_AstNode"
        let parts: Vec<&str> = path_str.split('/').collect();
        if parts.len() < 2 {
            return Err("Invalid virtual module path".to_string());
        }

        let module_path = parts[1];
        let path_components: Vec<&str> = module_path.split("::").collect();

        // Create a virtual module with the appropriate exports
        let mut exports = HashMap::new();
        let mut asts = Vec::new();

        // Check if this is a zeta:: import
        if !path_components.is_empty() && path_components[0] == "zeta" {
            // Handle different zeta:: imports based on path components
            // We create virtual modules that export all items from that module
            match &path_components[..] {
                ["zeta", "frontend", "ast"] => {
                    // Create a virtual enum for AstNode
                    let ast_node_enum = AstNode::EnumDef {
                        name: "AstNode".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        variants: vec![
                            ("Program".to_string(), vec![]),
                            ("FuncDef".to_string(), vec![]),
                            ("Call".to_string(), vec![]),
                            ("Lit".to_string(), vec![]),
                            ("Var".to_string(), vec![]),
                            ("BinaryOp".to_string(), vec![]),
                            ("If".to_string(), vec![]),
                            ("Return".to_string(), vec![]),
                            ("Let".to_string(), vec![]),
                            ("Match".to_string(), vec![]),
                            ("StructDef".to_string(), vec![]),
                            ("EnumDef".to_string(), vec![]),
                            ("ImplBlock".to_string(), vec![]),
                            ("Use".to_string(), vec![]),
                            ("ModDef".to_string(), vec![]),
                            ("Unit".to_string(), vec![]),
                        ],
                        attrs: vec![],
                        doc: "Abstract Syntax Tree node for Zeta language".to_string(),
                        pub_: true,
                        where_clauses: vec![],
                    };

                    asts.push(ast_node_enum.clone());
                    exports.insert("AstNode".to_string(), ast_node_enum);
                    ;
                }
                ["zeta", "frontend", "parser", "top_level"] => {
                    // Create parse_zeta function
                    let parse_zeta_func = AstNode::FuncDef {
                        name: "parse_zeta".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        params: vec![("input".to_string(), "&str".to_string())],
                        ret: "Result<(&str, Vec<AstNode>), &'static str>".to_string(),
                        body: vec![],
                        attrs: vec![],
                        ret_expr: None,
                        single_line: false,
                        doc: "Parse Zeta source code".to_string(),
                        pub_: true,
                        async_: false,
                        const_: false,
                        comptime_: false,
                        where_clauses: vec![],
                    };
                    asts.push(parse_zeta_func.clone());
                    exports.insert("parse_zeta".to_string(), parse_zeta_func);
                    ;
                }
                ["zeta", "middle", "resolver", "resolver"] => {
                    // Create Resolver struct
                    let resolver_struct = AstNode::StructDef {
                        name: "Resolver".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        fields: vec![],
                        attrs: vec![],
                        doc: "Semantic analysis and monomorphization engine".to_string(),
                        pub_: true,
                        where_clauses: vec![],
                    };
                    asts.push(resolver_struct.clone());
                    exports.insert("Resolver".to_string(), resolver_struct);
                    ;
                }
                ["zeta", "middle", "mir", "mir"] => {
                    // Create Mir struct
                    let mir_struct = AstNode::StructDef {
                        name: "Mir".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        fields: vec![],
                        attrs: vec![],
                        doc: "Mid-level Intermediate Representation".to_string(),
                        pub_: true,
                        where_clauses: vec![],
                    };
                    asts.push(mir_struct.clone());
                    exports.insert("Mir".to_string(), mir_struct);
                    ;
                }
                ["zeta", "middle", "specialization"] => {
                    // Create all specialization functions
                    let is_cache_safe_func = AstNode::FuncDef {
                        name: "is_cache_safe".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        params: vec![("type_name".to_string(), "&str".to_string())],
                        ret: "bool".to_string(),
                        body: vec![],
                        attrs: vec![],
                        ret_expr: None,
                        single_line: false,
                        doc: "Check if a type is cache-safe for specialization".to_string(),
                        pub_: true,
                        async_: false,
                        const_: false,
                        comptime_: false,
                        where_clauses: vec![],
                    };
                    asts.push(is_cache_safe_func.clone());
                    exports.insert("is_cache_safe".to_string(), is_cache_safe_func);

                    let lookup_specialization_func = AstNode::FuncDef {
                        name: "lookup_specialization".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        params: vec![("key".to_string(), "&str".to_string())],
                        ret: "Option<String>".to_string(),
                        body: vec![],
                        attrs: vec![],
                        ret_expr: None,
                        single_line: false,
                        doc: "Look up a specialization in the cache".to_string(),
                        pub_: true,
                        async_: false,
                        const_: false,
                        comptime_: false,
                        where_clauses: vec![],
                    };
                    asts.push(lookup_specialization_func.clone());
                    exports.insert(
                        "lookup_specialization".to_string(),
                        lookup_specialization_func,
                    );

                    let record_specialization_func = AstNode::FuncDef {
                        name: "record_specialization".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        params: vec![
                            ("key".to_string(), "&str".to_string()),
                            ("value".to_string(), "&str".to_string()),
                        ],
                        ret: "()".to_string(),
                        body: vec![],
                        attrs: vec![],
                        ret_expr: None,
                        single_line: false,
                        doc: "Record a specialization in the cache".to_string(),
                        pub_: true,
                        async_: false,
                        const_: false,
                        comptime_: false,
                        where_clauses: vec![],
                    };
                    asts.push(record_specialization_func.clone());
                    exports.insert(
                        "record_specialization".to_string(),
                        record_specialization_func,
                    );
                    ;
                }
                ["zeta", "backend", "codegen", "codegen"] => {
                    // Create LLVMCodegen struct
                    let llvmcodegen_struct = AstNode::StructDef {
                        name: "LLVMCodegen".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        fields: vec![],
                        attrs: vec![],
                        doc: "LLVM-based code generator".to_string(),
                        pub_: true,
                        where_clauses: vec![],
                    };
                    asts.push(llvmcodegen_struct.clone());
                    exports.insert("LLVMCodegen".to_string(), llvmcodegen_struct);
                    ;
                }
                ["zeta", "runtime", "actor", "channel"] => {
                    // Create Channel struct (generic)
                    let channel_struct = AstNode::StructDef {
                        name: "Channel".to_string(),
                        generics: vec![crate::frontend::ast::GenericParam::Type {
                            name: "T".to_string(),
                            bounds: vec![],
                        }],
                        lifetimes: vec![],
                        fields: vec![],
                        attrs: vec![],
                        doc: "Actor communication channel".to_string(),
                        pub_: true,
                        where_clauses: vec![],
                    };
                    asts.push(channel_struct.clone());
                    exports.insert("Channel".to_string(), channel_struct);
                    ;
                }
                ["zeta", "runtime", "actor", "scheduler"] => {
                    // Create Scheduler struct
                    let scheduler_struct = AstNode::StructDef {
                        name: "Scheduler".to_string(),
                        generics: vec![],
                        lifetimes: vec![],
                        fields: vec![],
                        attrs: vec![],
                        doc: "Actor scheduler".to_string(),
                        pub_: true,
                        where_clauses: vec![],
                    };
                    asts.push(scheduler_struct.clone());
                    exports.insert("Scheduler".to_string(), scheduler_struct);
                    ;
                }
                _ => {
                    ;
                }
            }
        }

        // Create the virtual module
        let module = Module {
            name: module_path.to_string(),
            path: PathBuf::from(path_str),
            asts,
            exports,
        };

        self.modules.insert(path_str.to_string(), module);
        Ok(self.modules.get(path_str).unwrap())
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

    /// Create a minimal stub for a std module that doesn't exist yet
    fn create_std_stub(&mut self, module_path: &[String]) -> Result<PathBuf, String> {
        // Create directory structure
        let mut stub_path = PathBuf::from("stub_types");

        for component in module_path {
            stub_path.push(component);
        }

        // Create parent directories
        if let Some(parent) = stub_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create stub directory: {}", e))?;
        }

        // Create .z file (not mod.z)
        stub_path.set_extension("z");

        // Generate stub content based on module path
        let stub_content = self.generate_std_stub_content(module_path);

        fs::write(&stub_path, stub_content)
            .map_err(|e| format!("Failed to write stub file: {}", e))?;

        ;
        Ok(stub_path)
    }

    /// Generate stub content for a std module
    fn generate_std_stub_content(&self, module_path: &[String]) -> String {
        // Generate different stubs based on what's being imported
        let last_component = module_path.last().map(|s| s.as_str()).unwrap_or("");

        match last_component {
            "collections" => {
                // Create a single file with HashMap definition
                r#"//! Stub for std::collections::HashMap
pub struct HashMap<K, V> {
    // Stub implementation
}

impl<K, V> HashMap<K, V> {
    pub fn new() -> Self {
        HashMap {}
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        None
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        None
    }
}"#
                .to_string()
            }
            "ffi" => {
                // Create a single file with c_void definition
                r#"//! Stub for std::ffi::c_void
// c_void is used for raw pointers to opaque C data

/// Opaque type representing a void pointer in C
pub enum c_void {
    /// Variant 1
    __variant1,
    /// Variant 2
    __variant2,
}"#
                .to_string()
            }
            _ => {
                // Generic stub
                format!(
                    "//! Stub for std::{}\npub struct Stub;\n",
                    module_path.join("::")
                )
            }
        }
    }

    /// Create a minimal stub for an external crate that doesn't exist yet
    fn create_external_stub(&mut self, module_path: &[String]) -> Result<PathBuf, String> {
        // Create directory structure
        let mut stub_path = PathBuf::from("stub_types/external");

        // Create all parent directories first
        fs::create_dir_all(&stub_path)
            .map_err(|e| format!("Failed to create external stub base directory: {}", e))?;

        for component in module_path {
            stub_path.push(component);
        }

        // Create parent directories
        if let Some(parent) = stub_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create external stub directory: {}", e))?;
        }

        // Create .z file (not mod.z)
        stub_path.set_extension("z");

        // Generate stub content based on module path
        let stub_content = self.generate_external_stub_content(module_path);

        fs::write(&stub_path, stub_content)
            .map_err(|e| format!("Failed to write external stub file: {}", e))?;

        ;
        Ok(stub_path)
    }

    /// Generate stub content for an external crate module
    fn generate_external_stub_content(&self, module_path: &[String]) -> String {
        // Generate different stubs based on what's being imported
        let crate_name = module_path.first().map(|s| s.as_str()).unwrap_or("");

        match crate_name {
            "reqwest" => {
                if module_path.len() > 1 && module_path[1] == "blocking" {
                    r#"//! Stub for reqwest::blocking
pub struct Client {}

impl Client {
    pub fn new() -> Self {
        Client {}
    }
}"#
                    .to_string()
                } else {
                    // For reqwest crate, create a single file with Client
                    r#"//! Stub for reqwest::blocking::Client
pub struct Client {}

impl Client {
    pub fn new() -> Self {
        Client {}
    }
}"#
                    .to_string()
                }
            }
            "serde" => r#"//! Stub for serde
// Traits are not fully supported in Zeta, using empty concepts as stubs
pub concept Deserialize {}
pub concept Serialize {}"#
                .to_string(),
            "serde_json" => r#"//! Stub for serde_json
pub fn to_string<T>(_value: &T) -> Result<String, ()> {
    Ok("{}".to_string())
}

pub fn from_str<T>(_s: &str) -> Result<T, ()> {
    unimplemented!()
}"#
            .to_string(),
            _ => {
                // Generic stub
                format!(
                    "//! Stub for {}\npub struct Stub;\n",
                    module_path.join("::")
                )
            }
        }
    }

    /// Create a minimal stub for a zorb package manager module that doesn't exist yet
    fn create_zorb_package_stub(&mut self, module_path: &[String]) -> Result<PathBuf, String> {
        // Create directory structure
        let mut stub_path = PathBuf::from("zorb");

        for component in module_path {
            stub_path.push(component);
        }

        // Create parent directories
        if let Some(parent) = stub_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create zorb stub directory: {}", e))?;
        }

        // Create .z file (not mod.z)
        stub_path.set_extension("z");

        // Generate stub content based on module path
        let stub_content = self.generate_zorb_package_stub_content(module_path);

        fs::write(&stub_path, stub_content)
            .map_err(|e| format!("Failed to write zorb stub file: {}", e))?;

        ;
        Ok(stub_path)
    }

    /// Generate stub content for a zorb package manager module
    fn generate_zorb_package_stub_content(&self, module_path: &[String]) -> String {
        // Generate different stubs based on what's being imported
        let last_component = module_path.last().map(|s| s.as_str()).unwrap_or("");

        match last_component {
            "package" => {
                r#"//! Stub for zorb::package::Package
pub struct Package {
    name: String,
    version: String,
}

impl Package {
    pub fn new(name: &str) -> Self {
        Package {
            name: name.to_string(),
            version: "0.1.0".to_string(),
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn version(&self) -> &str {
        &self.version
    }
}"#
                .to_string()
            }
            "manifest" => {
                r#"//! Stub for zorb::manifest::Manifest
pub struct Manifest {
    package: Package,
    dependencies: Vec<Dependency>,
}

pub struct Dependency {
    name: String,
    version: String,
}

impl Manifest {
    pub fn new(package: Package) -> Self {
        Manifest {
            package,
            dependencies: Vec::new(),
        }
    }

    pub fn add_dependency(&mut self, name: &str, version: &str) {
        self.dependencies.push(Dependency {
            name: name.to_string(),
            version: version.to_string(),
        });
    }
}"#
                .to_string()
            }
            "registry" => {
                r#"//! Stub for zorb::registry::Registry
pub struct Registry {}

impl Registry {
    pub fn new() -> Self {
        Registry {}
    }

    pub fn publish(&self, _package: &Package) -> Result<(), String> {
        Ok(())
    }
}"#
                .to_string()
            }
            _ => {
                // Generic stub
                format!(
                    "//! Stub for {}\npub struct Stub;\n",
                    module_path.join("::")
                )
            }
        }
    }
}
