// src/middle/resolver/resolver.rs
//! # Resolver — Semantic Analysis & Monomorphization Engine
//!
//! Single source of truth for type resolution, specialization caching,
//! borrow checking coordination, and MIR lowering.

use crate::frontend::ast::AstNode;
use crate::frontend::borrow::BorrowChecker;
use crate::frontend::macro_expand::MacroExpander;
use crate::middle::mir::mir::Mir;
use crate::middle::resolver::module_resolver::ModuleResolver;
use crate::middle::resolver::typecheck_new::NewTypeCheck;
use crate::middle::types::ArraySize;
use crate::middle::types::identity::{CapabilityLevel, IdentityType};
use crate::middle::specialization::{
    CACHE, MonoKey, MonoValue, is_cache_safe, record_specialization,
};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

pub use crate::middle::types::Type;

const SPECIALIZATION_CACHE_FILE: &str = ".zeta_specialization_cache.json";

#[derive(Serialize, Deserialize)]
struct CacheFile {
    entries: HashMap<MonoKey, MonoValue>,
}

pub struct Resolver {
    pub impls: HashMap<(String, String), Vec<AstNode>>,
    pub cached_mirs: HashMap<String, Mir>,
    pub mono_mirs: HashMap<MonoKey, Mir>,
    pub borrow_checker: RefCell<BorrowChecker>,
    pub associated_types: HashMap<(String, String), String>,
    pub ctfe_consts: HashMap<AstNode, i64>,
    funcs: HashMap<String, FuncSignature>,
    /// Registered function ASTs (including module functions)
    registered_funcs: HashMap<String, AstNode>,
    /// Module resolver for Zorb imports
    module_resolver: ModuleResolver,
    /// Macro expander for macro processing
    macro_expander: MacroExpander,
}

// Learning: Complex type factored into type definition per clippy suggestion
type FuncSignature = (Vec<(String, Type)>, Type, bool);

impl Resolver {
    pub fn new() -> Self {
        let mut r = Self {
            impls: HashMap::new(),
            cached_mirs: HashMap::new(),
            mono_mirs: HashMap::new(),
            borrow_checker: RefCell::new(BorrowChecker::new()),
            associated_types: HashMap::new(),
            ctfe_consts: HashMap::new(),
            funcs: HashMap::new(),
            registered_funcs: HashMap::new(),
            module_resolver: ModuleResolver::new("."),
            macro_expander: MacroExpander::new(),
        };

        // Register built-in runtime functions
        r.register_builtin_functions();

        r.load_specialization_cache();
        r
    }

    fn load_specialization_cache(&mut self) {
        let path = PathBuf::from(SPECIALIZATION_CACHE_FILE);
        if let Ok(data) = fs::read_to_string(&path)
            && let Ok(cache) = serde_json::from_str::<CacheFile>(&data)
        {
            // OPTIMIZATION: Iterate without cloning keys
            for (key, value) in cache.entries {
                // Use key directly without clone when possible
                self.mono_mirs.insert(key.clone(), Mir::default());
                record_specialization(key, value);
            }
        }
    }

    pub fn persist_specialization_cache(&self) {
        let cache_guard = CACHE.read().unwrap();
        let entries = cache_guard.clone();
        let cache_file = CacheFile { entries };
        if let Ok(json) = serde_json::to_string_pretty(&cache_file) {
            let _ = fs::write(SPECIALIZATION_CACHE_FILE, json);
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::Use { path } => {
                println!("[RESOLVER] Processing use statement: {}", path.join("::"));
                // Process use statement to load module
                match self.module_resolver.process_use_statement(&path) {
                    Ok(module_asts) => {
                        println!(
                            "[RESOLVER] Successfully loaded module, got {} ASTs",
                            module_asts.len()
                        );
                        // Register only enum/struct definitions, not impl blocks
                        for module_ast in module_asts {
                            match &module_ast {
                                AstNode::EnumDef { name, variants, .. } => {
                                    println!("[RESOLVER] Registering enum from module: {}", name);
                                    self.register(module_ast.clone());

                                    // Also register enum variant constructors as functions
                                    for (variant_name, variant_params) in variants {
                                        // Create a function name like "Option::Some"
                                        let func_name = format!("{}::{}", name, variant_name);
                                        println!(
                                            "[RESOLVER] Registering enum variant constructor: {}",
                                            func_name
                                        );

                                        // Create a fake function AST for the variant constructor
                                        // The return type is the enum with its type parameters
                                        let ret_type = if variant_params.is_empty() {
                                            name.clone()
                                        } else {
                                            // For generic enums like Option<T>, we need to handle type parameters
                                            // For now, just use the base name
                                            name.clone()
                                        };

                                        // Create parameter types from variant params
                                        let params: Vec<(String, String)> = variant_params
                                            .iter()
                                            .enumerate()
                                            .map(|(i, param_type)| {
                                                (i.to_string(), param_type.clone())
                                            })
                                            .collect();

                                        // Create a fake function definition for the variant constructor
                                        let variant_func = AstNode::FuncDef {
                                            name: func_name.clone(),
                                            generics: vec![], // TODO: Handle generics
                                            lifetimes: vec![], // TODO: Handle lifetimes
                                            params,
                                            ret: ret_type,
                                            body: vec![],
                                            attrs: vec![],
                                            ret_expr: None,
                                            single_line: false,
                                            doc: "".to_string(),
                                            pub_: true, // Variant constructors are always public
                                            async_: false, // Variant constructors are not async
                                            const_: false, // Variant constructors are not const
                                            comptime_: false, // Variant constructors are not comptime
                                            where_clauses: vec![],
                                        };

                                        // Register the variant constructor
                                        self.register(variant_func);
                                    }
                                }
                                AstNode::StructDef { name, .. } => {
                                    println!("[RESOLVER] Registering struct from module: {}", name);
                                    self.register(module_ast);
                                }
                                AstNode::TypeAlias { name, .. } => {
                                    println!(
                                        "[RESOLVER] Registering type alias from module: {}",
                                        name
                                    );
                                    self.register(module_ast);
                                }
                                AstNode::ConstDef { name, comptime_, .. } => {
                                    println!("[RESOLVER] Registering const from module: {}", name);
                                    self.register(module_ast);
                                }
                                AstNode::FuncDef { name, .. } => {
                                    println!(
                                        "[RESOLVER] Registering function from module: {}",
                                        name
                                    );
                                    // When importing via `use std::malloc`, register with simple name
                                    // The function will be available as `malloc` in current scope
                                    self.register(module_ast);
                                }
                                // Skip impl blocks for now - they cause issues
                                _ => {
                                    println!(
                                        "[RESOLVER] Skipping non-export AST from module: {:?}",
                                        module_ast
                                    );
                                }
                            }
                        }
                    }
                    Err(e) => {
                        eprintln!(
                            "Warning: Failed to process use statement {}: {}",
                            path.join("::"),
                            e
                        );
                    }
                }
            }
            AstNode::FuncDef {
                ref name,
                ref params,
                ref ret,
                ref async_,
                ..
            } => {
                // Convert string types to Type enum
                let typed_params: Vec<(String, Type)> = params
                    .iter()
                    .map(|(name, ty_str)| (name.clone(), self.string_to_type(ty_str)))
                    .collect();
                let typed_ret = self.string_to_type(ret);
                println!(
                    "[RESOLVER] Registering function: {} with {} params",
                    name,
                    params.len()
                );
                let name_clone = name.clone();
                self.funcs
                    .insert(name_clone.clone(), (typed_params, typed_ret, *async_));
                self.registered_funcs.insert(name_clone, ast.clone());
            }
            AstNode::ExternFunc {
                name,
                generics: _,
                lifetimes: _,
                params,
                ret,
                where_clauses: _,
            } => {
                // Convert string types to Type enum
                let typed_params: Vec<(String, Type)> = params
                    .iter()
                    .map(|(name, ty_str)| (name.clone(), self.string_to_type(ty_str)))
                    .collect();
                let typed_ret = self.string_to_type(&ret);
                self.funcs.insert(name, (typed_params, typed_ret, true));
            }
            AstNode::ImplBlock {
                concept, ty, body, ..
            } => {
                self.impls.insert((concept, ty.clone()), body.clone());
                // Register functions with qualified names
                for b in body.clone() {
                    if let AstNode::FuncDef {
                        name, params, ret, ..
                    } = &b
                    {
                        // Create qualified name: Type::method
                        let qualified_name = format!("{}::{}", ty, name);
                        // Convert string types to Type enum
                        let typed_params: Vec<(String, Type)> = params
                            .iter()
                            .map(|(name, ty_str)| (name.clone(), self.string_to_type(ty_str)))
                            .collect();
                        let typed_ret = self.string_to_type(ret);
                        self.funcs
                            .insert(qualified_name, (typed_params, typed_ret, false));
                    }
                    // Register normally (which will register with simple name)
                    self.register(b);
                }
            }
            AstNode::ConceptDef { methods, .. } => {
                for m in methods {
                    self.register(m);
                }
            }
            AstNode::Method {
                name, params, ret, ..
            } => {
                // Convert string types to Type enum
                let typed_params: Vec<(String, Type)> = params
                    .iter()
                    .map(|(name, ty_str)| (name.clone(), self.string_to_type(ty_str)))
                    .collect();
                let typed_ret = self.string_to_type(&ret);
                self.funcs.insert(name, (typed_params, typed_ret, false));
            }
            AstNode::ConstDef {
                ref name,
                ref ty,
                ref value,
                attrs: _,
                pub_: _,
                comptime_: _,
            } => {
                // Register constant for compile-time evaluation
                // For now, we'll store it in ctfe_consts if it's a simple literal
                if let AstNode::Lit(val) = **value {
                    self.ctfe_consts.insert(ast.clone(), val);
                }
                // Also register as a function-like entity for name resolution
                let typed_ret = self.string_to_type(ty);
                self.funcs.insert(name.clone(), (vec![], typed_ret, false));
            }
            AstNode::EnumDef { name, variants, .. } => {
                // Register enum and its variants
                // For now, we'll register each variant as a function-like entity
                for (variant_name, _) in variants {
                    let full_name = name.clone() + "::" + &variant_name;
                    let typed_ret = Type::Named(name.clone(), vec![]);
                    self.funcs.insert(full_name, (vec![], typed_ret, false));
                }
            }
            AstNode::ModDef {
                name: module_name,
                items,
                pub_,
                ..
            } => {
                // Register module and its items
                println!(
                    "[RESOLVER] Registering module: {} (pub: {})",
                    module_name, pub_
                );
                // Register all items in the module with module-qualified names
                for item in items {
                    // Create a copy with module-qualified name if needed
                    let qualified_item = match &item {
                        AstNode::FuncDef {
                            name: func_name, ..
                        } => {
                            let mut new_item = item.clone();
                            if let AstNode::FuncDef { ref mut name, .. } = new_item {
                                *name = format!("{}::{}", module_name, func_name);
                            }
                            new_item
                        }
                        AstNode::EnumDef {
                            name: enum_name, ..
                        } => {
                            let mut new_item = item.clone();
                            if let AstNode::EnumDef { ref mut name, .. } = new_item {
                                // For enums, we need to register the enum itself and its variants
                                *name = format!("{}::{}", module_name, enum_name);
                            }
                            new_item
                        }
                        AstNode::StructDef {
                            name: struct_name, ..
                        } => {
                            let mut new_item = item.clone();
                            if let AstNode::StructDef { ref mut name, .. } = new_item {
                                *name = format!("{}::{}", module_name, struct_name);
                            }
                            new_item
                        }
                        AstNode::TypeAlias {
                            name: alias_name, ..
                        } => {
                            let mut new_item = item.clone();
                            if let AstNode::TypeAlias { ref mut name, .. } = new_item {
                                *name = format!("{}::{}", module_name, alias_name);
                            }
                            new_item
                        }
                        AstNode::ConstDef {
                            name: const_name,
                            comptime_,
                            ..
                        } => {
                            let mut new_item = item.clone();
                            if let AstNode::ConstDef { ref mut name, .. } = new_item {
                                *name = format!("{}::{}", module_name, const_name);
                            }
                            new_item
                        }
                        _ => item.clone(),
                    };
                    self.register(qualified_item);
                }
            }
            AstNode::MacroDef { name, patterns } => {
                // Parse and register the macro
                match crate::frontend::macro_expand::parse_macro_rules(&patterns) {
                    Ok(macro_def) => {
                        self.macro_expander
                            .register_declarative_macro(name.clone(), macro_def);
                    }
                    Err(e) => {
                        eprintln!("Warning: Failed to parse macro {}: {}", name, e);
                    }
                }
            }
            _ => {}
        }
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Vec<AstNode>> {
        self.impls
            .get(&(concept.to_string(), ty.to_string()))
            .cloned()
    }

    /// Get function signature for type checking
    #[allow(clippy::type_complexity)]
    pub fn get_func_signature(&self, name: &str) -> Option<&(Vec<(String, Type)>, Type, bool)> {
        println!("[RESOLVER] Looking up function: {}", name);
        let result = self.funcs.get(name);
        if result.is_none() {
            println!("[RESOLVER] Function not found: {}", name);
            println!(
                "[RESOLVER] Available functions: {:?}",
                self.funcs.keys().collect::<Vec<_>>()
            );
        }
        result
    }

    /// Get all function signatures (for type inference)
    pub fn get_all_func_signatures(&self) -> &HashMap<String, (Vec<(String, Type)>, Type, bool)> {
        &self.funcs
    }

    pub fn is_abi_stable(&self, key: &MonoKey) -> bool {
        key.type_args.iter().all(|t| is_cache_safe(t))
    }

    pub fn record_mono(&mut self, key: MonoKey, mut mir: Mir) {
        let mangled = key.mangle();
        mir.name = Some(mangled.clone());
        self.mono_mirs.insert(key.clone(), mir);
        record_specialization(
            key.clone(),
            MonoValue {
                llvm_func_name: mangled,
                cache_safe: self.is_abi_stable(&key),
            },
        );
    }

    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = crate::middle::mir::r#gen::MirGen::new();
        mir_gen.lower_to_mir(ast)
    }

    pub fn collect_used_specializations(
        &mut self,
        asts: &[AstNode],
    ) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();
        fn walk(
            node: &AstNode,
            used: &mut HashMap<String, Vec<Vec<String>>>,
            resolver: &mut Resolver,
        ) {
            if let AstNode::Call {
                receiver,
                method,
                type_args,
                ..
            } = node
            {
                let mut args = type_args.clone();
                if let Some(r) = receiver {
                    let ty = resolver.infer_type(r);
                    if args.is_empty() {
                        // Convert Type to string for compatibility
                        args = vec![ty.display_name()];
                    }
                }
                if !args.is_empty() {
                    used.entry(method.clone()).or_default().push(args);
                }
            }
            match node {
                AstNode::FuncDef { body, .. } => body.iter().for_each(|s| walk(s, used, resolver)),
                AstNode::Return(inner) => walk(inner, used, resolver),
                AstNode::ExprStmt { expr } => walk(expr, used, resolver),
                AstNode::BinaryOp { left, right, .. } => {
                    walk(left, used, resolver);
                    walk(right, used, resolver);
                }
                AstNode::If {
                    cond, then, else_, ..
                } => {
                    walk(cond, used, resolver);
                    then.iter().for_each(|s| walk(s, used, resolver));
                    else_.iter().for_each(|s| walk(s, used, resolver));
                }
                AstNode::Let { expr, .. } => walk(expr, used, resolver),
                AstNode::Assign(lhs, rhs) => {
                    walk(lhs, used, resolver);
                    walk(rhs, used, resolver);
                }
                AstNode::Call { receiver, args, .. } => {
                    if let Some(r) = receiver {
                        walk(r, used, resolver);
                    }
                    args.iter().for_each(|a| walk(a, used, resolver));
                }
                _ => {}
            }
        }
        for ast in asts {
            walk(ast, &mut used, self);
        }
        used
    }

    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        let mut mono = ast.clone();
        let mut subst: HashMap<String, String> = key
            .type_args
            .iter()
            .cloned()
            .zip(key.type_args.iter().cloned())
            .collect();
        if !key.type_args.is_empty() {
            subst.insert("Self".to_string(), key.type_args[0].clone());
        }
        fn substitute(node: &mut AstNode, subst: &HashMap<String, String>) {
            match node {
                AstNode::FuncDef {
                    generics,
                    params,
                    ret,
                    body,
                    ..
                } => {
                    generics.clear();
                    for (_, ty) in params.iter_mut() {
                        if let Some(r) = subst.get(ty) {
                            *ty = r.clone();
                        }
                    }
                    if let Some(r) = subst.get(ret) {
                        *ret = r.clone();
                    }
                    for s in body {
                        substitute(s, subst);
                    }
                }
                AstNode::Call { type_args, .. } => type_args.clear(),
                AstNode::TimingOwned { ty, inner } => {
                    if let Some(r) = subst.get(ty) {
                        *ty = r.clone();
                    }
                    substitute(inner, subst);
                }
                AstNode::BinaryOp { left, right, .. } => {
                    substitute(left, subst);
                    substitute(right, subst);
                }
                AstNode::If {
                    cond, then, else_, ..
                } => {
                    substitute(cond, subst);
                    for s in then {
                        substitute(s, subst);
                    }
                    for s in else_ {
                        substitute(s, subst);
                    }
                }
                _ => {}
            }
        }
        substitute(&mut mono, &subst);
        mono
    }

    /// Expand macros in AST nodes
    pub fn expand_macros(&mut self, asts: &[AstNode]) -> Result<Vec<AstNode>, String> {
        let mut expanded = Vec::new();

        for ast in asts {
            let result = self.expand_macros_in_node(ast)?;
            expanded.extend(result);
        }

        Ok(expanded)
    }

    /// Expand macros in a single AST node
    fn expand_macros_in_node(&mut self, node: &AstNode) -> Result<Vec<AstNode>, String> {
        match node {
            AstNode::MacroCall { name, args } => {
                // Expand macro call
                self.macro_expander.expand_macro_call(name, args)
            }
            AstNode::FuncDef { attrs, .. }
            | AstNode::StructDef { attrs, .. }
            | AstNode::EnumDef { attrs, .. }
            | AstNode::ConceptDef { attrs, .. }
            | AstNode::ImplBlock { attrs, .. } => {
                // Process attributes
                let mut nodes = vec![node.clone()];
                let attr_expansions =
                    crate::frontend::macro_expand::process_attributes(attrs, node)?;
                nodes.extend(attr_expansions);
                Ok(nodes)
            }
            AstNode::Program(nodes) => {
                // Recursively expand macros in program
                let mut expanded_nodes = Vec::new();
                for node in nodes {
                    let result = self.expand_macros_in_node(node)?;
                    expanded_nodes.extend(result);
                }
                Ok(vec![AstNode::Program(expanded_nodes)])
            }
            _ => {
                // For other nodes, just return them as-is
                Ok(vec![node.clone()])
            }
        }
    }

    /// Get all registered function ASTs
    pub fn get_registered_funcs(&self) -> Vec<AstNode> {
        println!(
            "[RESOLVER DEBUG] Returning {} registered functions",
            self.registered_funcs.len()
        );
        for (name, _) in &self.registered_funcs {
            println!("[RESOLVER DEBUG] Registered function: {}", name);
        }
        self.registered_funcs.values().cloned().collect()
    }

    /// Register built-in runtime functions that are required for compilation
    fn register_builtin_functions(&mut self) {
        // clone_i64(value: i64) -> i64
        self.funcs.insert(
            "clone_i64".to_string(),
            (
                vec![("value".to_string(), Type::I64)],
                Type::I64,
                false, // not async
            ),
        );

        // is_null_i64(value: i64) -> bool
        self.funcs.insert(
            "is_null_i64".to_string(),
            (
                vec![("value".to_string(), Type::I64)],
                Type::Bool,
                false, // not async
            ),
        );

        // to_string_str(value: str) -> str
        self.funcs.insert(
            "to_string_str".to_string(),
            (
                vec![("value".to_string(), Type::Str)],
                Type::Str,
                false, // not async
            ),
        );

        // to_string_i64(value: i64) -> str
        self.funcs.insert(
            "to_string_i64".to_string(),
            (
                vec![("value".to_string(), Type::I64)],
                Type::Str,
                false, // not async
            ),
        );

        // to_string_bool(value: bool) -> str
        self.funcs.insert(
            "to_string_bool".to_string(),
            (
                vec![("value".to_string(), Type::Bool)],
                Type::Str,
                false, // not async
            ),
        );

        // String runtime functions
        // str_concat(a: str, b: str) -> str
        self.funcs.insert(
            "str_concat".to_string(),
            (
                vec![("a".to_string(), Type::Str), ("b".to_string(), Type::Str)],
                Type::Str,
                false, // not async
            ),
        );

        // str_len(s: str) -> i64
        self.funcs.insert(
            "str_len".to_string(),
            (
                vec![("s".to_string(), Type::Str)],
                Type::I64,
                false, // not async
            ),
        );

        // Identity-aware string functions
        // read_only_string(value: str) -> identity(value)[read]
        self.funcs.insert(
            "read_only_string".to_string(),
            (
                vec![("value".to_string(), Type::Str)],
                Type::Identity(Box::new(IdentityType {
                    value: None,
                    capabilities: vec![CapabilityLevel::Read],
                    delegatable: false,
                    constraints: vec![],
                })),
                false, // not async
            ),
        );

        // read_write_string(value: str) -> identity(value)[read, write]
        self.funcs.insert(
            "read_write_string".to_string(),
            (
                vec![("value".to_string(), Type::Str)],
                Type::Identity(Box::new(IdentityType {
                    value: None,
                    capabilities: vec![CapabilityLevel::Read, CapabilityLevel::Write],
                    delegatable: false,
                    constraints: vec![],
                })),
                false, // not async
            ),
        );

        // owned_string(value: str) -> identity(value)[read, write, owned]
        self.funcs.insert(
            "owned_string".to_string(),
            (
                vec![("value".to_string(), Type::Str)],
                Type::Identity(Box::new(IdentityType {
                    value: None,
                    capabilities: vec![CapabilityLevel::Read, CapabilityLevel::Write, CapabilityLevel::Owned],
                    delegatable: false,
                    constraints: vec![],
                })),
                false, // not async
            ),
        );

        // str_to_lowercase(s: str) -> str
        self.funcs.insert(
            "str_to_lowercase".to_string(),
            (
                vec![("s".to_string(), Type::Str)],
                Type::Str,
                false, // not async
            ),
        );

        // str_to_uppercase(s: str) -> str
        self.funcs.insert(
            "str_to_uppercase".to_string(),
            (
                vec![("s".to_string(), Type::Str)],
                Type::Str,
                false, // not async
            ),
        );

        // str_trim(s: str) -> str
        self.funcs.insert(
            "str_trim".to_string(),
            (
                vec![("s".to_string(), Type::Str)],
                Type::Str,
                false, // not async
            ),
        );

        // str_starts_with(haystack: str, needle: str) -> bool
        self.funcs.insert(
            "str_starts_with".to_string(),
            (
                vec![("haystack".to_string(), Type::Str), ("needle".to_string(), Type::Str)],
                Type::Bool,
                false, // not async
            ),
        );

        // str_ends_with(haystack: str, needle: str) -> bool
        self.funcs.insert(
            "str_ends_with".to_string(),
            (
                vec![("haystack".to_string(), Type::Str), ("needle".to_string(), Type::Str)],
                Type::Bool,
                false, // not async
            ),
        );

        // str_contains(haystack: str, needle: str) -> bool
        self.funcs.insert(
            "str_contains".to_string(),
            (
                vec![("haystack".to_string(), Type::Str), ("needle".to_string(), Type::Str)],
                Type::Bool,
                false, // not async
            ),
        );

        // str_replace(s: str, old: str, new: str) -> str
        self.funcs.insert(
            "str_replace".to_string(),
            (
                vec![("s".to_string(), Type::Str), ("old".to_string(), Type::Str), ("new".to_string(), Type::Str)],
                Type::Str,
                false, // not async
            ),
        );

        // Array runtime functions
        // array_new(capacity: usize) -> i64 (pointer to array)
        self.funcs.insert(
            "array_new".to_string(),
            (
                vec![("capacity".to_string(), Type::Usize)],
                Type::I64,
                false, // not async
            ),
        );
        
        // array_push(arr: i64, value: i64) -> void
        self.funcs.insert(
            "array_push".to_string(),
            (
                vec![("arr".to_string(), Type::I64), ("value".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        
        // array_len(arr: i64) -> i64
        self.funcs.insert(
            "array_len".to_string(),
            (
                vec![("arr".to_string(), Type::I64)],
                Type::I64,
                false, // not async
            ),
        );
        
        // array_get(arr: i64, index: i64) -> i64
        self.funcs.insert(
            "array_get".to_string(),
            (
                vec![("arr".to_string(), Type::I64), ("index".to_string(), Type::I64)],
                Type::I64,
                false, // not async
            ),
        );
        
        // array_set(arr: i64, index: i64, value: i64) -> void
        self.funcs.insert(
            "array_set".to_string(),
            (
                vec![("arr".to_string(), Type::I64), ("index".to_string(), Type::I64), ("value".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        
        // array_free(arr: i64) -> void
        self.funcs.insert(
            "array_free".to_string(),
            (
                vec![("arr".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        
        // Memory allocation functions
        // runtime_malloc(size: usize) -> i64 (pointer to allocated memory)
        self.funcs.insert(
            "runtime_malloc".to_string(),
            (
                vec![("size".to_string(), Type::Usize)],
                Type::I64,
                false, // not async
            ),
        );
        println!("[RESOLVER] Registered runtime_malloc");
        
        // map_get(map: i64, key: i64) -> i64
        self.funcs.insert(
            "map_get".to_string(),
            (
                vec![("map".to_string(), Type::I64), ("key".to_string(), Type::I64)],
                Type::I64,
                false, // not async
            ),
        );
        
        // print_i64(value: i64) -> void
        self.funcs.insert(
            "print_i64".to_string(),
            (
                vec![("value".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        
        // println() -> void
        self.funcs.insert(
            "println".to_string(),
            (
                vec![],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );

        // Vector constructor for Vector<u64, 8> (most common for Murphy's Sieve)
        // Note: This is a hack - we should handle generic vector types properly
        self.funcs.insert(
            "Vector::new".to_string(),
            (
                vec![("a0".to_string(), Type::U64), 
                     ("a1".to_string(), Type::U64),
                     ("a2".to_string(), Type::U64),
                     ("a3".to_string(), Type::U64),
                     ("a4".to_string(), Type::U64),
                     ("a5".to_string(), Type::U64),
                     ("a6".to_string(), Type::U64),
                     ("a7".to_string(), Type::U64)],
                Type::Vector(Box::new(Type::U64), ArraySize::Literal(8)),
                false, // not async
            ),
        );
        // Vector splat for Vector<u64, 8>
        self.funcs.insert(
            "Vector::splat".to_string(),
            (
                vec![("value".to_string(), Type::U64)],
                Type::Vector(Box::new(Type::U64), ArraySize::Literal(8)),
                false, // not async
            ),
        );
        
        // SIMD runtime functions for Vector<u64, 8>
        self.funcs.insert(
            "vector_make_u64x8".to_string(),
            (
                vec![("a0".to_string(), Type::I64),
                     ("a1".to_string(), Type::I64),
                     ("a2".to_string(), Type::I64),
                     ("a3".to_string(), Type::I64),
                     ("a4".to_string(), Type::I64),
                     ("a5".to_string(), Type::I64),
                     ("a6".to_string(), Type::I64),
                     ("a7".to_string(), Type::I64)],
                Type::I64, // Returns pointer to vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_splat_u64x8".to_string(),
            (
                vec![("value".to_string(), Type::I64)],
                Type::I64, // Returns pointer to vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_add_u64x8".to_string(),
            (
                vec![("a".to_string(), Type::I64), ("b".to_string(), Type::I64)],
                Type::I64, // Returns pointer to new vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_sub_u64x8".to_string(),
            (
                vec![("a".to_string(), Type::I64), ("b".to_string(), Type::I64)],
                Type::I64, // Returns pointer to new vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_mul_u64x8".to_string(),
            (
                vec![("a".to_string(), Type::I64), ("b".to_string(), Type::I64)],
                Type::I64, // Returns pointer to new vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_get_u64x8".to_string(),
            (
                vec![("ptr".to_string(), Type::I64), ("index".to_string(), Type::I64)],
                Type::I64, // Returns element value
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_set_u64x8".to_string(),
            (
                vec![("ptr".to_string(), Type::I64), ("index".to_string(), Type::I64), ("value".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_free_u64x8".to_string(),
            (
                vec![("ptr".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        
        // SIMD runtime functions for Vector<i32, 4>
        self.funcs.insert(
            "vector_make_i32x4".to_string(),
            (
                vec![("a0".to_string(), Type::I64),
                     ("a1".to_string(), Type::I64),
                     ("a2".to_string(), Type::I64),
                     ("a3".to_string(), Type::I64)],
                Type::I64, // Returns pointer to vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_splat_i32x4".to_string(),
            (
                vec![("value".to_string(), Type::I64)],
                Type::I64, // Returns pointer to vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_add_i32x4".to_string(),
            (
                vec![("a".to_string(), Type::I64), ("b".to_string(), Type::I64)],
                Type::I64, // Returns pointer to new vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_sub_i32x4".to_string(),
            (
                vec![("a".to_string(), Type::I64), ("b".to_string(), Type::I64)],
                Type::I64, // Returns pointer to new vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_mul_i32x4".to_string(),
            (
                vec![("a".to_string(), Type::I64), ("b".to_string(), Type::I64)],
                Type::I64, // Returns pointer to new vector
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_get_i32x4".to_string(),
            (
                vec![("ptr".to_string(), Type::I64), ("index".to_string(), Type::I64)],
                Type::I64, // Returns element value
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_set_i32x4".to_string(),
            (
                vec![("ptr".to_string(), Type::I64), ("index".to_string(), Type::I64), ("value".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        self.funcs.insert(
            "vector_free_i32x4".to_string(),
            (
                vec![("ptr".to_string(), Type::I64)],
                Type::Tuple(vec![]), // void
                false, // not async
            ),
        );
        
        println!(
            "[RESOLVER] Registered built-in runtime functions: clone_i64, is_null_i64, to_string_str, to_string_i64, to_string_bool, array_new, array_push, array_len, array_get, array_set, array_free, runtime_malloc, map_get, print_i64, println, Vector::new, vector_make_u64x8, vector_add_i32x4, etc."
        );
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for Resolver {
    fn drop(&mut self) {
        self.persist_specialization_cache();
    }
}
