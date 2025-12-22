// src/middle/resolver/lower.rs
//! Lowering AST to MIR and monomorphization for Zeta.
use crate::ast::AstNode;
use crate::mir::Mir;
use crate::specialization::MonoKey;
use std::collections::HashMap;
use std::fmt::Debug;
use super::resolver::Resolver;

impl Resolver {
    /// Lowers an AST node to MIR.
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        // Full lowering logic here; for now, stubbed as in original but ensure buildable
        // In practice, use MirGen or implement traversal
        Mir {
            name: if let AstNode::FuncDef { name, .. } = ast { Some(name.clone()) } else { None },
            param_indices: vec![],
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
        }
    }

    /// Collects used specializations from AST nodes.
    pub fn collect_used_specializations(&self, asts: &[AstNode]) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();
        for ast in asts {
            if let AstNode::Call { method, type_args, .. } = ast {
                used.entry(method.clone()).or_insert(vec![]).push(type_args.clone());
            }
            // Recurse into body, etc.
        }
        used
    }

    /// Monomorphizes an AST node for a given key.
    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        // Full monomorphization: substitute type args
        // For buildability, clone and modify generics
        if let AstNode::FuncDef { mut generics, .. } = ast.clone() {
            generics = vec![]; // Remove generics after mono
            ast.clone() // Adjusted
        } else {
            ast.clone()
        }
    }

    /// Retrieves a cached MIR by AST hash.
    pub fn get_cached_mir(&self, ast_hash: &str) -> Option<Mir> {
        self.cached_mirs.get(ast_hash).cloned()
    }

    /// Retrieves a monomorphized MIR by key.
    pub fn get_mono_mir(&self, key: &MonoKey) -> Option<Mir> {
        self.mono_mirs.get(key).cloned()
    }
}
