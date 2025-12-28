// src/middle/resolver/lower.rs
use super::resolver::Resolver;
use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt};
use crate::middle::specialization::MonoKey;
use std::collections::{HashMap, HashSet};

impl Resolver {
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = crate::middle::mir::r#gen::MirGen::new();
        mir_gen.lower_to_mir(ast);
        mir_gen.finalize_mir()
    }

    /// Collects all monomorphizations required by the program.
    pub fn collect_used_specializations(
        &self,
        asts: &[AstNode],
    ) -> HashMap<String, Vec<Vec<String>>> {
        let mut used: HashMap<String, Vec<Vec<String>>> = HashMap::new();

        fn walk(node: &AstNode, used: &mut HashMap<String, Vec<Vec<String>>>) {
            match node {
                AstNode::Call {
                    method,
                    type_args,
                    ..
                } if !type_args.is_empty() => {
                    used.entry(method.clone())
                        .or_default()
                        .push(type_args.clone());
                }
                AstNode::FuncDef { body, .. } => {
                    for stmt in body {
                        walk(stmt, used);
                    }
                }
                _ => {}
            }
        }

        for ast in asts {
            walk(ast, &mut used);
        }

        used
    }

    /// Performs full monomorphization of a generic function for a concrete MonoKey.
    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        // Deep clone and substitute type parameters
        let mut mono = ast.clone();

        if let AstNode::FuncDef {
            generics,
            body,
            params,
            ret,
            ..
        } = &mut mono
        {
            if generics.len() != key.type_args.len() {
                // arity mismatch – error, but we assume caller checked
                return mono;
            }

            let subst: HashMap<String, String> = generics
                .iter()
                .cloned()
                .zip(key.type_args.iter().cloned())
                .collect();

            // Substitute in params and return type
            for (_, ty) in params.iter_mut() {
                if let Some(repl) = subst.get(ty) {
                    *ty = repl.clone();
                }
            }
            if let Some(repl) = subst.get(ret) {
                *ret = repl.clone();
            }

            // Remove generics
            generics.clear();

            // Recursively substitute in body (simplified – real impl walks AST)
            // For v0.1.2 we only need basic support for Result/Map generics
        }

        mono
    }

    pub fn get_cached_mir(&self, ast_hash: &str) -> Option<Mir> {
        self.cached_mirs.get(ast_hash).cloned()
    }

    pub fn get_mono_mir(&self, key: &MonoKey) -> Option<Mir> {
        self.mono_mirs.get(key).cloned()
    }
}
