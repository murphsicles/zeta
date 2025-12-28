// src/middle/resolver/lower.rs
use super::resolver::Resolver;
use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::MonoKey;
use std::collections::HashMap;

impl Resolver {
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = crate::middle::mir::r#gen::MirGen::new();
        mir_gen.lower_to_mir(ast)
    }

    /// Collects all used generic specializations in the program.
    pub fn collect_used_specializations(
        &self,
        asts: &[AstNode],
    ) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();

        fn walk(node: &AstNode, used: &mut HashMap<String, Vec<Vec<String>>>) {
            if let AstNode::Call {
                method,
                type_args,
                ..
            } = node
            {
                if !type_args.is_empty() {
                    used.entry(method.clone())
                        .or_insert_with(Vec::new)
                        .push(type_args.clone());
                }
            }

            // Recurse into bodies
            if let AstNode::FuncDef { body, .. } = node {
                for stmt in body {
                    walk(stmt, used);
                }
            }
        }

        for ast in asts {
            walk(ast, &mut used);
        }

        used
    }

    /// Full monomorphization with type substitution.
    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        let mut mono = ast.clone();

        if let AstNode::FuncDef {
            generics,
            params,
            ret,
            body,
            ..
        } = &mut mono
        {
            if generics.len() != key.type_args.len() {
                return mono; // arity mismatch – should be reported earlier
            }

            let subst: HashMap<String, String> = generics
                .iter()
                .cloned()
                .zip(key.type_args.iter().cloned())
                .collect();

            // Substitute parameter and return types
            for (_, ty) in params.iter_mut() {
                if let Some(repl) = subst.get(ty) {
                    *ty = repl.clone();
                }
            }
            if let Some(repl) = subst.get(ret) {
                *ret = repl.clone();
            }

            // Clear generics
            generics.clear();

            // TODO: deep substitution in body for associated types and nested generics
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
