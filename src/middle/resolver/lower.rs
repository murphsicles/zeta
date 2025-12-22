// src/middle/resolver/lower.rs
//! Lowering AST to MIR and monomorphization for Zeta.
use crate::frontend::ast::AstNode;
use crate::middle::mir::mir::Mir;
use crate::middle::specialization::MonoKey;
use std::collections::HashMap;
use std::fmt::Debug;
use super::resolver::Resolver;

impl Resolver {
    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut mir_gen = crate::middle::mir::r#gen::MirGen::new();
        mir_gen.lower_to_mir(ast)
    }

    pub fn collect_used_specializations(&self, asts: &[AstNode]) -> HashMap<String, Vec<Vec<String>>> {
        let mut used = HashMap::new();
        for ast in asts {
            if let AstNode::Call { method, type_args, .. } = ast {
                if !type_args.is_empty() {
                    used.entry(method.clone()).or_insert(vec![]).push(type_args.clone());
                }
            }
            if let AstNode::FuncDef { body, .. } = ast {
                let body_used = self.collect_used_specializations(body);
                for (fn_name, specs) in body_used {
                    used.entry(fn_name).or_insert(vec![]).extend(specs);
                }
            }
        }
        used
    }

    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        if let AstNode::FuncDef { generics, .. } = ast {
            let mut mono_ast = ast.clone();
            if let AstNode::FuncDef { generics: ref mut g, .. } = mono_ast {
                *g = vec![];
            }
            mono_ast
        } else {
            ast.clone()
        }
    }

    pub fn get_cached_mir(&self, ast_hash: &str) -> Option<Mir> {
        self.cached_mirs.get(ast_hash).cloned()
    }

    pub fn get_mono_mir(&self, key: &MonoKey) -> Option<Mir> {
        self.mono_mirs.get(key).cloned()
    }
}
