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

            match node {
                AstNode::FuncDef { body, .. } => {
                    for stmt in body {
                        walk(stmt, used);
                    }
                }
                AstNode::If { then: then_body, else_: else_body, .. } => {
                    for stmt in then_body {
                        walk(stmt, used);
                    }
                    for stmt in else_body {
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

    pub fn monomorphize(&self, key: MonoKey, ast: &AstNode) -> AstNode {
        let mut mono = ast.clone();

        let subst: HashMap<String, String> = key
            .type_args
            .iter()
            .cloned()
            .zip(key.type_args.iter().cloned())
            .collect();

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
                        if let Some(repl) = subst.get(ty) {
                            *ty = repl.clone();
                        }
                    }
                    if let Some(repl) = subst.get(ret) {
                        *ret = repl.clone();
                    }
                    for stmt in body {
                        substitute(stmt, subst);
                    }
                }
                AstNode::Call { type_args, .. } => {
                    type_args.clear();
                }
                AstNode::TimingOwned { ty, inner } => {
                    if let Some(repl) = subst.get(ty) {
                        *ty = repl.clone();
                    }
                    substitute(inner, subst);
                }
                AstNode::BinaryOp { left, right, .. } => {
                    substitute(left, subst);
                    substitute(right, subst);
                }
                AstNode::If { cond, then, else_, .. } => {
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

    pub fn get_cached_mir(&self, ast_hash: &str) -> Option<Mir> {
        self.cached_mirs.get(ast_hash).cloned()
    }

    pub fn get_mono_mir(&self, key: &MonoKey) -> Option<Mir> {
        self.mono_mirs.get(key).cloned()
    }
}
