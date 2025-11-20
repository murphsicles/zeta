// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::{BorrowChecker, BorrowState};
use crate::mir::{Mir, MirGen, MirStmt, SemiringOp};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};

#[derive(Debug)]
pub struct Resolver {
    concepts: HashMap<String, AstNode>,
    impls: HashMap<(String, String), AstNode>,
    funcs: HashMap<String, AstNode>,
    structs: HashMap<String, AstNode>,
    common_traits: HashSet<String>,
    memo_impl: Mutex<HashMap<(String, String), Option<Arc<AstNode>>>>,
}

impl Resolver {
    pub fn new() -> Self {
        let mut common_traits = HashSet::new();
        for t in ["Send", "Sync", "Addable", "CacheSafe", "Copy", "Eq"] {
            common_traits.insert(t.to_string());
        }
        Self {
            concepts: HashMap::new(),
            impls: HashMap::new(),
            funcs: HashMap::new(),
            structs: HashMap::new(),
            common_traits,
            memo_impl: Mutex::new(HashMap::new()),
        }
    }

    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ConceptDef { ref name, .. } => {
                self.concepts.insert(name.clone(), ast);
            }
            AstNode::ImplBlock { ref concept, ref ty, .. } => {
                self.impls.insert((concept.clone(), ty.clone()), ast);
            }
            AstNode::FuncDef { ref name, .. } => {
                self.funcs.insert(name.clone(), ast);
            }
            AstNode::StructDef { ref name, .. } => {
                self.structs.insert(name.clone(), ast);
            }
            AstNode::Derive { ty, traits } => {
                for tr in traits {
                    self.impls.insert(
                        (tr.clone(), ty.clone()),
                        AstNode::ImplBlock {
                            concept: tr,
                            ty: ty.clone(),
                            body: vec![],
                        },
                    );
                }
            }
            _ => {}
        }
    }

    pub fn lower_to_mir(&self, ast: &AstNode) -> Mir {
        let mut gen = MirGen::new();
        gen.gen_mir(ast)
    }

    pub fn fold_semiring_chains(&self, mir: &mut Mir) -> bool {
        let mut changed = false;
        let mut i = 0;
        while i < mir.stmts.len() {
            if let MirStmt::Call { func, args, dest } = &mir.stmts[i] {
                let op = if func.ends_with(".add") {
                    Some(SemiringOp::Add)
                } else if func.ends_with(".mul") {
                    Some(SemiringOp::Mul)
                } else {
                    None
                };

                if let Some(op) = op {
                    let mut chain = vec![args[0]];
                    let mut current = *dest;
                    let mut j = i + 1;

                    while j < mir.stmts.len() {
                        if let MirStmt::Call {
                            func: next_func,
                            args: next_args,
                            dest: next_dest,
                        } = &mir.stmts[j]
                        {
                            if next_func == func && next_args[0] == current {
                                chain.push(next_args[1]);
                                current = *next_dest;
                                j += 1;
                                changed = true;
                            } else {
                                break;
                            }
                        } else {
                            break;
                        }
                    }

                    if chain.len() >= 3 {
                        mir.stmts[i] = MirStmt::SemiringFold {
                            op,
                            values: chain,
                            result: current,
                        };
                        mir.stmts.drain(i + 1..j);
                    }
                }
            }
            i += 1;
        }
        changed
    }

    pub fn resolve_impl(&self, concept: &str, ty: &str) -> Option<Arc<AstNode>> {
        let key = (concept.to_string(), ty.to_string());
        {
            let memo = self.memo_impl.lock().unwrap();
            if let Some(cached) = memo.get(&key) {
                return cached.clone();
            }
        }
        let result = self.impls.get(&key).map(|n| Arc::new(n.clone()));
        self.memo_impl.lock().unwrap().insert(key, result.clone());
        result
    }

    pub fn has_method(&self, concept: &str, ty: &str, method: &str) -> bool {
        self.resolve_impl(concept, ty)
            .map_or(false, |imp| {
                if let AstNode::ImplBlock { body, .. } = imp.as_ref() {
                    body.iter().any(|m| matches!(m, AstNode::Method { name, .. } if name == method))
                } else {
                    false
                }
            })
    }

    pub fn typecheck(&self, asts: &[AstNode]) -> bool {
        asts.iter().all(|ast| {
            match ast {
                AstNode::FuncDef {
                    params,
                    body,
                    attrs,
                    ..
                } => {
                    let stable_abi_ok = !attrs.contains(&"stable_abi".to_string())
                        || !params.iter().any(|(_, t)| t.contains('<'));
                    let mut bc = BorrowChecker::new();
                    for (n, _) in params {
                        bc.declare(n.clone(), BorrowState::Owned);
                    }
                    let borrow_ok = body.iter().all(|n| bc.check(n))
                        && bc.validate_affine(body)
                        && bc.validate_speculative(body);
                    stable_abi_ok && borrow_ok
                }
                AstNode::TimingOwned { ty, .. } => self.resolve_impl("CacheSafe", ty).is_some(),
                AstNode::SpawnActor { actor_ty, .. } => {
                    self.resolve_impl("Send", actor_ty).is_some()
                        && self.resolve_impl("CacheSafe", actor_ty).is_some()
                }
                _ => true,
            }
        })
    }
}
