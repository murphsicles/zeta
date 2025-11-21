// src/resolver.rs
use crate::ast::AstNode;
use crate::borrow::BorrowChecker;
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
    borrow_checker: BorrowChecker,
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
            borrow_checker: BorrowChecker::new(),
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
        let mut mir_gen = MirGen::new();
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

    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;
        for ast in asts {
            match ast {
                AstNode::FuncDef { body, .. } => {
                    for stmt in body {
                        if !self.borrow_checker.check(stmt) {
                            ok = false;
                        }
                    }
                    if !self.borrow_checker.validate_affine(body) {
                        ok = false;
                    }
                    if !self.borrow_checker.validate_speculative(body) {
                        ok = false;
                    }
                }
                _ => {}
            }
        }
        ok
    }

    pub fn resolve_impl(&self, trait_name: &str, ty: &str) -> Option<&AstNode> {
        self.impls.get(&(trait_name.to_string(), ty.to_string()))
    }
}
