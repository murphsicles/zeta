// src/borrow.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowState {
    Owned,
    Borrowed,
    MutBorrowed,
    Consumed,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpeculativeState {
    Safe,
    Speculative,
    Poisoned,
}

#[derive(Debug, Clone)]
pub struct BorrowChecker {
    borrows: HashMap<String, BorrowState>,
    affine_moves: HashMap<String, bool>,
    speculative: HashMap<String, SpeculativeState>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            affine_moves: HashMap::new(),
            speculative: HashMap::new(),
        }
    }

    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var, SpeculativeState::Safe);
    }

    pub fn check(&mut self, node: &AstNode) -> bool {
        match node {
            AstNode::Var(v) => self.borrows.get(v).map_or(true, |state| {
                *state != BorrowState::Consumed && *state != BorrowState::MutBorrowed
            }),
            AstNode::Assign(v, expr) => {
                if !self.check(expr.as_ref()) {
                    return false;
                }
                if let Some(state) = self.borrows.get(v) {
                    if *state == BorrowState::Owned {
                        self.affine_moves.insert(v.clone(), true);
                    }
                }
                self.declare(v.clone(), BorrowState::Owned);
                true
            },
            AstNode::Call { receiver, args, .. } => {
                if !self.check(receiver.as_ref()) {
                    return false;
                }
                for arg in args {
                    if !self.check(arg) {
                        return false;
                    }
                    if let AstNode::Var(name) = arg {
                        if let Some(moved) = self.affine_moves.get_mut(name) {
                            if !*moved {
                                *moved = true;
                                self.borrows.insert(name.clone(), BorrowState::Consumed);
                            }
                        }
                    }
                }
                if let AstNode::Var(name) = receiver.as_ref() {
                    self.mark_speculative(name);
                }
                for arg in args {
                    if let AstNode::Var(name) = arg {
                        self.mark_speculative(name);
                    }
                }
                true
            },
            AstNode::TimingOwned { inner, .. } => {
                if !self.check(inner.as_ref()) {
                    return false;
                }
                if let AstNode::Var(name) = inner.as_ref() {
                    if let Some(spec) = self.speculative.get_mut(name) {
                        if *spec == SpeculativeState::Speculative {
                            *spec = SpeculativeState::Poisoned;
                        }
                    }
                }
                true
            },
            _ => true,
        }
    }

    fn mark_speculative(&mut self, var: &str) {
        if let Some(spec) = self.speculative.get_mut(var) {
            if *spec == SpeculativeState::Safe {
                *spec = SpeculativeState::Speculative;
            }
        }
    }

    pub fn validate_affine(&self, body: &[AstNode]) -> bool {
        for node in body {
            if let AstNode::Var(v) = node {
                if self.affine_moves.get(v).copied().unwrap_or(false)
                    && self.borrows.get(v) == Some(&BorrowState::Consumed)
                {
                    return false;
                }
            }
        }
        true
    }

    pub fn validate_speculative(&self, body: &[AstNode]) -> bool {
        for node in body {
            if let AstNode::Var(v) = node {
                if self.speculative.get(v) == Some(&SpeculativeState::Poisoned) {
                    return false;
                }
            }
        }
        let spec_vars: Vec<_> = self
            .speculative
            .iter()
            .filter(|(_, s)| **s == SpeculativeState::Speculative)
            .map(|(v, _)| v.clone())
            .collect();
        spec_vars.is_empty() || body.iter().any(|n| matches!(n, AstNode::TimingOwned { .. }))
    }
}
