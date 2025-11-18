// src/borrow.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowState {
    Owned,
    Borrowed,
    MutBorrowed,
    Consumed, // Affine: moved/consumed, unusable after
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpeculativeState {
    Safe,        // No spec-sensitive data
    Speculative, // Potential timing leak
    Poisoned,    // Invalid spec path
}

#[derive(Debug, Clone)]
pub struct BorrowChecker {
    borrows: HashMap<String, BorrowState>,
    affine_moves: HashMap<String, bool>, // Track if affine move occurred
    speculative: HashMap<String, SpeculativeState>, // Speculative exec tracking
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            affine_moves: HashMap::new(),
            speculative: HashMap::new(),
        }
    }

    pub fn enter_scope(&mut self) {}

    pub fn exit_scope(&mut self) {}

    pub fn declare(&mut self, var: String, state: BorrowState) {
        let var_clone = var.clone();
        self.borrows.insert(var_clone.clone(), state);
        self.affine_moves.insert(var_clone, false);
        self.speculative.insert(var, SpeculativeState::Safe);
    }

    pub fn check(&mut self, node: &AstNode) -> bool {
        match node {
            AstNode::Var(v) => {
                if let Some(state) = self.borrows.get(v) {
                    match state {
                        BorrowState::MutBorrowed | BorrowState::Consumed => false,
                        _ => true,
                    }
                } else {
                    true
                }
            }
            AstNode::Borrow(v) => {
                if let Some(state) = self.borrows.get(v) {
                    match state {
                        BorrowState::Owned | BorrowState::Borrowed => {
                            self.borrows.insert(v.clone(), BorrowState::Borrowed);
                            // Spec: Borrow may enable spec paths
                            if let Some(spec) = self.speculative.get_mut(v) {
                                if *spec == SpeculativeState::Safe {
                                    *spec = SpeculativeState::Speculative;
                                }
                            }
                            true
                        }
                        BorrowState::MutBorrowed | BorrowState::Consumed => false,
                    }
                } else {
                    true
                }
            }
            AstNode::Assign(v, expr) => {
                // Affine: Assign consumes rhs if owned
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
            }
            AstNode::Call { receiver, args, .. } => {
                // Affine move on receiver/args if owned
                if !self.check(&AstNode::Var(receiver.clone())) {
                    return false;
                }
                for arg in args {
                    if !self.check(&AstNode::Var(arg.clone())) {
                        return false;
                    }
                    // Mark as consumed if affine
                    if let Some(moved) = self.affine_moves.get_mut(arg) {
                        if !*moved {
                            *moved = true;
                            self.borrows.insert(arg.clone(), BorrowState::Consumed);
                        }
                    }
                }
                // Spec: Calls may branch speculatively
                self.mark_speculative(receiver);
                for arg in args {
                    self.mark_speculative(arg);
                }
                true
            }
            AstNode::TimingOwned { ty: _, inner } => {
                // Affine/Spec: TimingOwned consumes inner, tracks spec erase
                if !self.check(inner.as_ref()) {
                    return false;
                }
                // Speculative exec: Ensure no leak in constant-time paths
                if let Some(expr_var) = self.extract_var(inner.as_ref()) {
                    if let Some(spec) = self.speculative.get_mut(&expr_var) {
                        if *spec == SpeculativeState::Speculative {
                            *spec = SpeculativeState::Poisoned; // Invalidate leak
                        }
                    }
                }
                true
            }
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

    fn extract_var(&self, node: &AstNode) -> Option<String> {
        match node {
            AstNode::Var(v) => Some(v.clone()),
            _ => None,
        }
    }

    pub fn validate_affine(&self, body: &[AstNode]) -> bool {
        // Post-check: Ensure affine vars not used post-move
        for node in body {
            if let AstNode::Var(v) = node {
                if let (Some(true), Some(BorrowState::Consumed)) =
                    (self.affine_moves.get(v), self.borrows.get(v))
                {
                    return false; // Used after consume
                }
            }
        }
        true
    }

    pub fn validate_speculative(&self, body: &[AstNode]) -> bool {
        // Post-check: No poisoned spec states in final paths
        for node in body {
            if let AstNode::Var(v) = node {
                if let Some(SpeculativeState::Poisoned) = self.speculative.get(v) {
                    return false; // Leak in spec path
                }
            }
        }
        // Ensure TimingOwned covers all speculative vars
        let spec_vars: Vec<_> = self
            .speculative
            .iter()
            .filter(|(_, s)| **s == SpeculativeState::Speculative)
            .map(|(v, _)| v)
            .collect();
        // Stub: Check coverage (e.g., all wrapped in TimingOwned)
        spec_vars.is_empty()
            || body
                .iter()
                .any(|n| matches!(n, AstNode::TimingOwned { .. }))
    }
}
