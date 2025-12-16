// src/borrow.rs
//! Borrow checker module for Zeta.
//! Implements affine borrow checking with speculative states for safe concurrency.
//! Tracks ownership, borrows, and moves to ensure memory safety without garbage collection.
//! Updated Dec 13, 2025: Implicit &str borrows from str; no lifetimes.
//! Updated Dec 16, 2025: Updated for TryProp, DictLit, Subscript (as lvalue), Return; changed check for Assign with complex lhs.

use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowState {
    /// Variable is fully owned and movable.
    Owned,
    /// Immutable borrow in scope; no moves allowed.
    Borrowed,
    /// Mutable borrow in scope; exclusive access.
    MutBorrowed,
    /// Variable consumed; no further access.
    Consumed,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpeculativeState {
    /// No speculation; state is committed.
    Safe,
    /// Under speculative execution; rollback possible.
    Speculative,
    /// Speculation failed; invalidate path.
    Poisoned,
}

#[derive(Debug, Clone)]
pub struct BorrowChecker {
    /// Maps variable names to their current borrow state.
    borrows: HashMap<String, BorrowState>,
    /// Tracks affine moves: true if moved once.
    affine_moves: HashMap<String, bool>,
    /// Speculative states for each variable.
    speculative: HashMap<String, SpeculativeState>,
}

impl Default for BorrowChecker {
    /// Creates a new borrow checker with empty maps.
    fn default() -> Self {
        Self::new()
    }
}

impl BorrowChecker {
    /// Initializes an empty borrow checker.
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            affine_moves: HashMap::new(),
            speculative: HashMap::new(),
        }
    }

    /// Declares a new variable with initial owned state.
    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var, SpeculativeState::Safe);
    }

    /// Checks borrow rules for an AST node, updating states as needed.
    /// Returns true if valid, false if violation detected.
    pub fn check(&mut self, node: &AstNode) -> bool {
        match node {
            AstNode::Var(v) => self
                .borrows
                .get(v)
                .is_none_or(|s| matches!(s, BorrowState::Owned | BorrowState::Borrowed)),
            AstNode::Assign(lhs, rhs) => {
                if !self.check(rhs) {
                    return false;
                }
                match **lhs {
                    AstNode::Var(ref v) => {
                        self.declare(v.clone(), BorrowState::Owned);
                        true
                    }
                    AstNode::Subscript { ref base, ref index } => {
                        if !self.check(base) || !self.check(index) {
                            return false;
                        }
                        if let AstNode::Var(ref name) = **base {
                            if let Some(&state) = self.borrows.get(name) {
                                if !matches!(state, BorrowState::Owned | BorrowState::MutBorrowed) {
                                    return false;
                                }
                                self.borrows.insert(name.clone(), BorrowState::MutBorrowed);
                            }
                        }
                        true
                    }
                    _ => false,
                }
            }
            AstNode::BinaryOp { left, right, .. } => self.check(left) && self.check(right),
            AstNode::FString(parts) => parts.iter().all(|p| self.check(p)),
            AstNode::TimingOwned { inner, .. } => self.check(inner),
            AstNode::Defer(inner) => self.check(inner),
            AstNode::Call { receiver, args, .. } => {
                if let Some(r) = receiver.as_ref()
                    && !self.check(r)
                {
                    return false;
                }
                for arg in args {
                    if !self.check(arg) {
                        return false;
                    }
                    // Implicit borrow for str args
                    if let AstNode::Var(name) = arg
                        && let Some(state) = self.borrows.get(name)
                    {
                        if *state == BorrowState::Owned && name.ends_with("_str") { // Stub for str
                            // Allow implicit borrow without consume
                        } else if !*self.affine_moves.get(name).unwrap_or(&false) {
                            *self.affine_moves.entry(name.clone()).or_insert(false) = true;
                            self.borrows.insert(name.clone(), BorrowState::Consumed);
                        }
                    }
                }
                true
            }
            AstNode::TryProp { expr } => self.check(expr),
            AstNode::DictLit { entries } => entries.iter().all(|(k, v)| self.check(k) && self.check(v)),
            AstNode::Subscript { base, index } => self.check(base) && self.check(index),
            AstNode::Return(inner) => self.check(inner),
            _ => true,
        }
    }
}
