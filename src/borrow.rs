// src/borrow.rs
//! Borrow checker module for Zeta.
//! Implements affine borrow checking with speculative states for safe concurrency.
//! Tracks ownership, borrows, and moves to ensure memory safety without garbage collection.

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
            AstNode::Assign(v, expr) => {
                if !self.check(expr) {
                    return false;
                }
                self.declare(v.clone(), BorrowState::Owned);
                true
            }
            AstNode::Call { receiver, args, .. } => {
                if let Some(r) = receiver
                    && !self.check(r)
                {
                    return false;
                }
                for arg in args {
                    if !self.check(arg) {
                        return false;
                    }
                    if let AstNode::Var(name) = arg
                        && let Some(moved) = self.affine_moves.get_mut(name)
                        && !*moved
                    {
                        *moved = true;
                        self.borrows.insert(name.clone(), BorrowState::Consumed);
                    }
                }
                true
            }
            AstNode::TimingOwned { inner, .. } => self.check(inner),
            _ => true,
        }
    }
}
