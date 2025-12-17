// src/borrow.rs
//! Borrow checker for Zeta language.
//! Enforces affine type rules with speculative states for concurrency safety.
//! Tracks ownership, borrows, and moves to prevent memory errors.
use crate::ast::AstNode;
use std::collections::HashMap;
/// Represents the borrow state of a variable.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowState {
    /// Fully owned and movable.
    Owned,
    /// Immutable borrow active; moves disallowed.
    Borrowed,
    /// Mutable borrow active; exclusive access.
    MutBorrowed,
    /// Consumed; no further access allowed.
    Consumed,
}
/// Represents speculative execution state for variables.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpeculativeState {
    /// Committed state; no speculation.
    Safe,
    /// Under speculation; may rollback.
    Speculative,
    /// Speculation failed; path invalid.
    Poisoned,
}
/// Manages borrow checking with state tracking.
#[derive(Debug, Clone)]
pub struct BorrowChecker {
    /// Variable to borrow state mapping.
    borrows: HashMap<String, BorrowState>,
    /// Variable to affine move flag mapping.
    affine_moves: HashMap<String, bool>,
    /// Variable to speculative state mapping.
    speculative: HashMap<String, SpeculativeState>,
}
impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}
impl BorrowChecker {
    /// Creates a new empty borrow checker.
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            affine_moves: HashMap::new(),
            speculative: HashMap::new(),
        }
    }
    /// Declares a variable with initial state.
    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var, SpeculativeState::Safe);
    }
    /// Validates borrow rules for an AST node and updates states.
    /// Returns true if valid, false on violation.
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
                    AstNode::Subscript {
                        ref base,
                        ref index,
                    } => {
                        if !self.check(base) || !self.check(index) {
                            return false;
                        }
                        if let AstNode::Var(ref name) = **base
                            && let Some(&state) = self.borrows.get(name)
                            && !matches!(state, BorrowState::Owned | BorrowState::MutBorrowed)
                        {
                            return false;
                        } else if let AstNode::Var(ref name) = **base {
                            self.borrows.insert(name.clone(), BorrowState::MutBorrowed);
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
            AstNode::DictLit { entries } => {
                entries.iter().all(|(k, v)| self.check(k) && self.check(v))
            }
            AstNode::Subscript { base, index } => self.check(base) && self.check(index),
            AstNode::Return(inner) => self.check(inner),
            _ => true,
        }
    }
}
