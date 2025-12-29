// src/frontend/borrow.rs
//! Borrow checker for Zeta language.
//! Enforces affine type rules with speculative states for concurrency safety.
//! Tracks ownership, borrows, and moves to prevent memory errors.
use crate::frontend::ast::AstNode;
use crate::middle::resolver::resolver::{Resolver, Type};
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
    /// Variable to type mapping.
    types: HashMap<String, Type>,
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
            types: HashMap::new(),
        }
    }
    /// Declares a variable with initial state and type.
    pub fn declare(&mut self, var: String, state: BorrowState, ty: Type) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var.clone(), SpeculativeState::Safe);
        self.types.insert(var, ty);
    }
    /// Validates borrow rules for an AST node and updates states.
    /// Returns true if valid, false on violation.
    pub fn check(&mut self, node: &AstNode, resolver: &mut Resolver) -> bool {
        match node {
            AstNode::Var(v) => self
                .borrows
                .get(v)
                .is_some_and(|s| matches!(s, BorrowState::Owned | BorrowState::Borrowed)),
            AstNode::Assign(lhs, rhs) => {
                if !self.check(rhs, resolver) {
                    return false;
                }
                match **lhs {
                    AstNode::Var(ref v) => {
                        let ty = resolver.infer_type(rhs);
                        self.declare(v.clone(), BorrowState::Owned, ty);
                        true
                    }
                    AstNode::Subscript {
                        ref base,
                        ref index,
                    } => {
                        if !self.check(base, resolver) || !self.check(index, resolver) {
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
            AstNode::BinaryOp { left, right, .. } => {
                self.check(left, resolver) && self.check(right, resolver)
            }
            AstNode::FString(parts) => parts.iter().all(|p| self.check(p, resolver)),
            AstNode::TimingOwned { inner, .. } => self.check(inner, resolver),
            AstNode::Defer(inner) => self.check(inner, resolver),
            AstNode::Call { receiver, args, .. } => {
                if let Some(r) = receiver.as_ref()
                    && !self.check(r, resolver)
                {
                    return false;
                }
                for arg in args {
                    if !self.check(arg, resolver) {
                        return false;
                    }
                    if let AstNode::Var(ref name) = *arg
                        && let Some(ty) = self.types.get(name)
                        && !resolver.is_copy(ty)
                        && !*self.affine_moves.get(name).unwrap_or(&false)
                    {
                        *self.affine_moves.entry(name.clone()).or_insert(false) = true;
                        self.borrows.insert(name.clone(), BorrowState::Consumed);
                    }
                }
                true
            }
            AstNode::TryProp { expr } => self.check(expr, resolver),
            AstNode::DictLit { entries } => entries
                .iter()
                .all(|(k, v)| self.check(k, resolver) && self.check(v, resolver)),
            AstNode::Subscript { base, index } => {
                self.check(base, resolver) && self.check(index, resolver)
            }
            AstNode::Return(inner) => self.check(inner, resolver),
            _ => true,
        }
    }
}
