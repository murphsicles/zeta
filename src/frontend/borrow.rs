// src/frontend/borrow.rs
//! # Borrow Checker
//!
//! Enforces affine/ownership rules and speculative concurrency safety.

use crate::frontend::ast::AstNode;
use crate::middle::resolver::resolver::{Resolver, Type};
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
    types: HashMap<String, Type>,
}

impl Default for BorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            borrows: HashMap::new(),
            affine_moves: HashMap::new(),
            speculative: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn declare(&mut self, var: String, state: BorrowState, ty: Type) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var.clone(), SpeculativeState::Safe);
        self.types.insert(var, ty);
    }

    pub fn check(&mut self, node: &AstNode, resolver: &Resolver) -> bool {
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
                        if let AstNode::Var(ref name) = **base {
                            if let Some(&state) = self.borrows.get(name) {
                                if !matches!(state, BorrowState::Owned | BorrowState::MutBorrowed) {
                                    return false;
                                }
                            }
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
                if let Some(r) = receiver {
                    if !self.check(r, resolver) {
                        return false;
                    }
                    if let AstNode::Var(ref name) = **r {
                        self.borrows.insert(name.clone(), BorrowState::Borrowed);
                    }
                }
                for arg in args {
                    if !self.check(arg, resolver) {
                        return false;
                    }
                    if let AstNode::Var(ref name) = *arg {
                        if let Some(ty) = self.types.get(name) {
                            if !resolver.is_copy(ty)
                                && !*self.affine_moves.get(name).unwrap_or(&false)
                            {
                                *self.affine_moves.entry(name.clone()).or_insert(false) = true;
                                self.borrows.insert(name.clone(), BorrowState::Consumed);
                            }
                        }
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
            AstNode::ExprStmt { expr } => self.check(expr, resolver),
            _ => true,
        }
    }
}
