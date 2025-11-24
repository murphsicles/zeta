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
        }
    }

    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var, SpeculativeState::Safe);
    }

    pub fn check(&mut self, node: &AstNode) -> bool {
        match node {
            AstNode::Var(v) => self.borrows.get(v).is_none_or(|s| {
                matches!(s, BorrowState::Owned | BorrowState::Borrowed)
            }),
            AstNode::Assign(v, expr) => {
                if !self.check(expr) {
                    return false;
                }
                self.declare(v.clone(), BorrowState::Owned);
                true
            }
            AstNode::Call { receiver, args, .. } => {
                if let Some(r) = receiver && !self.check(r) {
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
