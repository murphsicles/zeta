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

#[derive(Debug, Clone)]
pub struct BorrowChecker {
    borrows: HashMap<String, BorrowState>,
    affine_moves: HashMap<String, bool>, // Track if affine move occurred
}

impl BorrowChecker {
    pub fn new() -> Self { 
        Self { 
            borrows: HashMap::new(), 
            affine_moves: HashMap::new() 
        } 
    }

    pub fn enter_scope(&mut self) {}

    pub fn exit_scope(&mut self) {}

    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var, state);
        self.affine_moves.insert(var, false);
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
                self.check(expr)?;
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
                if !self.check(&AstNode::Var(receiver.clone())) { return false; }
                for arg in args {
                    if !self.check(&AstNode::Var(arg.clone())) { return false; }
                    // Mark as consumed if affine
                    if let Some(false) = self.affine_moves.get_mut(arg) {
                        *self.affine_moves.get_mut(arg).unwrap() = true;
                        self.borrows.insert(arg.clone(), BorrowState::Consumed);
                    }
                }
                true
            }
            AstNode::TimingOwned { ty: _, inner } => {
                // Affine: TimingOwned consumes inner for constant-time erase
                self.check(inner)?;
                true // Stub: Extend for spec exec tracking
            }
            _ => true,
        }
    }

    pub fn validate_affine(&self, body: &[AstNode]) -> bool {
        // Post-check: Ensure affine vars not used post-move
        for node in body {
            if let AstNode::Var(v) = node {
                if let Some(true) = self.affine_moves.get(v) {
                    if let Some(BorrowState::Consumed) = self.borrows.get(v) {
                        return false; // Used after consume
                    }
                }
            }
        }
        true
    }
}
