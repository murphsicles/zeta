// src/borrow.rs
use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum BorrowState {
    Owned,
    Borrowed,
    MutBorrowed,
}

#[derive(Debug, Clone)]
pub struct BorrowChecker {
    borrows: HashMap<String, BorrowState>,
}

impl BorrowChecker {
    pub fn new() -> Self { Self { borrows: HashMap::new() } }

    pub fn enter_scope(&mut self) {}

    pub fn exit_scope(&mut self) {}

    pub fn check(&mut self, node: &AstNode) -> bool {
        match node {
            AstNode::Var(v) => {
                if let Some(state) = self.borrows.get(v) {
                    match state {
                        BorrowState::MutBorrowed => false,
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
                        BorrowState::MutBorrowed => false,
                    }
                } else {
                    true
                }
            }
            AstNode::Call { receiver, .. } => self.check(&AstNode::Var(receiver.clone())),
            _ => true,
        }
    }

    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var, state);
    }
}
