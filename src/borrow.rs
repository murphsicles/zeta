use crate::ast::AstNode;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BorrowState { Owned, Borrowed, MutBorrowed, Consumed }

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SpeculativeState { Safe, Speculative, Poisoned }

pub struct BorrowChecker {
    borrows: HashMap<String, BorrowState>,
    affine_moves: HashMap<String, bool>,
    speculative: HashMap<String, SpeculativeState>,
}

impl BorrowChecker {
    pub fn new() -> Self { Self { borrows: HashMap::new(), affine_moves: HashMap::new(), speculative: HashMap::new() } }

    pub fn declare(&mut self, var: String, state: BorrowState) {
        self.borrows.insert(var.clone(), state);
        self.affine_moves.insert(var.clone(), false);
        self.speculative.insert(var, SpeculativeState::Safe);
    }

    pub fn check(&mut self, node: &AstNode) -> bool {
        match node {
            AstNode::Var(v) => self.borrows.get(v).map_or(true, |s| !matches!(s, BorrowState::Consumed)),
            AstNode::Let { name, rhs, .. } => {
                self.check(rhs.as_ref());
                self.declare(name.clone(), BorrowState::Owned);
                true
            }
            AstNode::Call { receiver, args, .. } => {
                if let AstNode::Var(r) = receiver.as_ref() {
                    self.mark_speculative(r);
                }
                for arg in args {
                    if let AstNode::Var(a) = arg {
                        self.mark_speculative(a);
                    }
                }
                true
            }
            _ => true,
        }
    }

    fn mark_speculative(&mut self, var: &str) {
        if let Some(s) = self.speculative.get_mut(var) {
            if *s == SpeculativeState::Safe { *s = SpeculativeState::Speculative; }
        }
    }

    pub fn validate_affine(&self, _body: &[AstNode]) -> bool { true }
    pub fn validate_speculative(&self, _body: &[AstNode]) -> bool { true }
}
