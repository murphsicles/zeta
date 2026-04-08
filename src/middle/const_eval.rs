// src/middle/const_eval.rs
//! Compile-Time Function Evaluation (CTFE) for Zeta v0.5.0
//!
//! This module provides the public interface for CTFE, re-exporting
//! the new CTFE engine implementation while maintaining backward
//! compatibility.

// Use the simple CTFE implementation for now
pub use crate::middle::ctfe::simple_evaluator::SimpleConstEvaluator as ConstEvaluator;

// Re-export types
pub use crate::middle::ctfe::value::ConstValue;
pub use crate::middle::ctfe::error::{CtfeError, CtfeResult};
pub use crate::middle::ctfe::context::ConstContext;

// For backward compatibility
type LegacyConstValue = ConstValue;
type LegacyConstEvaluator = ConstEvaluator;

pub fn evaluate_constants(asts: &[crate::frontend::ast::AstNode]) -> CtfeResult<Vec<crate::frontend::ast::AstNode>> {
    let evaluator = ConstEvaluator::new();
    evaluator.evaluate_program(asts)
}

/// Legacy evaluate_constants function for backward compatibility
pub fn legacy_evaluate_constants(asts: &[crate::frontend::ast::AstNode]) -> Result<Vec<crate::frontend::ast::AstNode>, String> {
    match evaluate_constants(asts) {
        Ok(result) => Ok(result),
        Err(e) => Err(e.to_string()),
    }
}

/// Legacy ConstEvaluator struct for backward compatibility
pub struct LegacyCompatConstEvaluator {
    inner: ConstEvaluator,
}

impl LegacyCompatConstEvaluator {
    pub fn new() -> Self {
        Self {
            inner: ConstEvaluator::new(),
        }
    }
    
    pub fn eval_const_expr(&mut self, expr: &crate::frontend::ast::AstNode) -> Result<ConstValue, String> {
        match self.inner.eval_const_expr(expr) {
            Ok(value) => Ok(value),
            Err(e) => Err(e.to_string()),
        }
    }
}