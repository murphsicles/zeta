// src/middle/const_eval.rs
//! Compile-Time Function Evaluation (CTFE) for Zeta v0.5.0
//!
//! This module provides the public interface for CTFE, re-exporting
//! the new CTFE engine implementation while maintaining backward
//! compatibility.

pub use crate::middle::ctfe::context::ConstContext;
pub use crate::middle::ctfe::evaluator::ConstEvaluator;
pub use crate::middle::ctfe::value::ConstValue;
pub use crate::middle::ctfe::error::{CtfeError, CtfeResult};
pub use crate::middle::ctfe::evaluate_constants;

// Re-export the legacy types and functions for backward compatibility
pub use crate::middle::ctfe::value::ConstValue as LegacyConstValue;
pub use crate::middle::ctfe::evaluator::ConstEvaluator as LegacyConstEvaluator;

/// Legacy evaluate_constants function for backward compatibility
pub fn legacy_evaluate_constants(asts: &[crate::frontend::ast::AstNode]) -> Result<Vec<crate::frontend::ast::AstNode>, String> {
    let result: CtfeResult<_> = evaluate_constants(asts);
    match result {
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
    
    pub fn try_eval_const_call(
        &mut self,
        func: &crate::frontend::ast::AstNode,
        args: &[crate::frontend::ast::AstNode],
    ) -> Result<Option<ConstValue>, String> {
        self.inner.try_eval_const_call(func, args).map(|v| Some(v)).map_err(|e: CtfeError| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::ast::AstNode;
    
    #[test]
    fn test_simple_const_evaluation() {
        let expr = AstNode::BinaryOp {
            op: "+".to_string(),
            left: Box::new(AstNode::Lit(100)),
            right: Box::new(AstNode::Lit(200)),
        };
        
        let mut evaluator = ConstEvaluator::new();
        let result = evaluator.eval_const_expr(&expr);
        
        assert!(result.is_ok());
        if let Ok(ConstValue::Int(value)) = result {
            assert_eq!(value, 300);
        } else {
            panic!("Expected integer result");
        }
    }
    
    #[test]
    fn test_const_def_evaluation() {
        let asts = vec![
            AstNode::ConstDef {
                name: "N".to_string(),
                ty: "usize".to_string(),
                value: Box::new(AstNode::BinaryOp {
                    op: "+".to_string(),
                    left: Box::new(AstNode::Lit(100)),
                    right: Box::new(AstNode::Lit(200)),
                }),
                attrs: vec![],
                pub_: false,
                comptime_: false,
            },
        ];
        
        let result = evaluate_constants(&asts);
        assert!(result.is_ok());
        
        let transformed = result.unwrap();
        assert_eq!(transformed.len(), 1);
        
        if let AstNode::ConstDef { value, .. } = &transformed[0] {
            if let AstNode::Lit(n) = &**value {
                assert_eq!(*n, 300);
            } else {
                panic!("Expected literal value after evaluation");
            }
        } else {
            panic!("Expected ConstDef");
        }
    }
    
    #[test]
    fn test_backward_compatibility() {
        let asts = vec![
            AstNode::ConstDef {
                name: "TEST".to_string(),
                ty: "i32".to_string(),
                value: Box::new(AstNode::Lit(42)),
                attrs: vec![],
                pub_: false,
                comptime_: false,
            },
        ];
        
        let result = legacy_evaluate_constants(&asts);
        assert!(result.is_ok());
    }
}