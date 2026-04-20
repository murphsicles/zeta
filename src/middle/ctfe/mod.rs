//! Compile-Time Function Evaluation (CTFE) module
//!
//! This module provides the ability to evaluate constant expressions and
//! functions at compile time, replacing them with their computed values.

pub mod context;
pub mod error;
pub mod evaluator;
pub mod value;
pub mod visitor;

pub use context::ConstContext;
pub use error::{CtfeError, CtfeResult};
pub use evaluator::{ConstEvaluator, eval_const_expr, evaluate_program};
pub use value::ConstValue;

/// Evaluate constants in a program, replacing them with their computed values
pub fn evaluate_constants(asts: &[crate::frontend::ast::AstNode]) -> CtfeResult<Vec<crate::frontend::ast::AstNode>> {
    let mut evaluator = ConstEvaluator::new();
    evaluator.evaluate_program(asts)
}