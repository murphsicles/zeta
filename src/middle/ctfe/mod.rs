//! Compile-Time Function Evaluation (CTFE) module.
//! Provides constant expression evaluation at compile time.

pub mod context;
pub mod evaluator;
pub mod value;
pub mod error;

pub use context::ConstContext;
pub use evaluator::ConstEvaluator;
pub use value::ConstValue;
pub use error::{CtfeError, CtfeResult};

/// Evaluate constants in a program (list of AST nodes)
pub fn evaluate_constants(asts: &[crate::frontend::ast::AstNode]) -> Result<Vec<crate::frontend::ast::AstNode>, CtfeError> {
    let mut evaluator = ConstEvaluator::new();
    evaluator.evaluate_program(asts)
}