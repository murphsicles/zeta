//! Compile-Time Function Evaluation (CTFE) module
//!
//! This module provides infrastructure for evaluating constant expressions
//! and functions at compile time.

pub mod context;
pub mod error;
pub mod simple_evaluator;
pub mod value;

// Re-export the simple evaluator for now
pub use simple_evaluator::SimpleConstEvaluator;