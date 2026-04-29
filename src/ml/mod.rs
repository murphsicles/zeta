//! Machine Learning module for v0.3.48
//!
//! This module provides neural network framework, ML algorithms,
//! training pipelines, and AI/ML application development capabilities.

pub mod data;
pub mod inference;
pub mod models;
pub mod nn;
pub mod optim;
pub mod tensor;
pub mod training;

// Re-exports
pub use data::*;
pub use inference::*;
pub use models::*;
pub use nn::*;
pub use optim::*;
pub use tensor::*;
pub use training::*;
