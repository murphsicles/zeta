//! Machine Learning module for v0.3.48
//! 
//! This module provides neural network framework, ML algorithms,
//! training pipelines, and AI/ML application development capabilities.

pub mod tensor;
pub mod nn;
pub mod optim;
pub mod data;
pub mod training;
pub mod inference;
pub mod models;

// Re-exports
pub use tensor::*;
pub use nn::*;
pub use optim::*;
pub use data::*;
pub use training::*;
pub use inference::*;
pub use models::*;