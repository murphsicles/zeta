// src/backend/codegen/mod.rs
#![allow(clippy::module_inception)] // Learning: Module named same as parent is common pattern in Rust
mod codegen;
mod jit;
mod monomorphize;

pub use codegen::LLVMCodegen;
pub use jit::*;
pub use monomorphize::*;
