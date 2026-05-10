// src/backend/codegen/mod.rs
#![allow(clippy::module_inception)] // Learning: Module named same as parent is common pattern in Rust
mod codegen;
mod jit;
mod monomorphize;
// SIMD operations are now handled inline in codegen.rs via LLVM vector IR.
// No separate module needed — vectors are stack-allocated and operated on
// as LLVM native vector types.

pub use codegen::LLVMCodegen;
pub use jit::*;
pub use monomorphize::*;
