// src/lib.rs
//! Zeta compiler library crate.
//! Exports modules and utilities for parsing, type resolution, code generation, and runtime execution.
pub mod frontend {
    pub mod ast;
    pub mod borrow;
    pub mod parser;
}
pub mod middle {
    pub mod mir;
    pub mod resolver;
    pub mod specialization;
}
pub mod backend {
    pub mod codegen;
}
pub mod runtime {
    pub mod actor;
    pub mod std;
    pub mod xai;
    pub mod host;
}
pub use frontend::ast::AstNode;
pub use frontend::parser::top_level::parse_zeta;
pub use middle::mir::Mir;
pub use middle::resolver::resolver::Resolver;
pub use backend::codegen::codegen::LLVMCodegen;
pub use runtime::actor::{init_runtime, spawn};
