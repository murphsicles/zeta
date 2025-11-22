// src/lib.rs
pub mod ast;
pub mod borrow;
pub mod codegen;
pub mod mir;
pub mod parser;
pub mod resolver;
pub mod specialization;
pub mod actor;
pub mod std;

pub use parser::parse_zeta;
pub use resolver::Resolver;
pub use codegen::LLVMCodegen;
pub use actor::{init_runtime, spawn};
