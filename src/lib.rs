// src/lib.rs
pub mod ast;
pub mod borrow;
pub mod codegen;
pub mod parser;
pub mod resolver;

pub use ast::AstNode;
pub use borrow::BorrowChecker;
pub use codegen::{compile_and_run_zeta, LLVMCodegen};
pub use parser::parse_zeta;
pub use resolver::Resolver;
