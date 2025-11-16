pub mod ast;
pub mod borrow;
pub mod codegen;
pub mod mir;
pub mod parser;
pub mod resolver;
pub mod plan;
pub mod xai;

pub use ast::AstNode;
pub use borrow::BorrowChecker;
pub use codegen::{compile_and_run_zeta, LLVMCodegen};
pub use mir::{Mir, MirGen};
pub use parser::parse_zeta;
pub use resolver::Resolver;
pub use xai::XAIClient;
