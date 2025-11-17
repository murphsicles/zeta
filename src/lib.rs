pub mod ast;
pub mod borrow;
pub mod codegen;
pub mod mir;
pub mod parser;
pub mod plan;
pub mod resolver;
pub mod xai;

pub use ast::AstNode;
pub use borrow::BorrowChecker;
pub use codegen::{LLVMCodegen, compile_and_run_zeta};
pub use mir::{Mir, MirGen};
pub use parser::parse_zeta;
pub use resolver::Resolver;
pub use xai::XAIClient;
