//! Zeta module for v0.5.0 compatibility
//! This module re-exports all types needed for v0.5.0 compilation

pub mod frontend {
    pub use crate::frontend::ast;
    pub use crate::frontend::parser;
}

pub mod middle {
    pub use crate::middle::mir;
    pub use crate::middle::resolver;
    pub use crate::middle::specialization;
    pub use crate::middle::types;
}

pub mod backend {
    pub use crate::backend::codegen;
}

pub mod runtime {
    pub use crate::runtime::actor;
}
