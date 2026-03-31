// src/middle/resolver/mod.rs
#![allow(clippy::module_inception)] // Learning: Module named same as parent is common pattern in Rust
pub mod module_resolver;
pub mod new_resolver;
pub mod resolver;
pub mod type_cache;
pub mod typecheck;
pub mod typecheck_new;
pub mod unified_typecheck;
