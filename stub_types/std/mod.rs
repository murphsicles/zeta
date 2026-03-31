//! Stub for Rust standard library
//! Provides minimal implementations for std types needed for compilation

pub mod collections;
pub mod ffi;

// Re-export commonly used types
pub use collections::HashMap;
pub use ffi::c_void;