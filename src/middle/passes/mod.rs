//! # Compiler Passes
//!
//! Various compiler passes for analysis, transformation, and verification.

pub mod identity_verification;

/// Re-export commonly used passes
pub use identity_verification::{IdentityVerificationPass, verify_identities};