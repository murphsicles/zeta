//! Runtime capability validation for identity operations.
//!
//! This module provides runtime checking for identity capabilities,
//! allowing dynamic validation of identity operations that cannot be
//! fully verified at compile time.

use crate::middle::types::identity::{CapabilityLevel, IdentityType, IdentityConstraint};

/// Runtime capability validation error.
#[derive(Debug, Clone, PartialEq)]
pub enum ValidationError {
    /// Operation requires a capability that the identity doesn't have.
    MissingCapability {
        required: CapabilityLevel,
        actual: CapabilityLevel,
    },
    /// Constraint violation (e.g., length constraint, pattern constraint).
    ConstraintViolation {
        constraint: IdentityConstraint,
        value: String,
    },
    /// Identity verification failed.
    VerificationFailed {
        identity: String,
        reason: String,
    },
    /// Capability escalation not allowed.
    EscalationNotAllowed {
        from: CapabilityLevel,
        to: CapabilityLevel,
    },
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValidationError::MissingCapability { required, actual } => {
                write!(f, "Missing capability: required {:?}, but identity has {:?}", required, actual)
            }
            ValidationError::ConstraintViolation { constraint, value } => {
                write!(f, "Constraint violation: {:?} for value '{}'", constraint, value)
            }
            ValidationError::VerificationFailed { identity, reason } => {
                write!(f, "Identity verification failed for '{}': {}", identity, reason)
            }
            ValidationError::EscalationNotAllowed { from, to } => {
                write!(f, "Capability escalation not allowed: from {:?} to {:?}", from, to)
            }
        }
    }
}

impl std::error::Error for ValidationError {}

/// Runtime capability validator.
#[derive(Debug, Clone)]
pub struct CapabilityValidator {
    /// Current identity context.
    identity: IdentityType,
    /// Whether to allow capability escalation.
    allow_escalation: bool,
    /// Whether to log capability changes.
    audit_logging: bool,
}

impl CapabilityValidator {
    /// Create a new capability validator for the given identity.
    pub fn new(identity: IdentityType) -> Self {
        Self {
            identity,
            allow_escalation: false,
            audit_logging: false,
        }
    }

    /// Enable or disable capability escalation.
    pub fn with_escalation(mut self, allow: bool) -> Self {
        self.allow_escalation = allow;
        self
    }

    /// Enable or disable audit logging.
    pub fn with_audit_logging(mut self, enable: bool) -> Self {
        self.audit_logging = enable;
        self
    }

    /// Check if the identity has the required capability.
    pub fn check_capability(&self, required: CapabilityLevel) -> Result<(), ValidationError> {
        if self.identity.has_capability(required) {
            Ok(())
        } else {
            // Find the highest capability level the identity has
            let actual = self.identity.capabilities()
                .iter()
                .max()
                .copied()
                .unwrap_or(CapabilityLevel::Immutable);
            
            Err(ValidationError::MissingCapability {
                required,
                actual,
            })
        }
    }

    /// Check a constraint against a value.
    pub fn check_constraint(&self, constraint: &IdentityConstraint, value: &str) -> Result<(), ValidationError> {
        match constraint {
            IdentityConstraint::Pattern(pattern) => {
                // Simple pattern matching - in a real implementation, this would use regex
                if value.contains(pattern) {
                    Ok(())
                } else {
                    Err(ValidationError::ConstraintViolation {
                        constraint: constraint.clone(),
                        value: value.to_string(),
                    })
                }
            }
            IdentityConstraint::MaxLength(max) => {
                if value.len() <= *max {
                    Ok(())
                } else {
                    Err(ValidationError::ConstraintViolation {
                        constraint: constraint.clone(),
                        value: value.to_string(),
                    })
                }
            }
            IdentityConstraint::MinLength(min) => {
                if value.len() >= *min {
                    Ok(())
                } else {
                    Err(ValidationError::ConstraintViolation {
                        constraint: constraint.clone(),
                        value: value.to_string(),
                    })
                }
            }
            IdentityConstraint::Capability(required_cap) => {
                // Check if the identity has at least the required capability
                if self.identity.capabilities().iter().any(|cap| cap >= required_cap) {
                    Ok(())
                } else {
                    Err(ValidationError::MissingCapability {
                        required: *required_cap,
                        actual: self.identity.capabilities().iter().max().cloned().unwrap_or(CapabilityLevel::Read),
                    })
                }
            }
        }
    }

    /// Attempt to escalate capabilities.
    pub fn escalate_capability(&mut self, new_level: CapabilityLevel) -> Result<(), ValidationError> {
        // Get current highest capability
        let current = self.identity.capabilities()
            .iter()
            .max()
            .copied()
            .unwrap_or(CapabilityLevel::Immutable);
        
        if new_level <= current {
            // De-escalation is always allowed (more restrictive)
            // Remove all capabilities higher than new_level
            let new_caps: Vec<CapabilityLevel> = self.identity.capabilities()
                .iter()
                .filter(|&&cap| cap <= new_level)
                .copied()
                .collect();
            
            // Get the identity value if it exists
            let value = self.identity.value().cloned().unwrap_or_default();
            self.identity = IdentityType::with_value(value, new_caps);
            
            if self.audit_logging {
                println!("[AUDIT] Capability de-escalated: {:?} -> {:?}", current, new_level);
            }
            Ok(())
        } else if self.allow_escalation {
            // Escalation requires permission - add the new capability
            let mut new_caps = self.identity.capabilities().to_vec();
            if !new_caps.contains(&new_level) {
                new_caps.push(new_level);
            }
            
            // Get the identity value if it exists
            let value = self.identity.value().cloned().unwrap_or_default();
            self.identity = IdentityType::with_value(value, new_caps);
            
            if self.audit_logging {
                println!("[AUDIT] Capability escalated: {:?} -> {:?}", current, new_level);
            }
            Ok(())
        } else {
            Err(ValidationError::EscalationNotAllowed {
                from: current,
                to: new_level,
            })
        }
    }

    /// Get the current identity.
    pub fn identity(&self) -> &IdentityType {
        &self.identity
    }

    /// Get the current highest capability level.
    pub fn capability_level(&self) -> CapabilityLevel {
        self.identity.capabilities()
            .iter()
            .max()
            .copied()
            .unwrap_or(CapabilityLevel::Immutable)
    }
}

/// Runtime identity context for tracking identity state during execution.
#[derive(Debug, Clone)]
pub struct IdentityContext {
    /// Current validator.
    validator: CapabilityValidator,
    /// Stack of previous identity states (for nested contexts).
    stack: Vec<IdentityType>,
}

impl IdentityContext {
    /// Create a new identity context.
    pub fn new(identity: IdentityType) -> Self {
        Self {
            validator: CapabilityValidator::new(identity),
            stack: Vec::new(),
        }
    }

    /// Enter a new identity context (pushes current identity onto stack).
    pub fn enter_context(&mut self, new_identity: IdentityType) {
        self.stack.push(self.validator.identity().clone());
        self.validator = CapabilityValidator::new(new_identity);
    }

    /// Exit the current identity context (pops previous identity from stack).
    pub fn exit_context(&mut self) -> Result<(), ValidationError> {
        if let Some(prev_identity) = self.stack.pop() {
            self.validator = CapabilityValidator::new(prev_identity);
            Ok(())
        } else {
            // This shouldn't happen in correct usage
            Err(ValidationError::VerificationFailed {
                identity: "".to_string(),
                reason: "Cannot exit root identity context".to_string(),
            })
        }
    }

    /// Get the current validator.
    pub fn validator(&self) -> &CapabilityValidator {
        &self.validator
    }

    /// Get a mutable reference to the current validator.
    pub fn validator_mut(&mut self) -> &mut CapabilityValidator {
        &mut self.validator
    }
}

/// Identity validation hook for custom validation logic.
pub trait IdentityValidationHook: Send + Sync {
    /// Validate an identity operation.
    fn validate(&self, identity: &IdentityType, operation: &str, args: &[&str]) -> Result<(), ValidationError>;
    
    /// Verify an identity.
    fn verify_identity(&self, identity: &str) -> Result<IdentityType, ValidationError>;
}

/// Default identity validation hook that accepts all identities.
#[derive(Debug, Clone)]
pub struct DefaultValidationHook;

impl IdentityValidationHook for DefaultValidationHook {
    fn validate(&self, _identity: &IdentityType, _operation: &str, _args: &[&str]) -> Result<(), ValidationError> {
        // Default hook accepts all operations
        Ok(())
    }
    
    fn verify_identity(&self, identity: &str) -> Result<IdentityType, ValidationError> {
        // Default hook creates a basic identity with read capability
        Ok(IdentityType::with_value(
            identity.to_string(),
            vec![CapabilityLevel::Read],
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::identity::{CapabilityLevel, IdentityConstraint};

    #[test]
    fn test_capability_validation() {
        let identity = IdentityType::with_value(
            "test".to_string(),
            vec![CapabilityLevel::Read, CapabilityLevel::Write],
        );
        
        let validator = CapabilityValidator::new(identity);
        
        // Should allow read operation
        assert!(validator.check_capability(CapabilityLevel::Read).is_ok());
        
        // Should allow write operation
        assert!(validator.check_capability(CapabilityLevel::Write).is_ok());
        
        // Should fail for owned operation
        assert!(validator.check_capability(CapabilityLevel::Owned).is_err());
    }
    
    #[test]
    fn test_constraint_validation() {
        let identity = IdentityType::with_value(
            "test".to_string(),
            vec![CapabilityLevel::Read],
        );
        
        let validator = CapabilityValidator::new(identity);
        
        // Valid length
        assert!(validator.check_constraint(&IdentityConstraint::MinLength(3), "hello").is_ok());
        
        // Too short
        assert!(validator.check_constraint(&IdentityConstraint::MinLength(3), "hi").is_err());
        
        // Valid max length
        assert!(validator.check_constraint(&IdentityConstraint::MaxLength(10), "hello").is_ok());
        
        // Too long
        assert!(validator.check_constraint(&IdentityConstraint::MaxLength(5), "hello world").is_err());
        
        // Pattern match
        assert!(validator.check_constraint(&IdentityConstraint::Pattern("test".to_string()), "test123").is_ok());
        
        // Pattern mismatch
        assert!(validator.check_constraint(&IdentityConstraint::Pattern("test".to_string()), "hello").is_err());
    }
    
    #[test]
    fn test_capability_escalation() {
        let identity = IdentityType::with_value(
            "test".to_string(),
            vec![CapabilityLevel::Read],
        );
        
        let mut validator = CapabilityValidator::new(identity.clone());
        
        // De-escalation should work without permission
        assert!(validator.escalate_capability(CapabilityLevel::Read).is_ok());
        
        // Escalation should fail without permission
        assert!(validator.escalate_capability(CapabilityLevel::Write).is_err());
        
        // With escalation enabled, it should work
        let mut validator_with_escalation = CapabilityValidator::new(identity)
            .with_escalation(true);
        assert!(validator_with_escalation.escalate_capability(CapabilityLevel::Write).is_ok());
    }
    
    #[test]
    fn test_identity_context() {
        let root_identity = IdentityType::with_value(
            "root".to_string(),
            vec![CapabilityLevel::Read, CapabilityLevel::Write],
        );
        
        let mut context = IdentityContext::new(root_identity);
        
        // Enter new context with more restrictive identity
        let child_identity = IdentityType::with_value(
            "child".to_string(),
            vec![CapabilityLevel::Read],
        );
        
        context.enter_context(child_identity);
        assert_eq!(context.validator().capability_level(), CapabilityLevel::Read);
        
        // Exit back to root context
        assert!(context.exit_context().is_ok());
        assert_eq!(context.validator().capability_level(), CapabilityLevel::Write); // Write is higher than Read
    }
}