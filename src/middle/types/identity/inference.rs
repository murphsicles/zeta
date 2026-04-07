//! # Identity Type Inference
//!
//! Inference rules for identity types and capability checking.

use super::*;
use std::collections::{HashMap, HashSet};

/// Inference context for identity types
#[derive(Debug, Clone, Default)]
pub struct IdentityInferenceContext {
    /// Type variables and their inferred identity types
    type_vars: HashMap<String, IdentityType>,
    /// Constraints that need to be satisfied
    constraints: Vec<IdentityConstraint>,
    /// Subtyping relationships
    subtyping: Vec<(String, String)>,
    /// Error messages for failed inferences
    errors: Vec<String>,
}

impl IdentityInferenceContext {
    /// Create a new inference context
    pub fn new() -> Self {
        Self {
            type_vars: HashMap::new(),
            constraints: Vec::new(),
            subtyping: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Add a type variable with an initial identity type
    pub fn add_type_var(&mut self, name: String, identity_type: IdentityType) {
        self.type_vars.insert(name, identity_type);
    }

    /// Get the identity type for a type variable
    pub fn get_type_var(&self, name: &str) -> Option<&IdentityType> {
        self.type_vars.get(name)
    }

    /// Add a constraint that must be satisfied
    pub fn add_constraint(&mut self, constraint: IdentityConstraint) {
        self.constraints.push(constraint);
    }

    /// Add a subtyping relationship
    pub fn add_subtyping(&mut self, subtype: String, supertype: String) {
        self.subtyping.push((subtype, supertype));
    }

    /// Infer identity types for all type variables
    pub fn infer(&mut self) -> Result<HashMap<String, IdentityType>, Vec<String>> {
        // Apply constraints to refine identity types
        self.apply_constraints();
        
        // Check subtyping relationships
        self.check_subtyping();
        
        // If there are errors, return them
        if !self.errors.is_empty() {
            return Err(self.errors.clone());
        }
        
        // Return the inferred types
        Ok(self.type_vars.clone())
    }

    /// Apply constraints to refine identity types
    fn apply_constraints(&mut self) {
        for constraint in &self.constraints {
            match constraint {
                IdentityConstraint::Pattern(pattern) => {
                    // Apply pattern constraint to all type variables with known values
                    for (_, identity_type) in self.type_vars.iter_mut() {
                        if let Some(value) = &identity_type.value {
                            if !value.contains(pattern) {
                                self.errors.push(format!(
                                    "Identity value '{}' does not match pattern '{}'",
                                    value, pattern
                                ));
                            }
                        }
                    }
                }
                IdentityConstraint::MaxLength(max) => {
                    // Apply max length constraint
                    for (name, identity_type) in self.type_vars.iter_mut() {
                        if let Some(value) = &identity_type.value {
                            if value.len() > *max {
                                self.errors.push(format!(
                                    "Identity '{}' length {} exceeds maximum {}",
                                    name, value.len(), max
                                ));
                            }
                        }
                    }
                }
                IdentityConstraint::MinLength(min) => {
                    // Apply min length constraint
                    for (name, identity_type) in self.type_vars.iter_mut() {
                        if let Some(value) = &identity_type.value {
                            if value.len() < *min {
                                self.errors.push(format!(
                                    "Identity '{}' length {} is less than minimum {}",
                                    name, value.len(), min
                                ));
                            }
                        }
                    }
                }
                IdentityConstraint::Capability(required_cap) => {
                    // Apply capability constraint to all type variables
                    for (name, identity_type) in self.type_vars.iter_mut() {
                        if !identity_type.capabilities.iter().any(|cap| cap >= required_cap) {
                            self.errors.push(format!(
                                "Identity '{}' does not have required capability {}",
                                name, required_cap
                            ));
                        }
                    }
                }
            }
        }
    }

    /// Check subtyping relationships
    fn check_subtyping(&mut self) {
        for (subtype, supertype) in &self.subtyping {
            let sub_type = self.type_vars.get(subtype);
            let super_type = self.type_vars.get(supertype);
            
            match (sub_type, super_type) {
                (Some(sub_type), Some(super_type)) => {
                    if !sub_type.can_substitute(super_type) {
                        self.errors.push(format!(
                            "Identity '{}' cannot substitute '{}'",
                            subtype, supertype
                        ));
                    }
                }
                (None, _) => {
                    self.errors.push(format!("Unknown identity type: {}", subtype));
                }
                (_, None) => {
                    self.errors.push(format!("Unknown identity type: {}", supertype));
                }
            }
        }
    }

    /// Add an error message
    pub fn add_error(&mut self, error: String) {
        self.errors.push(error);
    }

    /// Check if inference has errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    /// Get all error messages
    pub fn errors(&self) -> &[String] {
        &self.errors
    }
}

/// Inference rules for identity operations
pub trait IdentityInferenceRules {
    /// Infer the result type of an identity operation
    fn infer_operation(&self, op: &IdentityOp, args: &[IdentityType]) -> Option<IdentityType>;
    
    /// Check if an operation is allowed with given arguments
    fn check_operation(&self, op: &IdentityOp, args: &[IdentityType]) -> Result<(), String>;
}

impl IdentityInferenceRules for IdentityContext {
    fn infer_operation(&self, op: &IdentityOp, args: &[IdentityType]) -> Option<IdentityType> {
        match op {
            IdentityOp::Create => {
                // Create returns a new identity with full capabilities
                Some(IdentityType::new(vec![
                    CapabilityLevel::Owned,
                    CapabilityLevel::Read,
                    CapabilityLevel::Write,
                    CapabilityLevel::Execute,
                ]))
            }
            IdentityOp::Verify => {
                // Verify returns a boolean, but also validates the identity
                // For now, return an identity with read capability
                Some(IdentityType::new(vec![CapabilityLevel::Read]))
            }
            IdentityOp::Delegate => {
                // Delegate requires owned identity and returns delegated identity
                if args.len() >= 1 {
                    let mut delegated = args[0].clone();
                    delegated.delegatable = true;
                    Some(delegated.downgrade(vec![CapabilityLevel::Owned]))
                } else {
                    None
                }
            }
            IdentityOp::Revoke => {
                // Revoke returns nothing (unit type)
                Some(IdentityType::new(vec![]))
            }
            IdentityOp::Combine => {
                // Combine returns an identity with union of capabilities
                if args.len() >= 2 {
                    let mut combined = args[0].clone();
                    for arg in &args[1..] {
                        for cap in arg.capabilities() {
                            if !combined.has_capability(*cap) {
                                combined = combined.upgrade(vec![*cap]);
                            }
                        }
                    }
                    Some(combined)
                } else {
                    None
                }
            }
            IdentityOp::Split => {
                // Split returns multiple identities (for now, return the first)
                if !args.is_empty() {
                    Some(args[0].clone())
                } else {
                    None
                }
            }
        }
    }
    
    fn check_operation(&self, op: &IdentityOp, args: &[IdentityType]) -> Result<(), String> {
        match op {
            IdentityOp::Create => {
                // Create requires no arguments
                if args.is_empty() {
                    Ok(())
                } else {
                    Err("Create operation takes no arguments".to_string())
                }
            }
            IdentityOp::Verify => {
                // Verify requires at least one identity to verify
                if args.len() >= 1 {
                    Ok(())
                } else {
                    Err("Verify operation requires at least one identity".to_string())
                }
            }
            IdentityOp::Delegate => {
                // Delegate requires owned capability
                if args.len() >= 1 && args[0].has_capability(CapabilityLevel::Owned) {
                    Ok(())
                } else {
                    Err("Delegate operation requires owned identity".to_string())
                }
            }
            IdentityOp::Revoke => {
                // Revoke requires owned capability
                if args.len() >= 1 && args[0].has_capability(CapabilityLevel::Owned) {
                    Ok(())
                } else {
                    Err("Revoke operation requires owned identity".to_string())
                }
            }
            IdentityOp::Combine => {
                // Combine requires at least two identities
                if args.len() >= 2 {
                    Ok(())
                } else {
                    Err("Combine operation requires at least two identities".to_string())
                }
            }
            IdentityOp::Split => {
                // Split requires at least one identity
                if args.len() >= 1 {
                    Ok(())
                } else {
                    Err("Split operation requires at least one identity".to_string())
                }
            }
        }
    }
}

/// Capability inference for operations
pub struct CapabilityInferencer {
    /// Required capabilities for common operations
    required_caps: HashMap<String, Vec<CapabilityLevel>>,
}

impl CapabilityInferencer {
    /// Create a new capability inferencer with default rules
    pub fn new() -> Self {
        let mut required_caps = HashMap::new();
        
        // String operations
        required_caps.insert("len".to_string(), vec![CapabilityLevel::Read]);
        required_caps.insert("concat".to_string(), vec![CapabilityLevel::Read]);
        required_caps.insert("split".to_string(), vec![CapabilityLevel::Read]);
        required_caps.insert("replace".to_string(), vec![CapabilityLevel::Write]);
        required_caps.insert("to_uppercase".to_string(), vec![CapabilityLevel::Write]);
        required_caps.insert("to_lowercase".to_string(), vec![CapabilityLevel::Write]);
        
        // File operations
        required_caps.insert("read_file".to_string(), vec![CapabilityLevel::Read]);
        required_caps.insert("write_file".to_string(), vec![CapabilityLevel::Write]);
        required_caps.insert("execute".to_string(), vec![CapabilityLevel::Execute]);
        
        Self { required_caps }
    }
    
    /// Infer required capabilities for an operation
    pub fn infer_capabilities(&self, operation: &str) -> Option<&[CapabilityLevel]> {
        self.required_caps.get(operation).map(|v| v.as_slice())
    }
    
    /// Check if an identity has the required capabilities for an operation
    pub fn check_capabilities(&self, operation: &str, identity: &IdentityType) -> Result<(), String> {
        if let Some(required_caps) = self.required_caps.get(operation) {
            for cap in required_caps {
                if !identity.has_capability(*cap) {
                    return Err(format!(
                        "Operation '{}' requires '{}' capability",
                        operation, cap
                    ));
                }
            }
            Ok(())
        } else {
            // Unknown operation - assume no special capabilities required
            Ok(())
        }
    }
    
    /// Add a new capability rule
    pub fn add_rule(&mut self, operation: String, capabilities: Vec<CapabilityLevel>) {
        self.required_caps.insert(operation, capabilities);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_inference_context() {
        let mut ctx = IdentityInferenceContext::new();
        
        // Add type variables
        ctx.add_type_var(
            "x".to_string(),
            IdentityType::with_value("user123".to_string(), vec![CapabilityLevel::Read])
        );
        
        ctx.add_type_var(
            "y".to_string(),
            IdentityType::new(vec![CapabilityLevel::Read, CapabilityLevel::Write])
        );
        
        // Add constraints
        ctx.add_constraint(IdentityConstraint::MinLength(3));
        ctx.add_constraint(IdentityConstraint::Pattern("user".to_string()));
        
        // Infer types
        let result = ctx.infer();
        assert!(result.is_ok());
        
        let types = result.unwrap();
        assert_eq!(types.len(), 2);
        assert!(types.get("x").is_some());
        assert!(types.get("y").is_some());
    }
    
    #[test]
    fn test_inference_with_errors() {
        let mut ctx = IdentityInferenceContext::new();
        
        // Add type variable with value that violates constraint
        ctx.add_type_var(
            "x".to_string(),
            IdentityType::with_value("ab".to_string(), vec![CapabilityLevel::Read])
        );
        
        // Add constraint that will be violated
        ctx.add_constraint(IdentityConstraint::MinLength(3));
        
        // Infer types - should fail
        let result = ctx.infer();
        assert!(result.is_err());
        
        let errors = result.unwrap_err();
        assert!(!errors.is_empty());
        assert!(errors[0].contains("less than minimum"));
    }
    
    #[test]
    fn test_capability_inferencer() {
        let inferencer = CapabilityInferencer::new();
        
        // Test known operation
        let caps = inferencer.infer_capabilities("read_file");
        assert!(caps.is_some());
        assert_eq!(caps.unwrap(), &[CapabilityLevel::Read]);
        
        // Test unknown operation
        let caps = inferencer.infer_capabilities("unknown_op");
        assert!(caps.is_none());
    }
    
    #[test]
    fn test_capability_checking() {
        let inferencer = CapabilityInferencer::new();
        
        // Create identity with read capability
        let identity = IdentityType::new(vec![CapabilityLevel::Read]);
        
        // Check operation that requires read
        let result = inferencer.check_capabilities("read_file", &identity);
        assert!(result.is_ok());
        
        // Check operation that requires write (should fail)
        let result = inferencer.check_capabilities("write_file", &identity);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("requires 'write' capability"));
    }
    
    #[test]
    fn test_operation_inference() {
        let ctx = IdentityContext::new();
        
        // Test Create operation
        let result = ctx.infer_operation(&IdentityOp::Create, &[]);
        assert!(result.is_some());
        let identity = result.unwrap();
        assert!(identity.has_capability(CapabilityLevel::Owned));
        
        // Test Verify operation
        let arg = IdentityType::new(vec![CapabilityLevel::Read]);
        let result = ctx.infer_operation(&IdentityOp::Verify, &[arg]);
        assert!(result.is_some());
        
        // Test Delegate operation
        let arg = IdentityType::new(vec![CapabilityLevel::Owned]);
        let result = ctx.infer_operation(&IdentityOp::Delegate, &[arg.clone()]);
        assert!(result.is_some());
        let delegated = result.unwrap();
        assert!(delegated.delegatable);
        assert!(!delegated.has_capability(CapabilityLevel::Owned));
    }
    
    #[test]
    fn test_operation_checking() {
        let ctx = IdentityContext::new();
        
        // Test Create - should succeed
        let result = ctx.check_operation(&IdentityOp::Create, &[]);
        assert!(result.is_ok());
        
        // Test Create with arguments - should fail
        let arg = IdentityType::new(vec![CapabilityLevel::Read]);
        let result = ctx.check_operation(&IdentityOp::Create, &[arg]);
        assert!(result.is_err());
        
        // Test Delegate without owned capability - should fail
        let arg = IdentityType::new(vec![CapabilityLevel::Read]);
        let result = ctx.check_operation(&IdentityOp::Delegate, &[arg]);
        assert!(result.is_err());
        
        // Test Delegate with owned capability - should succeed
        let arg = IdentityType::new(vec![CapabilityLevel::Owned]);
        let result = ctx.check_operation(&IdentityOp::Delegate, &[arg]);
        assert!(result.is_ok());
    }
}