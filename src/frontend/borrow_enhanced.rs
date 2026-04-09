// src/frontend/borrow_enhanced.rs
//! # Enhanced Borrow Checker for Zeta v0.3.35
//!
//! Implements Rust-like memory management with:
//! - Ownership tracking with move semantics
//! - Borrowing rules (immutable vs mutable)
//! - Lifetime-aware reference checking
//! - Memory safety patterns

use crate::frontend::ast::AstNode;
use crate::middle::resolver::resolver::{Resolver, Type};
use crate::middle::types::lifetime::{Lifetime, LifetimeContext};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OwnershipState {
    /// Variable is owned and can be used or moved
    Owned,
    /// Variable has been moved and cannot be used
    Moved,
    /// Variable is immutably borrowed (shared reference)
    ImmutablyBorrowed,
    /// Variable is mutably borrowed (exclusive reference)
    MutablyBorrowed,
}

#[derive(Debug, Clone)]
pub struct VariableInfo {
    pub state: OwnershipState,
    pub ty: Type,
    /// Track how many immutable borrows exist
    pub immutable_borrow_count: usize,
    /// Track if a mutable borrow exists
    pub mutable_borrow_exists: bool,
    /// Lifetime of the variable (for references)
    pub lifetime: Option<Lifetime>,
}

impl VariableInfo {
    pub fn new(ty: Type) -> Self {
        Self {
            state: OwnershipState::Owned,
            ty,
            immutable_borrow_count: 0,
            mutable_borrow_exists: false,
            lifetime: None,
        }
    }
    
    pub fn can_use(&self) -> bool {
        matches!(self.state, OwnershipState::Owned | OwnershipState::ImmutablyBorrowed | OwnershipState::MutablyBorrowed)
    }
    
    pub fn can_move(&self) -> bool {
        self.state == OwnershipState::Owned && 
        self.immutable_borrow_count == 0 && 
        !self.mutable_borrow_exists
    }
    
    pub fn can_borrow_immutably(&self) -> bool {
        self.state == OwnershipState::Owned && !self.mutable_borrow_exists
    }
    
    pub fn can_borrow_mutably(&self) -> bool {
        self.state == OwnershipState::Owned && 
        self.immutable_borrow_count == 0 && 
        !self.mutable_borrow_exists
    }
}

#[derive(Debug, Clone)]
pub struct EnhancedBorrowChecker {
    variables: HashMap<String, VariableInfo>,
    /// Track which variables are currently borrowed
    borrowed_vars: HashSet<String>,
    /// Lifetime context for checking reference lifetimes
    lifetime_context: LifetimeContext,
    /// Track variable scopes (for ownership transfer)
    scopes: Vec<HashSet<String>>,
}

impl Default for EnhancedBorrowChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl EnhancedBorrowChecker {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            borrowed_vars: HashSet::new(),
            lifetime_context: LifetimeContext::new(),
            scopes: vec![HashSet::new()], // Start with global scope
        }
    }
    
    /// Enter a new scope
    pub fn enter_scope(&mut self) {
        self.scopes.push(HashSet::new());
    }
    
    /// Exit current scope, dropping variables declared in it
    pub fn exit_scope(&mut self) {
        if let Some(current_scope) = self.scopes.pop() {
            for var_name in current_scope {
                self.variables.remove(&var_name);
                self.borrowed_vars.remove(&var_name);
            }
        }
    }
    
    /// Declare a new variable in current scope
    pub fn declare(&mut self, name: String, ty: Type) -> Result<(), String> {
        if self.variables.contains_key(&name) {
            return Err(format!("Variable '{}' already declared", name));
        }
        
        let info = VariableInfo::new(ty);
        self.variables.insert(name.clone(), info);
        
        // Add to current scope
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name);
        }
        
        Ok(())
    }
    
    /// Move a variable (transfer ownership)
    pub fn move_variable(&mut self, name: &str) -> Result<(), String> {
        let info = self.variables.get_mut(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;
        
        if !info.can_move() {
            return Err(format!("Cannot move '{}': still borrowed or not owned", name));
        }
        
        info.state = OwnershipState::Moved;
        Ok(())
    }
    
    /// Create an immutable borrow (&T)
    pub fn borrow_immutably(&mut self, name: &str, lifetime: Option<Lifetime>) -> Result<(), String> {
        let info = self.variables.get_mut(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;
        
        if !info.can_borrow_immutably() {
            return Err(format!("Cannot immutably borrow '{}': already mutably borrowed", name));
        }
        
        info.immutable_borrow_count += 1;
        info.lifetime = lifetime;
        self.borrowed_vars.insert(name.to_string());
        
        Ok(())
    }
    
    /// Create a mutable borrow (&mut T)
    pub fn borrow_mutably(&mut self, name: &str, lifetime: Option<Lifetime>) -> Result<(), String> {
        let info = self.variables.get_mut(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;
        
        if !info.can_borrow_mutably() {
            return Err(format!("Cannot mutably borrow '{}': already borrowed", name));
        }
        
        info.mutable_borrow_exists = true;
        info.lifetime = lifetime;
        self.borrowed_vars.insert(name.to_string());
        
        Ok(())
    }
    
    /// Release an immutable borrow
    pub fn release_immutable_borrow(&mut self, name: &str) -> Result<(), String> {
        let info = self.variables.get_mut(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;
        
        if info.immutable_borrow_count == 0 {
            return Err(format!("No immutable borrow to release for '{}'", name));
        }
        
        info.immutable_borrow_count -= 1;
        if info.immutable_borrow_count == 0 && !info.mutable_borrow_exists {
            self.borrowed_vars.remove(name);
        }
        
        Ok(())
    }
    
    /// Release a mutable borrow
    pub fn release_mutable_borrow(&mut self, name: &str) -> Result<(), String> {
        let info = self.variables.get_mut(name)
            .ok_or_else(|| format!("Variable '{}' not found", name))?;
        
        if !info.mutable_borrow_exists {
            return Err(format!("No mutable borrow to release for '{}'", name));
        }
        
        info.mutable_borrow_exists = false;
        if info.immutable_borrow_count == 0 {
            self.borrowed_vars.remove(name);
        }
        
        Ok(())
    }
    
    /// Check if a variable can be used
    pub fn can_use(&self, name: &str) -> bool {
        self.variables.get(name)
            .map(|info| info.can_use())
            .unwrap_or(false)
    }
    
    /// Check AST node with enhanced borrow checking
    pub fn check(&mut self, node: &AstNode, resolver: &Resolver) -> bool {
        match node {
            AstNode::Var(name) => {
                if !self.can_use(name) {
                    eprintln!("Borrow error: Cannot use variable '{}'", name);
                    return false;
                }
                true
            }
            
            AstNode::Assign(lhs, rhs) => {
                // Check RHS first
                if !self.check(rhs, resolver) {
                    return false;
                }
                
                match **lhs {
                    AstNode::Var(ref name) => {
                        // Assignment creates or reassigns a variable
                        let ty = resolver.infer_type(rhs);
                        if let Err(e) = self.declare(name.clone(), ty) {
                            // Variable already exists - check if we can reassign
                            if self.can_use(name) {
                                // For now, allow reassignment of owned variables
                                // In Rust, this would move if not Copy
                                true
                            } else {
                                eprintln!("Borrow error: {}", e);
                                false
                            }
                        } else {
                            true
                        }
                    }
                    AstNode::Subscript { ref base, ref index } => {
                        // Assignment through subscript (e.g., arr[i] = value)
                        if !self.check(base, resolver) || !self.check(index, resolver) {
                            return false;
                        }
                        
                        if let AstNode::Var(ref name) = **base {
                            // Need mutable access to the base
                            if !self.can_use(name) {
                                eprintln!("Borrow error: Cannot mutably access '{}'", name);
                                return false;
                            }
                            // In real implementation, would check if it's a mutable reference
                        }
                        true
                    }
                    _ => {
                        eprintln!("Borrow error: Invalid assignment target");
                        false
                    }
                }
            }
            
            AstNode::UnaryOp { op, expr } => {
                match op.as_str() {
                    "*" => {
                        // Dereferencing: check that expr is a reference
                        self.check(expr, resolver)
                    }
                    "&" => {
                        // Create immutable reference
                        if let AstNode::Var(ref name) = **expr {
                            if let Err(e) = self.borrow_immutably(name, None) {
                                eprintln!("Borrow error: {}", e);
                                return false;
                            }
                        }
                        self.check(expr, resolver)
                    }
                    "&mut" => {
                        // Create mutable reference
                        if let AstNode::Var(ref name) = **expr {
                            if let Err(e) = self.borrow_mutably(name, None) {
                                eprintln!("Borrow error: {}", e);
                                return false;
                            }
                        }
                        self.check(expr, resolver)
                    }
                    _ => self.check(expr, resolver),
                }
            }
            
            AstNode::Call { receiver, args, .. } => {
                // Check receiver if present
                if let Some(r) = receiver {
                    if !self.check(r, resolver) {
                        return false;
                    }
                    
                    // If receiver is a variable, it's borrowed
                    if let AstNode::Var(ref name) = **r {
                        if let Err(e) = self.borrow_immutably(name, None) {
                            eprintln!("Borrow error: {}", e);
                            return false;
                        }
                    }
                }
                
                // Check arguments
                for arg in args {
                    if !self.check(arg, resolver) {
                        return false;
                    }
                    
                    // Check if argument should be moved
                    if let AstNode::Var(ref name) = *arg {
                        let ty = self.variables.get(name)
                            .map(|info| info.ty.clone())
                            .unwrap_or(Type::Error);
                        
                        // Non-Copy types should be moved when passed by value
                        if !resolver.is_copy(&ty) {
                            if let Err(e) = self.move_variable(name) {
                                eprintln!("Borrow error: {}", e);
                                return false;
                            }
                        }
                    }
                }
                true
            }
            
            AstNode::Return(expr) => self.check(expr, resolver),
            
            AstNode::Block { body } => {
                self.enter_scope();
                let mut ok = true;
                for stmt in body {
                    if !self.check(stmt, resolver) {
                        ok = false;
                        break;
                    }
                }
                self.exit_scope();
                ok
            }
            
            AstNode::Let { pattern, expr, .. } => {
                if !self.check(expr, resolver) {
                    return false;
                }
                
                // Handle pattern binding
                if let AstNode::Var(name) = &**pattern {
                    let ty = resolver.infer_type(expr);
                    if let Err(e) = self.declare(name.clone(), ty) {
                        eprintln!("Borrow error: {}", e);
                        return false;
                    }
                }
                true
            }
            
            // Default case: check children recursively
            _ => {
                let mut ok = true;
                // Recursively check child nodes
                match node {
                    AstNode::BinaryOp { left, right, .. } => {
                        ok = self.check(left, resolver) && self.check(right, resolver);
                    }
                    AstNode::If { cond, then, else_ } => {
                        ok = self.check(cond, resolver);
                        if ok {
                            self.enter_scope();
                            for stmt in then {
                                if !self.check(stmt, resolver) {
                                    ok = false;
                                    break;
                                }
                            }
                            self.exit_scope();
                        }
                        if ok && !else_.is_empty() {
                            self.enter_scope();
                            for stmt in else_ {
                                if !self.check(stmt, resolver) {
                                    ok = false;
                                    break;
                                }
                            }
                            self.exit_scope();
                        }
                    }
                    AstNode::While { cond, body } => {
                        ok = self.check(cond, resolver);
                        if ok {
                            self.enter_scope();
                            for stmt in body {
                                if !self.check(stmt, resolver) {
                                    ok = false;
                                    break;
                                }
                            }
                            self.exit_scope();
                        }
                    }
                    AstNode::For { pattern, expr, body } => {
                        ok = self.check(expr, resolver);
                        if ok {
                            self.enter_scope();
                            // Bind pattern in loop scope
                            if let AstNode::Var(name) = &**pattern {
                                let ty = resolver.infer_type(expr);
                                if let Err(e) = self.declare(name.clone(), ty) {
                                    eprintln!("Borrow error: {}", e);
                                    ok = false;
                                }
                            }
                            if ok {
                                for stmt in body {
                                    if !self.check(stmt, resolver) {
                                        ok = false;
                                        break;
                                    }
                                }
                            }
                            self.exit_scope();
                        }
                    }
                    _ => {
                        // For other nodes, just return true for now
                        // In a full implementation, we'd handle all cases
                    }
                }
                ok
            }
        }
    }
    
    /// Solve lifetime constraints
    pub fn solve_lifetimes(&mut self) -> Result<(), String> {
        self.lifetime_context.solve()
    }
    
    /// Get lifetime context for external use
    pub fn lifetime_context(&self) -> &LifetimeContext {
        &self.lifetime_context
    }
    
    /// Add lifetime constraint
    pub fn add_lifetime_constraint(&mut self, longer: Lifetime, shorter: Lifetime) {
        self.lifetime_context.add_constraint(longer, shorter);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    
    #[test]
    fn test_enhanced_borrow_checker_basics() {
        let mut checker = EnhancedBorrowChecker::new();
        let resolver = Resolver::new();
        
        // Declare a variable
        assert!(checker.declare("x".to_string(), Type::I32).is_ok());
        assert!(checker.can_use("x"));
        
        // Move the variable
        assert!(checker.move_variable("x").is_ok());
        assert!(!checker.can_use("x"));
        
        // Cannot move again
        assert!(checker.move_variable("x").is_err());
        
        println!("✓ Enhanced borrow checker basics work");
    }
    
    #[test]
    fn test_borrowing_rules() {
        let mut checker = EnhancedBorrowChecker::new();
        
        // Declare variable
        assert!(checker.declare("data".to_string(), Type::I32).is_ok());
        
        // Multiple immutable borrows allowed
        assert!(checker.borrow_immutably("data", None).is_ok());
        assert!(checker.borrow_immutably("data", None).is_ok());
        
        // Cannot mutable borrow while immutably borrowed
        assert!(checker.borrow_mutably("data", None).is_err());
        
        // Release immutable borrows
        assert!(checker.release_immutable_borrow("data").is_ok());
        assert!(checker.release_immutable_borrow("data").is_ok());
        
        // Now can mutable borrow
        assert!(checker.borrow_mutably("data", None).is_ok());
        
        // Cannot immutable borrow while mutably borrowed
        assert!(checker.borrow_immutably("data", None).is_err());
        
        println!("✓ Borrowing rules enforced correctly");
    }
    
    #[test]
    fn test_scope_management() {
        let mut checker = EnhancedBorrowChecker::new();
        
        // Declare in global scope
        assert!(checker.declare("global".to_string(), Type::I32).is_ok());
        
        // Enter new scope
        checker.enter_scope();
        
        // Declare in inner scope
        assert!(checker.declare("local".to_string(), Type::I32).is_ok());
        assert!(checker.can_use("local"));
        
        // Exit scope - local should be removed
        checker.exit_scope();
        
        // Global should still exist
        assert!(checker.can_use("global"));
        // Local should not exist
        assert!(!checker.can_use("local"));
        
        println!("✓ Scope management works correctly");
    }
}