//! Context management for CTFE

use std::collections::HashMap;

use crate::frontend::ast::AstNode;

use super::error::{CtfeError, CtfeResult};
use super::value::ConstValue;

/// A single scope containing variable bindings
#[derive(Debug, Clone, Default)]
struct Scope {
    variables: HashMap<String, ConstValue>,
}

impl Scope {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
    
    fn define_variable(&mut self, name: String, value: ConstValue) -> CtfeResult<()> {
        self.variables.insert(name, value);
        Ok(())
    }
    
    fn get_variable(&self, name: &str) -> Option<ConstValue> {
        self.variables.get(name).cloned()
    }
    
    fn assign_variable(&mut self, name: &str, value: ConstValue) -> CtfeResult<()> {
        if self.variables.contains_key(name) {
            self.variables.insert(name.to_string(), value);
            Ok(())
        } else {
            Err(CtfeError::ScopeError(format!("variable '{}' not found in this scope", name)))
        }
    }
}

/// Context for compile-time evaluation
#[derive(Debug, Clone, Default)]
pub struct ConstContext {
    /// Stack of scopes for variable lookup
    scopes: Vec<Scope>,
    /// Global constants (can be referenced from anywhere)
    constants: HashMap<String, ConstValue>,
    /// Compile-time functions
    functions: HashMap<String, AstNode>,
    /// Whether we're inside a const/comptime function
    in_const_fn: bool,
}

impl ConstContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()], // Start with global scope
            constants: HashMap::new(),
            functions: HashMap::new(),
            in_const_fn: false,
        }
    }
    
    /// Register a const/comptime function
    pub fn register_function(&mut self, name: String, node: AstNode) {
        self.functions.insert(name, node);
    }
    
    /// Get a function definition by name
    pub fn get_function(&self, name: &str) -> Option<&AstNode> {
        self.functions.get(name)
    }
    
    /// Get a variable value (looks from innermost to outermost scope)
    pub fn get_variable(&self, name: &str) -> Option<ConstValue> {
        // Search scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get_variable(name) {
                return Some(value);
            }
        }
        None
    }
    
    /// Get a constant value
    pub fn get_constant(&self, name: &str) -> Option<ConstValue> {
        self.constants.get(name).cloned()
    }
    
    /// Define a variable in the current scope
    pub fn define_variable(&mut self, name: String, value: ConstValue) -> CtfeResult<()> {
        if self.scopes.is_empty() {
            return Err(CtfeError::ScopeError("no active scope".to_string()));
        }
        
        // Define in the innermost scope
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.define_variable(name, value)
    }
    
    /// Assign to an existing variable (search from innermost to outermost scope)
    pub fn assign_variable(&mut self, name: &str, value: ConstValue) -> CtfeResult<()> {
        if self.scopes.is_empty() {
            return Err(CtfeError::ScopeError("no active scope".to_string()));
        }
        
        // Search scopes from innermost to outermost
        for scope in self.scopes.iter_mut().rev() {
            if scope.variables.contains_key(name) {
                return scope.assign_variable(name, value);
            }
        }
        
        Err(CtfeError::ScopeError(format!("variable '{}' not found", name)))
    }
    
    /// Define a constant (global)
    pub fn define_constant(&mut self, name: String, value: ConstValue) -> CtfeResult<()> {
        self.constants.insert(name, value);
        Ok(())
    }
    
    /// Enter a new scope
    pub fn enter_scope(&mut self, is_loop: bool) -> CtfeResult<()> {
        self.scopes.push(Scope::new());
        Ok(())
    }
    
    /// Exit the current scope
    pub fn exit_scope(&mut self) -> CtfeResult<()> {
        if self.scopes.len() <= 1 {
            return Err(CtfeError::ScopeError("cannot exit global scope".to_string()));
        }
        self.scopes.pop();
        Ok(())
    }
    
    /// Mark that we're entering a const/comptime function
    pub fn enter_const_fn(&mut self) {
        self.in_const_fn = true;
    }
    
    /// Mark that we're exiting a const/comptime function
    pub fn exit_const_fn(&mut self) {
        self.in_const_fn = false;
    }
    
    /// Check if we're inside a const/comptime function
    pub fn in_const_fn(&self) -> bool {
        self.in_const_fn
    }

    /// Assign to an array element directly in-place, without cloning the whole array.
    /// Works for both `Array(Vec<ConstValue>)` and `IntArray(Vec<i64>)`.
    pub fn assign_array_element(&mut self, name: &str, index: usize, value: ConstValue) -> CtfeResult<()> {
        if self.scopes.is_empty() {
            return Err(CtfeError::ScopeError("no active scope".to_string()));
        }

        // Search scopes from innermost to outermost
        for scope in self.scopes.iter_mut().rev() {
            if let Some(current) = scope.variables.get_mut(name) {
                return current.array_set(index, value);
            }
        }

        Err(CtfeError::UndefinedVariable(name.to_string()))
    }

    /// Get a single element from an array variable without cloning the whole array.
    /// Returns the element as a ConstValue. For IntArray, returns ConstValue::Int.
    pub fn get_array_element(&self, name: &str, index: usize) -> CtfeResult<ConstValue> {
        // Search scopes from innermost to outermost
        for scope in self.scopes.iter().rev() {
            if let Some(current) = scope.variables.get(name) {
                match current {
                    ConstValue::Array(elements) => {
                        if index < elements.len() {
                            return Ok(elements[index].clone());
                        } else {
                            return Err(CtfeError::IndexOutOfBounds { index, length: elements.len() });
                        }
                    }
                    ConstValue::IntArray(elements) => {
                        if index < elements.len() {
                            return Ok(ConstValue::Int(elements[index]));
                        } else {
                            return Err(CtfeError::IndexOutOfBounds { index, length: elements.len() });
                        }
                    }
                    _ => {
                        return Err(CtfeError::TypeMismatch {
                            expected: "array or int_array".to_string(),
                            found: current.type_name().to_string(),
                        });
                    }
                }
            }
        }

        Err(CtfeError::UndefinedVariable(name.to_string()))
    }
}