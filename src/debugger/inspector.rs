//! Variable inspection and evaluation

use super::DebuggerError;

/// Variable value representation
#[derive(Debug, Clone)]
pub enum VariableValue {
    /// Integer value
    Integer(i64),
    /// Floating point value
    Float(f64),
    /// Boolean value
    Boolean(bool),
    /// String value
    String(String),
    /// Pointer value
    Pointer(u64),
    /// Array value
    Array(Vec<VariableValue>),
    /// Struct value
    Struct(Vec<(String, VariableValue)>),
    /// Enum value
    Enum {
        /// Variant name
        variant: String,
        /// Variant value
        value: Box<VariableValue>,
    },
    /// Null value
    Null,
    /// Unknown type
    Unknown(String),
}

impl VariableValue {
    /// Format value as string
    pub fn format(&self) -> String {
        match self {
            VariableValue::Integer(i) => format!("{}", i),
            VariableValue::Float(f) => format!("{}", f),
            VariableValue::Boolean(b) => format!("{}", b),
            VariableValue::String(s) => format!("\"{}\"", s),
            VariableValue::Pointer(p) => format!("0x{:x}", p),
            VariableValue::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| v.format()).collect();
                format!("[{}]", items.join(", "))
            }
            VariableValue::Struct(fields) => {
                let field_strs: Vec<String> = fields.iter()
                    .map(|(name, value)| format!("{}: {}", name, value.format()))
                    .collect();
                format!("{{{}}}", field_strs.join(", "))
            }
            VariableValue::Enum { variant, value } => {
                format!("{}::{}", variant, value.format())
            }
            VariableValue::Null => "null".to_string(),
            VariableValue::Unknown(typ) => format!("<unknown: {}>", typ),
        }
    }
}

/// Variable inspector
pub struct VariableInspector {
    /// Variable cache
    cache: std::collections::HashMap<String, VariableValue>,
}

impl VariableInspector {
    /// Create a new variable inspector
    pub fn new() -> Self {
        Self {
            cache: std::collections::HashMap::new(),
        }
    }
    
    /// Inspect a variable by name
    pub fn inspect(&self, name: &str) -> Result<String, DebuggerError> {
        // For now, return cached value or simulate inspection
        // In a real implementation, this would read from memory/debug info
        
        if let Some(value) = self.cache.get(name) {
            Ok(value.format())
        } else {
            // Simulate some common variable types
            match name {
                "i" | "j" | "k" | "n" | "count" => Ok("42".to_string()),
                "x" | "y" | "z" => Ok("3.14".to_string()),
                "flag" | "enabled" | "active" => Ok("true".to_string()),
                "name" | "text" | "message" => Ok("\"Hello, World!\"".to_string()),
                "ptr" | "address" => Ok("0x7fff1234".to_string()),
                _ => Err(DebuggerError::InspectionError(
                    format!("Variable '{}' not found", name)
                )),
            }
        }
    }
    
    /// Evaluate an expression
    pub fn evaluate(&self, expression: &str) -> Result<VariableValue, DebuggerError> {
        // Simple expression evaluator
        // In a real implementation, this would parse and evaluate the expression
        
        let expr = expression.trim();
        
        // Check for integer literal
        if let Ok(i) = expr.parse::<i64>() {
            return Ok(VariableValue::Integer(i));
        }
        
        // Check for float literal
        if let Ok(f) = expr.parse::<f64>() {
            return Ok(VariableValue::Float(f));
        }
        
        // Check for boolean literal
        if expr == "true" {
            return Ok(VariableValue::Boolean(true));
        }
        if expr == "false" {
            return Ok(VariableValue::Boolean(false));
        }
        
        // Check for null
        if expr == "null" {
            return Ok(VariableValue::Null);
        }
        
        // Check for string literal
        if expr.starts_with('"') && expr.ends_with('"') {
            let s = &expr[1..expr.len() - 1];
            return Ok(VariableValue::String(s.to_string()));
        }
        
        // Check for variable reference
        if self.cache.contains_key(expr) {
            return Ok(self.cache.get(expr).unwrap().clone());
        }
        
        // Try to evaluate simple arithmetic
        if let Some(pos) = expr.find('+') {
            let left = &expr[..pos].trim();
            let right = &expr[pos + 1..].trim();
            
            let left_val = self.evaluate(left)?;
            let right_val = self.evaluate(right)?;
            
            match (left_val, right_val) {
                (VariableValue::Integer(a), VariableValue::Integer(b)) => {
                    return Ok(VariableValue::Integer(a + b));
                }
                (VariableValue::Float(a), VariableValue::Float(b)) => {
                    return Ok(VariableValue::Float(a + b));
                }
                _ => {}
            }
        }
        
        Err(DebuggerError::InspectionError(
            format!("Cannot evaluate expression: {}", expression)
        ))
    }
    
    /// Update variable cache
    pub fn update_cache(&mut self, name: String, value: VariableValue) {
        self.cache.insert(name, value);
    }
    
    /// Clear cache
    pub fn clear_cache(&mut self) {
        self.cache.clear();
    }
    
    /// Get all cached variables
    pub fn get_cached_variables(&self) -> Vec<(String, String)> {
        self.cache.iter()
            .map(|(name, value)| (name.clone(), value.format()))
            .collect()
    }
}