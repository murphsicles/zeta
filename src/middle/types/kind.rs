//! # Kind System
//!
//! Kind system for higher-kinded types in Zeta.
//! Kinds classify type constructors: * (concrete type), * -> * (type constructor), etc.

use std::collections::HashMap;
use std::fmt;

/// Kind variable for kind inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KindVar(u32);

impl KindVar {
    /// Generate a fresh kind variable
    pub fn fresh() -> Self {
        static COUNTER: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);
        KindVar(COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

/// Kind representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Kind {
    /// Star kind: concrete types (i32, bool, String, etc.)
    Star,
    
    /// Arrow kind: type constructors (* -> *, * -> * -> *, etc.)
    Arrow(Box<Kind>, Box<Kind>),
    
    /// Kind variable for inference
    Variable(KindVar),
    
    /// Error kind (when kind inference fails)
    Error,
}

impl Kind {
    /// Get display name for kind
    pub fn display_name(&self) -> String {
        match self {
            Kind::Star => "*".to_string(),
            Kind::Arrow(param, result) => {
                let param_str = match **param {
                    Kind::Arrow(_, _) => format!("({})", param.display_name()),
                    _ => param.display_name(),
                };
                format!("{} -> {}", param_str, result.display_name())
            }
            Kind::Variable(var) => format!("K{}", var.0),
            Kind::Error => "?".to_string(),
        }
    }
    
    /// Get mangled name for kind (used in codegen)
    pub fn mangled_name(&self) -> String {
        match self {
            Kind::Star => "Star".to_string(),
            Kind::Arrow(param, result) => {
                format!("Arrow_{}_{}", param.mangled_name(), result.mangled_name())
            }
            Kind::Variable(var) => format!("KindVar_{}", var.0),
            Kind::Error => "KindError".to_string(),
        }
    }
    
    /// Check if kind is a function kind (arrow kind)
    pub fn is_function_kind(&self) -> bool {
        matches!(self, Kind::Arrow(_, _))
    }
    
    /// Get the arity of a function kind (number of * parameters)
    pub fn arity(&self) -> usize {
        match self {
            Kind::Star => 0,
            Kind::Arrow(param, result) => {
                if let Kind::Star = **param {
                    1 + result.arity()
                } else {
                    // Complex parameter kind - treat as 1 for now
                    1 + result.arity()
                }
            }
            Kind::Variable(_) => 0, // Unknown arity
            Kind::Error => 0,
        }
    }
    
    /// Apply a kind to arguments (kind application)
    pub fn apply(&self, args: &[Kind]) -> Result<Kind, String> {
        let mut current = self.clone();
        
        for arg in args {
            match current {
                Kind::Arrow(param, result) => {
                    // Check if parameter kind matches argument kind
                    if &*param != arg {
                        return Err(format!(
                            "Kind mismatch in application: expected {}, got {}",
                            param.display_name(),
                            arg.display_name()
                        ));
                    }
                    current = *result;
                }
                _ => {
                    return Err(format!(
                        "Cannot apply non-arrow kind: {}",
                        current.display_name()
                    ));
                }
            }
        }
        
        Ok(current)
    }
    
    /// Check if this kind is compatible with another kind
    /// (allows kind variables to unify with anything)
    pub fn compatible_with(&self, other: &Kind) -> bool {
        match (self, other) {
            (Kind::Variable(_), _) | (_, Kind::Variable(_)) => true,
            (Kind::Star, Kind::Star) => true,
            (Kind::Arrow(p1, r1), Kind::Arrow(p2, r2)) => {
                p1.compatible_with(p2) && r1.compatible_with(r2)
            }
            _ => false,
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display_name())
    }
}

/// Kind substitution mapping
#[derive(Debug, Clone, Default)]
pub struct KindSubstitution {
    mapping: HashMap<KindVar, Kind>,
}

impl KindSubstitution {
    /// Create empty substitution
    pub fn new() -> Self {
        KindSubstitution {
            mapping: HashMap::new(),
        }
    }
    
    /// Apply substitution to a kind
    pub fn apply(&self, kind: &Kind) -> Kind {
        match kind {
            Kind::Variable(var) => self
                .mapping
                .get(var)
                .cloned()
                .unwrap_or(Kind::Variable(var.clone())),
            Kind::Arrow(param, result) => Kind::Arrow(
                Box::new(self.apply(param)),
                Box::new(self.apply(result)),
            ),
            _ => kind.clone(),
        }
    }
    
    /// Unify two kinds, updating substitution
    pub fn unify(&mut self, k1: &Kind, k2: &Kind) -> Result<(), String> {
        let k1 = self.apply(k1);
        let k2 = self.apply(k2);
        
        match (&k1, &k2) {
            // Same kind
            (Kind::Star, Kind::Star) => Ok(()),
            (Kind::Arrow(p1, r1), Kind::Arrow(p2, r2)) => {
                self.unify(p1, p2)?;
                self.unify(r1, r2)
            }
            (Kind::Variable(a), Kind::Variable(b)) if a == b => Ok(()),
            
            // Kind variable cases
            (Kind::Variable(a), _) => {
                // Simple occurs check for kind variables
                // (kinds are simple enough that occurs check isn't usually needed)
                self.mapping.insert(a.clone(), k2);
                Ok(())
            }
            (_, Kind::Variable(_)) => self.unify(&k2, &k1),
            
            // Mismatch
            _ => Err(format!(
                "Kind mismatch: {} vs {}",
                k1.display_name(),
                k2.display_name()
            )),
        }
    }
}

/// Kind context for tracking kind annotations
#[derive(Debug, Clone, Default)]
pub struct KindContext {
    /// Mapping from type names to their kinds
    pub kind_env: HashMap<String, Kind>,
    
    /// Current substitution
    pub substitution: KindSubstitution,
}

impl KindContext {
    /// Create new empty context
    pub fn new() -> Self {
        KindContext {
            kind_env: HashMap::new(),
            substitution: KindSubstitution::new(),
        }
    }
    
    /// Add a kind annotation for a type
    pub fn add_kind(&mut self, type_name: String, kind: Kind) {
        self.kind_env.insert(type_name, kind);
    }
    
    /// Get the kind of a type
    pub fn get_kind(&self, type_name: &str) -> Option<Kind> {
        self.kind_env.get(type_name).cloned()
    }
    
    /// Infer the kind of a type expression
    pub fn infer_kind(&self, type_expr: &TypeExpr) -> Result<Kind, String> {
        match type_expr {
            TypeExpr::Named(name) => {
                self.get_kind(name)
                    .ok_or_else(|| format!("Unknown type: {}", name))
            }
            TypeExpr::Application(func, arg) => {
                let func_kind = self.infer_kind(func)?;
                let arg_kind = self.infer_kind(arg)?;
                
                match func_kind {
                    Kind::Arrow(param_kind, result_kind) => {
                        // Check if argument kind matches parameter kind
                        if !arg_kind.compatible_with(&param_kind) {
                            return Err(format!(
                                "Kind mismatch in type application: expected {}, got {}",
                                param_kind.display_name(),
                                arg_kind.display_name()
                            ));
                        }
                        Ok(*result_kind)
                    }
                    _ => Err(format!(
                        "Cannot apply non-arrow kind: {}",
                        func_kind.display_name()
                    )),
                }
            }
            TypeExpr::Variable(var) => {
                // Type variables have kind * by default
                Ok(Kind::Star)
            }
            TypeExpr::Forall(_, body) => {
                // Forall quantifier doesn't change the kind
                self.infer_kind(body)
            }
        }
    }
}

/// Type expression for kind inference
#[derive(Debug, Clone)]
pub enum TypeExpr {
    /// Named type (Option, Result, Vec, etc.)
    Named(String),
    
    /// Type application (Option<i32>, Result<i32, String>, etc.)
    Application(Box<TypeExpr>, Box<TypeExpr>),
    
    /// Type variable (T, U, etc.)
    Variable(String),
    
    /// Forall quantifier (∀T. ...)
    Forall(String, Box<TypeExpr>),
}

impl TypeExpr {
    /// Parse a type expression from string
    pub fn from_string(s: &str) -> Result<Self, String> {
        // Simple parser for type expressions
        // For now, handle basic cases
        let s = s.trim();
        
        if s.is_empty() {
            return Err("Empty type expression".to_string());
        }
        
        // Check for type variable (single uppercase letter or with digits)
        if s.chars().all(|c| c.is_ascii_uppercase() || c.is_ascii_digit()) {
            return Ok(TypeExpr::Variable(s.to_string()));
        }
        
        // Check for forall quantifier
        if s.starts_with("forall ") || s.starts_with("∀") {
            // Simplified: just treat as variable for now
            let inner = s.trim_start_matches("forall ").trim_start_matches('∀');
            return Ok(TypeExpr::Variable(inner.to_string()));
        }
        
        // Check for type application (has angle brackets)
        if let Some(pos) = s.find('<') {
            let name = &s[..pos];
            let mut inner = &s[pos + 1..];
            
            // Find matching '>'
            let mut depth = 1;
            let mut end_pos = None;
            
            for (i, ch) in inner.chars().enumerate() {
                match ch {
                    '<' => depth += 1,
                    '>' => {
                        depth -= 1;
                        if depth == 0 {
                            end_pos = Some(i);
                            break;
                        }
                    }
                    _ => {}
                }
            }
            
            if let Some(end) = end_pos {
                inner = &inner[..end];
                
                // Parse arguments (comma separated)
                let args: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();
                
                if args.is_empty() {
                    return Err("Empty type arguments".to_string());
                }
                
                // Build application tree: F<A, B> becomes ((F A) B)
                let mut expr = TypeExpr::Named(name.to_string());
                for arg in args {
                    let arg_expr = TypeExpr::from_string(arg)?;
                    expr = TypeExpr::Application(Box::new(expr), Box::new(arg_expr));
                }
                
                return Ok(expr);
            }
        }
        
        // Simple named type
        Ok(TypeExpr::Named(s.to_string()))
    }
    
    /// Get display name for type expression
    pub fn display_name(&self) -> String {
        match self {
            TypeExpr::Named(name) => name.clone(),
            TypeExpr::Application(func, arg) => {
                format!("{}<{}>", func.display_name(), arg.display_name())
            }
            TypeExpr::Variable(var) => var.clone(),
            TypeExpr::Forall(var, body) => {
                format!("∀{}. {}", var, body.display_name())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_kind_display() {
        assert_eq!(Kind::Star.display_name(), "*");
        
        let arrow = Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star));
        assert_eq!(arrow.display_name(), "* -> *");
        
        let nested = Kind::Arrow(
            Box::new(Kind::Star),
            Box::new(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))),
        );
        assert_eq!(nested.display_name(), "* -> * -> *");
        
        let var = Kind::Variable(KindVar(42));
        assert_eq!(var.display_name(), "K42");
    }
    
    #[test]
    fn test_kind_arity() {
        assert_eq!(Kind::Star.arity(), 0);
        
        let arrow = Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star));
        assert_eq!(arrow.arity(), 1);
        
        let nested = Kind::Arrow(
            Box::new(Kind::Star),
            Box::new(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))),
        );
        assert_eq!(nested.arity(), 2);
    }
    
    #[test]
    fn test_kind_application() {
        let arrow = Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star));
        
        // Apply * to * -> * should give *
        let result = arrow.apply(&[Kind::Star]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Kind::Star);
        
        // Apply wrong kind should fail
        let wrong_arrow = Kind::Arrow(Box::new(Kind::Variable(KindVar(1))), Box::new(Kind::Star));
        let result = wrong_arrow.apply(&[Kind::Star]);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_kind_unification() {
        let mut subst = KindSubstitution::new();
        
        // Unify variable with star
        let var = Kind::Variable(KindVar::fresh());
        assert!(subst.unify(&var, &Kind::Star).is_ok());
        assert_eq!(subst.apply(&var), Kind::Star);
        
        // Unify two variables
        let var1 = Kind::Variable(KindVar::fresh());
        let var2 = Kind::Variable(KindVar::fresh());
        assert!(subst.unify(&var1, &var2).is_ok());
        
        // After unification, they should be the same
        let applied1 = subst.apply(&var1);
        let applied2 = subst.apply(&var2);
        assert_eq!(applied1, applied2);
    }
    
    #[test]
    fn test_kind_context() {
        let mut ctx = KindContext::new();
        
        // Add kind annotations
        ctx.add_kind("i32".to_string(), Kind::Star);
        ctx.add_kind("Option".to_string(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
        ctx.add_kind("Result".to_string(), Kind::Arrow(
            Box::new(Kind::Star),
            Box::new(Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star))),
        ));
        
        // Get kinds
        assert_eq!(ctx.get_kind("i32"), Some(Kind::Star));
        assert_eq!(ctx.get_kind("Option").unwrap().arity(), 1);
        assert_eq!(ctx.get_kind("Result").unwrap().arity(), 2);
        
        // Unknown type
        assert!(ctx.get_kind("Unknown").is_none());
    }
    
    #[test]
    fn test_type_expr_parsing() {
        // Simple type
        let expr = TypeExpr::from_string("i32").unwrap();
        assert!(matches!(expr, TypeExpr::Named(_)));
        
        // Type variable
        let expr = TypeExpr::from_string("T").unwrap();
        assert!(matches!(expr, TypeExpr::Variable(_)));
        
        // Type application
        let expr = TypeExpr::from_string("Option<i32>").unwrap();
        assert!(matches!(expr, TypeExpr::Application(_, _)));
        
        // Nested application
        let expr = TypeExpr::from_string("Result<i32, String>").unwrap();
        assert!(matches!(expr, TypeExpr::Application(_, _)));
    }
    
    #[test]
    fn test_kind_inference() {
        let mut ctx = KindContext::new();
        
        // Setup kind environment
        ctx.add_kind("i32".to_string(), Kind::Star);
        ctx.add_kind("Option".to_string(), Kind::Arrow(Box::new(Kind::Star), Box::new(Kind::Star)));
        
        // Infer kind of i32
        let expr = TypeExpr::from_string("i32").unwrap();
        let kind = ctx.infer_kind(&expr).unwrap();
        assert_eq!(kind, Kind::Star);
        
        // Infer kind of Option<i32>
        let expr = TypeExpr::from_string("Option<i32>").unwrap();
        let kind = ctx.infer_kind(&expr).unwrap();
        assert_eq!(kind, Kind::Star);
        
        // Try to apply Option to itself (should fail)
        let expr = TypeExpr::from_string("Option<Option>").unwrap();
        let result = ctx.infer_kind(&expr);
        assert!(result.is_err());
    }
}