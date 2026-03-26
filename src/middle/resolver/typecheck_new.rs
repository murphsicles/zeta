//! New type checking system with unification
//! Bridge between old string-based types and new algebraic types

use super::resolver::Resolver;
use crate::frontend::ast::AstNode;
use crate::middle::types::{Type, Substitution, UnifyError};

/// Extended resolver with new type checking
pub trait NewTypeCheck {
    /// Type check with new system
    fn typecheck_new(&mut self, asts: &[AstNode]) -> Result<Substitution, Vec<UnifyError>>;
    
    /// Convert old string type to new Type
    fn string_to_type(&self, s: &str) -> Type;
    
    /// Convert new Type to old string
    fn type_to_string(&self, ty: &Type) -> String;
}

impl NewTypeCheck for Resolver {
    fn typecheck_new(&mut self, asts: &[AstNode]) -> Result<Substitution, Vec<UnifyError>> {
        use crate::middle::resolver::new_resolver;
        
        let mut context = new_resolver::InferContext::new();
        
        // Convert existing variable types from old system
        // (This would need access to Resolver's internal state)
        
        // Infer types for all AST nodes
        for ast in asts {
            if let Err(e) = context.infer(ast) {
                // Convert inference error to unification error
                return Err(vec![UnifyError::Mismatch(
                    Type::Error,
                    Type::Error
                )]);
            }
        }
        
        // Solve constraints
        context.solve()?;
        Ok(context.take_substitution())
    }
    
    fn string_to_type(&self, s: &str) -> Type {
        match s {
            "i64" => Type::I64,
            "i32" => Type::I32,
            "bool" => Type::Bool,
            "str" => Type::Str,
            "i8" => Type::I8,
            "i16" => Type::I16,
            "u8" => Type::U8,
            "u16" => Type::U16,
            "u32" => Type::U32,
            "u64" => Type::U64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            "char" => Type::Char,
            _ => {
                // Check if it's a generic type like "Map_i64_i64"
                if s.starts_with("Map_") {
                    // Simple parsing for now
                    Type::Named(s.to_string(), Vec::new())
                } else {
                    Type::Named(s.to_string(), Vec::new())
                }
            }
        }
    }
    
    fn type_to_string(&self, ty: &Type) -> String {
        ty.display_name()
    }
}

/// Migration wrapper for old type checking API
pub struct TypeCheckMigrator {
    resolver: Resolver,
    use_new_system: bool,
}

impl TypeCheckMigrator {
    pub fn new(resolver: Resolver) -> Self {
        TypeCheckMigrator {
            resolver,
            use_new_system: false, // Start with old system for compatibility
        }
    }
    
    pub fn enable_new_system(&mut self) {
        self.use_new_system = true;
    }
    
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        if self.use_new_system {
            // Use new system
            match self.resolver.typecheck_new(asts) {
                Ok(_) => true,
                Err(errors) => {
                    for error in errors {
                        eprintln!("Type error: {}", error);
                    }
                    false
                }
            }
        } else {
            // Use old system
            self.resolver.typecheck(asts)
        }
    }
    
    pub fn infer_type(&self, node: &AstNode) -> String {
        if self.use_new_system {
            // Use new inference
            use crate::middle::resolver::new_resolver;
            let mut context = new_resolver::InferContext::new();
            match context.infer(node) {
                Ok(ty) => self.resolver.type_to_string(&ty),
                Err(_) => "<?>".to_string(),
            }
        } else {
            // Use old inference
            self.resolver.infer_type(node)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::frontend::ast::AstNode;
    
    #[test]
    fn test_type_conversion() {
        let resolver = Resolver::new();
        
        assert_eq!(resolver.string_to_type("i64"), Type::I64);
        assert_eq!(resolver.string_to_type("bool"), Type::Bool);
        assert_eq!(resolver.string_to_type("str"), Type::Str);
        
        let i64_type = Type::I64;
        assert_eq!(resolver.type_to_string(&i64_type), "i64");
        
        let bool_type = Type::Bool;
        assert_eq!(resolver.type_to_string(&bool_type), "bool");
    }
    
    #[test]
    fn test_migrator() {
        let resolver = Resolver::new();
        let mut migrator = TypeCheckMigrator::new(resolver);
        
        // Should use old system by default
        let ast = vec![AstNode::Lit(42)];
        assert!(migrator.typecheck(&ast));
        
        // Enable new system
        migrator.enable_new_system();
        
        // Should still work with new system
        assert!(migrator.typecheck(&ast));
    }
}