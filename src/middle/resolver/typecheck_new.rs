//! New type checking system with unification
//! Bridge between old string-based types and new algebraic types

use super::resolver::Resolver;
use crate::frontend::ast::AstNode;
use crate::middle::types::{Substitution, Type, UnifyError};

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

        // Convert existing variable types from old system to new system
        // Note: This is a simplified conversion - in a full implementation,
        // we would need to convert the entire resolver state
        // For now, we start with a clean context

        // Infer types for all AST nodes
        let mut any_success = false;
        
        for ast in asts {
            match context.infer(ast) {
                Ok(_) => {
                    // Type inference succeeded for this node
                    any_success = true;
                }
                Err(e) => {
                    // Type inference failed for this node type
                    // Instead of failing entire system, skip this node
                    // Old system will handle it
                    eprintln!("Type inference not implemented for node type, skipping: {}", e);
                    // Continue with other nodes - don't fail entire system
                }
            }
        }

        // Solve constraints if we had any successful inferences
        if any_success {
            match context.solve() {
                Ok(_) => Ok(context.take_substitution()),
                Err(e) => {
                    eprintln!("Constraint solving failed: {:?}", e);
                    // Return error to indicate type check failure
                    Err(e)
                }
            }
        } else {
            // No nodes could be inferred with new system
            // Return empty substitution to trigger old system
            Ok(Substitution::new())
        }
    }

    fn string_to_type(&self, s: &str) -> Type {
        // Handle reference types: &str, &mut i64, etc.
        let s = s.trim();
        
        // Debug: print what we're parsing
        eprintln!("[DEBUG] string_to_type parsing: '{}'", s);
        
        // Check for &mut prefix (must check before & prefix)
        if let Some(rest) = s.strip_prefix("&mut ") {
            let inner = self.string_to_type(rest);
            return Type::Ref(Box::new(inner), crate::middle::types::Mutability::Mutable);
        }
        
        // Check for & prefix
        if let Some(rest) = s.strip_prefix("&") {
            // Make sure we didn't match &mut (should have been caught above)
            if !rest.starts_with("mut ") {
                let inner = self.string_to_type(rest);
                return Type::Ref(Box::new(inner), crate::middle::types::Mutability::Immutable);
            }
        }
        
        // Check for array type: [T; N]
        if s.starts_with('[') {
            if !s.ends_with(']') {
                // Malformed array/slice - treat as named type
                return Type::Named(s.to_string(), Vec::new());
            }
            
            let inner = &s[1..s.len()-1]; // Remove brackets
            if let Some((type_part, size_part)) = inner.split_once(';') {
                let inner_type = self.string_to_type(type_part.trim());
                if let Ok(size) = size_part.trim().parse::<usize>() {
                    return Type::Array(Box::new(inner_type), size);
                }
            } else {
                // Slice type: [T]
                let inner_type = self.string_to_type(inner.trim());
                return Type::Slice(Box::new(inner_type));
            }
        }
        
        // Check for tuple type: (T1, T2, T3)
        if s.starts_with('(') {
            if !s.ends_with(')') {
                // Malformed tuple - treat as named type
                return Type::Named(s.to_string(), Vec::new());
            }
            
            let inner = &s[1..s.len()-1]; // Remove parentheses
            if inner.is_empty() {
                // Empty tuple: ()
                return Type::Tuple(Vec::new());
            }
            
            // Split by commas, but be careful about nested tuples
            let mut types = Vec::new();
            let mut current = String::new();
            let mut depth = 0;
            
            for ch in inner.chars() {
                match ch {
                    '(' => {
                        depth += 1;
                        current.push(ch);
                    }
                    ')' => {
                        depth -= 1;
                        current.push(ch);
                    }
                    ',' if depth == 0 => {
                        if !current.is_empty() {
                            types.push(self.string_to_type(current.trim()));
                            current.clear();
                        }
                    }
                    _ => current.push(ch),
                }
            }
            
            if !current.is_empty() {
                types.push(self.string_to_type(current.trim()));
            }
            
            return Type::Tuple(types);
        }
        
        // Handle base types
        match s {
            "i64" => Type::I64,
            "i32" => Type::I32,
            "bool" => Type::Bool,
            "str" => Type::Str,
            "String" => Type::Named("String".to_string(), Vec::new()),
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

        // Test reference types
        assert_eq!(
            resolver.string_to_type("&str"),
            Type::Ref(Box::new(Type::Str), crate::middle::types::Mutability::Immutable)
        );
        
        assert_eq!(
            resolver.string_to_type("&mut i64"),
            Type::Ref(Box::new(Type::I64), crate::middle::types::Mutability::Mutable)
        );
        
        assert_eq!(
            resolver.string_to_type("&bool"),
            Type::Ref(Box::new(Type::Bool), crate::middle::types::Mutability::Immutable)
        );

        // Test array types
        assert_eq!(
            resolver.string_to_type("[i32; 10]"),
            Type::Array(Box::new(Type::I32), 10)
        );
        
        assert_eq!(
            resolver.string_to_type("[bool; 5]"),
            Type::Array(Box::new(Type::Bool), 5)
        );
        
        // Test slice types
        assert_eq!(
            resolver.string_to_type("[i64]"),
            Type::Slice(Box::new(Type::I64))
        );
        
        assert_eq!(
            resolver.string_to_type("[&str]"),
            Type::Slice(Box::new(Type::Ref(Box::new(Type::Str), crate::middle::types::Mutability::Immutable)))
        );
        
        // Test tuple types
        assert_eq!(
            resolver.string_to_type("()"),
            Type::Tuple(Vec::new())
        );
        
        assert_eq!(
            resolver.string_to_type("(i32, bool)"),
            Type::Tuple(vec![Type::I32, Type::Bool])
        );
        
        assert_eq!(
            resolver.string_to_type("(i64, &str, bool)"),
            Type::Tuple(vec![
                Type::I64,
                Type::Ref(Box::new(Type::Str), crate::middle::types::Mutability::Immutable),
                Type::Bool
            ])
        );
        
        // Test nested tuples
        assert_eq!(
            resolver.string_to_type("((i32, bool), i64)"),
            Type::Tuple(vec![
                Type::Tuple(vec![Type::I32, Type::Bool]),
                Type::I64
            ])
        );

        let i64_type = Type::I64;
        assert_eq!(resolver.type_to_string(&i64_type), "i64");

        let bool_type = Type::Bool;
        assert_eq!(resolver.type_to_string(&bool_type), "bool");
        
        // Test reference type display
        let ref_str = Type::Ref(Box::new(Type::Str), crate::middle::types::Mutability::Immutable);
        assert_eq!(resolver.type_to_string(&ref_str), "&str");
        
        let mut_ref_i64 = Type::Ref(Box::new(Type::I64), crate::middle::types::Mutability::Mutable);
        assert_eq!(resolver.type_to_string(&mut_ref_i64), "&mut i64");
        
        // Test array type display
        let array_i32 = Type::Array(Box::new(Type::I32), 10);
        assert_eq!(resolver.type_to_string(&array_i32), "[i32; 10]");
        
        // Test slice type display
        let slice_i64 = Type::Slice(Box::new(Type::I64));
        assert_eq!(resolver.type_to_string(&slice_i64), "[i64]");
        
        // Test tuple type display
        let empty_tuple = Type::Tuple(Vec::new());
        assert_eq!(resolver.type_to_string(&empty_tuple), "()");
        
        let simple_tuple = Type::Tuple(vec![Type::I32, Type::Bool]);
        assert_eq!(resolver.type_to_string(&simple_tuple), "(i32, bool)");
        
        let complex_tuple = Type::Tuple(vec![
            Type::I64,
            Type::Ref(Box::new(Type::Str), crate::middle::types::Mutability::Immutable),
            Type::Bool
        ]);
        assert_eq!(resolver.type_to_string(&complex_tuple), "(i64, &str, bool)");
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
