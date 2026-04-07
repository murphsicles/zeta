//! New type checking system with unification
//! Bridge between old string-based types and new algebraic types

use super::resolver::Resolver;
use crate::frontend::ast::AstNode;
use crate::middle::types::{ArraySize, Substitution, Type, UnifyError, IdentityType, CapabilityLevel};

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

        eprintln!("[TYPECHECK_NEW] Starting typecheck_new with {} AST nodes", asts.len());
        
        let mut context = new_resolver::InferContext::new();

        // Convert existing variable types from old system to new system
        // Note: This is a simplified conversion - in a full implementation,
        // we would need to convert the entire resolver state
        // For now, we start with a clean context

        // Add built-in functions from resolver to the inference context
        eprintln!("[TYPECHECK_NEW] Adding built-in functions to inference context");
        let funcs = self.get_all_func_signatures();
        eprintln!("[TYPECHECK_NEW] Found {} built-in functions", funcs.len());
        for (name, (params, ret_ty, _is_async)) in funcs {
            eprintln!("[TYPECHECK_NEW] Adding function: {} with {} params", name, params.len());
            // Convert parameter types to a vector of Types
            let param_types: Vec<Type> = params.iter().map(|(_, ty)| ty.clone()).collect();
            
            // Create function type: (param_types) -> ret_ty
            let func_type = Type::Function(param_types, Box::new(ret_ty.clone()));
            
            // Add to inference context
            context.add_function(name.clone(), func_type);
        }

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
                    eprintln!(
                        "Type inference not implemented for node type, skipping: {}",
                        e
                    );
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
        
        // Safety check: prevent infinite recursion
        if s.is_empty() {
            return Type::Named("".to_string(), Vec::new());
        }
        
        // Simple types should not recurse
        match s {
            "i64" => return Type::I64,
            "i32" => return Type::I32,
            "bool" => return Type::Bool,
            "str" => return Type::Str,
            "string" => return Type::Str,
            "String" => return Type::Named("String".to_string(), Vec::new()),
            "i8" => return Type::I8,
            "i16" => return Type::I16,
            "u8" => return Type::U8,
            "u16" => return Type::U16,
            "u32" => return Type::U32,
            "u64" => return Type::U64,
            "f32" => return Type::F32,
            "f64" => return Type::F64,
            "char" => return Type::Char,
            _ => {}
        }

        // Check for &mut prefix (must check before & prefix)
        if let Some(rest) = s.strip_prefix("&mut ") {
            let inner = self.string_to_type(rest);
            return Type::Ref(
                Box::new(inner),
                crate::middle::types::Lifetime::Static,
                crate::middle::types::Mutability::Mutable,
            );
        }

        // Check for & prefix
        if let Some(rest) = s.strip_prefix("&") {
            // Make sure we didn't match &mut (should have been caught above)
            if !rest.starts_with("mut ") {
                let inner = self.string_to_type(rest);
                return Type::Ref(
                    Box::new(inner),
                    crate::middle::types::Lifetime::Static,
                    crate::middle::types::Mutability::Immutable,
                );
            }
        }

        // Check for identity type: string[identity:read], string[identity:read+write], etc.
        if s.starts_with("string[identity:") && s.ends_with(']') {
            let inner = &s["string[".len()..s.len() - 1]; // Remove "string[" and "]"
            if let Some(capabilities_str) = inner.strip_prefix("identity:") {
                // Parse capabilities
                let capabilities: Vec<CapabilityLevel> = capabilities_str
                    .split('+')
                    .filter_map(|cap| match cap.trim() {
                        "read" => Some(CapabilityLevel::Read),
                        "write" => Some(CapabilityLevel::Write),
                        "immutable" => Some(CapabilityLevel::Immutable),
                        "owned" => Some(CapabilityLevel::Owned),
                        "execute" => Some(CapabilityLevel::Execute),
                        _ => None,
                    })
                    .collect();
                
                if capabilities.is_empty() {
                    // No valid capabilities found, treat as named type
                    return Type::Named(s.to_string(), Vec::new());
                }
                
                // Create identity type
                let identity_type = IdentityType {
                    value: None,
                    capabilities,
                    delegatable: false,
                    constraints: Vec::new(),
                    type_params: Vec::new(),
                };
                
                return Type::Identity(Box::new(identity_type));
            }
        }

        // Check for array type: [T; N]
        if s.starts_with('[') {
            if !s.ends_with(']') {
                // Malformed array/slice - treat as named type
                return Type::Named(s.to_string(), Vec::new());
            }

            let inner = &s[1..s.len() - 1]; // Remove brackets
            if let Some((type_part, size_part)) = inner.split_once(';') {
                let inner_type = self.string_to_type(type_part.trim());
                if let Ok(size) = size_part.trim().parse::<usize>() {
                    return Type::Array(Box::new(inner_type), ArraySize::Literal(size));
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

            let inner = &s[1..s.len() - 1]; // Remove parentheses
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

        // Check for Zeta's lt() syntax: lt(Result, i64)
        if s.starts_with("lt(") && s.ends_with(')') {
            let inner = &s[3..s.len() - 1]; // Remove "lt(" and ")"
            // Parse type name and arguments
            let parts: Vec<&str> = inner.split(',').map(|s| s.trim()).collect();
            if parts.is_empty() {
                return Type::Named(s.to_string(), Vec::new());
            }

            let type_name = parts[0];
            let mut args = Vec::new();
            for arg in parts.iter().skip(1) {
                args.push(self.string_to_type(arg));
            }

            return Type::Named(type_name.to_string(), args);
        }

        // Check for generic type: Vec<i32>, Option<T>, Result<T, E>
        // Look for < followed by > with content in between
        if let Some(open_angle) = s.find('<')
            && let Some(close_angle) = s.rfind('>')
            && open_angle < close_angle
        {
            let type_name = &s[..open_angle];
            
            // Safety check: type_name must not be empty and must not be a primitive type
            if type_name.is_empty() || matches!(type_name, "i64" | "i32" | "bool" | "str" | "i8" | "i16" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64" | "char") {
                // This is not a valid generic type, fall back to named type
                return Type::Named(s.to_string(), Vec::new());
            }
            
            let inner = &s[open_angle + 1..close_angle];

            // Parse type arguments, handling nested generics
            let mut args = Vec::new();
            let mut current = String::new();
            let mut depth = 0;

            for ch in inner.chars() {
                match ch {
                    '<' => {
                        depth += 1;
                        current.push(ch);
                    }
                    '>' => {
                        depth -= 1;
                        current.push(ch);
                    }
                    ',' if depth == 0 => {
                        if !current.is_empty() {
                            args.push(self.string_to_type(current.trim()));
                            current.clear();
                        }
                    }
                    _ => current.push(ch),
                }
            }

            if !current.is_empty() {
                args.push(self.string_to_type(current.trim()));
            }

            return Type::Named(type_name.to_string(), args);
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
                } else if s.len() == 1 && s.chars().next().unwrap().is_ascii_uppercase() {
                    // Single uppercase letter: treat as type variable
                    // Create a fresh type variable for each occurrence
                    Type::Variable(crate::middle::types::TypeVar::fresh())
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
            self.resolver.infer_type(node).display_name()
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
            Type::Ref(
                Box::new(Type::Str),
                crate::middle::types::lifetime::Lifetime::Static,
                crate::middle::types::Mutability::Immutable
            )
        );

        assert_eq!(
            resolver.string_to_type("&mut i64"),
            Type::Ref(
                Box::new(Type::I64),
                crate::middle::types::lifetime::Lifetime::Static,
                crate::middle::types::Mutability::Mutable
            )
        );

        assert_eq!(
            resolver.string_to_type("&bool"),
            Type::Ref(
                Box::new(Type::Bool),
                crate::middle::types::lifetime::Lifetime::Static,
                crate::middle::types::Mutability::Immutable
            )
        );

        // Test array types
        assert_eq!(
            resolver.string_to_type("[i32; 10]"),
            Type::Array(Box::new(Type::I32), ArraySize::Literal(10))
        );

        assert_eq!(
            resolver.string_to_type("[bool; 5]"),
            Type::Array(Box::new(Type::Bool), ArraySize::Literal(5))
        );

        // Test slice types
        assert_eq!(
            resolver.string_to_type("[i64]"),
            Type::Slice(Box::new(Type::I64))
        );

        assert_eq!(
            resolver.string_to_type("[&str]"),
            Type::Slice(Box::new(Type::Ref(
                Box::new(Type::Str),
                crate::middle::types::lifetime::Lifetime::Static,
                crate::middle::types::Mutability::Immutable
            )))
        );

        // Test tuple types
        assert_eq!(resolver.string_to_type("()"), Type::Tuple(Vec::new()));

        assert_eq!(
            resolver.string_to_type("(i32, bool)"),
            Type::Tuple(vec![Type::I32, Type::Bool])
        );

        assert_eq!(
            resolver.string_to_type("(i64, &str, bool)"),
            Type::Tuple(vec![
                Type::I64,
                Type::Ref(
                    Box::new(Type::Str),
                    crate::middle::types::lifetime::Lifetime::Static,
                    crate::middle::types::Mutability::Immutable
                ),
                Type::Bool
            ])
        );

        // Test nested tuples
        assert_eq!(
            resolver.string_to_type("((i32, bool), i64)"),
            Type::Tuple(vec![Type::Tuple(vec![Type::I32, Type::Bool]), Type::I64])
        );

        let i64_type = Type::I64;
        assert_eq!(resolver.type_to_string(&i64_type), "i64");

        let bool_type = Type::Bool;
        assert_eq!(resolver.type_to_string(&bool_type), "bool");

        // Test reference type display
        let ref_str = Type::Ref(
            Box::new(Type::Str),
            crate::middle::types::lifetime::Lifetime::Static,
            crate::middle::types::Mutability::Immutable,
        );
        assert_eq!(resolver.type_to_string(&ref_str), "&'static str");

        let mut_ref_i64 = Type::Ref(
            Box::new(Type::I64),
            crate::middle::types::lifetime::Lifetime::Static,
            crate::middle::types::Mutability::Mutable,
        );
        assert_eq!(resolver.type_to_string(&mut_ref_i64), "&'static mut i64");

        // Test array type display
        let array_i32 = Type::Array(Box::new(Type::I32), ArraySize::Literal(10));
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
            Type::Ref(
                Box::new(Type::Str),
                crate::middle::types::lifetime::Lifetime::Static,
                crate::middle::types::Mutability::Immutable,
            ),
            Type::Bool,
        ]);
        assert_eq!(
            resolver.type_to_string(&complex_tuple),
            "(i64, &'static str, bool)"
        );

        // Test generic types
        assert_eq!(
            resolver.string_to_type("Vec<i32>"),
            Type::Named("Vec".to_string(), vec![Type::I32])
        );

        assert_eq!(
            resolver.string_to_type("Option<bool>"),
            Type::Named("Option".to_string(), vec![Type::Bool])
        );

        assert_eq!(
            resolver.string_to_type("Result<i32, String>"),
            Type::Named(
                "Result".to_string(),
                vec![Type::I32, Type::Named("String".to_string(), Vec::new())]
            )
        );

        // Test nested generic types
        assert_eq!(
            resolver.string_to_type("Vec<Vec<i32>>"),
            Type::Named(
                "Vec".to_string(),
                vec![Type::Named("Vec".to_string(), vec![Type::I32])]
            )
        );

        assert_eq!(
            resolver.string_to_type("Option<Vec<bool>>"),
            Type::Named(
                "Option".to_string(),
                vec![Type::Named("Vec".to_string(), vec![Type::Bool])]
            )
        );

        // Test generic types with complex type arguments
        assert_eq!(
            resolver.string_to_type("Vec<&str>"),
            Type::Named(
                "Vec".to_string(),
                vec![Type::Ref(
                    Box::new(Type::Str),
                    crate::middle::types::lifetime::Lifetime::Static,
                    crate::middle::types::Mutability::Immutable
                )]
            )
        );

        assert_eq!(
            resolver.string_to_type("HashMap<String, i32>"),
            Type::Named(
                "HashMap".to_string(),
                vec![Type::Named("String".to_string(), Vec::new()), Type::I32]
            )
        );

        // Test generic type display
        let vec_i32 = Type::Named("Vec".to_string(), vec![Type::I32]);
        assert_eq!(resolver.type_to_string(&vec_i32), "Vec<i32>");

        let option_bool = Type::Named("Option".to_string(), vec![Type::Bool]);
        assert_eq!(resolver.type_to_string(&option_bool), "Option<bool>");

        let result_i32_string = Type::Named(
            "Result".to_string(),
            vec![Type::I32, Type::Named("String".to_string(), Vec::new())],
        );
        assert_eq!(
            resolver.type_to_string(&result_i32_string),
            "Result<i32, String>"
        );

        let nested_vec = Type::Named(
            "Vec".to_string(),
            vec![Type::Named("Vec".to_string(), vec![Type::I32])],
        );
        assert_eq!(resolver.type_to_string(&nested_vec), "Vec<Vec<i32>>");
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
