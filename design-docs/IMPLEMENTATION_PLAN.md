# Generic Type System Implementation Plan

## Phase 1: Foundation (Complete by 20:00 GMT)

### File: `src/middle/types/mod.rs`
**Changes:**
1. Add `TraitBound` enum after `Type` enum:
```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitBound {
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    // Add more as needed
}
```

2. Add `TypeParam` struct:
```rust
#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: String,
    pub bounds: Vec<TraitBound>,
}
```

3. Add `GenericContext` struct:
```rust
#[derive(Debug, Clone)]
pub struct GenericContext {
    pub type_params: Vec<TypeParam>,
    pub parent: Option<Box<GenericContext>>,
}

impl GenericContext {
    pub fn new() -> Self {
        GenericContext {
            type_params: Vec::new(),
            parent: None,
        }
    }
    
    pub fn with_parent(parent: GenericContext) -> Self {
        GenericContext {
            type_params: Vec::new(),
            parent: Some(Box::new(parent)),
        }
    }
    
    pub fn find_type_param(&self, name: &str) -> Option<&TypeParam> {
        self.type_params.iter().find(|p| p.name == name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.find_type_param(name)))
    }
}
```

4. Extend `Substitution` struct:
```rust
impl Substitution {
    /// Improved generic instantiation with bounds checking
    pub fn instantiate_generic(
        &self,
        generic_ty: &Type,
        type_args: &[Type],
        context: &GenericContext,
    ) -> Result<Type, String> {
        match generic_ty {
            Type::Named(name, generic_params) => {
                // Check arity
                if generic_params.len() != type_args.len() {
                    return Err(format!(
                        "Wrong number of type arguments for {}: expected {}, got {}",
                        name,
                        generic_params.len(),
                        type_args.len()
                    ));
                }
                
                // Create substitution mapping
                let mut substitution = Substitution::new();
                
                // Map type parameters to arguments
                for (param, arg) in generic_params.iter().zip(type_args.iter()) {
                    if let Type::Variable(var) = param {
                        // Check if this type variable has bounds
                        if let Some(type_param) = context.find_type_param(&format!("T{}", var.0)) {
                            // Verify bounds are satisfied
                            for bound in &type_param.bounds {
                                if !self.satisfies_bound(arg, bound) {
                                    return Err(format!(
                                        "Type {} doesn't satisfy bound {:?}",
                                        arg.display_name(),
                                        bound
                                    ));
                                }
                            }
                        }
                        substitution.mapping.insert(var.clone(), arg.clone());
                    } else {
                        // Complex parameter - recursively instantiate
                        let instantiated_param = self.instantiate_generic(param, type_args, context)?;
                        // For now, require exact match for non-variable parameters
                        if &instantiated_param != arg {
                            return Err(format!(
                                "Type argument mismatch: expected {}, got {}",
                                instantiated_param.display_name(),
                                arg.display_name()
                            ));
                        }
                    }
                }
                
                // Apply substitution
                Ok(substitution.apply(generic_ty))
            }
            
            // Handle other type constructors recursively
            Type::Array(inner, size) => {
                let instantiated_inner = self.instantiate_generic(inner, type_args, context)?;
                Ok(Type::Array(Box::new(instantiated_inner), *size))
            }
            
            Type::Slice(inner) => {
                let instantiated_inner = self.instantiate_generic(inner, type_args, context)?;
                Ok(Type::Slice(Box::new(instantiated_inner)))
            }
            
            Type::Tuple(types) => {
                let instantiated_types: Result<Vec<Type>, String> = types
                    .iter()
                    .map(|t| self.instantiate_generic(t, type_args, context))
                    .collect();
                Ok(Type::Tuple(instantiated_types?))
            }
            
            Type::Ref(inner, lifetime, mutability) => {
                let instantiated_inner = self.instantiate_generic(inner, type_args, context)?;
                Ok(Type::Ref(
                    Box::new(instantiated_inner),
                    lifetime.clone(),
                    *mutability,
                ))
            }
            
            Type::Function(params, ret) => {
                let instantiated_params: Result<Vec<Type>, String> = params
                    .iter()
                    .map(|p| self.instantiate_generic(p, type_args, context))
                    .collect();
                let instantiated_ret = self.instantiate_generic(ret, type_args, context)?;
                Ok(Type::Function(
                    instantiated_params?,
                    Box::new(instantiated_ret),
                ))
            }
            
            // Type variables get replaced via substitution
            Type::Variable(var) => {
                if let Some(ty) = self.mapping.get(var) {
                    Ok(ty.clone())
                } else {
                    // Unbound type variable remains
                    Ok(Type::Variable(var.clone()))
                }
            }
            
            // Primitive types remain unchanged
            _ => Ok(generic_ty.clone()),
        }
    }
    
    /// Check if a type satisfies a trait bound
    pub fn satisfies_bound(&self, ty: &Type, bound: &TraitBound) -> bool {
        let ty = self.apply(ty);
        
        match bound {
            TraitBound::Copy => self.is_copy(&ty),
            TraitBound::Clone => self.is_clone(&ty),
            TraitBound::Debug => self.is_debug(&ty),
            TraitBound::Default => self.is_default(&ty),
            TraitBound::PartialEq => self.is_partial_eq(&ty),
            TraitBound::Eq => self.is_eq(&ty),
            // Add more trait checks as needed
            _ => false, // Default to false for unimplemented traits
        }
    }
    
    /// Helper methods for trait checks
    fn is_copy(&self, ty: &Type) -> bool {
        match ty {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 |
            Type::U8 | Type::U16 | Type::U32 | Type::U64 |
            Type::F32 | Type::F64 | Type::Bool | Type::Char => true,
            
            Type::Ref(_, _, Mutability::Immutable) => true, // Shared references are Copy
            
            Type::Tuple(types) => types.iter().all(|t| self.is_copy(t)),
            
            Type::Named(name, args) => {
                // Check if this is a known Copy type
                match name.as_str() {
                    "Option" if args.len() == 1 => self.is_copy(&args[0]),
                    "Result" if args.len() == 2 => self.is_copy(&args[0]) && self.is_copy(&args[1]),
                    _ => false,
                }
            }
            
            _ => false,
        }
    }
    
    fn is_clone(&self, ty: &Type) -> bool {
        // For now, assume all types are Clone
        // In a real implementation, we'd track which types implement Clone
        true
    }
    
    fn is_debug(&self, ty: &Type) -> bool {
        // Assume all types are Debug for now
        true
    }
    
    fn is_default(&self, ty: &Type) -> bool {
        match ty {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 |
            Type::U8 | Type::U16 | Type::U32 | Type::U64 |
            Type::F32 | Type::F64 | Type::Bool | Type::Char => true,
            
            Type::Tuple(types) => types.iter().all(|t| self.is_default(t)),
            
            Type::Named(name, args) => {
                match name.as_str() {
                    "Option" if args.len() == 1 => self.is_default(&args[0]),
                    "Vec" if args.len() == 1 => self.is_default(&args[0]),
                    _ => false,
                }
            }
            
            _ => false,
        }
    }
    
    fn is_partial_eq(&self, ty: &Type) -> bool {
        // Most types are PartialEq
        !matches!(ty, Type::Error)
    }
    
    fn is_eq(&self, ty: &Type) -> bool {
        self.is_partial_eq(ty) // For now, same as PartialEq
    }
}
```

5. Add tests for new functionality at the end of the file:
```rust
#[test]
fn test_trait_bounds() {
    let mut subst = Substitution::new();
    
    // i32 should satisfy Copy, Clone, Debug
    assert!(subst.satisfies_bound(&Type::I32, &TraitBound::Copy));
    assert!(subst.satisfies_bound(&Type::I32, &TraitBound::Clone));
    assert!(subst.satisfies_bound(&Type::I32, &TraitBound::Debug));
    
    // &i32 should be Copy (shared reference)
    let ref_i32 = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Immutable,
    );
    assert!(subst.satisfies_bound(&ref_i32, &TraitBound::Copy));
    
    // &mut i32 should NOT be Copy
    let mut_ref_i32 = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Mutable,
    );
    assert!(!subst.satisfies_bound(&mut_ref_i32, &TraitBound::Copy));
}

#[test]
fn test_generic_instantiation_with_bounds() {
    let context = GenericContext::new();
    let mut subst = Substitution::new();
    
    // Create a generic type Vec<T>
    let t_var = Type::Variable(TypeVar::fresh());
    let vec_t = Type::Named("Vec".to_string(), vec![t_var.clone()]);
    
    // Instantiate with i32
    let result = subst.instantiate_generic(&vec_t, &[Type::I32], &context);
    assert!(result.is_ok());
    let vec_i32 = result.unwrap();
    
    // Should be Vec<i32>
    if let Type::Named(name, args) = vec_i32 {
        assert_eq!(name, "Vec");
        assert_eq!(args.len(), 1);
        assert_eq!(args[0], Type::I32);
    } else {
        panic!("Expected Named type");
    }
}
```

### File: `src/middle/resolver/new_resolver.rs`
**Changes:**
1. Update `InferContext` struct:
```rust
pub struct InferContext {
    /// Variable to type mapping
    variables: HashMap<String, Type>,
    
    /// Function signatures (name -> return type)
    functions: HashMap<String, Type>,
    
    /// Current substitution
    substitution: Substitution,
    
    /// Collected constraints
    constraints: Vec<Constraint>,
    
    /// Generic context for type parameters
    generic_context: GenericContext,
    
    /// Type of the last expression (for debugging)
    last_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub enum Constraint {
    Equality(Type, Type),
    Bound(Type, TraitBound),
}
```

2. Add generic inference methods:
```rust
impl InferContext {
    /// Enter a new generic context
    pub fn enter_generic_scope(&mut self, type_params: Vec<TypeParam>) {
        let new_context = GenericContext {
            type_params,
            parent: Some(Box::new(self.generic_context.clone())),
        };
        self.generic_context = new_context;
    }
    
    /// Exit current generic context
    pub fn exit_generic_scope(&mut self) {
        if let Some(parent) = self.generic_context.parent.take() {
            self.generic_context = *parent;
        }
    }
    
    /// Infer type for generic function call
    pub fn infer_generic_call(
        &mut self,
        name: &str,
        type_args: &[Type],
        value_args: &[AstNode],
    ) -> Result<Type, String> {
        // Look up generic function
        let generic_ty = self.functions.get(name)
            .ok_or_else(|| format!("Unknown generic function: {}", name))?;
        
        // Instantiate with type arguments
        let instantiated_ty = self.substitution.instantiate_generic(
            generic_ty,
            type_args,
            &self.generic_context,
        )?;
        
        // Check if it's a function type
        match &instantiated_ty {
            Type::Function(param_types, ret_ty) => {
                // Check value arguments match parameter types
                if value_args.len() != param_types.len() {
                    return Err(format!(
                        "Wrong number of arguments: expected {}, got {}",
                        param_types.len(),
                        value_args.len()
                    ));
                }
                
                // Type check each argument
                for (i, (arg, param_ty)) in value_args.iter().zip(param_types.iter()).enumerate() {
                    let arg_ty = self.infer(arg)?;
                    self.constrain(Constraint::Equality(arg_ty, param_ty.clone()));
                }
                
                Ok(*ret_ty.clone())
            }
            _ => Err(format!("{} is not a function", name)),
        }
    }
    
    /// Add a constraint
    pub fn constrain(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
    }
    
    /// Solve all constraints
    pub fn solve(&mut self) -> Result<(), Vec<UnifyError>> {
        let mut errors = Vec::new();
        
        for constraint in self.constraints.drain(..) {
            match constraint {
                Constraint::Equality(t1, t2) => {
                    if let Err(e) = self.substitution.unify(&t1, &t2) {
                        errors.push(e);
                    }
                }
                Constraint::Bound(ty, bound) => {
                    if !self.substitution.satisfies_bound(&ty, &bound) {
                        errors.push(UnifyError::Mismatch(
                            ty,
                            Type::Named(format!("T: {:?}", bound), Vec::new()),
                        ));
                    }
                }
            }
        }
        
        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}
```

3. Update `infer` method to handle generic nodes:
```rust
pub fn infer(&mut self, node: &AstNode) -> Result<Type, String> {
    match node {
        // Add case for generic function definitions
        AstNode::FuncDef {
            name,
            generics,  // NEW: generic type parameters
            params,
            ret,
            body,
            ret_expr,
            ..
        } => {
            // Parse generic type parameters if present
            let type_params = if !generics.is_empty() {
                // Parse "T: Clone + Copy" style bounds
                self.parse_generic_params(generics)?
            } else {
                Vec::new()
            };
            
            // Enter generic scope
            self.enter_generic_scope(type_params);
            
            // Parse return type in generic context
            let return_ty = self.parse_type_string(ret)?;
            
            // Register function with generic parameters
            // For now, store as generic function type
            self.functions.insert(name.clone(), return_ty.clone());
            
            // Process parameters and body in generic context
            // ... existing code ...
            
            // Exit generic scope
            self.exit_generic_scope();
            
            Ok(Type::Tuple(vec![]))
        }
        
        // Add case for generic struct definitions
        AstNode::StructDef {
            name,
            generics,  // NEW
            fields,
            ..
        } => {
            // Parse generic parameters
            let type_params = self.parse_generic_params(generics)?;
            
            // Register struct type with generic parameters
            // For now, store as generic type
            let struct_ty = Type::Named(name.clone(), 
                type_params.iter()
                    .map(|p| Type::Variable(TypeVar::fresh()))
                    .collect()
            );
            
            // Store field types with generic context
            // ... existing code ...
            
            Ok(Type::Tuple(vec![]))
        }
        
        // Update Call case to handle generic calls
        AstNode::Call {
            receiver,
            method,
            args,
            generic_args,  // NEW: type arguments like ::<i32>
        } => {
            if let Some(type_args) = generic_args {
                // Generic function call
                let parsed_type_args: Result<Vec<Type>, String> = type_args
                    .iter()
                    .map(|s| self.parse_type_string(s))
                    .collect();
                
                self.infer_generic_call(method, &parsed_type_args?, args)
            } else {
                // Regular function call
                // ... existing code ...
            }
        }
        
        // ... rest of existing match arms ...
    }
}
```

4. Add helper method to parse generic parameters:
```rust
fn parse_generic_params(&self, generics: &[String]) -> Result<Vec<TypeParam>, String> {
    let mut type_params = Vec::new();
    
    for generic in generics {
        // Parse "T: Clone + Copy" or just "T"
        let parts: Vec<&str> = generic.split(':').map(|s| s.trim()).collect();
        
        if parts.is_empty() {
            return Err("Empty generic parameter".to_string());
        }
        
        let name = parts[0].to_string();
        let mut bounds = Vec::new();
        
        if parts.len() > 1 {
            // Parse bounds: "Clone + Copy + Debug"
            let bound_parts: Vec<&str> = parts[1].split('+').map(|s| s.trim()).collect();
            
            for bound_str in bound_parts {
                match bound_str {
                    "Clone" => bounds.push(TraitBound::Clone),
                    "Copy" => bounds.push(TraitBound::Copy),
                    "