# Type System Design for Zeta v0.3.9+
## Applying Rust Skill Patterns to Semantic Foundation

**Designer:** SEM (Zeta Code Implementer)  
**Date:** 2026-03-26 05:19 GMT  
**Goal:** Type checking unification and semantic foundation

---

## Core Principles from Rust Skill

1. **Ownership in Types** - Types track ownership/borrowing at compile time
2. **Algebraic Data Types** - Sum and product types for expressive type system
3. **Trait-based Polymorphism** - Concepts as type classes
4. **Type Inference** - Local inference with global type annotations

---

## Type Representation (Phase 1)

```rust
// src/middle/types/mod.rs

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitive types
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Bool,
    Char,
    Str,
    
    // Compound types
    Array(Box<Type>, usize),  // [T; N]
    Slice(Box<Type>),         // [T]
    Tuple(Vec<Type>),         // (T1, T2, ...)
    Ptr(Box<Type>),           // *T
    Ref(Box<Type>, Mutability), // &T, &mut T
    
    // Named types
    Named(String, Vec<Type>), // Struct/enum with generics
    
    // Function types
    Function(Vec<Type>, Box<Type>), // (T1, T2, ...) -> R
    
    // Type variables (for inference)
    Variable(TypeVar),
    
    // Error type
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar(u32);

impl TypeVar {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        TypeVar(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}
```

---

## Type Environment (Context)

```rust
pub struct TypeEnv {
    // Variable to type mapping
    variables: HashMap<String, Type>,
    
    // Type variable substitutions
    substitution: Substitution,
    
    // Current scope level
    scope_level: u32,
    
    // Parent environment (for nested scopes)
    parent: Option<Rc<TypeEnv>>,
}

pub struct Substitution {
    mapping: HashMap<TypeVar, Type>,
}

impl Substitution {
    pub fn new() -> Self {
        Substitution { mapping: HashMap::new() }
    }
    
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable(var) => self.mapping.get(var).cloned()
                .unwrap_or(Type::Variable(var.clone())),
            Type::Array(inner, size) => 
                Type::Array(Box::new(self.apply(inner)), *size),
            Type::Function(params, ret) =>
                Type::Function(
                    params.iter().map(|p| self.apply(p)).collect(),
                    Box::new(self.apply(ret))
                ),
            _ => ty.clone(),
        }
    }
    
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), UnifyError> {
        // Hindley-Milner unification algorithm
        // ...
    }
}
```

---

## Unification Algorithm (Hindley-Milner)

```rust
pub enum UnifyError {
    Mismatch(Type, Type),
    OccursCheck(TypeVar, Type),
    ArityMismatch(usize, usize),
}

impl Substitution {
    fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), UnifyError> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);
        
        match (&t1, &t2) {
            // Type variable cases
            (Type::Variable(a), Type::Variable(b)) if a == b => Ok(()),
            (Type::Variable(a), _) => {
                if occurs_check(a, &t2) {
                    Err(UnifyError::OccursCheck(a.clone(), t2))
                } else {
                    self.mapping.insert(a.clone(), t2);
                    Ok(())
                }
            }
            (_, Type::Variable(b)) => self.unify(&t2, &t1),
            
            // Primitive types
            (Type::I64, Type::I64) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            // ... other primitives
            
            // Function types
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(UnifyError::ArityMismatch(
                        params1.len(), 
                        params2.len()
                    ));
                }
                
                for (p1, p2) in params1.iter().zip(params2) {
                    self.unify(p1, p2)?;
                }
                self.unify(ret1, ret2)
            }
            
            // Mismatch
            _ => Err(UnifyError::Mismatch(t1, t2)),
        }
    }
    
    fn occurs_check(&self, var: &TypeVar, ty: &Type) -> bool {
        match ty {
            Type::Variable(v) => v == var || 
                self.mapping.get(v).map_or(false, |t| self.occurs_check(var, t)),
            Type::Array(inner, _) => self.occurs_check(var, inner),
            Type::Function(params, ret) =>
                params.iter().any(|p| self.occurs_check(var, p)) ||
                self.occurs_check(var, ret),
            _ => false,
        }
    }
}
```

---

## Type Inference Engine

```rust
pub struct Infer {
    env: TypeEnv,
    constraints: Vec<(Type, Type)>,
}

impl Infer {
    pub fn new() -> Self {
        Infer {
            env: TypeEnv::new(),
            constraints: Vec::new(),
        }
    }
    
    pub fn infer_expr(&mut self, expr: &AstNode) -> Result<Type, TypeError> {
        match expr {
            AstNode::Lit(n) => {
                // Infer integer literal type
                if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                    Ok(Type::I32)
                } else {
                    Ok(Type::I64)
                }
            }
            
            AstNode::Var(name) => {
                self.env.lookup(name)
                    .ok_or_else(|| TypeError::UnboundVariable(name.clone()))
            }
            
            AstNode::BinaryOp { op, left, right } => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                
                // Add constraint: left_ty == right_ty
                self.constraints.push((left_ty.clone(), right_ty.clone()));
                
                // Determine result type based on operator
                match op.as_str() {
                    "+" | "-" | "*" | "/" => {
                        // Numeric operations
                        Ok(left_ty) // After unification, types will match
                    }
                    "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        Ok(Type::Bool)
                    }
                    "&&" | "||" => {
                        // Both must be bool
                        self.constraints.push((left_ty.clone(), Type::Bool));
                        self.constraints.push((right_ty.clone(), Type::Bool));
                        Ok(Type::Bool)
                    }
                    _ => Err(TypeError::UnknownOperator(op.clone())),
                }
            }
            
            AstNode::Call { receiver, method, args, .. } => {
                // Method call inference
                let receiver_ty = if let Some(r) = receiver {
                    self.infer_expr(r)?
                } else {
                    // Static method
                    Type::Named("Self".to_string(), Vec::new())
                };
                
                // Look up method signature
                let (param_tys, ret_ty) = self.lookup_method(&receiver_ty, method)?;
                
                // Check arguments
                if args.len() != param_tys.len() {
                    return Err(TypeError::ArityMismatch(
                        method.clone(),
                        param_tys.len(),
                        args.len()
                    ));
                }
                
                for (arg, param_ty) in args.iter().zip(param_tys) {
                    let arg_ty = self.infer_expr(arg)?;
                    self.constraints.push((arg_ty, param_ty));
                }
                
                Ok(ret_ty)
            }
            
            _ => {
                // Default to error for unimplemented nodes
                Err(TypeError::Unimplemented(format!("{:?}", expr)))
            }
        }
    }
    
    pub fn solve_constraints(mut self) -> Result<Substitution, Vec<TypeError>> {
        let mut subst = Substitution::new();
        let mut errors = Vec::new();
        
        for (t1, t2) in self.constraints {
            if let Err(e) = subst.unify(&t1, &t2) {
                errors.push(TypeError::from(e));
            }
        }
        
        if errors.is_empty() {
            Ok(subst)
        } else {
            Err(errors)
        }
    }
}
```

---

## Integration with Existing System

### Step 1: Replace String Types
```rust
// In resolver.rs
// OLD: pub type Type = String;
// NEW:
pub use crate::middle::types::Type;
```

### Step 2: Update Type Checking
```rust
impl Resolver {
    pub fn typecheck(&mut self, asts: &[AstNode]) -> Result<Substitution, Vec<TypeError>> {
        let mut infer = Infer::new();
        
        for ast in asts {
            infer.infer_expr(ast)?;
        }
        
        infer.solve_constraints()
    }
}
```

### Step 3: Update Borrow Checker
```rust
impl BorrowChecker {
    pub fn declare(&mut self, var: String, state: BorrowState, ty: Type) {
        // Now uses proper Type enum
        self.types.insert(var, ty);
    }
}
```

---

## Migration Strategy

### Phase 1: Foundation (v0.3.9)
1. Create `src/middle/types/` module with Type enum
2. Implement basic unification
3. Update type checking to use new system
4. Keep backward compatibility shim

### Phase 2: Inference (v0.4.0)
1. Implement full Hindley-Milner
2. Add generic type inference
3. Integrate with concept system

### Phase 3: Optimization (v0.4.1)
1. Add type inference caching
2. Implement visitor pattern
3. Add comprehensive error messages

---

## Rust Skill Patterns Applied

### 1. **Algebraic Data Types**
- Using `enum Type` instead of strings
- Pattern matching for type operations
- Recursive types with `Box`

### 2. **Ownership Patterns**
- `TypeEnv` with parent references using `Rc`
- `Substitution` with `HashMap` for mutable updates
- `Box` for recursive type definitions

### 3. **Error Handling**
- `Result<Type, TypeError>` instead of `bool`
- Comprehensive error types
- Error propagation with `?`

### 4. **Type Safety**
- No stringly-typed operations
- Compile-time checking of type operations
- Proper type variable management

---

## Testing Strategy

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_unify_primitives() {
        let mut subst = Substitution::new();
        assert!(subst.unify(&Type::I64, &Type::I64).is_ok());
        assert!(subst.unify(&Type::I64, &Type::Bool).is_err());
    }
    
    #[test]
    fn test_unify_variables() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());
        let b = Type::Variable(TypeVar::fresh());
        
        assert!(subst.unify(&a, &Type::I64).is_ok());
        assert!(subst.unify(&b, &a).is_ok());
        assert_eq!(subst.apply(&b), Type::I64);
    }
    
    #[test]
    fn test_occurs_check() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());
        let list = Type::Named("List".to_string(), vec![a.clone()]);
        
        // a = List<a> should fail occurs check
        assert!(subst.unify(&a, &list).is_err());
    }
}
```

---

## Self-Improving Documentation

**Lessons for Zeta Type System:**
1. **Type representation matters** - Strings don't scale, enums do
2. **Unification before checking** - Collect constraints, then solve
3. **Type variables need fresh generation** - Global counter with atomic
4. **Occurs check prevents infinite types** - Critical for soundness
5. **Error messages are UX** - Don't just return true/false

**Rust Patterns Applied:**
- Algebraic data types for type representation
- Result types for error handling  
- Atomic counters for fresh variable generation
- Visitor pattern for AST traversal (planned)

---

**Design Complete** - Ready for implementation in v0.3.9 semantic foundation work.