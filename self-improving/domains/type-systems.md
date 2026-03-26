# Type Systems Domain Knowledge
## Learned from Zeta Semantic Foundation Work

**Date:** 2026-03-26  
**Context:** Applying Rust skill to type checking unification

---

## Core Principles

### 1. Type Representation
- **Never use strings for types** - Use algebraic data types (enums)
- **Recursive types need boxing** - `Box<Type>` for arrays, functions, etc.
- **Type variables need fresh generation** - Atomic counter with `TypeVar(u32)`

### 2. Unification Algorithm (Hindley-Milner)
- **Collect constraints first, solve later** - Don't check immediately
- **Occurs check is critical** - Prevents infinite recursive types
- **Most general unifier** - Find substitution that makes types equal
- **Type variables unify with anything** (except themselves in occurs check)

### 3. Type Environment
- **Scoped mappings** - Variables to types with parent environments
- **Substitution application** - Apply substitution to types recursively
- **Fresh variable generation** - Global counter for inference

### 4. Error Handling
- **Never return bool for type errors** - Use `Result<Type, TypeError>`
- **Rich error types** - Mismatch, occurs check, arity mismatch, unbound variable
- **Error messages with context** - Line numbers, suggestions

---

## Rust Patterns for Type Systems

### Algebraic Data Types
```rust
enum Type {
    Primitive(PrimType),
    Variable(TypeVar),
    Function(Vec<Type>, Box<Type>),
    // ...
}
```

### Visitor Pattern for AST
- Traverse AST once, collect constraints
- Separate traversal from type operations
- Cleaner than manual recursion

### Result Types
- `Result<Type, TypeError>` instead of `Option<Type>`
- Propagate errors with `?`
- Collect multiple errors when possible

### Atomic Counters
```rust
impl TypeVar {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        TypeVar(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}
```

---

## Common Pitfalls

### 1. Stringly-Typed Systems
- **Problem**: `type Type = String`
- **Solution**: Proper enum with validation

### 2. Immediate Checking
- **Problem**: Check types as you traverse
- **Solution**: Collect constraints, solve later

### 3. No Occurs Check
- **Problem**: Infinite types like `a = List<a>`
- **Solution**: Check if variable occurs in type before unifying

### 4. Poor Error Messages
- **Problem**: `false` or `None` for errors
- **Solution**: Rich error types with context

---

## Zeta-Specific Learnings

### Current System Limitations
1. **No type variables** - Can't infer generic functions
2. **No unification** - Simple equality checking only
3. **String types** - Error-prone representation
4. **Mixed concerns** - Type checking, borrow checking, CTFE in one file

### Required Improvements
1. **Proper type representation** - Replace `String` with `enum Type`
2. **Unification engine** - Hindley-Milner algorithm
3. **Type inference** - With constraint collection
4. **Modular architecture** - Separate type inference, unification, checking

---

## Implementation Order

### Phase 1: Foundation
1. Create `Type` enum with variants
2. Implement `TypeVar` with fresh generation
3. Create `Substitution` with unification
4. Basic occurs check

### Phase 2: Inference
1. `TypeEnv` for variable mapping
2. Constraint collection
3. Integrate with AST traversal
4. Error reporting

### Phase 3: Integration
1. Replace existing type checking
2. Update borrow checker
3. Add tests
4. Error message improvements

---

## Testing Strategy

### Unit Tests
- Unification of primitives
- Type variable substitution
- Occurs check
- Constraint solving

### Integration Tests
- Simple expression inference
- Function type inference
- Error cases
- Backward compatibility

### Property Tests
- Unification is symmetric
- Substitution is idempotent
- Type inference is deterministic

---

## References

### Rust Skill Patterns
- Algebraic data types for domain modeling
- Result types for error handling
- Visitor pattern for AST traversal
- Atomic operations for fresh generation

### Code Review Insights
- Security: Type string validation
- Performance: Inference caching
- Maintainability: Separate concerns
- Correctness: Comprehensive testing

---

**Status:** Applied to Zeta v0.3.9 semantic foundation design
**Next:** Implement Phase 1 - Type representation and unification