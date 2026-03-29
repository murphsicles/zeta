# Type System Code Review
## Applying Code-Review Skill to Zeta's Semantic Foundation

**Reviewer:** SEM (Zeta Code Implementer)  
**Date:** 2026-03-26 05:19 GMT  
**Context:** Semantic foundation work for type checking unification

---

## Executive Summary

The current type system has a basic foundation but lacks critical features for a modern language compiler. The implementation is simple but not scalable for complex type inference, generics, or trait resolution.

---

## Security Review

### [MINOR] Type Representation as String
- **Issue**: `type Type = String` representation is vulnerable to injection and makes type operations error-prone
- **Risk**: No validation of type strings, could allow malicious type names
- **Fix**: Create proper `Type` enum with validation

### [MINOR] No Type Safety in CTFE
- **Issue**: Constant folding (`ctfe_eval`) assumes `i64` for all arithmetic
- **Risk**: Type mismatches not caught at compile time
- **Fix**: Add type checking to constant folding

---

## Performance Review

### [MAJOR] No Type Inference Cache
- **Issue**: `infer_type` recurses without memoization
- **Impact**: Exponential time for nested expressions
- **Fix**: Add caching of inferred types

### [MINOR] String-based Type Comparison
- **Issue**: `lty != rty` uses string comparison
- **Impact**: O(n) comparison vs O(1) for enum tags
- **Fix**: Use proper type equality method

---

## Correctness Review

### [CRITICAL] No Type Variables or Unification
- **Issue**: Cannot infer types for generic functions or expressions
- **Example**: `fn id<T>(x: T) -> T` impossible to implement
- **Fix**: Implement Hindley-Milner type inference with unification

### [MAJOR] Limited Type Inference
- **Issue**: Only infers `i64` for literals, `str` for strings
- **Example**: `let x = 3.14` would infer `i64` (wrong for float)
- **Fix**: Extend inference to handle all primitive types

### [MAJOR] No Subtyping or Coercion
- **Issue**: `i32` to `i64` conversion not supported
- **Fix**: Add implicit conversion rules and coercion sites

### [MINOR] Error Messages Unhelpful
- **Issue**: Returns `false` for type errors without context
- **Fix**: Add error reporting with line numbers and suggestions

---

## Maintainability Review

### [MAJOR] Monolithic Type Checking
- **Issue**: `typecheck.rs` mixes borrow checking, type inference, CTFE
- **Fix**: Separate concerns into modules:
  - `type_inference.rs`
  - `borrow_checking.rs` 
  - `constant_folding.rs`
  - `unification.rs`

### [MINOR] Stringly-Typed API
- **Issue**: Functions return `String` for types, `bool` for success
- **Fix**: Use `Result<Type, TypeError>` with proper error types

### [MINOR] No Visitor Pattern
- **Issue**: Manual recursion over AST nodes
- **Fix**: Implement AST visitor trait for cleaner traversal

---

## Testing Review

### [MAJOR] No Type System Tests
- **Issue**: No tests for type inference, unification, or error cases
- **Fix**: Add comprehensive test suite for type system

### [MINOR] No Property Tests
- **Issue**: No generative testing for type inference properties
- **Fix**: Add QuickCheck-style property tests

---

## Architectural Issues

### 1. **No Type Representation Hierarchy**
```rust
// Current
type Type = String;

// Should be
enum Type {
    Primitive(PrimType),
    Generic(String, Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Variable(TypeVar),
}
```

### 2. **No Unification Engine**
- Missing: Type variable substitution
- Missing: Occurs check for recursive types
- Missing: Most general unifier algorithm

### 3. **No Constraint Collection**
- Type checking should collect constraints, then solve
- Current: Immediate checking fails for interdependent types

### 4. **No Type Environment**
- Missing: Mapping from names to types with scoping
- Missing: Generic type parameter tracking

---

## Priority Recommendations

### Phase 1: Foundation (Critical)
1. **Create proper Type enum** - Replace `type Type = String`
2. **Implement TypeVar and unification** - Basic Hindley-Milner
3. **Add type environment** - Scoped type mappings
4. **Separate modules** - Type inference, unification, checking

### Phase 2: Features (Major)
1. **Generic type inference** - For functions and structs
2. **Trait resolution** - Connect to concept system
3. **Better error messages** - With suggestions
4. **Constant type checking** - In CTFE

### Phase 3: Polish (Minor)
1. **Type inference caching** - Performance
2. **Visitor pattern** - Cleaner AST traversal
3. **Comprehensive tests** - Property-based testing

---

## Example of Current Limitations

```zeta
// Current system fails on:
fn identity<T>(x: T) -> T { x }  // No type variables
let x = identity(5);             // Can't infer T = i64
let y = 3.14;                    // Infers i64, not f64
fn add(a, b) { a + b }           // Can't infer parameter types
```

---

## Self-Improving Learnings

From this review, document for future work:
1. **Type systems need algebraic representation** - not strings
2. **Unification before checking** - collect constraints then solve  
3. **Separate inference from checking** - cleaner architecture
4. **Error messages are part of UX** - not just true/false

---

**Review Complete** - Semantic foundation needs significant work to support bootstrap acceleration.