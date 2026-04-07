# Generic Type System Design for Zeta v0.3.20 → v0.5.0 Compatibility

## Overview
This document outlines the design for adding full generic type support to Zeta's type system, enabling 100% compatibility with Zeta v0.5.0's generic type features.

## Current State Analysis

### ✅ Already Implemented
1. **Type Representation**: `Type::Named(String, Vec<Type>)` supports generic types
2. **Type Unification**: Basic unification for `Named` types with type arguments
3. **Type Parsing**: Can parse `Vec<i32>`, `Result<T,E>`, `&mut T`, etc.
4. **Reference Types**: Full support for `&T` and `&mut T`
5. **Type Variables**: `Type::Variable(TypeVar)` for inference

### ❌ Missing Features
1. **Generic Type Parameter Scoping**: No tracking of type parameter scopes
2. **Trait Bounds**: No support for `T: Clone`, `T: Copy` constraints
3. **Generic Function Inference**: Can't infer `fn identity<T>(x: T) -> T`
4. **Constraint Solving**: No solving of trait bound constraints
5. **Generic Instantiation**: Limited `instantiate_generic` method

## Design Goals

### Primary Goals
1. Support `Vec<T>`, `Result<T,E>`, `Option<T>` with full type checking
2. Support generic functions: `fn foo<T>(x: T) -> T`
3. Support trait bounds: `T: Clone`, `T: Copy`
4. Support reference types: `&T`, `&mut T`
5. Achieve 100% v0.5.0 compatibility

### Secondary Goals
1. Efficient type inference with constraint solving
2. Proper error messages for type mismatches
3. Integration with existing borrow checker
4. Support for nested generics: `Vec<Vec<i32>>`

## Architecture

### 1. Type System Extensions

#### 1.1 Type Parameter Context
```rust
struct TypeParam {
    name: String,
    bounds: Vec<TraitBound>,  // e.g., T: Clone + Copy
}

struct GenericContext {
    type_params: Vec<TypeParam>,
    parent: Option<Box<GenericContext>>,  // For nested generics
}
```

#### 1.2 Trait Bound Representation
```rust
enum TraitBound {
    Clone,
    Copy,
    Debug,
    // Other traits as needed
}

struct Constraint {
    ty: Type,
    bound: TraitBound,
}
```

### 2. Type Inference Algorithm

#### 2.1 Constraint Collection Phase
- Walk AST, collect type constraints
- For generic calls: `Vec::<i32>::new()` → `Vec<T>` instantiated with `T=i32`
- For generic functions: `identity(42)` → infer `T=i32`

#### 2.2 Constraint Solving Phase
1. **Unification**: Solve type equality constraints
2. **Bound Checking**: Verify trait bounds are satisfied
3. **Instantiation**: Create concrete types from generic templates

#### 2.3 Algorithm Pseudocode
```
function infer(node, context):
    match node:
        case GenericCall(name, type_args, value_args):
            generic_ty = lookup_generic(name)
            instantiated = instantiate(generic_ty, type_args)
            check_args(value_args, instantiated)
            
        case GenericFunction(name, type_params, body):
            new_context = context.with_type_params(type_params)
            infer(body, new_context)
            
        case TraitBoundCheck(ty, bound):
            if not satisfies_bound(ty, bound):
                error "Type {ty} doesn't satisfy {bound}"
```

### 3. Generic Instantiation

#### 3.1 Type Substitution
```rust
struct TypeSubstitution {
    mapping: HashMap<TypeVar, Type>,
    // Also handles lifetime substitution
}

impl TypeSubstitution {
    fn apply(&self, ty: &Type) -> Type {
        // Recursively substitute type variables
    }
    
    fn instantiate_generic(
        &self, 
        generic_ty: &Type, 
        type_args: &[Type]
    ) -> Result<Type, String> {
        // Map generic parameters to concrete types
    }
}
```

#### 3.2 Instantiation Rules
1. `Vec<T>` + `[i32]` → `Vec<i32>`
2. `Result<T,E>` + `[i32, String]` → `Result<i32, String>`
3. `&'a T` + `[i32]` → `&'a i32` (lifetime preserved)

### 4. Trait Bound Checking

#### 4.1 Bound Satisfaction Table
| Type | Clone | Copy | Debug | Default |
|------|-------|------|-------|---------|
| `i32` | ✅ | ✅ | ✅ | ✅ |
| `bool` | ✅ | ✅ | ✅ | ✅ |
| `String` | ✅ | ❌ | ✅ | ✅ |
| `&T` | ✅ | ✅ | ✅ | ❌ |
| `Vec<T>` | if `T: Clone` | ❌ | if `T: Debug` | ✅ |

#### 4.2 Bound Propagation
- `Vec<T>: Clone` requires `T: Clone`
- `&T: Copy` always true (shared references are Copy)
- `Result<T,E>: Debug` requires `T: Debug` and `E: Debug`

## Implementation Plan

### Phase 1: Foundation (2 hours)
1. **Extend Type Representation** (30 min)
   - Add `GenericContext` to type checking
   - Add `TraitBound` enum
   
2. **Enhance Type Unification** (1 hour)
   - Extend `Substitution::unify` for generic types
   - Add occurs check for type variables
   
3. **Basic Generic Instantiation** (30 min)
   - Improve `instantiate_generic` method
   - Add tests for `Vec<i32>`, `Option<bool>`

### Phase 2: Inference (2 hours)
1. **Constraint Collection** (1 hour)
   - Modify `InferContext` to collect trait bounds
   - Add generic parameter scoping
   
2. **Constraint Solving** (1 hour)
   - Implement bound checking algorithm
   - Add error reporting for bound violations

### Phase 3: Integration (1.5 hours)
1. **Parser Integration** (30 min)
   - Coordinate with LEX on parser capabilities
   - Ensure AST includes generic info
   
2. **Code Generation** (30 min)
   - Coordinate with GEN on monomorphization
   - Ensure MIR can handle generic types
   
3. **Testing** (30 min)
   - Add comprehensive test suite
   - Test edge cases and error messages

### Phase 4: Polish (30 min)
1. **Error Messages** (15 min)
   - User-friendly type errors
   - Helpful suggestions
   
2. **Performance** (15 min)
   - Optimize constraint solving
   - Add caching where possible

## File-by-File Changes

### `src/middle/types/mod.rs`
```rust
// Add:
struct TypeParam {
    name: String,
    bounds: Vec<TraitBound>,
}

enum TraitBound {
    Clone,
    Copy,
    Debug,
    Default,
    // ...
}

struct GenericContext {
    type_params: Vec<TypeParam>,
    parent: Option<Box<GenericContext>>,
}

// Extend Substitution:
impl Substitution {
    fn instantiate_generic(
        &self,
        generic_ty: &Type,
        type_args: &[Type],
        context: &GenericContext,
    ) -> Result<Type, String> {
        // Improved implementation
    }
    
    fn check_bounds(&self, ty: &Type, bounds: &[TraitBound]) -> Result<(), String> {
        // Check trait bounds
    }
}
```

### `src/middle/resolver/new_resolver.rs`
```rust
// Extend InferContext:
struct InferContext {
    // ... existing fields ...
    generic_context: GenericContext,
    constraints: Vec<Constraint>,  // Changed from (Type, Type)
}

enum Constraint {
    Equality(Type, Type),
    Bound(Type, TraitBound),
}

impl InferContext {
    fn infer_generic_call(&mut self, name: &str, type_args: &[Type]) -> Result<Type, String> {
        // Handle generic function/type calls
    }
    
    fn solve_constraints(&mut self) -> Result<(), Vec<UnifyError>> {
        // Solve both equality and bound constraints
    }
}
```

### `src/middle/resolver/typecheck_new.rs`
```rust
// Extend string_to_type to parse trait bounds:
fn parse_type_with_bounds(&self, s: &str) -> Result<(Type, Vec<TraitBound>), String> {
    // Parse "T: Clone + Copy" style bounds
}
```

## Test Cases

### Basic Generic Types
```rust
// Should pass
let v: Vec<i32> = Vec::new();
let o: Option<bool> = Some(true);
let r: Result<i32, String> = Ok(42);
```

### Generic Functions
```rust
fn identity<T>(x: T) -> T { x }
let x: i32 = identity(42);
let y: &str = identity("hello");
```

### Trait Bounds
```rust
fn clone_and_return<T: Clone>(x: T) -> T { x.clone() }
let s = String::from("hello");
let s2 = clone_and_return(s);  // String: Clone ✅

fn copy_value<T: Copy>(x: T) -> T { x }
let n = copy_value(42);  // i32: Copy ✅
// let s = copy_value(String::from("hello"));  // ERROR: String not Copy
```

### Reference Types
```rust
fn process<T>(x: &mut T) -> &T { x }
let mut n = 42;
let r = process(&mut n);
```

### Nested Generics
```rust
let v: Vec<Vec<i32>> = vec![vec![1, 2], vec![3, 4]];
let m: HashMap<String, Vec<i32>> = HashMap::new();
```

## Error Cases to Handle

### Type Mismatch
```rust
let v: Vec<i32> = Vec::<bool>::new();  // ERROR: expected Vec<i32>, got Vec<bool>
```

### Missing Trait Bound
```rust
fn needs_clone<T>(x: T) -> T { x.clone() }  // ERROR: T doesn't implement Clone
```

### Arity Mismatch
```rust
let r: Result<i32> = Ok(42);  // ERROR: Result expects 2 type parameters
```

### Occurs Check
```rust
struct Infinite<T> { next: Box<Infinite<T>> }  // Should be okay
fn bad<T>(x: T) -> T { bad(x) }  // Type inference should handle recursion
```

## Integration Points

### With Parser (LEX)
- Ensure AST nodes include generic type parameters
- Parse trait bounds in function signatures
- Parse generic struct/enum definitions

### With Code Generator (GEN)
- Monomorphization of generic functions
- Specialization cache for generic types
- Code generation for trait method calls

### With Borrow Checker
- Generic types in borrow checking
- Lifetime parameters in generic types
- Reference types with generic parameters

## Performance Considerations

1. **Constraint Solving Complexity**: O(n²) worst case, but typical code is linear
2. **Caching**: Cache solved constraints for generic functions
3. **Monomorphization**: Generate specialized code only when needed
4. **Lazy Solving**: Solve constraints only when types are actually used

## Success Metrics

1. **Compatibility**: 100% of v0.5.0 generic type features supported
2. **Performance**: Type checking within 2x of non-generic code
3. **Error Messages**: Clear, actionable error messages
4. **Test Coverage**: 90%+ line coverage for generic type code

## Timeline
- **Design Complete**: 19:30 GMT
- **Implementation Start**: 19:35 GMT  
- **Phase 1 Complete**: 20:00 GMT
- **Phase 2 Complete**: 21:00 GMT
- **Phase 3 Complete**: 21:30 GMT
- **Testing Complete**: 21:45 GMT
- **Final Review**: 21:55 GMT

## Risks and Mitigations

### Risk 1: Infinite Recursion in Type Inference
**Mitigation**: Implement occurs check in unification

### Risk 2: Performance Degradation
**Mitigation**: Add caching and lazy solving

### Risk 3: Complex Error Messages
**Mitigation**: Invest in error message formatting early

### Risk 4: Integration Issues
**Mitigation**: Coordinate hourly with LEX and GEN teams

## Conclusion

This design provides a complete roadmap for implementing generic types in Zeta v0.3.20. The phased approach ensures incremental progress with regular integration points. The architecture builds on existing type system foundations while adding the necessary components for full generic type support.