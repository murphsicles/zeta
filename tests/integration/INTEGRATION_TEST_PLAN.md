# Generic Type System Integration Test Plan

## Overview
This document outlines the integration testing strategy for Zeta's generic type system, ensuring all components work together correctly.

## Test Architecture

### 1. Parser ↔ Type System Integration
**Objective**: Verify parsed generic constructs are correctly represented in the type system.

**Test Cases**:
1. Parse `struct Point<T> { x: T, y: T }` → Type representation
2. Parse `fn identity<T>(x: T) -> T` → Function type with variables
3. Parse `impl<T> Point<T>` → Generic impl block handling
4. Parse `where T: Display` clauses → Trait bound representation

**Validation**:
- AST nodes contain correct generic parameters
- Type variables are properly created
- Trait bounds are attached to type variables

### 2. Type Checker ↔ Resolver Integration
**Objective**: Ensure type checking works with generic types and inference.

**Test Cases**:
1. Generic function call with type inference
2. Multiple generic parameters with constraints
3. Nested generic type checking
4. Recursive generic type validation

**Validation**:
- Type variables are correctly unified
- Trait bounds are checked during instantiation
- Error messages point to correct locations
- Inference works across function boundaries

### 3. MIR Generation ↔ Generic Types
**Objective**: Verify MIR can represent generic operations.

**Test Cases**:
1. Generic struct instantiation in MIR
2. Generic function monomorphization
3. Trait method dispatch for generic types
4. Lifetime annotations in MIR

**Validation**:
- MIR contains placeholders for type parameters
- Monomorphization creates concrete instances
- Lifetime information preserved in MIR

### 4. Code Generation ↔ Monomorphization
**Objective**: Ensure LLVM codegen works with monomorphized types.

**Test Cases**:
1. Generate code for `Vec<i32>`
2. Generate code for generic function with multiple instantiations
3. Handle generic types in runtime calls
4. Optimize monomorphized code

**Validation**:
- LLVM IR contains correct type information
- Multiple instantiations don't conflict
- Runtime calls work with generic types
- Optimizations apply to monomorphized code

## End-to-End Test Scenarios

### Scenario 1: Generic Collections
```zeta
struct Vec<T> { data: *mut T, len: i64, cap: i64 }

impl<T> Vec<T> {
    fn new() -> Vec<T> { ... }
    fn push(&mut self, item: T) { ... }
    fn get(&self, index: i64) -> &T { ... }
}

fn main() -> i64 {
    let mut v = Vec::<i32>::new();
    v.push(1);
    v.push(2);
    *v.get(0) + *v.get(1)
}
```

**Test Points**:
- Generic struct parsing and type checking
- Generic impl block handling
- Type parameter instantiation (`Vec<i32>`)
- Method calls on generic type
- Reference types with generics

### Scenario 2: Generic Algorithms
```zeta
fn find<T: Eq>(items: &[T], target: &T) -> Option<usize> {
    for i in 0..items.len() {
        if items[i] == *target {
            return Some(i);
        }
    }
    None
}

fn main() -> i64 {
    let arr = [1, 2, 3, 4, 5];
    match find(&arr, &3) {
        Some(idx) => idx as i64,
        None => -1,
    }
}
```

**Test Points**:
- Generic function with trait bound
- Slice types with generic elements
- Trait bound checking (`T: Eq`)
- Pattern matching with generic `Option`
- Type inference across function boundaries

### Scenario 3: Complex Generic Patterns
```zeta
struct Result<T, E> { ... }

fn map<T, U, E>(result: Result<T, E>, f: fn(T) -> U) -> Result<U, E> {
    match result {
        Ok(value) => Ok(f(value)),
        Err(e) => Err(e),
    }
}

fn chain<T, U, E>(
    result: Result<T, E>,
    f: fn(T) -> Result<U, E>
) -> Result<U, E> {
    match result {
        Ok(value) => f(value),
        Err(e) => Err(e),
    }
}
```

**Test Points**:
- Multiple type parameters
- Higher-order functions with generics
- Nested generic type matching
- Function pointer types with generics
- Complex control flow with generics

## Performance Integration Tests

### Test 1: Compilation Time Scaling
**Objective**: Measure how compilation time scales with generic usage.

**Method**:
1. Compile program with N generic instantiations
2. Measure compilation time vs N
3. Compare with theoretical O(N) expectation

**Acceptance Criteria**:
- Linear or sub-linear scaling
- No exponential blowup
- Memory usage bounded

### Test 2: Binary Size Impact
**Objective**: Measure binary size increase from monomorphization.

**Method**:
1. Compile with/without generics
2. Compare binary sizes
3. Analyze duplication vs code sharing

**Acceptance Criteria**:
- Reasonable size increase per instantiation
- Code sharing where possible
- No redundant monomorphization

### Test 3: Runtime Performance
**Objective**: Ensure generic code has same performance as non-generic.

**Method**:
1. Create generic and specialized versions
2. Benchmark execution time
3. Compare assembly output

**Acceptance Criteria**:
- Same performance within 5%
- No unnecessary indirection
- Optimizations apply equally

## Error Handling Integration

### Test 1: Type Error Messages
**Objective**: Ensure helpful error messages for generic type errors.

**Test Cases**:
1. Missing type argument: `Vec<>`
2. Wrong number: `Result<i32>`
3. Trait bound violation: `Vec<T> where T: !Clone`
4. Lifetime error: `&'a T` where `'a` too short

**Validation**:
- Error points to correct location
- Suggests possible fixes
- Shows type context

### Test 2: Compilation Error Recovery
**Objective**: Test that one generic error doesn't prevent other checks.

**Method**:
1. Introduce multiple generic errors
2. Verify all are reported
3. Check compilation continues where possible

**Acceptance Criteria**:
- Multiple errors reported
- Non-erroneous code still checked
- Clear separation between errors

## Cross-Component Test Matrix

| Component | Parser | Type Checker | MIR | Codegen | Runtime |
|-----------|--------|--------------|-----|---------|---------|
| Basic Generics | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Trait Bounds | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Lifetimes | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Where Clauses | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Nested Generics | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |

**Key**:
- ✅ = Implemented and tested
- ⚠️ = Partially implemented
- ❌ = Not implemented

## Test Execution Strategy

### Phase 1: Component Tests (Current)
- Unit tests for each component
- Mock interfaces between components
- Focus on correctness in isolation

### Phase 2: Integration Tests (Next)
- End-to-end compilation tests
- Real-world generic patterns
- Performance measurements

### Phase 3: System Tests (Future)
- Large codebase compilation
- Stress tests with many generics
- Compatibility with existing projects

## Success Metrics

### Technical Metrics
1. **Correctness**: 100% of generic test cases pass
2. **Performance**: <10% compilation overhead for generics
3. **Compatibility**: 100% of v0.5.0 code still works
4. **Error Quality**: Helpful messages for 95% of generic errors

### Process Metrics
1. **Test Coverage**: >90% of generic code paths
2. **Regression Prevention**: All tests run before commits
3. **Documentation**: All features have usage examples
4. **Debugging**: Easy to trace generic type issues

## Risk Mitigation

### High Risk Areas
1. **Trait bound checking** - Complex inference required
2. **Lifetime integration** - Affects borrow checker
3. **Monomorphization** - Can cause code bloat

### Mitigation Strategies
1. Incremental implementation with frequent testing
2. Fallback to non-generic code paths
3. Size limits on monomorphization
4. Early user feedback on complex cases

## Timeline

### Week 1-2: Foundation
- Complete parser integration tests
- Basic type checker integration
- Simple end-to-end tests

### Week 3-4: Features
- Trait bound integration tests
- Lifetime integration tests
- Performance baseline tests

### Week 5-6: Polish
- Error message integration tests
- Complex scenario tests
- Documentation and examples

### Ongoing: Maintenance
- Regression test suite
- Performance monitoring
- User feedback integration