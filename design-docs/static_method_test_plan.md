# Static Method Test Plan
## For VER (Verification Team)

**Integration Architecture Reference:** `static_method_integration_architecture.md`

---

## Test Categories

### 1. Parser Tests (LEX)
```rust
// Test cases for parser
test_cases = [
    // Basic static method calls
    ("Point::new(10, 20)", "should parse static method call"),
    ("Vec::<i32>::new()", "should parse generic static method"),
    ("Type::method::<T>()", "should parse with type arguments"),
    
    // Edge cases
    ("A::B::C::method()", "should parse nested path"),
    ("mod::Type::method()", "should parse with module path"),
    
    // Mixed with instance methods
    ("obj.method().Type::static()", "should chain static after instance"),
    ("Type::static().method()", "should chain instance after static"),
]
```

### 2. Type Checker Tests (SEM)
```rust
// Type resolution tests
test_cases = [
    // Static method resolution
    ("Point::new(10, 20) -> Point", "should resolve return type"),
    ("Vec::<i32>::new() -> Vec<i32>", "should resolve generic type"),
    
    // Error cases
    ("NonExistent::method()", "should error: static method not found"),
    ("Point::non_existent()", "should error: method not in type"),
    ("Point::new(10)", "should error: wrong number of arguments"),
    
    // Overload resolution
    ("Type::method(1)", "should resolve i32 overload"),
    ("Type::method(1.0)", "should resolve f64 overload"),
]
```

### 3. MIR Generation Tests
```rust
// MIR output verification
test_cases = [
    ("Point::new(10, 20)", "should generate StaticCall MIR"),
    ("obj.method()", "should generate regular Call MIR"),
    
    // Complex cases
    ("Type::method(a, b).other()", "should chain MIR statements"),
    ("if cond { Type::a() } else { Type::b() }", "should generate conditional MIR"),
]
```

### 4. Code Generation Tests (GEN)
```rust
// LLVM IR verification
test_cases = [
    ("Point::new(10, 20)", "should generate correct LLVM call"),
    ("Vec::new()", "should generate generic instantiation"),
    
    // Performance tests
    ("benchmark static vs instance calls", "should have similar performance"),
]
```

### 5. Integration Tests (Full Pipeline)
```rust
// End-to-end tests
test_cases = [
    // Simple programs
    ("
        struct Point { x: i32, y: i32 }
        impl Point {
            fn new(x: i32, y: i32) -> Point { Point { x, y } }
        }
        let p = Point::new(10, 20);
    ", "should compile and run"),
    
    // Complex scenarios
    ("
        trait Factory {
            fn create() -> Self;
        }
        
        impl Factory for Widget {
            fn create() -> Widget { Widget {} }
        }
        
        let w = Widget::create();
    ", "should work with traits"),
]
```

---

## Test Infrastructure

### 1. Test Utilities
```rust
// Helper functions for testing
mod test_utils {
    pub fn parse_and_check(source: &str) -> Result<TypeCheckedAST, CompilerError> {
        // Parse, type check, return result
    }
    
    pub fn compile_and_run(source: &str) -> Result<i64, String> {
        // Full compilation and execution
    }
    
    pub fn benchmark(source: &str, iterations: usize) -> f64 {
        // Performance measurement
    }
}
```

### 2. Test Data Generation
```rust
// Generate test cases programmatically
fn generate_static_method_tests() -> Vec<TestCase> {
    // Generate combinations of:
    // - Different type paths
    // - Different argument counts/types
    // - Nested calls
    // - Generic parameters
}
```

### 3. Comparison Tests
```rust
// Compare with existing behavior
fn test_backward_compatibility() {
    // All existing instance method tests should still pass
    // New static method tests should also pass
}
```

---

## Test Execution Schedule

### Phase 1: Component Tests (Parallel with Implementation)
- **Day 1**: Parser tests (LEX)
- **Day 1**: Type checker tests (SEM)  
- **Day 2**: MIR generation tests
- **Day 2**: Code generation tests

### Phase 2: Integration Tests (During Integration Phase)
- **Day 3**: Simple integration tests
- **Day 3**: Complex scenario tests
- **Day 4**: Performance comparison tests

### Phase 3: Regression Tests (After Integration)
- **Day 5**: Full test suite run
- **Day 5**: Backward compatibility verification
- **Day 5**: Performance regression check

---

## Success Criteria

### 1. Functional Requirements
- ✅ All test cases pass
- ✅ No regressions in existing functionality
- ✅ Clear error messages for failures

### 2. Performance Requirements
- ✅ Static method calls no slower than instance calls (+10% tolerance)
- ✅ No memory leaks
- ✅ Compilation time increase <5%

### 3. Quality Requirements
- ✅ Code coverage >80% for new functionality
- ✅ All error cases tested
- ✅ Edge cases covered

---

## Reporting

### Daily Status Reports
```markdown
## VER Status Report - Day X
**Tests Run:** X/Y
**Passing:** A%
**Failures:** [List]
**Blockers:** [If any]
```

### Final Report
```markdown
## VER Final Report
**Overall Status:** PASS/FAIL
**Test Coverage:** X%
**Performance Impact:** +Y%
**Recommendations:** [If any]
```

---

## Files to Create

1. `tests/parser/static_methods.rs` - Parser tests
2. `tests/type_checker/static_methods.rs` - Type checking tests  
3. `tests/mir/static_methods.rs` - MIR generation tests
4. `tests/codegen/static_methods.rs` - Code generation tests
5. `tests/integration/static_methods.rs` - Integration tests

---

*VER Team: Begin test implementation immediately. Coordinate with component teams for test data.*