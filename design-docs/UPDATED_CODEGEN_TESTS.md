# Updated Codegen Tests for Generic Type System

## Overview
This document outlines the updated test suite for code generation with generic type support (monomorphization). The tests verify that generic functions and types are correctly instantiated and generate proper LLVM IR.

## Test Categories

### 1. Type Mangling Tests
Verify that type names are correctly mangled for monomorphization.

#### Test Cases:
1. **Basic primitive types**
   - `i32` → `"i32"`
   - `i64` → `"i64"`
   - `f64` → `"f64"`

2. **Generic types with single parameter**
   - `Vec<i32>` → `"Vec_i32"`
   - `Option<i64>` → `"Option_i64"`

3. **Generic types with multiple parameters**
   - `HashMap<String, i32>` → `"HashMap_str_i32"`
   - `Result<i32, String>` → `"Result_i32_str"`

4. **Nested generic types**
   - `Option<Box<i32>>` → `"Option_Box_i32"`
   - `Vec<Option<String>>` → `"Vec_Option_str"`

5. **Complex type combinations**
   - `&mut Vec<i32>` → `"Ref_ltvar_0_Vec_i32_mut"`
   - `*const Option<String>` → `"Ptr_const_Option_str"`

### 2. Function Name Mangling Tests
Verify that function names are correctly mangled with type arguments.

#### Test Cases:
1. **Simple generic function calls**
   - `vec_new::<i32>()` → `"vec_new_inst_i32"`
   - `option_some::<i64>(42)` → `"option_some_inst_i64"`

2. **Static method calls on generic types**
   - `Vec::<i32>::new()` → `"Vec_i32_new"`
   - `HashMap::<String, i32>::new()` → `"HashMap_str_i32_new"`

3. **Method calls with complex types**
   - `Option::<Box<i32>>::some(Box::new(42))` → `"Option_Box_i32_some"`

### 3. Monomorphization Tests
Verify that generic functions are correctly instantiated.

#### Test Cases:
1. **Basic monomorphization**
   - Generic function `identity::<T>(x: T) -> T`
   - Instantiation `identity::<i32>` generates `identity_inst_i32`
   - Function body correctly handles i32 values

2. **Multiple type parameters**
   - Generic function `pair::<T, U>(a: T, b: U) -> (T, U)`
   - Instantiation `pair::<i32, String>` generates `pair_inst_i32_str`

3. **Recursive generic calls**
   - `Vec::<i32>::new()` calls internal generic functions
   - All internal generics are correctly instantiated

4. **Caching behavior**
   - Calling `vec_new::<i32>()` multiple times
   - Only one instantiation is generated
   - Subsequent calls reuse cached function

### 4. Type Generation Tests
Verify that LLVM types are correctly generated from generic type definitions.

#### Test Cases:
1. **Struct type generation**
   - Generic struct `struct Vec<T> { data: *mut T, len: usize, cap: usize }`
   - `Vec<i32>` generates proper LLVM struct type
   - Fields have correct types (i32* for data, i64 for len/cap)

2. **Enum type generation**
   - Generic enum `enum Option<T> { Some(T), None }`
   - `Option<i32>` generates proper LLVM union/discriminated union

3. **Pointer type generation**
   - `Box<i32>` generates `i32*` LLVM type
   - `*const String` generates `i8**` LLVM type

### 5. Integration Tests
Verify end-to-end compilation of generic code.

#### Test Cases:
1. **Simple generic function compilation**
   ```rust
   fn identity<T>(x: T) -> T { x }
   fn main() { let x = identity::<i32>(42); }
   ```
   - Compiles without errors
   - Generates `identity_inst_i32` function
   - Returns correct value

2. **Generic struct usage**
   ```rust
   struct Vec<T> { data: *mut T, len: usize }
   impl<T> Vec<T> {
       fn new() -> Vec<T> { /* ... */ }
   }
   fn main() { let v = Vec::<i32>::new(); }
   ```
   - Compiles without errors
   - Generates `Vec_i32` struct type
   - Generates `Vec_i32_new` function

3. **Multiple instantiations**
   ```rust
   fn use_vec() {
       let v1 = Vec::<i32>::new();
       let v2 = Vec::<String>::new();
       let v3 = Vec::<i32>::new(); // Should reuse v1's instantiation
   }
   ```
   - Generates `Vec_i32_new` and `Vec_String_new`
   - Only one `Vec_i32_new` generated (cached)

### 6. Edge Case Tests
Test boundary conditions and error cases.

#### Test Cases:
1. **Empty type arguments**
   - `vec_new::<>()` should use default type or error
   - Currently falls back to non-generic version

2. **Wrong number of type arguments**
   - `Vec::<i32, String>::new()` should error (Vec only has 1 type param)

3. **Recursive type bounds**
   - `struct TreeNode<T> { value: T, children: Vec<TreeNode<T>> }`
   - Handles recursive type definitions

4. **Type parameter substitution in nested positions**
   - `Option<Box<T>>` where `T = i32`
   - Correctly substitutes `T` in nested position

## Test Implementation Strategy

### Unit Tests
```rust
// Example: Type mangling test
#[test]
fn test_type_mangling() {
    use zetac::middle::types::Type;
    
    let ty = Type::Named("Vec".to_string(), vec![Type::I32]);
    assert_eq!(ty.mangled_name(), "Vec_i32");
    
    let ty = Type::Named(
        "HashMap".to_string(), 
        vec![Type::Str, Type::I32]
    );
    assert_eq!(ty.mangled_name(), "HashMap_str_i32");
}

// Example: Function name mangling test
#[test]
fn test_function_name_mangling() {
    use zetac::backend::codegen::LLVMCodegen;
    use inkwell::context::Context;
    use zetac::middle::types::Type;
    
    let context = Context::create();
    let codegen = LLVMCodegen::new(&context, "test");
    
    let mangled = codegen.mangle_function_name("vec_new", &[Type::I32]);
    assert_eq!(mangled, "vec_new_inst_i32");
}
```

### Integration Tests
```rust
// Example: End-to-end generic function test
#[test]
fn test_generic_function_compilation() {
    use zetac::compile_and_run_zeta;
    
    let code = r#"
        fn identity<T>(x: T) -> T { x }
        fn main() -> i64 { 
            let x = identity::<i64>(42);
            x
        }
    "#;
    
    let result = compile_and_run_zeta(code);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 42);
}
```

### Smoke Tests
```rust
// Example: Codegen smoke test with generics
#[test]
fn test_codegen_generic_smoke() {
    use zetac::backend::codegen::LLVMCodegen;
    use inkwell::context::Context;
    use zetac::middle::mir::mir::{Mir, MirStmt, MirExpr};
    use zetac::middle::types::Type;
    use std::collections::HashMap;
    
    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "test_generic");
    
    // Create a simple MIR with generic call
    let mir = Mir {
        name: Some("main".to_string()),
        param_indices: vec![],
        stmts: vec![
            MirStmt::Call {
                func: "vec_new".to_string(),
                args: vec![],
                dest: 1,
                type_args: vec![Type::I32],
            },
            MirStmt::Return { val: 1 },
        ],
        exprs: {
            let mut map = HashMap::new();
            map.insert(1, MirExpr::Lit(0)); // Default value
            map
        },
        ctfe_consts: HashMap::new(),
        type_map: HashMap::new(),
    };
    
    // This should not panic
    codegen.gen_mirs(&[mir]);
    
    // Verify function was declared (though not necessarily generated yet)
    // The actual generation happens on-demand
    println!("Generic codegen smoke test passed");
}
```

## Test Data Files

### Sample Generic Code for Testing
Create test files in `tests/generic_code_samples/`:

1. `simple_generic_fn.zeta` - Basic generic function
2. `generic_struct.zeta` - Generic struct with methods
3. `multiple_instantiations.zeta` - Multiple uses of same generic
4. `nested_generics.zeta` - Nested generic types
5. `recursive_generic.zeta` - Recursive generic types

## Expected Test Outcomes

### Success Criteria
1. All type mangling tests pass
2. All function name mangling tests pass
3. Generic functions are correctly instantiated
4. LLVM types are correctly generated
5. Caching prevents redundant instantiations
6. Existing non-generic tests continue to pass

### Failure Modes to Test
1. Missing type arguments
2. Wrong number of type arguments
3. Unresolved type parameters
4. Recursive type explosion (should be caught)
5. Memory leaks in caching

## Performance Tests

### Test Cases:
1. **Instantiation time** - Measure time to monomorphize functions
2. **Cache hit rate** - Verify caching works efficiently
3. **Memory usage** - Monitor memory for many instantiations
4. **Compilation time** - Compare generic vs non-generic compilation

## Test Coverage Goals

### Code Coverage Targets:
1. `Type::mangled_name()` - 100% coverage for all variants
2. `LLVMCodegen::mangle_function_name()` - 100% coverage
3. `LLVMCodegen::get_function_with_types()` - 100% coverage
4. `LLVMCodegen::monomorphize_function()` - 100% coverage
5. `LLVMCodegen::is_generic_function()` - 100% coverage

### Integration Coverage:
1. Generic function calls in MIR → LLVM IR
2. Generic struct type → LLVM type
3. Multiple compilation units sharing instantiations
4. Error handling for invalid generic usage

## Test Execution

### Running Tests:
```bash
# Run all codegen tests
cargo test codegen

# Run generic-specific tests
cargo test generic_codegen

# Run integration tests
cargo test --test codegen_smoke

# Run with verbose output
cargo test -- --nocapture
```

### Test Environment:
- LLVM 17+ installed
- Debug build for better error messages
- Memory sanitizer for leak detection
- Timeouts for performance tests

## Conclusion

This updated test suite ensures that the generic type system implementation is robust, correct, and efficient. The tests cover all aspects of monomorphization from type mangling to LLVM code generation, with special attention to edge cases and performance considerations.