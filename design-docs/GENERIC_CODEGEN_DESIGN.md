# Generic Code Generation Design

## Overview
This document outlines the design for implementing generic type instantiation (monomorphization) in Zeta's code generation system. The goal is to generate concrete LLVM IR for generic functions and types based on their type arguments.

## Current State Analysis

### Current Codegen Architecture
1. **MIR Representation**: `MirStmt::Call` contains `type_args: Vec<Type>` field
2. **Type System**: `Type` enum supports generic types via `Named(String, Vec<Type>)`
3. **Codegen**: Currently ignores `type_args` in function calls
4. **Function Lookup**: `get_function()` doesn't handle type-parameterized functions

### Key Gaps
1. No monomorphization - generic functions aren't instantiated
2. No type-parameterized LLVM type generation
3. No specialization cache for reusing generated code
4. No mangling scheme for generic function names

## Monomorphization Strategy

### Approach: Rust-style Monomorphization
Generate concrete versions of generic functions for each unique set of type arguments.

### Name Mangling Scheme
For a generic function `vec_new::<T>()`:
- `vec_new::<i32>()` → `vec_new_i32`
- `vec_new::<String>()` → `vec_new_String`
- `Vec::<T>::new()` → `Vec_T_new` (where T is instantiated)

For nested generics:
- `Option::<Box<i32>>::new()` → `Option_Box_i32_new`

### Type Representation in LLVM
1. **Primitive types**: Direct LLVM types (i64, f64, etc.)
2. **Struct types**: Generate LLVM struct types based on type parameters
3. **Pointer types**: Convert to i64 (current approach) or generate proper pointer types

## Implementation Plan

### Phase 1: Core Monomorphization Infrastructure

#### 1.1 Update `LLVMCodegen` Struct
```rust
pub struct LLVMCodegen<'ctx> {
    // ... existing fields ...
    pub specialized_fns: HashMap<String, FunctionValue<'ctx>>, // Cache for monomorphized functions
    pub specialized_types: HashMap<String, inkwell::types::StructType<'ctx>>, // Cache for monomorphized types
}
```

#### 1.2 Add Type Mangling Function
```rust
fn mangle_type_name(base_name: &str, type_args: &[Type]) -> String {
    let mut mangled = base_name.to_string();
    for ty in type_args {
        mangled.push('_');
        mangled.push_str(&ty.mangled_name());
    }
    mangled
}

impl Type {
    fn mangled_name(&self) -> String {
        match self {
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::Named(name, args) => {
                let mut mangled = name.clone();
                if !args.is_empty() {
                    mangled.push('_');
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { mangled.push('_'); }
                        mangled.push_str(&arg.mangled_name());
                    }
                }
                mangled
            }
            // ... other type variants ...
        }
    }
}
```

#### 1.3 Update `get_function()` to Handle Type Arguments
```rust
fn get_function_with_types(&mut self, name: &str, type_args: &[Type]) -> FunctionValue<'ctx> {
    if type_args.is_empty() {
        return self.get_function(name);
    }
    
    let mangled_name = self.mangle_type_name(name, type_args);
    
    // Check cache first
    if let Some(&f) = self.specialized_fns.get(&mangled_name) {
        return f;
    }
    
    // Generate monomorphized version
    let original_fn = self.get_function(name);
    let monomorphized_fn = self.monomorphize_function(original_fn, name, type_args);
    
    // Cache and return
    self.specialized_fns.insert(mangled_name.clone(), monomorphized_fn);
    monomorphized_fn
}
```

### Phase 2: Function Monomorphization

#### 2.1 `monomorphize_function()` Implementation
```rust
fn monomorphize_function(
    &mut self,
    original_fn: FunctionValue<'ctx>,
    name: &str,
    type_args: &[Type]
) -> FunctionValue<'ctx> {
    // Create new function with mangled name
    let mangled_name = self.mangle_type_name(name, type_args);
    let fn_type = self.get_monomorphized_fn_type(original_fn, type_args);
    let new_fn = self.module.add_function(&mangled_name, fn_type, None);
    
    // Copy function body with type substitutions
    self.clone_function_body(original_fn, new_fn, type_args);
    
    new_fn
}
```

#### 2.2 Type Substitution in Function Body
```rust
fn clone_function_body(
    &mut self,
    src_fn: FunctionValue<'ctx>,
    dst_fn: FunctionValue<'ctx>,
    type_args: &[Type]
) {
    // Create type substitution map
    let type_map = self.build_type_substitution_map(src_fn, type_args);
    
    // Clone basic blocks
    for src_block in src_fn.get_basic_blocks() {
        let dst_block = self.context.append_basic_block(dst_fn, "block");
        self.builder.position_at_end(dst_block);
        
        // Clone instructions with type substitutions
        for instr in src_block.get_instructions() {
            self.clone_instruction(instr, &type_map);
        }
    }
}
```

### Phase 3: Type Generation

#### 3.1 LLVM Type Generation from Type Parameters
```rust
fn get_llvm_type_for(&mut self, ty: &Type) -> inkwell::types::BasicTypeEnum<'ctx> {
    match ty {
        Type::I64 => self.i64_type.into(),
        Type::F64 => self.f64_type.into(),
        Type::Named(name, args) => {
            let mangled_name = self.mangle_type_name(name, args);
            
            // Check cache
            if let Some(struct_ty) = self.specialized_types.get(&mangled_name) {
                return struct_ty.as_basic_type_enum();
            }
            
            // Generate struct type based on generic definition
            let struct_ty = self.generate_struct_type(name, args);
            self.specialized_types.insert(mangled_name, struct_ty);
            struct_ty.as_basic_type_enum()
        }
        // ... handle other types ...
    }
}
```

### Phase 4: Integration with Existing Codegen

#### 4.1 Update `gen_stmt()` for Calls
```rust
MirStmt::Call { func, args, dest, type_args } => {
    let callee = if !type_args.is_empty() {
        self.get_function_with_types(func, type_args)
    } else {
        self.get_function(func)
    };
    // ... rest of call generation ...
}
```

#### 4.2 Update `gen_mirs()` for Generic Functions
```rust
pub fn gen_mirs(&mut self, mirs: &[Mir]) {
    // First pass: declare all functions (including generic ones)
    for mir in mirs {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let is_generic = self.is_generic_function(mir);
        
        if is_generic {
            // For generic functions, we'll generate them on-demand when called
            self.declare_generic_function(&fn_name, mir);
        } else {
            // Non-generic functions as before
            let param_types: Vec<_> = (0..mir.param_indices.len())
                .map(|_| self.i64_type.into())
                .collect();
            let fn_type = self.i64_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(&fn_name, fn_type, None);
            self.fns.insert(fn_name.clone(), fn_val);
        }
    }
    
    // Second pass: generate function bodies
    for mir in mirs {
        if !self.is_generic_function(mir) {
            self.gen_fn(mir);
        }
        // Generic functions are generated on-demand via monomorphization
    }
}
```

## Challenges and Solutions

### Challenge 1: Recursive Generics
**Problem**: `Option<Box<T>>` where `T` is also generic
**Solution**: Recursive mangling and caching
- `Option_Box_T` where T is instantiated separately
- Cache intermediate results

### Challenge 2: Type Parameter Substitution
**Problem**: Substituting type parameters in function bodies
**Solution**: Two-phase approach
1. Parse generic function definition to identify type parameters
2. Create substitution map when instantiating

### Challenge 3: Performance
**Problem**: Repeated monomorphization for same type arguments
**Solution**: Comprehensive caching
- Cache by mangled name
- Cache by type argument hash
- Reuse across compilation sessions

### Challenge 4: Debug Information
**Problem**: Debug symbols for monomorphized functions
**Solution**: Preserve source information
- Include original generic name in debug info
- Map mangled names back to source

## Testing Strategy

### Unit Tests
1. **Type Mangling**: Test `mangle_type_name()` with various type arguments
2. **Function Monomorphization**: Test generating concrete functions from generic templates
3. **Type Generation**: Test LLVM type generation from generic type definitions

### Integration Tests
1. **Generic Function Calls**: `vec_new::<i32>()` → `vec_new_i32`
2. **Generic Structs**: `Vec<i32>` → proper LLVM struct type
3. **Nested Generics**: `Option<Box<i32>>`
4. **Multiple Type Parameters**: `HashMap<String, i32>`

### Smoke Tests
1. Compile and run generic code examples
2. Verify generated LLVM IR contains monomorphized functions
3. Test performance with caching

## Coordination Requirements

### With SEM (Type System)
1. **Type Information**: Need access to generic function definitions
2. **Type Parameter Mapping**: Map type parameters to concrete types
3. **Type Constraints**: Handle trait bounds (future extension)

### With LEX (Parser)
1. **Parser Output**: Verify `type_args` in `MirStmt::Call` is populated correctly
2. **Generic Syntax**: Ensure parser handles `::<T>` syntax

### Timeline
- **Phase 1 (Core)**: 1 hour - Basic infrastructure
- **Phase 2 (Functions)**: 1 hour - Function monomorphization  
- **Phase 3 (Types)**: 1 hour - Type generation
- **Phase 4 (Integration)**: 1 hour - Integration and testing

## Deliverables

### By 21:00 GMT
1. ✅ This design document
2. ✅ Phase 1 implementation (Core infrastructure)
3. ✅ Updated codegen tests for generic functions

### Follow-up Work
1. Phase 2-4 implementation
2. Performance optimization
3. Debug information support
4. Generic trait implementation (future)

## Risk Assessment

### High Risk
- Recursive generic handling
- Type substitution correctness

### Medium Risk  
- Performance with many instantiations
- Debug symbol generation

### Low Risk
- Basic monomorphization
- Simple type mangling

## Success Criteria
1. Generic function calls generate correct monomorphized LLVM IR
2. Type-parameterized structs generate correct LLVM types
3. Caching prevents redundant code generation
4. All existing tests continue to pass
5. New generic codegen tests pass