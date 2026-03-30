# Static Methods Implementation Plan

## Phase 1: Type System Integration

### Step 1.1: Enhance Resolver to Track Static Methods
**File**: `src/middle/resolver/resolver.rs`
- Add `static_methods: HashMap<String, Vec<String>>` to track type -> static methods
- Modify `register` function to extract static methods from impl blocks
- Static methods have no `self` parameter in their signature

### Step 1.2: Update Impl Block Processing
**File**: `src/middle/resolver/resolver.rs`
- When processing `AstNode::ImplBlock`, identify static methods
- Static methods: `Method` nodes with no `self` parameter
- Register as `Type::method_name` in function table

### Step 1.3: Enhance PathCall Type Checking
**File**: `src/middle/resolver/new_resolver.rs`
- Extend `infer` function for `AstNode::PathCall`
- Resolve `path` to type name
- Look up static method in resolver's function table
- Check argument types against method signature
- Return method's return type

### Step 1.4: Update Old Type Checker
**File**: `src/middle/resolver/typecheck.rs`
- Ensure `PathCall` nodes are handled in old type system
- Fallback to qualified name lookup

## Phase 2: Code Generation

### Step 2.1: Enhance MIR Generation for PathCall
**File**: `src/middle/mir/gen.rs`
- Current implementation constructs qualified names correctly
- Verify function lookup works with resolver's function table
- Ensure no `self` parameter is added for static methods

### Step 2.2: Update LLVM Code Generation
**File**: `src/backend/codegen/codegen.rs`
- Static method calls should use same code path as regular functions
- Verify qualified name mangling works correctly
- No special handling needed beyond regular function calls

### Step 2.3: Runtime Support
- No runtime changes needed for static methods
- They compile to regular function calls

## Phase 3: Testing

### Step 3.1: Update Existing Test
**File**: `tests/module_system_integration.rs`
- Update `test_rust_like_code` to use `Point::new()` instead of `create_point()`
- Verify test passes with static method syntax

### Step 3.2: Add New Tests
Create `tests/static_methods.rs`:
```rust
#[test]
fn test_basic_static_method() {
    let code = r#"
        struct Point { x: i32, y: i32 }
        impl Point {
            fn new(x: i32, y: i32) -> Point { Point { x, y } }
        }
        fn main() -> i32 {
            let p = Point::new(10, 20);
            p.x + p.y
        }
    "#;
    // Test compilation and execution
}

#[test]
fn test_multiple_static_methods() {
    let code = r#"
        struct Point { x: i32, y: i32 }
        impl Point {
            fn new(x: i32, y: i32) -> Point { Point { x, y } }
            fn origin() -> Point { Point::new(0, 0) }
        }
        fn main() -> i32 {
            let p1 = Point::new(5, 10);
            let p2 = Point::origin();
            p1.x + p1.y + p2.x + p2.y
        }
    "#;
}

#[test]
fn test_static_method_without_self() {
    let code = r#"
        struct Counter { value: i32 }
        impl Counter {
            fn start() -> Counter { Counter { value: 0 } }
            fn increment(&mut self) { self.value += 1; }
        }
        fn main() -> i32 {
            let mut c = Counter::start();
            c.increment();
            c.value
        }
    "#;
}
```

### Step 3.3: Run Full Test Suite
- Execute all 108 existing tests
- Ensure no regressions
- Add new static method tests to CI

## Detailed Code Changes

### 1. Resolver Enhancement

```rust
// In src/middle/resolver/resolver.rs
pub struct Resolver {
    // ... existing fields ...
    pub static_methods: HashMap<String, Vec<String>>, // type_name -> [method_name]
}

impl Resolver {
    pub fn register(&mut self, ast: AstNode) {
        match ast {
            AstNode::ImplBlock { concept, ty, body, .. } => {
                // Check if this is a regular impl (not trait impl)
                if concept.is_empty() || concept == ty {
                    for item in body {
                        if let AstNode::Method { name, params, .. } = item {
                            // Check if it's a static method (no self parameter)
                            let is_static = params.first().map_or(true, |(name, _)| name != "self");
                            if is_static {
                                self.static_methods
                                    .entry(ty.clone())
                                    .or_insert_with(Vec::new)
                                    .push(name.clone());
                                
                                // Register as qualified function
                                let qualified_name = format!("{}::{}", ty, name);
                                self.funcs.insert(qualified_name, (params, ret, false));
                            }
                        }
                    }
                }
            }
            // ... rest of register function
        }
    }
}
```

### 2. PathCall Type Checking

```rust
// In src/middle/resolver/new_resolver.rs
impl<'a> InferContext<'a> {
    fn infer(&mut self, node: &AstNode) -> Result<Type, String> {
        match node {
            AstNode::PathCall { path, method, args } => {
                // Construct type name from path
                let type_name = path.join("::");
                
                // Check if this is a static method call
                let qualified_name = format!("{}::{}", type_name, method);
                
                // Look up in resolver's function table
                if let Some((params, ret_ty, _)) = self.resolver.funcs.get(&qualified_name) {
                    // Check argument count matches
                    if args.len() != params.len() {
                        return Err(format!(
                            "Static method {} expects {} arguments, got {}",
                            qualified_name,
                            params.len(),
                            args.len()
                        ));
                    }
                    
                    // Check argument types
                    for (i, (arg, (param_name, param_ty))) in 
                        args.iter().zip(params.iter()).enumerate() 
                    {
                        let arg_ty = self.infer(arg)?;
                        let expected_ty = self.resolver.string_to_type(param_ty);
                        self.unify(&arg_ty, &expected_ty)
                            .map_err(|e| format!("Argument {}: {}", i + 1, e))?;
                    }
                    
                    // Return method's return type
                    Ok(self.resolver.string_to_type(ret_ty))
                } else {
                    Err(format!("Static method not found: {}", qualified_name))
                }
            }
            // ... rest of infer function
        }
    }
}
```

## Success Criteria

1. ✅ `Point::new(10, 20)` compiles and runs correctly
2. ✅ All 108 existing tests pass
3. ✅ Static methods work with both old and new type systems
4. ✅ No performance regression
5. ✅ Clear error messages for missing static methods

## Rollback Plan

If issues arise:
1. Revert to using `PathCall` as-is (no static method resolution)
2. Keep test modified to avoid static methods
3. Document limitation for v0.3.20

## Timeline Estimates

- **Day 1**: Implement resolver changes (4 hours)
- **Day 2**: Update type checking (3 hours)  
- **Day 3**: Testing and bug fixes (3 hours)
- **Day 4**: Documentation and final validation (2 hours)

**Total**: 12 hours over 4 days