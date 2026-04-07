# Static Method Support Design for Zeta v0.3.20

## Analysis Findings

### Current Implementation Status

1. **Parser (`src/frontend/parser/expr.rs`)**:
   - `parse_path_expr` function handles path-based calls like `Point::new(10, 20)`
   - Creates `AstNode::PathCall` for paths with ≥2 segments (e.g., `Point::new`)
   - Creates `AstNode::Call` with `receiver: None` for single-segment paths
   - `parse_postfix` handles instance method calls like `point.sum()` via `AstNode::Call` with `receiver: Some(...)`

2. **AST (`src/frontend/ast.rs`)**:
   - `Call` variant: `{ receiver: Option<Box<AstNode>>, method: String, args: Vec<AstNode>, type_args: Vec<String>, structural: bool }`
   - `PathCall` variant: `{ path: Vec<String>, method: String, args: Vec<AstNode> }`
   - Current design already separates instance calls (via `Call`) from path-based calls (via `PathCall`)

3. **Type Checking (`src/middle/resolver/typecheck_new.rs`)**:
   - New type checking system with unification
   - `PathCall` handling exists but appears incomplete
   - Instance method calls handled through `Call` nodes

4. **Code Generation (`src/backend/codegen/`)**:
   - MIR generation handles both `Call` and `PathCall` nodes
   - `PathCall` constructs qualified names like `Point::new`
   - Instance method calls generate specialized function names via monomorphization

5. **Test Status**:
   - Test `test_rust_like_code` was modified to avoid static methods
   - Uses regular function `create_point()` instead of `Point::new()`
   - All 108 tests pass in v0.3.19

### Key Observations

1. The infrastructure for static methods already exists via `PathCall`
2. The parser correctly distinguishes `Point::new()` (path call) from `point.sum()` (instance call)
3. Type checking and code generation need enhancements for proper static method support
4. Backward compatibility is achievable since `PathCall` already exists

## Design Options

### Option A: Extend `Call` with `Option<Expr>` for receiver (NOT RECOMMENDED)
- **Pros**: Unified call representation
- **Cons**: 
  - Breaks existing pattern matching on `Call` nodes
  - Requires changes throughout codebase
  - `PathCall` already exists and serves this purpose

### Option B: Enhance `PathCall` for Static Methods (RECOMMENDED)
- **Pros**: 
  - Leverages existing `PathCall` infrastructure
  - Minimal changes required
  - Clear semantic distinction from instance calls
  - Backward compatible
- **Cons**: Need to complete implementation in type checking and code generation

## Recommended Approach: Enhance PathCall

### 1. AST Changes
No changes needed - `PathCall` already exists with appropriate fields:
```rust
PathCall {
    path: Vec<String>,  // e.g., ["Point"] for Point::new()
    method: String,     // e.g., "new"
    args: Vec<AstNode>, // e.g., [10, 20]
}
```

### 2. Parser Changes
Minimal changes - `parse_path_expr` already correctly creates `PathCall` nodes.

### 3. Type Checking Rules

**Static Method Resolution**:
1. Resolve `path` to a type (struct/enum)
2. Look for static method `method` in associated impl blocks
3. Check argument types against method signature
4. Return method's return type

**Type Checking Implementation**:
- Extend `new_resolver.rs` to properly resolve `PathCall` nodes
- Add static method lookup in resolver's function table
- Handle qualified names like `Point::new`

### 4. Code Generation Approach

**MIR Generation** (`src/middle/mir/gen.rs`):
- Already constructs qualified names: `Point::new`
- Need to ensure proper function lookup and calling convention

**LLVM Code Generation**:
- Generate calls to qualified function names
- No `self` parameter for static methods
- Same calling convention as regular functions

### 5. Implementation Plan

**Phase 1: Type System Integration (2-3 hours)**
1. Enhance resolver to recognize static methods in impl blocks
2. Add static method registration during impl processing
3. Update type checking for `PathCall` nodes

**Phase 2: Code Generation (2-3 hours)**
1. Ensure MIR generation creates correct function calls
2. Verify LLVM code generation for static methods
3. Add runtime support if needed

**Phase 3: Testing & Validation (1-2 hours)**
1. Update `test_rust_like_code` to use `Point::new()`
2. Add new tests for static methods
3. Run full test suite (108 tests)

### 6. Test Plan

1. **Basic Static Method**:
   ```rust
   struct Point { x: i32, y: i32 }
   impl Point {
       fn new(x: i32, y: i32) -> Point { Point { x, y } }
   }
   fn main() -> i32 {
       let p = Point::new(10, 20);
       p.x + p.y
   }
   ```

2. **Multiple Static Methods**:
   ```rust
   impl Point {
       fn origin() -> Point { Point::new(0, 0) }
       fn from_tuple((x, y): (i32, i32)) -> Point { Point::new(x, y) }
   }
   ```

3. **Static Methods with Generics** (future):
   ```rust
   impl<T> Option<T> {
       fn none() -> Option<T> { None }
       fn some(value: T) -> Option<T> { Some(value) }
   }
   ```

### 7. Risks and Mitigation

**Risk 1**: Breaking existing `PathCall` usage
- **Mitigation**: `PathCall` is rarely used in current codebase; test thoroughly

**Risk 2**: Type system complexity
- **Mitigation**: Start with simple non-generic static methods

**Risk 3**: Performance impact
- **Mitigation**: Static methods should have same performance as regular functions

**Risk 4**: Backward compatibility
- **Mitigation**: All existing tests must pass; use feature flag if needed

## Estimated Timeline

- **Analysis & Design**: 45 minutes (complete)
- **Implementation**: 6-8 hours
- **Testing & Validation**: 2-3 hours
- **Total**: 8-12 hours

## Deliverables

1. Working static method support for `Point::new()` pattern
2. Updated `test_rust_like_code` using proper static methods
3. All 108 existing tests passing
4. Documentation for static method usage

## Conclusion

The recommended approach leverages existing `PathCall` infrastructure with minimal changes. This provides a clear path to implementing static methods while maintaining backward compatibility and following Rust-like semantics.