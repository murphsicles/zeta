# Zeta v0.3.15 - Impl Block Method Support

## Status: IN PROGRESS

**Target Date:** 2026-03-29  
**Current Progress:** Partially implemented, basic functionality working

## Overview

This release adds support for method calls from impl blocks, fixing the long-standing issue where `Point::new(10, 20)` and `p.sum()` would fail to compile.

## Changes Made

### 1. Parser Fixes
- **File:** `src/frontend/parser/expr.rs`
- **Change:** Modified `parse_path_expr` to create `PathCall` nodes for expressions like `Point::new(10, 20)`
- **Before:** `Call { receiver: None, method: "Point::new", args: [...] }`
- **After:** `PathCall { path: ["Point"], method: "new", args: [...] }`

### 2. Resolver Updates
- **File:** `src/middle/resolver/resolver.rs`
- **Change:** Modified `ImplBlock` registration to also register functions with qualified names
- **Before:** Functions registered only with simple names (e.g., `"new"`)
- **After:** Functions registered with both qualified names (e.g., `"Point::new"`) and simple names

### 3. Type Checker Fixes
- **File:** `src/middle/resolver/new_resolver.rs`
- **Change:** Added handling for `PathCall` and `ImplBlock` nodes
- **Before:** `PathCall` and `ImplBlock` fell through to default case with "Type inference not implemented"
- **After:** `PathCall` looks up functions (falling back to simple names), `ImplBlock` returns unit type

### 4. MIR Generator Updates
- **File:** `src/middle/mir/gen.rs`
- **Change:** Added handling for `PathCall` nodes
- **Before:** `PathCall` fell through to default case, returning literal 0
- **After:** `PathCall` generates call statements with function names

## Current Status

### ✅ Working
- Basic impl block parsing and registration
- `Point::new(10, 20)` syntax parsing as `PathCall`
- Method calls from impl blocks compile and run
- All existing tests pass (135/136)

### ⚠️ Partially Working
- `test_rust_like_code` test still fails when run with `--include-ignored`
- Field access returns 0 instead of actual field values
- Qualified name resolution uses simple names as fallback

### ❌ Not Yet Implemented
- Proper qualified name resolution (e.g., disambiguating `Point::new` vs `Other::new`)
- Method lookup based on receiver type
- Field access returning actual values

## Test Results

```bash
# All tests pass except the ignored one
cargo test -- --nocapture
# Result: 135 passed, 0 failed, 1 ignored

# Test program compiles and runs
cargo run -- test_impl_methods.zeta
# Result: Program compiles and returns 0 (field access issue)
```

## Example Code

```zeta
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Point {
        Point { x, y }
    }
    
    fn sum(&self) -> i32 {
        self.x + self.y
    }
}

fn main() -> i32 {
    let p = Point::new(10, 20);
    p.sum()  // Should return 30, currently returns 0
}
```

## Next Steps

1. Fix the remaining issue with `test_rust_like_code` test
2. Investigate why `compile_and_run_zeta` can't find function `new` but direct compilation works
3. Implement proper field access to return actual values
4. Add more comprehensive tests for impl block functionality

## Notes

This is a significant step forward for Zeta's type system. While not complete, it demonstrates that the basic infrastructure for impl blocks and method calls is now in place. The remaining issues are primarily around name resolution and field access, which can be addressed in follow-up releases.