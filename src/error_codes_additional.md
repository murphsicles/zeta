# Additional Error Codes for Zeta
## To be added to error_codes.rs when integrated

## E2XXX - Type System Errors (Additional)

### E2006: Constraint solving failed
**Description:** Type unification constraints cannot be satisfied
**Example:** `Mismatch(I64, I32)` in type unification
**Suggestion:** Check type annotations or add explicit casts

### E2007: Recursive type detected  
**Description:** Infinite type recursion without indirection
**Example:** `type List = Option<Box<List>>` without proper boxing
**Suggestion:** Use boxing or other indirection for recursive types

### E2008: Type variable escape
**Description:** Generic type variable used outside its scope
**Example:** Generic type escapes function boundary
**Suggestion:** Add type parameters or use concrete types

### E2009: Invalid type coercion
**Description:** Attempting impossible type conversion
**Example:** Trying to coerce `i64` to `bool`
**Suggestion:** Use explicit conversion or change types

### E2010: Missing type implementation
**Description:** Required trait not implemented for type
**Example:** Trait `Display` not implemented for custom struct
**Suggestion:** Implement the required trait or use different type

## E3XXX - Semantic/Symbol Errors (Additional)

### E3003: Cyclic dependency detected
**Description:** Circular dependency between modules/types
**Example:** Module A imports B, B imports A
**Suggestion:** Restructure to break circular dependency

### E3004: Invalid visibility
**Description:** Attempt to use private item from outside module
**Example:** Using private function from different module
**Suggestion:** Make item public or use within module

### E3005: Invalid main function signature
**Description:** `main` function has wrong signature
**Example:** `fn main() -> String` (should return `i64`)
**Suggestion:** Change main to `fn main() -> i64`

## E4XXX - Code Generation Errors (Additional)

### E4003: Stack overflow in code generation
**Description:** Recursive code generation exceeds stack
**Example:** Deeply nested expressions or recursion
**Suggestion:** Increase recursion limit or restructure code

### E4004: Invalid LLVM intrinsic
**Description:** Unsupported or invalid LLVM intrinsic
**Example:** Using platform-specific intrinsic on wrong target
**Suggestion:** Check target compatibility or use alternative

### E4005: Memory alignment error
**Description:** Invalid memory alignment for type
**Example:** Misaligned memory access for SIMD type
**Suggestion:** Check alignment requirements or use aligned alloc

## E9XXX - Tooling/Configuration Errors (Additional)

### E9003: Invalid optimization level
**Description:** Unsupported optimization level specified
**Example:** `--opt-level 5` (only 0-3, s, z supported)
**Suggestion:** Use valid optimization level (0-3, s, z)

### E9004: Target triple not supported
**Description:** Compilation target not supported
**Example:** `--target unknown-unknown-unknown`
**Suggestion:** Check supported targets or build for host

### E9005: Feature not enabled
**Description:** Required feature not enabled in build
**Example:** Using `async` without `async` feature
**Suggestion:** Enable feature in Cargo.toml or use `--features`

## Error Code Integration Notes

These error codes should be added to `src/error_codes.rs` when:
1. The error code system is integrated into compiler
2. Corresponding error detection is implemented
3. Testing confirms error codes work correctly

## Usage Example

```rust
use crate::error_codes::{Diagnostic, common};

// Instead of:
eprintln!("Constraint solving failed: {:?}", errors);

// Use:
let diag = Diagnostic::new(
    common::CONSTRAINT_SOLVING_FAILED, // E2006
    format!("Constraint solving failed: {:?}", errors)
);
eprintln!("{}", diag.format());
```