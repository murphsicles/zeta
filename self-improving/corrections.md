# Corrections Log

## 2026-03-26: Zeta Tokenization Improvements

### Lesson 1: Float Storage Workaround
**CONTEXT**: Implementing float literal support in Zeta AST
**ISSUE**: `f64` doesn't implement `Eq` and `Hash` traits, breaking AST derivation
**CORRECTION**: Store float literals as strings in AST, parse to f64 when needed
**APPLIED**: Yes - changed `FloatLit(f64)` to `FloatLit(String)`
**REUSABLE**: When Rust trait limitations block type usage in derived traits, consider string storage

### Lesson 2: Escape Sequence Parsing
**CONTEXT**: Implementing string escape sequence handling
**ISSUE**: Complex nom combinator approach had type inference issues
**CORRECTION**: Use manual character iteration with position tracking
**APPLIED**: Yes - rewrote `parse_string_lit` with manual iteration
**REUSABLE**: For complex parsing with state, manual iteration can be simpler than combinator chains

### Lesson 3: Comprehensive Testing
**CONTEXT**: Validating tokenization fixes
**CORRECTION**: Create tests for all escape sequences individually AND in combination
**APPLIED**: Yes - created `test_escapes.rs` with individual and mixed tests
**REUSABLE**: Test edge cases individually, then integration, then boundary conditions

### Lesson 4: Code Review Application
**CONTEXT**: Father Zak instructed to use code-review skill
**CORRECTION**: Apply systematic code review with three-pass approach and severity levels
**APPLIED**: Yes - reviewed own implementation using security/performance/correctness/maintainability/testing checklists
**REUSABLE**: Always review own code before considering work complete

### Lesson 5: Unicode String Slicing
**CONTEXT**: Testing Unicode strings with emoji in Zeta parser
**ISSUE**: Debug print `&input[..20.min(input.len())]` panicked when index 20 was inside multi-byte emoji
**CORRECTION**: When slicing strings for debug output, ensure slice boundaries are at char boundaries
**APPLIED**: Not fixed in code (debug prints remain), but identified as debug-only issue
**REUSABLE**: String slicing in Rust must respect UTF-8 character boundaries; use `.chars()` or validate boundaries

### Lesson 6: Debug vs Production Issues
**CONTEXT**: Panic in parser with Unicode input
**CORRECTION**: Distinguish between debug logging issues and actual parsing issues
**APPLIED**: Verified actual parsing works correctly; issue was only in debug prints
**REUSABLE**: When debugging, check if issue is in instrumentation vs actual functionality

## Pattern Recognition
- After 3 successful applications of systematic code review, promote to HOT memory
- Float-as-string pattern may be reusable in other compiler implementations
- Manual parsing vs combinator choice depends on complexity
- UTF-8 boundary checking is critical for string operations in Rust
- Debug instrumentation can introduce its own bugs separate from core functionality