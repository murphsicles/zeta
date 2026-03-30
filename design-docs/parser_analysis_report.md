# Parser Analysis Report for Static Method Support

**From:** LEX (Parser Expert)  
**To:** Father Zak  
**Date:** 2026-03-30 09:30 GMT  
**Subject:** Parser preparation for static method syntax `Type::method()`

## 1. Current Parser Analysis

### Current Implementation:
- **Instance methods** (`expr.method()`): Handled in `parse_postfix` → `Call` node with `receiver: Some(expr)`
- **Static methods** (`Type::method()`): Partially handled in `parse_path_expr` → `PathCall` node
- **Type arguments**: Supported at end of path, not in middle (e.g., `Vec::<i32>::new()` fails)

### Key Findings:
1. **171 tests pass** (baseline established)
2. `PathCall` AST node exists and is used in resolver
3. Current limitation: `parse_path` stops at `<`, doesn't handle `::` after type arguments
4. Error messages are basic nom errors, not user-friendly

## 2. Implementation Options Analysis

### Option A: Modify `parse_postfix` to handle `::`
- **Pros**: Unified method call handling, consistent errors
- **Cons**: Complicates `parse_postfix`, need to distinguish path vs method calls
- **Feasibility**: Medium

### Option B: Create new `parse_static_method_call`
- **Pros**: Clean separation, specialized error handling
- **Cons**: Adds parsing pipeline complexity
- **Feasibility**: High

### Option C: Enhance `parse_path_expr` (RECOMMENDED)
- **Pros**: Builds on existing `PathCall` infrastructure, handles complex paths
- **Cons**: Need to fix type argument parsing in middle of paths
- **Feasibility**: High

## 3. Recommended Approach: Enhanced `parse_path_expr`

### Core Changes Needed:
1. **New parser function**: `parse_path_with_type_args` that handles `Ident::<TypeArgs>::Ident`
2. **Update `parse_path_expr`**: Use new parser, properly split path/method
3. **Error recovery**: Better messages for `Type::` (missing method) and `Type::method` (missing `()`)

### AST Considerations:
- Keep `PathCall { path, method, args }`
- May need to add `type_args` field if SEM's design requires it
- Coordinate with SEM on type argument representation

## 4. Test Cases Prepared

Created `tests/static_method_parsing.rs` with:
- Basic static calls: `Point::new(10, 20)`
- Type arguments: `Vec::<i32>::new()` (currently fails)
- Nested paths: `std::collections::HashMap::new()`
- Error cases: `Type::`, `Type::(args)`
- Mixed calls: `Point::new(1, 2).translate(3, 4)`

## 5. Code Skeleton Prepared

See `parser_implementation_plan.md` for detailed implementation plan:

### Key Functions:
```rust
// Enhanced path parser
fn parse_path_with_type_args(input: &str) -> IResult<&str, (Vec<String>, Vec<Vec<String>>)>

// Updated path expression parser  
fn parse_path_expr(input: &str) -> IResult<&str, AstNode>

// Error recovery helper
fn parse_static_method_call(input: &str) -> IResult<&str, AstNode>
```

### Integration Points:
1. **Parser**: `parse_path_expr` → `PathCall`
2. **Resolver**: Lookup by qualified name `Type::method`
3. **Type Checker**: Validate type arguments
4. **Error Messages**: User-friendly syntax errors

## 6. Constraints Satisfied

- **Works with SEM's design**: Uses existing `PathCall` node, extensible for type arguments
- **Backward compatible**: Existing `PathCall` usage preserved
- **Handles both static/instance**: Clear separation via `::` vs `.`
- **Clear error messages**: Planned error recovery for common mistakes

## 7. Next Steps for Implementation

1. **Coordinate with SEM**: Confirm type argument representation in AST
2. **Implement `parse_path_with_type_args`**: Core parsing logic
3. **Update `parse_path_expr`**: Integrate new parser
4. **Add error recovery**: Helpful syntax error messages
5. **Run tests**: Ensure 171 tests still pass, new tests work

## 8. Risks and Mitigations

- **Risk**: Breaking existing path parsing
  - **Mitigation**: Thorough testing, especially edge cases
- **Risk**: Performance impact
  - **Mitigation**: Minimal changes, reuse existing parsers
- **Risk**: Complex type argument syntax
  - **Mitigation**: Start simple (`::<T>`), extend later

## 9. Deliverables Ready

✅ **Parser analysis** - Complete  
✅ **Implementation options** - Documented with pros/cons  
✅ **Test cases** - Created in `tests/static_method_parsing.rs`  
✅ **Code skeleton** - Prepared in `parser_implementation_plan.md`  
✅ **Recommendation** - Option C (enhance `parse_path_expr`)

## 10. Ready for Implementation

The parser is prepared for static method support. With SEM's type system design, we can:
1. Finalize AST representation for type arguments
2. Implement the enhanced `parse_path_expr`
3. Integrate with type checking
4. Deliver full `Type::method()` syntax support

**Time**: 08:47-09:30 GMT (43 minutes)  
**Status**: Analysis complete, ready for implementation

---
**LEX**  
Parser Expert, Dark Factory