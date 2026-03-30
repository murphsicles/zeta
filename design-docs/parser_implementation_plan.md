# Parser Implementation Plan for Static Method Support

## Current Status Analysis

### Current Parser Behavior:
1. `expr.method()` → `Call { receiver: Some(expr), ... }` (in `parse_postfix`)
2. `Type::method()` → `PathCall { path: ["Type"], method: "method", ... }` (in `parse_path_expr`)
3. `Vec::<i32>::new()` → Fails (parses `Vec` as `Var`, leaves `::new()` unparsed)

### Issues:
1. Type arguments in middle of path not supported (`Vec::<i32>::new()`)
2. `parse_path_expr` doesn't continue parsing after type arguments
3. Error messages for malformed static calls could be better

## Recommended Approach: Option C (Enhance `parse_path_expr`)

### Changes Needed:

#### 1. Modify `parse_path` function (in parser/mod.rs) or enhance `parse_path_expr`:
- Need to handle type arguments in the middle of paths
- Path parsing should continue after `::<T>`

#### 2. Update `parse_path_expr` in `expr.rs`:
- After parsing path and optional type arguments, check if there's another `::`
- If yes, continue parsing the rest of the path
- Handle multiple type argument segments

#### 3. AST Considerations:
- `PathCall` node already exists and seems appropriate
- May need to add type arguments field to `PathCall` if not already handled

#### 4. Integration with SEM's Type System:
- `PathCall` should resolve to appropriate static method
- Type arguments need to be passed to type checker
- Error messages should reference correct line/column

## Code Skeleton

### 1. Enhanced Path Parsing Function:

```rust
fn parse_path_with_type_args(input: &str) -> IResult<&str, (Vec<String>, Vec<Vec<String>>)> {
    // Parse path segments separated by ::
    // Also parse type arguments at each segment
    // Returns (path_segments, type_args_per_segment)
}
```

### 2. Updated `parse_path_expr`:

```rust
fn parse_path_expr(input: &str) -> IResult<&str, AstNode> {
    // Parse path with potential type arguments
    let (input, (path_segments, type_args_list)) = parse_path_with_type_args(input)?;
    
    // Check for method call
    let (input, args_opt) = opt(delimited(
        ws(tag("(")),
        terminated(
            separated_list0(ws(tag(",")), ws(parse_expr)),
            opt(ws(tag(","))),
        ),
        ws(tag(")")),
    ))
    .parse(input)?;

    if let Some(args) = args_opt {
        // Static method call
        // Split into path and method (last segment is method name)
        if path_segments.len() >= 2 {
            let (method_path, method_name) = path_segments.split_at(path_segments.len() - 1);
            Ok((
                input,
                AstNode::PathCall {
                    path: method_path.to_vec(),
                    method: method_name[0].clone(),
                    args,
                    // May need to add type_args field
                },
            ))
        } else {
            // Single segment - function call
            Ok((
                input,
                AstNode::Call {
                    receiver: None,
                    method: path_segments.join("::"),
                    args,
                    type_args: vec![], // Handle type arguments
                    structural: false,
                },
            ))
        }
    } else {
        // Not a call - could be struct literal or variable
        // ... existing code ...
    }
}
```

### 3. Error Recovery:

```rust
fn parse_static_method_call(input: &str) -> IResult<&str, AstNode> {
    // Try to parse Type::method()
    // If fails with specific error, provide helpful message
    // "Expected method name after '::'"
    // "Expected '(' after method name"
}
```

## Test Cases to Implement:

1. Basic: `Point::new(10, 20)`
2. With type args: `Vec::<i32>::new()`
3. Nested: `std::collections::HashMap::new()`
4. Multiple type args: `HashMap::<K, V>::new()`
5. Chained: `Vec::new().push(1)`
6. Error cases: `Type::`, `Type::(args)`

## Integration Points:

1. **Parser**: `parse_path_expr` → `PathCall`
2. **Resolver**: Look up static method by qualified name
3. **Type Checker**: Verify type arguments match method signature
4. **Codegen**: Generate appropriate function call

## Backward Compatibility:

- Existing `PathCall` usage should continue to work
- Instance method calls (`expr.method()`) unchanged
- Module function calls (`module::function()`) should still work

## Deliverables:

1. Enhanced `parse_path_expr` function
2. Test suite for static method calls
3. Updated error messages
4. Documentation of changes

## Timeline:
- 08:47-09:00: Analysis complete
- 09:00-09:15: Implement core parsing logic
- 09:15-09:25: Add tests and error handling
- 09:25-09:30: Final review and report

## Coordination with SEM:
- Need to know how type arguments should be represented in AST
- Need to know resolution rules for static methods
- Need error message format preferences