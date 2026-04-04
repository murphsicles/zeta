# Test Plan for Array Return Type Fix

## Changes Made:

1. **Added `complete` combinator import** to parser.rs
2. **Wrapped `parse_primezeta_array` with `complete()`** to ensure it doesn't consume input on partial failure
3. **Updated `parse_ident` to reject built-in types** like `u64`, `i64`, etc.

## Expected Behavior:

### Test Case 1: `[u64; 10]` (Zeta style)
1. `parse_array_type` is called with `"[u64; 10]"`
2. It tries `complete(parse_primezeta_array)` first
3. `parse_primezeta_array` parses `[`
4. Tries to parse size: `digit1` fails (not a digit), `parse_ident` fails (`u64` is now rejected)
5. `parse_primezeta_array` fails without consuming `u64` (thanks to `complete`)
6. Falls back to Zeta style parsing
7. Parses `[`
8. Calls `parse_type` for element type: parses `u64`
9. Parses `; 10]`
10. Returns `"[u64; 10]"`

### Test Case 2: `[NUM_RESIDUES]u64` (PrimeZeta style)
1. `parse_array_type` is called with `"[NUM_RESIDUES]u64"`
2. `complete(parse_primezeta_array)` succeeds
3. Parses `[`
4. Parses size: `digit1` fails, `parse_ident` succeeds with `"NUM_RESIDUES"`
5. Parses `]`
6. Calls `parse_type` for element type: parses `u64`
7. Returns `"[u64; NUM_RESIDUES]"`

### Test Case 3: `[10]i64` (PrimeZeta style)
1. `parse_array_type` is called with `"[10]i64"`
2. `complete(parse_primezeta_array)` succeeds
3. Parses `[`
4. Parses size: `digit1` succeeds with `"10"`
5. Parses `]`
6. Calls `parse_type` for element type: parses `i64`
7. Returns `"[i64; 10]"`

### Test Case 4: Function with array return type: `fn test() -> [u64; 10]`
1. `parse_func` is called
2. Parses function signature
3. When parsing return type, calls `parse_type` with `[u64; 10]`
4. `parse_type` tries alternatives, eventually calls `parse_array_type`
5. `parse_array_type` works as in Test Case 1
6. Returns success

## Potential Issues:

1. **Order of alternatives in `parse_type`**: `parse_array_type` comes before built-in types like `tag("u64")`. When parsing `u64` as an element type, `parse_type` will try `parse_array_type` first (which will fail quickly), then try `tag("u64")`. This is inefficient but should work.

2. **Recursive calls**: `parse_array_type` calls `parse_type` for element type, and `parse_type` includes `parse_array_type` as an alternative. This could cause infinite recursion if not careful, but should be fine because `parse_array_type` starts with `[`, and we're parsing `u64` which doesn't start with `[`.

3. **Whitespace handling**: The `ws()` wrapper should handle whitespace correctly.

## Verification:

The fix should resolve the issue where `comptime fn generate_residues() -> [u64; NUM_RESIDUES]` fails to parse.