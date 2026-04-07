# Float Literal Edge Case Analysis

## Overview
Analysis of edge cases for float literal parsing and processing in Zeta v0.3.8/v0.3.9.

## Current Implementation (v0.3.8)
- **Format**: `digits.digits` (basic decimal notation)
- **Parser**: Manual float parser in `src/frontend/parser/expr.rs`
- **AST**: `FloatLit(String)` variant
- **Limitations**: No scientific notation, no special values

## Edge Cases to Test

### 1. Basic Decimal Notation

#### Valid Cases
```
3.14
0.5
123.456
.5        # Should this be allowed? (leading decimal)
5.        # Should this be allowed? (trailing decimal)
0.0
1.0
```

#### Invalid Cases
```
.         # Just decimal point
.         # Just decimal point with whitespace
3.        # Trailing decimal only (currently invalid)
.14       # Leading decimal only (currently invalid)
3..14     # Multiple decimal points
3.1.4     # Multiple decimal points
```

### 2. Zero Handling

#### Special Zero Cases
```
0.0
-0.0      # Negative zero (if supported)
+0.0      # Explicit positive zero (if supported)
0.00000000000000000000000000000000000000000000000000  # Many zeros
```

### 3. Large/Small Values

#### Boundary Cases
```
999999999999999.999999999999999  # Large integer + fraction
0.000000000000000000000000000001  # Very small
123456789012345.123456789012345   # Precision limits
```

### 4. Leading/Trailing Zeros

#### Valid but Tricky
```
001.100    # Leading zeros in integer part
1.1000     # Trailing zeros in fractional part
000.000    # All zeros
00.00      # Multiple zeros
```

### 5. Future Scientific Notation (v0.3.9+)

#### Expected Format
```
1.23e4     # 1.23 × 10^4
1.23e+4    # Explicit positive exponent
1.23e-4    # Negative exponent
1.23E4     # Capital E
0.5e0      # Exponent zero
```

#### Edge Cases for Scientific
```
1e5        # No decimal point
.e5        # No digits before/after decimal
1.23e      # No exponent digits
1.23e+     # No exponent digits after sign
1.23e-     # No exponent digits after sign
1.23e12345678901234567890  # Very large exponent
```

### 6. Special Float Values

#### IEEE 754 Special Values (Future)
```
NaN        # Not a Number
Infinity   # Positive infinity
-Infinity  # Negative infinity
+Infinity  # Explicit positive infinity
```

#### Denormalized Numbers (Future)
```
1e-308     # Near underflow
1e-324     # Denormal range
```

### 7. Locale/Format Issues

#### Decimal Separator
- **Standard**: `.` (period)
- **Alternative**: `,` (comma) - should reject
- **Mixed**: `1,234.56` vs `1.234,56`

#### Thousands Separator
```
1,000.5    # With comma thousands separator
1 000.5    # With space thousands separator
1_000.5    # With underscore separator (common in programming)
```

### 8. Type System Integration

#### Type Inference Edge Cases
```zeta
// Mixed integer/float operations
let x = 3.14 + 1      # Should this promote 1 to float?
let y = 1 + 3.14      # Same question

// Float literals in const expressions
const PI: f64 = 3.141592653589793;
const HALF: f64 = 0.5;

// Array of floats
let arr: [f64; 3] = [1.0, 2.0, 3.0];

// Function parameters/return
fn add(x: f64, y: f64) -> f64 { x + y }
```

### 9. Parser Recovery

#### Error Recovery Cases
```
3.14 +     # Float followed by incomplete expression
3.14 "string"  # Float followed by string
3.14, 2.71     # Float in list context
3.14)          # Float before closing paren
```

### 10. Performance Considerations

#### Many Float Literals
```zeta
// Many consecutive float literals
let x = 1.1 + 2.2 + 3.3 + 4.4 + 5.5 + 6.6 + 7.7 + 8.8 + 9.9;

// Nested expressions with floats
let y = (1.1 * (2.2 + (3.3 / (4.4 - 5.5))));
```

## Test Strategy

### 1. Unit Tests
- Individual parser function tests
- Type inference tests for float literals
- Const evaluation tests

### 2. Property Tests
- Round-trip: parse → display → parse should be equal
- Valid floats always parse successfully
- Invalid floats always rejected
- Type consistency: float literals get float types

### 3. Integration Tests
- Complete compilation of programs with floats
- Mixed integer/float expressions
- Float literals in all language contexts

### 4. Fuzz Tests
- Random byte streams to parser
- Valid float strings with random mutations
- Stress testing with many/large floats

## Implementation Notes

### Current Parser Limitations
1. No scientific notation support
2. No special values (NaN, Infinity)
3. Strict `digits.digits` format
4. No sign support (positive/negative)

### Recommendations for v0.3.9
1. Add scientific notation (`1.23e-4`)
2. Consider sign support (`+3.14`, `-3.14`)
3. Add basic special values (`NaN`, `Infinity`)
4. Improve error messages for invalid floats

## Test Cases to Implement

### Immediate (v0.3.8)
```rust
// test_valid_floats()
assert_parses("3.14");
assert_parses("0.5");
assert_parses("123.456");
assert_parses("0.0");

// test_invalid_floats()
assert_rejects(".");
assert_rejects("3.");
assert_rejects(".14");
assert_rejects("3..14");
```

### Future (v0.3.9+)
```rust
// test_scientific_notation()
assert_parses("1.23e4");
assert_parses("1.23e-4");
assert_parses("1E5");

// test_special_values()
assert_parses("NaN");
assert_parses("Infinity");
assert_parses("-Infinity");
```

## Success Criteria

### v0.3.8
- ✅ All valid basic floats parse correctly
- ✅ All invalid floats rejected with clear errors
- ✅ Float literals receive correct type (`f64`)
- ✅ Basic float operations work

### v0.3.9
- ✅ Scientific notation supported
- ✅ Better error messages
- ✅ Performance acceptable for typical use
- ✅ No regressions in existing functionality

---

**Verified by:** ✅ VER (Verification Master)  
**Date:** 2026-03-27  
**Status:** Analysis complete, tests needed