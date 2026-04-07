# Const Parsing Edge Case Analysis

## Overview
Analysis of edge cases for constant parsing in Zeta v0.3.8/v0.3.9. Const parsing is critical for v0.3.7 source compatibility and bootstrap progress.

## Current Implementation (v0.3.8)
- **Syntax**: `const NAME: TYPE = EXPR;`
- **AST**: `ConstDef` variant
- **Scope**: Global, compile-time evaluation
- **Types**: Integer, boolean, string, computed expressions

## Edge Cases to Test

### 1. Basic Const Definitions

#### Valid Cases
```zeta
// Simple integer
const MAX_SIZE: i64 = 4096;

// Boolean
const DEBUG: bool = true;

// String
const APP_NAME: &str = "Zeta Compiler";

// Computed expression
const TIMEOUT: i64 = 30 * 1000;

// Using other constants
const HEADER: i64 = 128;
const TOTAL: i64 = MAX_SIZE + HEADER;
```

#### Invalid Cases
```zeta
// Missing type annotation
const X = 5;

// Missing value
const Y: i64;

// Invalid type
const Z: invalid_type = 5;

// Invalid expression
const W: i64 = undefined();

// Duplicate name
const X: i64 = 1;
const X: i64 = 2;  // Redefinition
```

### 2. Type Annotations

#### Complex Types
```zeta
// Array type
const BUFFER: [u8; 1024] = [0; 1024];

// Tuple type
const ORIGIN: (i64, i64) = (0, 0);

// Reference type
const GREETING: &str = "Hello";

// Function pointer (future)
const HANDLER: fn(i64) -> i64 = some_function;
```

#### Generic Types (Future)
```zeta
// With type parameters
const DEFAULT: Option<i64> = None;

// Multiple parameters
const RESULT: Result<i64, String> = Ok(42);
```

### 3. Expression Complexity

#### Arithmetic Expressions
```zeta
// Basic arithmetic
const A: i64 = 1 + 2 * 3 - 4 / 2;

// Parentheses
const B: i64 = (1 + 2) * (3 - 4);

// Bit operations
const FLAGS: i64 = 0x1 | 0x2 | 0x4;

// Shift operations
const SHIFTED: i64 = 1 << 8;
```

#### Boolean Expressions
```zeta
// Logical operations
const ENABLED: bool = true && !false;

// Comparisons
const VALID: bool = 5 > 3 && 10 <= 20;

// Complex boolean
const CHECK: bool = (x > 0) || (y < 0) && !(z == 0);
```

#### String Expressions
```zeta
// Concatenation (if supported)
const PATH: &str = "/home/" + "user";

// Escape sequences
const MESSAGE: &str = "Line 1\nLine 2\tTab";

// Raw strings (future)
const REGEX: &str = r"\d+";
```

### 4. Const Dependencies

#### Forward References
```zeta
// Forward reference (should this work?)
const B: i64 = A + 1;
const A: i64 = 5;

// Circular reference (should fail)
const X: i64 = Y + 1;
const Y: i64 = X * 2;
```

#### Mutual Dependencies
```zeta
// Mutual but not circular (tree structure)
const WIDTH: i64 = 800;
const HEIGHT: i64 = 600;
const AREA: i64 = WIDTH * HEIGHT;
```

### 5. Scope and Visibility

#### Module Boundaries
```zeta
// Public constant
pub const API_VERSION: i64 = 1;

// Private constant
const INTERNAL: i64 = 42;

// In different modules
mod config {
    pub const SETTING: i64 = 100;
}

// Usage across modules
use config::SETTING;
const USED: i64 = SETTING * 2;
```

#### Shadowing
```zeta
const X: i64 = 1;

fn test() {
    // Can local variables shadow consts?
    let X = 2;
    
    // Can parameters shadow consts?
    fn inner(X: i64) { }
}
```

### 6. Compile-Time Evaluation

#### Constant Folding
```zeta
// Should be evaluated at compile time
const EXPR: i64 = 2 + 3 * 4;  // = 14

// With function calls (if const fn)
const LEN: usize = "hello".len();

// Array size
const SIZE: usize = 10;
const ARR: [i64; SIZE] = [0; SIZE];
```

#### Evaluation Limits
```zeta
// Infinite loop at compile time (should fail)
const LOOP: i64 = {
    let mut x = 0;
    while true { x += 1; }
    x
};

// Stack overflow (should fail)
const RECURSIVE: i64 = factorial(1000000);
```

### 7. Error Cases

#### Type Mismatch
```zeta
// Value doesn't match type
const X: i64 = "string";

// Expression type mismatch
const Y: bool = 5 + 3;

// Array size mismatch
const Z: [i64; 3] = [1, 2];  // Missing element
```

#### Parse Errors
```zeta
// Missing semicolon
const X: i64 = 5

// Missing equals
const Y: i64 42;

// Extra tokens
const Z: i64 = 5 extra;

// Invalid identifier
const 123: i64 = 5;
```

### 8. Performance and Size

#### Large Constants
```zeta
// Large array
const BIG: [u8; 1_000_000] = [0; 1_000_000];

// Many constants
const C1: i64 = 1;
const C2: i64 = 2;
// ... hundreds of constants
const C1000: i64 = 1000;
```

#### Complex Computations
```zeta
// Heavy computation at compile time
const PRIME: i64 = find_nth_prime(10000);

// String processing
const PROCESSED: &str = process_large_string(original);
```

### 9. Integration with Other Features

#### With Functions
```zeta
// Const used in function
const DEFAULT: i64 = 100;

fn process(value: i64 = DEFAULT) { }

// Const calling const fn
const fn double(x: i64) -> i64 { x * 2 }
const RESULT: i64 = double(21);
```

#### With Structs/Enums
```zeta
// Const in struct definition
struct Config {
    version: i64 = CURRENT_VERSION,
}

// Const for enum discriminant
enum Status {
    Ok = SUCCESS_CODE,
    Err = ERROR_CODE,
}
```

#### With Match Statements
```zeta
const MODE: i64 = 2;

match value {
    MODE => println!("Match!"),
    _ => println!("No match"),
}
```

### 10. Future Extensions

#### Const Generics (Future)
```zeta
// Using constants as type parameters
struct Array<T, const N: usize> {
    data: [T; N],
}

const SIZE: usize = 10;
type MyArray = Array<i64, SIZE>;
```

#### Compile-Time Reflection (Future)
```zeta
// Getting const metadata
const NAME: &str = const_name!(MAX_SIZE);
const TYPE: &str = const_type!(MAX_SIZE);
const VALUE: &str = const_value!(MAX_SIZE);
```

## Test Strategy

### 1. Unit Tests
- Individual const parsing tests
- Type checking tests
- Evaluation tests

### 2. Property Tests
- Valid const definitions always parse
- Type annotations are checked
- Evaluation produces correct values
- No circular dependencies

### 3. Integration Tests
- Const usage in functions
- Cross-module const references
- Const in complex expressions

### 4. Fuzz Tests
- Random const definitions
- Stress testing with many constants
- Invalid input rejection

## Implementation Notes

### Current Limitations
1. Limited expression support in const values
2. No const function support
3. Simple type checking only
4. No compile-time evaluation beyond literals

### Recommendations for v0.3.9
1. Enhance expression support in const values
2. Add basic const function support
3. Improve type checking for consts
4. Better error messages for const errors

## Test Cases to Implement

### Immediate (v0.3.8)
```rust
// test_basic_consts()
assert_parses("const X: i64 = 5;");
assert_parses("const DEBUG: bool = true;");
assert_parses("const NAME: &str = \"test\";");

// test_const_expressions()
assert_parses("const SUM: i64 = 1 + 2 * 3;");
assert_parses("const FLAG: bool = true && false;");

// test_const_dependencies()
assert_parses("const A: i64 = 5; const B: i64 = A + 1;");
```

### Future (v0.3.9+)
```rust
// test_complex_consts()
assert_parses("const ARR: [i64; 3] = [1, 2, 3];");
assert_parses("const TUP: (i64, bool) = (5, true);");

// test_const_functions()
assert_parses("const fn double(x: i64) -> i64 { x * 2 }");
assert_parses("const RESULT: i64 = double(21);");
```

## Success Criteria

### v0.3.8
- ✅ Basic const definitions parse correctly
- ✅ Type annotations are validated
- ✅ Simple expressions supported
- ✅ v0.3.7 source consts are compatible

### v0.3.9
- ✅ Enhanced expression support
- ✅ Better type checking
- ✅ Const function support (basic)
- ✅ No regressions in existing functionality

---

**Verified by:** ✅ VER (Verification Master)  
**Date:** 2026-03-27  
**Status:** Analysis complete, tests needed