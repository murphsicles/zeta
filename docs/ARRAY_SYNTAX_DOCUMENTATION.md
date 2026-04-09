# Array Syntax Documentation

## New Feature: Array Repeat Syntax

The Zeta compiler now supports array repeat syntax: `[value; size]`

### Syntax
```zeta
// Create an array with 4 elements, all initialized to 0
let arr1 = [0; 4];  // Equivalent to [0, 0, 0, 0]

// Create a boolean array for prime sieve
let sieve = [true; 100];

// With variable size
let size = 10;
let arr2 = [0; size];
```

### Multi-dimensional Arrays
```zeta
// Create a 3x4 matrix initialized to 0
let matrix = [[0; 4]; 3];  // 3 rows, 4 columns

// Equivalent to:
// [
//   [0, 0, 0, 0],
//   [0, 0, 0, 0],
//   [0, 0, 0, 0]
// ]
```

### PrimeZeta Compatibility

The new syntax is fully compatible with PrimeZeta requirements:

1. **Prime sieve pattern**: `[true; limit]` now works
2. **Array initialization**: `[0; SIZE]` now parses correctly
3. **Multi-dimensional arrays**: Nested repeat syntax works

### Example: Prime Sieve Implementation

```zeta
comptime fn prime_sieve(limit: i64) -> [limit]bool {
    let mut sieve = [true; limit];
    sieve[0] = false;
    sieve[1] = false;
    
    for i in 2..limit {
        if sieve[i] {
            let mut j = i * i;
            while j < limit {
                sieve[j] = false;
                j += i;
            }
        }
    }
    sieve
}
```

### AST Representation

The new syntax creates an `ArrayRepeat` AST node:
```rust
AstNode::ArrayRepeat {
    value: Box<AstNode>,  // The value to repeat
    size: Box<AstNode>,   // The number of repetitions
}
```

### Backward Compatibility

The existing array literal syntax continues to work:
```zeta
let arr1 = [1, 2, 3];  // Regular array literal
let arr2 = [];         // Empty array
let arr3 = [true, false, true];  // Boolean array
```

### Error Messages

Clear error messages are provided for malformed syntax:
- Missing semicolon: `[0 4]` → Error: expected ';'
- Missing size: `[0;]` → Error: expected expression
- Missing value: `[; 4]` → Error: expected expression

### Performance

Array repeat syntax is optimized for:
- Compile-time evaluation when size is constant
- Efficient initialization of large arrays
- Type checking to ensure value type matches array element type

### Integration with Type System

The syntax works with Zeta's type system:
- Type inference: `[0; 4]` has type `[4]i64`
- Generic compatibility: Works with any type that can be repeated
- Compile-time size checking when possible