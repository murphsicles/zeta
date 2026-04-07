# Const Generics Design for Zeta

## Overview
Extend Zeta's type system to support const generic parameters, enabling:
```rust
fn sieve<const N: usize>() -> [bool; N]
```

## Design Goals
1. Support const parameters in generic declarations
2. Allow const expressions in array sizes
3. Integrate with compile-time function execution (CTFE)
4. Maintain backward compatibility

## Type System Extensions

### 1. New Generic Parameter Type
Extend `GenericParam` enum:
```rust
enum GenericParam {
    Type { name: String, kind: Kind, bounds: Vec<AssocTraitBound> },
    Lifetime { name: String },
    Const { name: String, ty: Type, value: Option<ConstValue> },
}
```

### 2. Const Value Representation
```rust
enum ConstValue {
    Int(i64),
    UInt(u64),
    Bool(bool),
    Char(char),
    Array(Vec<ConstValue>),
    Struct(HashMap<String, ConstValue>),
    // Could add more as needed
}
```

### 3. Extended Type Representation
Modify array type to support const expressions:
```rust
enum Type {
    // Current: Array(Box<Type>, usize)
    // New: Array(Box<Type>, ConstExpr)
    Array(Box<Type>, ConstExpr),
    // ...
}

enum ConstExpr {
    Literal(ConstValue),
    Variable(String), // const parameter name
    BinaryOp(Box<ConstExpr>, BinOp, Box<ConstExpr>),
    UnaryOp(UnOp, Box<ConstExpr>),
    Call(String, Vec<ConstExpr>), // CTFE function call
}
```

## Compile-Time Evaluation (CTFE)

### 1. CTFE Engine
- Evaluate const expressions during type checking
- Support basic arithmetic, logical operations
- Limited function execution (no side effects)

### 2. Const Functions
```rust
comptime fn square(x: i64) -> i64 {
    x * x
}

// Usage in type:
fn foo<const N: i64>() -> [i32; square(N)]
```

## Type Checking Changes

### 1. Const Parameter Resolution
- During type instantiation, substitute const parameters with values
- Verify const expressions evaluate to concrete values

### 2. Array Size Checking
- Array size must be compile-time known
- Can be: literal, const parameter, const expression
- Must evaluate to positive integer

### 3. Monomorphization
- Generate specialized versions for each const value
- Similar to type parameter monomorphization

## Syntax Extensions

### 1. Const Generic Parameters
```rust
fn foo<const N: usize, T>() -> [T; N]
struct Bar<const SIZE: usize> { data: [u8; SIZE] }
```

### 2. Const Expressions in Types
```rust
type Matrix<const N: usize> = [[f64; N]; N];
fn identity<const N: usize>() -> Matrix<N>
```

### 3. CTFE Annotations
```rust
comptime fn factorial(n: i64) -> i64 {
    if n <= 1 { 1 } else { n * factorial(n - 1) }
}
```

## Implementation Phases

### Phase 1: Basic Const Parameters
1. Extend `GenericParam` with const variant
2. Add const parameter handling to type checker
3. Support literal const values in array sizes

### Phase 2: Const Expressions
1. Implement `ConstExpr` AST node
2. Basic CTFE for arithmetic expressions
3. Const variable substitution

### Phase 3: CTFE Functions
1. `comptime` function annotation
2. Limited function evaluation
3. Recursive CTFE support

### Phase 4: Advanced Features
1. Const generics in structs/enums
2. Const trait bounds
3. Const-dependent types beyond arrays

## Murphy's Sieve Solution

With this design, Murphy's Sieve becomes:
```rust
comptime fn sieve<const LIMIT: usize>() -> [bool; LIMIT] {
    let mut sieve = [true; LIMIT];
    // ... sieve algorithm
    sieve
}

// Usage:
let primes_up_to_1000 = sieve::<1000>();
```

## Alternative: Runtime Array Wrapper

If full const generics are too complex, consider:
```rust
struct RuntimeArray<T> {
    data: Vec<T>,
    size: usize,
}

impl<T> RuntimeArray<T> {
    fn new(size: usize) -> Self {
        Self {
            data: Vec::with_capacity(size),
            size,
        }
    }
    
    // Implement Index, IndexMut for array-like syntax
}

fn sieve(limit: usize) -> RuntimeArray<bool> {
    let mut arr = RuntimeArray::new(limit);
    // ... algorithm
    arr
}
```

## Complexity Assessment

### Low Complexity
- Basic const parameters with literals
- Simple CTFE (arithmetic only)

### Medium Complexity  
- Full const expressions
- CTFE function calls
- Const generics in structs

### High Complexity
- Dependent types beyond const generics
- Full CTFE with loops, conditionals
- Const trait bounds

## Recommendation

Start with Phase 1 (basic const parameters) to unblock Murphy's Sieve:
1. Allow `fn sieve<const N: usize>() -> [bool; N]`
2. Require `N` to be literal or const variable
3. Monomorphize for each `N`

This provides immediate value while building toward more advanced features.