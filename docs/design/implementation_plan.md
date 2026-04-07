# Implementation Plan: Const Generics for Zeta

## Current State Analysis

### Type System (`src/middle/types/mod.rs`)
- `Type::Array(Box<Type>, usize)` - fixed size array
- `GenericContext` manages type parameters
- `GenericParam` in `associated.rs` only supports type/lifetime params

### Parser (`src/frontend/`)
- AST has `ArrayLit` and `ArrayRepeat` nodes
- Type parsing in `Type::from_string()` handles `[T; N]` syntax

### Type Checker (`src/middle/resolver/typecheck.rs`)
- Basic type checking
- No const expression evaluation

## Required Changes

### Phase 1: Extend Generic Parameters

#### 1.1 Update `GenericParam` enum (`src/middle/types/associated.rs`)
```rust
enum GenericParam {
    Type { name: String, kind: Kind, bounds: Vec<AssocTraitBound> },
    Lifetime { name: String },
    Const { name: String, ty: Type, default: Option<ConstValue> },
}
```

#### 1.2 Add `ConstValue` type (`src/middle/types/const_value.rs`)
```rust
pub enum ConstValue {
    Int(i64),
    UInt(u64),
    Bool(bool),
    Char(char),
    // Add more as needed
}
```

#### 1.3 Add `ConstExpr` type (`src/middle/types/const_expr.rs`)
```rust
pub enum ConstExpr {
    Literal(ConstValue),
    Variable(String),
    BinaryOp(Box<ConstExpr>, BinOp, Box<ConstExpr>),
    // etc.
}
```

### Phase 2: Update Array Type Representation

#### 2.1 Modify `Type` enum (`src/middle/types/mod.rs`)
```rust
enum Type {
    // Change from: Array(Box<Type>, usize)
    // To: Array(Box<Type>, ArraySize)
    Array(Box<Type>, ArraySize),
    // ...
}

enum ArraySize {
    Literal(usize),
    ConstParam(String),
    Expr(Box<ConstExpr>),
}
```

#### 2.2 Update type parsing
- Extend `Type::from_string()` to parse const expressions
- Support `[T; N]`, `[T; const_expr]`, `[T; const_param]`

### Phase 3: Const Evaluation Engine

#### 3.1 Create `ConstEvaluator` (`src/middle/const_eval.rs`)
- Evaluate const expressions
- Support basic arithmetic, logical ops
- Track const variable values

#### 3.2 Integrate with type checking
- During type checking, evaluate array sizes
- Verify const expressions are compile-time known

### Phase 4: Function Instantiation

#### 4.1 Extend function signature handling
- Store const parameters in function signatures
- Instantiate with const arguments

#### 4.2 Monomorphization support
- Generate specialized versions for const values
- Similar to type parameter monomorphization

### Phase 5: Syntax Support

#### 5.1 Parser updates
- Parse `fn foo<const N: usize>()` syntax
- Parse const expressions in array sizes

#### 5.2 AST updates
- Add const generic parameters to `FuncDef`, `StructDef`, etc.
- Add const expression nodes

## Murphy's Sieve Integration

### Target Syntax
```rust
comptime fn sieve<const LIMIT: usize>() -> [bool; LIMIT] {
    let mut sieve = [true; LIMIT];
    // ... algorithm
    sieve
}
```

### Implementation Steps
1. Parse `comptime` keyword and const generics
2. Type check with const parameter `LIMIT`
3. Evaluate `[true; LIMIT]` array literal
4. Generate monomorphized version for each `LIMIT` value

## Complexity Assessment

### Easy Changes (1-2 days)
- Add `ConstValue` and `ConstExpr` types
- Extend `GenericParam` enum
- Basic const evaluation

### Medium Changes (3-5 days)
- Update `Type::Array` representation
- Modify type parsing
- Integrate const evaluation with type checking

### Hard Changes (1-2 weeks)
- Full parser support for const generics
- Monomorphization with const parameters
- CTFE for complex expressions

## Alternative Approaches

### Option A: Minimal Const Support
- Only support literal const values
- `fn sieve<const N: usize>() -> [bool; N]` where N must be literal
- Simplest to implement

### Option B: Const Expressions
- Support basic arithmetic expressions
- `fn foo<const N: usize>() -> [bool; N * 2]`
- More flexible but more complex

### Option C: Full CTFE
- `comptime` functions
- Complex const expressions
- Most powerful but most complex

## Recommendation

**Start with Option A (Minimal Const Support)** for Murphy's Sieve:
1. Support `fn sieve<const N: usize>() -> [bool; N]`
2. Require `N` to be literal constant
3. Simple monomorphization

This unblocks the immediate need while building foundation for more advanced features.

## Testing Strategy

1. Unit tests for const evaluation
2. Integration tests for const generics
3. Murphy's Sieve as end-to-end test
4. Regression tests for existing functionality

## Risk Mitigation

1. **Backward Compatibility**: Ensure existing code still works
2. **Incremental Implementation**: Add features gradually
3. **Fallback Mechanism**: If const evaluation fails, provide clear error
4. **Testing**: Comprehensive test suite

## Timeline Estimate

- Week 1: Basic const value support
- Week 2: Array size with const parameters  
- Week 3: Parser and syntax support
- Week 4: Murphy's Sieve integration and testing

This provides a working solution for the PrimeZeta competition within a month.