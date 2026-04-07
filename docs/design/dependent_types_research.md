# Dependent Types Research for Murphy's Sieve

## Problem Statement
We need to support function signatures like:
```rust
fn sieve(limit: usize) -> [bool; limit]
```

This requires dependent types because:
1. The return type `[bool; limit]` depends on the value `limit`
2. `limit` is a runtime value (function parameter), not a compile-time constant
3. Array sizes in Rust/Zeta must be known at compile time

## Options Analysis

### 1. Basic Dependent Type Support
Add values in types:
- Extend type system to allow value-dependent types
- Requires tracking values through type system
- Complex type inference and checking

### 2. Type-Level Integers with Const Generics (Rust approach)
```rust
fn sieve<const N: usize>() -> [bool; N]
```
- Requires `N` to be known at compile time
- Can't use runtime `limit` parameter directly
- Would need to generate monomorphized versions for each `limit`

### 3. Type-Level Natural Numbers
Create a Peano arithmetic system:
```rust
trait Nat {}
struct Z; // Zero
struct S<N: Nat>(PhantomData<N>); // Successor

fn sieve<N: Nat>() -> Array<bool, N>
```
- More expressive but complex
- Requires converting runtime values to type-level

### 4. Heap Allocation Alternative
```rust
fn sieve(limit: usize) -> Vec<bool>
```
- Simplest solution
- Avoids dependent types entirely
- But loses compile-time size guarantees

## Rust's Const Generics Implementation

Rust's approach:
- `const` parameters in generics: `fn foo<const N: usize>()`
- Values must be compile-time constants
- Can't use runtime values
- Monomorphization creates separate versions for each `N`

## Zig's Approach

Zig has compile-time function execution (CTFE):
```zig
fn sieve(comptime limit: usize) [limit]bool
```
- `comptime` parameters must be known at compile time
- Can call with compile-time known values
- Still can't use runtime values

## Idris/Agda (Full Dependent Types)

True dependent types:
```idris
sieve : (limit : Nat) -> Vect limit Bool
```
- Values are first-class in types
- Requires sophisticated type checker
- Complex but most expressive

## Minimal Dependent Type System Design

For Zeta, we could implement a limited form:

### Option A: Const-dependent arrays
- Allow `[T; expr]` where `expr` is a compile-time evaluable expression
- Extend CTFE to handle array size expressions
- Type checking: verify `expr` evaluates to integer constant

### Option B: Dependent function types
- `fn(limit: usize) -> [bool; limit]`
- Treat `limit` as a value parameter in return type
- Requires tracking value flow through type system

### Option C: Hybrid approach
- Use const generics for known sizes
- Use heap allocation for dynamic sizes
- Provide conversion between them

## Implementation Complexity Assessment

1. **Const Generics**: Medium complexity
   - Extend generic parameter system
   - Add const parameter support
   - Modify type checking and monomorphization

2. **CTFE-based**: High complexity  
   - Implement full compile-time evaluation
   - Track constness through expressions
   - Integrate with type system

3. **True Dependent Types**: Very high complexity
   - Complete type system overhaul
   - Value-dependent typing rules
   - Complex inference and checking

4. **Heap Allocation**: Low complexity
   - Just use `Vec<bool>` or similar
   - But changes algorithm semantics

## Recommendation

Given the constraints and Murphy's Sieve requirements:

**Short-term**: Implement const generics with CTFE
- Allow `fn sieve<const N: usize>() -> [bool; N]`
- Use CTFE to allow `N` to be computed from expressions
- User writes `sieve::<1000>()` instead of `sieve(1000)`

**Medium-term**: Add limited dependent types
- Allow value parameters in array sizes
- Restrict to compile-time evaluable expressions
- Gradually expand expressiveness

**Long-term**: Consider full dependent types if needed

## Next Steps

1. Examine current const handling in Zeta
2. Design const generic extension
3. Prototype implementation
4. Test with Murphy's Sieve pattern