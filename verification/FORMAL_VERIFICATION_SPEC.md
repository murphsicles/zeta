# FORMAL VERIFICATION SYSTEM SPECIFICATION - Zeta v0.4.0

## Overview
This document defines the formal verification system for Zeta, enabling mathematically verified programs at compile time. The system combines refinement types with SMT solving to prove algorithm correctness.

## 1. Design Philosophy

### Verified by Construction
- **Compile-time proofs**: Verification happens during compilation
- **Zero runtime overhead**: Proofs are erased after verification
- **Gradual verification**: Programs can mix verified and unverified code
- **Practical usability**: Simple annotations for common cases

### Mathematical Foundations
- **Refinement Types**: `{x: T | P(x)}` where P is a logical predicate
- **Verification Conditions**: Logical formulas generated from code
- **SMT Solving**: Use Z3 to discharge proof obligations
- **Soundness**: Verified programs are guaranteed correct

## 2. System Architecture

### 2.1 Type System Extension
```
ExtendedType = BaseType
             | RefinedType { variable: BaseType | predicate }
             | DependentType (x: Type) -> Type
             
BaseType = u8 | u16 | u32 | u64 | i8 | i16 | i32 | i64 
         | bool | string | [Type] | (Type, Type, ...)
```

### 2.2 Verification Pipeline
```
Source Code 
    → Parse + AST
    → Type Check + VC Generation
    → SMT Solver (Z3)
    → Proof Results
    → Code Generation (if all proofs pass)
```

### 2.3 Annotation Syntax
```zeta
// Function contracts
fn sqrt(x: {n: u64 | n >= 0}) -> {r: u64 | r * r <= x && (r + 1) * (r + 1) > x} {
    // Implementation
}

// Loop invariants
fn sum(arr: [u64]) -> u64 {
    let mut total: u64 = 0;
    let mut i: u64 = 0;
    while i < arr.len() {
        // @invariant total == sum(arr[0..i])
        total += arr[i];
        i += 1;
    }
    return total;
}

// Assertions
fn divide(a: u64, b: {n: u64 | n > 0}) -> u64 {
    // @assert b != 0  // Already enforced by refinement type
    return a / b;
}
```

## 3. Murphy's Sieve Verification

### 3.1 Correctness Properties
1. **Soundness**: No composite numbers marked as prime
   ```zeta
   ∀n ∈ [0, limit). bits[n] == 0 → is_prime(n)
   ```

2. **Completeness**: All primes are marked as prime
   ```zeta
   ∀n ∈ [0, limit). is_prime(n) → bits[n] == 0
   ```

3. **Count Accuracy**: Prime count matches mathematical definition
   ```zeta
   count == |{n ∈ [0, limit) | is_prime(n)}|
   ```

### 3.2 Annotated Implementation
```zeta
// Verified Murphy's Sieve
fn murphy_sieve(limit: {n: u64 | n >= 2}) -> 
    {count: u64 | count == prime_count(limit)} 
{
    let bits: [dynamic]u8 = [dynamic]u8{};
    
    // Initialize array
    let mut i: u64 = 0;
    while i < limit {
        bits.push(0);
        i += 1;
    }
    
    // Mark 0 and 1 as composite
    bits[0] = 1;
    bits[1] = 1;
    
    // Sieve with loop invariant
    let mut p: u64 = 2;
    while p * p < limit {
        // @invariant ∀k ∈ [2, p). ∀m ∈ [k*k, limit). bits[m] == 1 → ¬is_prime(m)
        if bits[p] == 0 {
            let mut multiple: u64 = p * p;
            while multiple < limit {
                // @invariant ∀m ∈ [p*p, multiple). bits[m] == 1
                bits[multiple] = 1;
                multiple += p;
            }
        }
        p += 1;
    }
    
    // Count primes with invariant
    let mut count: u64 = 0;
    let mut n: u64 = 0;
    while n < limit {
        // @invariant count == |{k ∈ [0, n) | bits[k] == 0}|
        if bits[n] == 0 {
            count += 1;
        }
        n += 1;
    }
    
    return count;
}
```

## 4. Implementation Plan

### Phase 1: Foundation (2 hours)
1. **Refinement Type Parser** (30 min)
   - Extend AST with refinement types
   - Parse `{x: T | P(x)}` syntax
   
2. **VC Generator** (1 hour)
   - Generate verification conditions from annotations
   - Handle function contracts, loop invariants, assertions
   
3. **Z3 Integration** (30 min)
   - Interface with Z3 solver
   - Translate VCs to SMT-LIB2 format

### Phase 2: Murphy's Sieve Verification (1 hour)
1. **Annotate Murphy's Sieve** (20 min)
   - Add refinement types and invariants
   
2. **Generate and Prove VCs** (30 min)
   - Generate VCs for sieve correctness
   - Verify with Z3
   
3. **Integration Testing** (10 min)
   - Test with existing Murphy's Sieve implementations

### Phase 3: Polish and Documentation (1 hour)
1. **Error Reporting** (20 min)
   - Clear error messages for failed proofs
   
2. **Documentation** (20 min)
   - User guide for verification annotations
   
3. **Examples** (20 min)
   - Additional verified examples

## 5. Technical Details

### 5.1 Z3 Integration
```rust
use z3::{Config, Context, Solver, SatResult};

struct Verifier {
    context: Context,
    solver: Solver,
}

impl Verifier {
    fn new() -> Self {
        let config = Config::new();
        let context = Context::new(&config);
        let solver = Solver::new(&context);
        Self { context, solver }
    }
    
    fn verify(&mut self, vc: VerificationCondition) -> bool {
        // Convert VC to Z3 assertions
        let assertions = self.encode_vc(vc);
        
        // Add to solver
        for assertion in assertions {
            self.solver.assert(&assertion);
        }
        
        // Check satisfiability
        match self.solver.check() {
            SatResult::Sat => true,    // VC is provable
            SatResult::Unsat => false, // VC is false
            SatResult::Unknown => false, // Can't determine
        }
    }
}
```

### 5.2 VC Generation Rules
1. **Function Contract**:
   ```
   fn f(x: {x: T | P(x)}) -> {y: U | Q(x, y)}
   ```
   Generates VCs:
   - Precondition: `P(x)` at call sites
   - Postcondition: `Q(x, return_value)` in function body

2. **Loop Invariant**:
   ```
   while cond {
       // @invariant I
       body
   }
   ```
   Generates VCs:
   - Init: `I` holds before loop
   - Preservation: `I ∧ cond → I'` after body
   - Post: `I ∧ ¬cond` after loop

3. **Assertion**:
   ```
   // @assert P
   ```
   Generates VC: `P` must hold at that point

## 6. Success Criteria

### Functional
- ✅ Refinement types parse and type check
- ✅ VC generation for all annotation types
- ✅ Z3 integration works correctly
- ✅ Murphy's Sieve verification passes
- ✅ Clear error messages for failed proofs

### Performance
- ✅ Verification time < 1 second for small programs
- ✅ No impact on compilation time for unverified code
- ✅ Memory usage reasonable

### Usability
- ✅ Simple annotation syntax
- ✅ Good documentation
- ✅ Helpful error messages
- ✅ Gradual adoption path

## 7. Future Extensions

### Short-term
1. **Dependent Types**: `(n: u64) -> [u64; n]`
2. **Ghost Variables**: For specification only
3. **Lemma Functions**: Reusable proofs

### Medium-term
1. **Proof Automation**: Auto-generate invariants
2. **Standard Library Verification**: Verified stdlib
3. **IDE Integration**: Live verification feedback

### Long-term
1. **Full Dependent Types**: Idris/Agda-like
2. **Proof Extraction**: Extract proofs to Coq/Lean
3. **Verified Compiler**: Self-verified Zeta compiler

## 8. Testing Strategy

### Unit Tests
- Parser tests for refinement types
- VC generation tests
- Z3 integration tests

### Integration Tests
- Murphy's Sieve verification
- Other algorithm verifications
- Edge case handling

### Property Tests
- Round-trip: parse → verify → compile
- Soundness: verified code behaves correctly
- Completeness: correct code can be verified

---

**Implementation Start**: 2026-04-04 00:48 GMT+1  
**Target Completion**: 2026-04-04 04:48 GMT+1  
**Goal**: Make Zeta the first systems language with built-in formal verification