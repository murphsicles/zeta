---
name: formal-verification
description: Formal verification techniques for proving correctness of Rust code. Mathematical proofs, model checking, and specification-based verification.
metadata:
  {
    "openclaw":
      {
        "requires": { "bins": [] },
        "install": [],
      },
  }
---

# Formal Verification Skill

## Purpose
Prove correctness of code using mathematical methods. Go beyond testing to provide guarantees that code satisfies specifications.

## Core Concepts

### 1. **Formal Methods Spectrum**
```
Testing → Property Testing → Model Checking → Theorem Proving
(Least formal)                          (Most formal)
```

### 2. **Key Techniques**
- **Model Checking**: Exhaustively check all possible states
- **Theorem Proving**: Construct mathematical proofs of correctness
- **Abstract Interpretation**: Analyze program behavior without executing
- **Symbolic Execution**: Execute with symbolic values instead of concrete

### 3. **Rust-Specific Considerations**
- **Ownership system**: Formal verification of memory safety
- **Type system**: Proving type safety properties
- **Unsafe code**: Special verification requirements

## Mathematical Foundations

### 1. **Propositional Logic**
```rust
// Example: De Morgan's Laws
fn de_morgans_law(a: bool, b: bool) -> bool {
    // ¬(a ∧ b) ≡ ¬a ∨ ¬b
    !(a && b) == (!a || !b)
}

// Can be proven for all a, b ∈ {true, false}
```

### 2. **Predicate Logic**
```rust
// For all integers x, x + 0 = x
∀x ∈ ℤ: x + 0 = x

// There exists an integer that is its own square
∃x ∈ ℤ: x² = x  // x = 0 or x = 1
```

### 3. **Hoare Logic**
```
{P} C {Q}

Where:
- P: Precondition (what must be true before execution)
- C: Command/program
- Q: Postcondition (what will be true after execution)
```

## Zeta-Specific Formal Verification

### 1. **Parser Correctness**
```rust
// Specification: Parser should be total for valid Zeta programs
Theorem: ∀p ∈ ValidZetaPrograms, parse_zeta(p) ≠ ⊥

// Proof sketch:
// 1. Define grammar G for Zeta
// 2. Show parser implements recognition algorithm for G
// 3. Prove algorithm terminates for all strings in L(G)
```

### 2. **Type Safety**
```rust
// Theorem: Well-typed programs don't go wrong
Theorem: If ⊢ e : τ, then either:
  1. e reduces to a value v with ⊢ v : τ, or
  2. e diverges (runs forever), or
  3. e encounters a runtime error allowed by semantics

// This is the standard type safety theorem (Progress + Preservation)
```

### 3. **Compiler Correctness**
```rust
// Theorem: Compilation preserves semantics
Theorem: ∀e ∈ ZetaExpressions,
  ⟦e⟧ = ⟦compile(e)⟧

Where:
- ⟦·⟧: Denotational semantics
- compile: Zeta → Target (e.g., x86, LLVM)
```

## Practical Techniques

### 1. **Invariant Discovery**
```rust
// Find loop invariants for verification
fn factorial(n: u32) -> u32 {
    let mut result = 1;
    let mut i = 1;
    
    // Invariant: result = (i-1)!
    while i <= n {
        // Invariant holds here
        result *= i;
        i += 1;
        // Invariant: result = (i-1)!
    }
    
    result  // Postcondition: result = n!
}
```

### 2. **Pre/Postcondition Specification**
```rust
/// Binary search implementation with formal specification
///
/// Precondition:
/// - arr is sorted in non-decreasing order
/// - 0 ≤ left ≤ right < arr.len()
///
/// Postcondition:
/// - Returns Some(index) where arr[index] == target
/// - Returns None if target not in arr[left..=right]
fn binary_search(arr: &[i32], target: i32, left: usize, right: usize) -> Option<usize> {
    let mut low = left;
    let mut high = right;
    
    // Invariant: if target ∈ arr[left..=right], then target ∈ arr[low..=high]
    while low <= high {
        let mid = low + (high - low) / 2;
        
        match arr[mid].cmp(&target) {
            Ordering::Less => low = mid + 1,
            Ordering::Greater => high = mid - 1,
            Ordering::Equal => return Some(mid),
        }
    }
    
    None
}
```

### 3. **Model Checking with `kani`**
```rust
// Cargo.toml
[dev-dependencies]
kani = "0.5"

// Verification with Kani Rust Verifier
#[cfg(kani)]
mod verification {
    use kani::*;
    
    #[kani::proof]
    fn verify_binary_search() {
        // Create symbolic array and indices
        let arr: [i32; 5] = kani::any();
        let target: i32 = kani::any();
        let left: usize = kani::any_where(|&i| i < 5);
        let right: usize = kani::any_where(|&i| left <= i && i < 5);
        
        // Assume array is sorted (precondition)
        kani::assume(is_sorted(&arr));
        
        // Call function
        let result = binary_search(&arr, target, left, right);
        
        // Verify postcondition
        match result {
            Some(idx) => {
                assert!(left <= idx && idx <= right);
                assert!(arr[idx] == target);
            }
            None => {
                // Target not in array segment
                for i in left..=right {
                    assert!(arr[i] != target);
                }
            }
        }
    }
    
    fn is_sorted(arr: &[i32]) -> bool {
        arr.windows(2).all(|w| w[0] <= w[1])
    }
}
```

## Zeta Compiler Verification Targets

### 1. **Parser Verification**
```rust
// Verify parser properties with Kani
#[cfg(kani)]
mod parser_verification {
    use zeta::parser::parse_expr;
    
    #[kani::proof]
    fn parser_no_panic() {
        let input: String = kani::any();
        
        // Parser should never panic, even on invalid input
        let _ = parse_expr(&input);
    }
    
    #[kani::proof]
    fn parser_idempotent() {
        let input: String = kani::any();
        
        // Parsing twice should give same result
        let result1 = parse_expr(&input);
        let result2 = parse_expr(&input);
        
        assert!(result1 == result2);
    }
}
```

### 2. **Type System Verification**
```rust
// Verify type system properties
#[cfg(kani)]
mod type_system_verification {
    use zeta::types::{Type, unify, Substitution};
    
    #[kani::proof]
    fn unification_reflexive() {
        let ty: Type = kani::any();
        
        // A type should unify with itself
        assert!(unify(&ty, &ty).is_ok());
    }
    
    #[kani::proof]
    fn substitution_preserves_typing() {
        let ty: Type = kani::any();
        let subst: Substitution = kani::any();
        
        // Applying substitution should produce valid type
        let applied = subst.apply(&ty);
        assert!(applied.is_ok());
    }
}
```

### 3. **Code Generation Verification**
```rust
// Verify code generator properties
#[cfg(kani)]
mod codegen_verification {
    use zeta::mir::{MIR, basic_block};
    
    #[kani::proof]
    fn codegen_terminates() {
        let mir: MIR = kani::any();
        
        // Code generation should terminate for all MIR
        let _ = codegen(&mir);
    }
    
    #[kani::proof]
    fn register_allocation_complete() {
        let mir: MIR = kani::any();
        
        // Register allocation should succeed or fail gracefully
        let result = allocate_registers(&mir);
        assert!(result.is_ok() || result.is_err());
    }
}
```

## Specification Languages

### 1. **Rust Contracts (Experimental)**
```rust
#![feature(contracts)]

use std::contracts::*;

#[requires(x >= 0)]
#[ensures(result >= 0)]
fn sqrt(x: f64) -> f64 {
    // Newton's method implementation
    let mut guess = x / 2.0;
    for _ in 0..10 {
        guess = (guess + x / guess) / 2.0;
    }
    guess
}
```

### 2. **Prusti (Rust Verifier)**
```rust
// Requires Prusti: https://viperproject.github.io/prusti-dev/
use prusti_contracts::*;

#[pure]
#[ensures(result >= 0)]
fn abs(x: i32) -> i32 {
    if x >= 0 { x } else { -x }
}

#[requires(x >= 0 && y >= 0)]
#[ensures(result == x + y)]
fn add(x: i32, y: i32) -> i32 {
    x + y
}
```

### 3. **Creusot (Rust Verification)**
```rust
// Creusot: https://github.com/xldenis/creusot
use creusot_contracts::*;

#[logic]
#[requires(x >= 0)]
#[ensures(result >= 0)]
fn sqrt_logic(x: Int) -> Int {
    // Logical definition
    pearlite! {
        exists|r: Int| r * r <= x && x < (r + 1) * (r + 1)
    }
}

#[requires(x >= 0)]
#[ensures(@result * @result <= @x && @x < (@result + 1) * (@result + 1))]
fn sqrt(x: u32) -> u32 {
    // Implementation
    // ...
}
```

## Verification Workflow

### 1. **Specification Phase**
```rust
// 1. Write formal specification
/// Sorts array in ascending order
///
/// Precondition: arr is mutable slice
/// Postcondition: ∀i, j: 0 ≤ i < j < arr.len() ⇒ arr[i] ≤ arr[j]
fn sort(arr: &mut [i32]) {
    // ...
}
```

### 2. **Implementation Phase**
```rust
// 2. Implement with verification in mind
fn sort(arr: &mut [i32]) {
    for i in 0..arr.len() {
        // Invariant: arr[0..i] is sorted
        let mut min_idx = i;
        for j in i + 1..arr.len() {
            if arr[j] < arr[min_idx] {
                min_idx = j;
            }
        }
        arr.swap(i, min_idx);
    }
}
```

### 3. **Verification Phase**
```rust
// 3. Verify with model checker
#[cfg(kani)]
mod verification {
    #[kani::proof]
    fn verify_sort() {
        let mut arr: [i32; 5] = kani::any();
        sort(&mut arr);
        
        // Verify sorted
        for i in 0..4 {
            assert!(arr[i] <= arr[i + 1]);
        }
    }
}
```

## Zeta-Specific Verification Plan

### Phase 1: Basic Safety
1. **Parser safety**: No panics on any input
2. **Memory safety**: No undefined behavior in unsafe blocks
3. **Type safety**: Well-typed programs don't crash

### Phase 2: Functional Correctness
1. **Parser correctness**: Parses according to grammar
2. **Type inference correctness**: Infers most general type
3. **Code generation correctness**: Preserves semantics

### Phase 3: Advanced Properties
1. **Compiler optimization correctness**: Optimizations preserve semantics
2. **Concurrency safety**: No data races in parallel features
3. **Security properties**: No injection vulnerabilities

## Tools and Resources

### Rust Verification Tools
- **Kani**: Model checking for Rust
- **Prusti**: Deductive verification
- **Creusot**: Rust verification using Why3
- **RustVerify**: Experimental verifier
- **MIRAI**: Abstract interpretation

### Learning Resources
- [Software Foundations](https://softwarefoundations.cis.upenn.edu/) (Coq)
- [Program Proofs](https://program-proofs.org/) (Dafny)
- [Verified Functional Algorithms](https://softwarefoundations.cis.upenn.edu/vfa-current/) (Coq)
- [Rust Verification Workshop](https://rust-verification.github.io/)

### Books
- "Practical Formal Verification" by Daniel Kroening
- "Software Abstractions" by Daniel Jackson
- "Introduction to Software Verification" by Bernhard Beckert

## Best Practices

### 1. **Start Small**
- Verify small, critical functions first
- Build up to larger components
- Use compositional verification

### 2. **Write Specifications First**
- Specify before implementing
- Keep specifications simple
- Document assumptions

### 3. **Use Multiple Techniques**
- Combine testing and formal verification
- Use model checking for state exploration
- Use theorem proving for deep properties

### 4. **Manage Complexity**
- Break verification into lemmas
- Use abstraction to reduce state space
- Leverage Rust's type system

### 5. **Integrate with Development**
- Run verification in CI
- Treat verification failures as bugs
- Document verification results

## Conclusion

Formal verification provides the highest level of assurance for critical software like compilers. While more expensive than testing, it can prove properties that testing cannot.

For Zeta, focus on:
1. **Parser safety** (no panics)
2. **Type safety** (well-typed programs don't go wrong)
3. **Compiler correctness** (preservation of semantics)

Start with Kani for model checking, then progress to more advanced techniques as needed.

---

**Formal verification proves that your code is correct, not just that it hasn't failed yet. For a production compiler, this level of assurance is essential.**