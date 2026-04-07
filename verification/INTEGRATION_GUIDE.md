# Integration Guide: Formal Verification System

This guide explains how to integrate the formal verification system with the Zeta compiler.

## Overview

The formal verification system provides:
1. **Refinement types**: `{x: T | P(x)}` for specifying pre/postconditions
2. **Verification conditions**: Automatic generation from annotations
3. **SMT solving**: Integration with Z3 to prove correctness
4. **Compile-time verification**: Proofs happen during compilation

## Integration Steps

### 1. Add Verification Crate to Zeta

Add to `Cargo.toml`:
```toml
[dependencies]
zeta-verification = { path = "../verification" }
```

### 2. Extend Type System

Modify `src/middle/types.rs` to support refinement types:

```rust
pub enum Type {
    // Existing types...
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    Bool, String,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    
    // New: Refinement type
    Refined {
        variable: String,
        base: Box<Type>,
        predicate: Predicate,
    },
}
```

### 3. Add Verification Pass

Add a verification pass to the compilation pipeline in `src/main.rs`:

```rust
use zeta_verification::Verifier;

fn compile_with_verification(source: &str) -> Result<(), Box<dyn Error>> {
    // Parse and type check as usual
    let asts = parse_zeta(source)?;
    let mut resolver = Resolver::new();
    
    // Run verification before code generation
    let mut verifier = Verifier::new();
    match verifier.verify(source) {
        Ok(result) => {
            if !result.all_proven {
                eprintln!("Verification failed:");
                for (vc, vc_result) in &result.vcs {
                    if !vc_result.is_proven() {
                        eprintln!("  Failed: {}", vc.name);
                    }
                }
                return Err("Verification failed".into());
            }
            println!("All verification conditions proven!");
        }
        Err(err) => {
            eprintln!("Verification error: {}", err);
            return Err("Verification error".into());
        }
    }
    
    // Continue with normal compilation
    // ...
}
```

### 4. Add Annotation Parser

Extend the Zeta parser to handle verification annotations:

```rust
// In src/frontend/parser/
mod verification_annotations {
    pub fn parse_annotation(input: &str) -> Option<Annotation> {
        // Parse @invariant, @assert, @pre, @post
    }
}
```

### 5. Integrate with Type Checker

Modify `src/middle/resolver/typecheck.rs` to handle refinement types:

```rust
impl Resolver {
    fn check_refinement_type(&self, ty: &Type, value: &AstNode) -> bool {
        if let Type::Refined { variable, base, predicate } = ty {
            // Check that value satisfies predicate
            // Generate verification condition
            let vc = self.generate_vc(predicate, variable, value);
            self.verifier.add_vc(vc);
            true
        } else {
            // Regular type checking
            self.check_type(ty, value)
        }
    }
}
```

## Annotation Syntax

### Function Contracts
```zeta
fn sqrt(x: {n: u64 | n >= 0}) -> {r: u64 | r * r <= x && (r + 1) * (r + 1) > x} {
    // Implementation
}
```

### Loop Invariants
```zeta
while i < n {
    // @invariant total == sum(arr[0..i])
    total += arr[i];
    i += 1;
}
```

### Assertions
```zeta
// @assert x != 0
let y = 1 / x;
```

### Pre/Postconditions
```zeta
// @pre x > 0
// @post result > 0
fn absolute(x: i64) -> i64 {
    if x < 0 { -x } else { x }
}
```

## Murphy's Sieve Integration

To verify Murphy's Sieve:

1. **Add annotations** to the existing implementation
2. **Generate VCs** during type checking
3. **Prove VCs** with Z3
4. **Compile only if all proofs pass**

Example integration:

```rust
// In the Murphy's Sieve benchmark runner
fn run_murphy_sieve_verified() {
    let source = include_str!("../verification/examples/murphy_sieve_verified.z");
    
    // Verify before compiling
    let mut verifier = Verifier::new();
    if verifier.verify(source).unwrap().all_proven {
        println!("Murphy's Sieve verification PASSED!");
        // Compile and run the verified implementation
        compile_and_run(source);
    } else {
        println!("Murphy's Sieve verification FAILED!");
        // Fall back to unverified implementation
        compile_and_run(unverified_murphy_sieve);
    }
}
```

## Testing the Integration

### 1. Unit Tests
```bash
cd verification
cargo test
```

### 2. Integration Test
```bash
# Test Murphy's Sieve verification
cargo run --bin test-verification
```

### 3. Zeta Compiler Integration Test
```bash
# From zeta root directory
cargo test --features verification
```

## Performance Considerations

1. **VC Generation**: Should be fast (linear in program size)
2. **SMT Solving**: Can be expensive for complex proofs
   - Use timeouts (default: 10 seconds per VC)
   - Cache proof results
   - Skip verification in release builds (optional)
3. **Compilation Time**: Verification adds to compile time
   - Offer `--no-verify` flag for development
   - Parallelize VC solving

## Error Messages

Provide clear error messages when verification fails:

```
Verification failed at line 42:
  Failed: Loop invariant "total == sum(arr[0..i])"
  Counterexample: i = 5, total = 10, sum(arr[0..5]) = 12
  Suggestion: Check array bounds in loop body
```

## Future Extensions

1. **Dependent Types**: `(n: u64) -> [u64; n]`
2. **Proof Automation**: Auto-generate simple invariants
3. **IDE Integration**: Live verification feedback
4. **Proof Certificates**: Export proofs for external validation
5. **Verified Standard Library**: Core functions with proofs

## Troubleshooting

### Z3 Not Found
```bash
# Install Z3
# On Ubuntu/Debian:
sudo apt-get install z3

# On macOS:
brew install z3

# On Windows:
choco install z3
```

### Verification Timeouts
- Reduce timeout: `Verifier::new().with_timeout(5)`
- Simplify predicates
- Use simpler loop invariants

### Type Checker Conflicts
- Ensure refinement types are parsed before regular type checking
- Handle type inference with refinement types

## Conclusion

The formal verification system makes Zeta unique among systems programming languages by providing mathematically verified programs at compile time. Start with Murphy's Sieve as a showcase, then expand to other critical algorithms.