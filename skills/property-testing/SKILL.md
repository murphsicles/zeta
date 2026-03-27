---
name: property-testing
description: Property-based testing for Rust using proptest and quickcheck. Generate thousands of test cases automatically to verify mathematical properties of code.
metadata:
  {
    "openclaw":
      {
        "requires": { "bins": ["cargo"] },
        "install":
          [
            {
              "id": "rust-deps",
              "kind": "cargo",
              "packages": ["proptest", "quickcheck"],
              "label": "Install property testing crates",
            },
          ],
      },
  }
---

# Property-Based Testing Skill

## Purpose
Generate comprehensive test cases automatically to verify mathematical properties of your code. Property-based testing finds edge cases that example-based testing misses.

## Core Concepts

### 1. **Property-Based Testing vs Example-Based Testing**
- **Example-based**: `assert_eq!(add(2, 2), 4)`
- **Property-based**: `for_all(|(a, b)| add(a, b) == add(b, a))` (commutativity)

### 2. **Key Properties to Test**
- **Commutativity**: `f(a, b) == f(b, a)`
- **Associativity**: `f(a, f(b, c)) == f(f(a, b), c)`
- **Identity**: `f(a, identity) == a`
- **Inverse**: `f(a, inverse(a)) == identity`
- **Idempotence**: `f(a, a) == a`
- **Distributivity**: `f(a, g(b, c)) == g(f(a, b), f(a, c))`

### 3. **Rust Crates**
- **proptest**: Advanced property testing with strategies
- **quickcheck**: Classic QuickCheck implementation

## Usage Patterns

### Basic Property Test
```rust
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_addition_commutative(a: i32, b: i32) {
        assert_eq!(a + b, b + a);
    }
}
```

### Custom Strategies
```rust
proptest! {
    #[test]
    fn test_float_parsing(s in r"[0-9]+\.[0-9]+") {
        // Test that all strings matching float pattern parse correctly
        let parsed: Result<f64, _> = s.parse();
        assert!(parsed.is_ok());
    }
}
```

### Type System Properties
```rust
proptest! {
    #[test]
    fn test_type_substitution_reflexive(ty in any_type_strategy()) {
        // A type should unify with itself
        assert!(unify(&ty, &ty).is_ok());
    }
    
    #[test] 
    fn test_type_substitution_transitive(
        a in any_type_strategy(),
        b in any_type_strategy(),
        c in any_type_strategy()
    ) {
        // If A unifies with B and B unifies with C, then A unifies with C
        if unify(&a, &b).is_ok() && unify(&b, &c).is_ok() {
            assert!(unify(&a, &c).is_ok());
        }
    }
}
```

## Zeta-Specific Patterns

### 1. **Parser Properties**
```rust
// All valid Zeta programs should parse without panics
proptest! {
    #[test]
    fn parser_never_panics(s in r"[a-zA-Z0-9_+\-*/=<>!&|^%#@$\\. \n\t(){}\[\];:,]+") {
        let _ = parse_zeta(&s); // Should not panic
    }
}
```

### 2. **Type System Properties**
```rust
// Type inference should be deterministic
proptest! {
    #[test]
    fn type_inference_deterministic(program in valid_zeta_program_strategy()) {
        let ty1 = infer_types(&program);
        let ty2 = infer_types(&program);
        assert_eq!(ty1, ty2);
    }
}
```

### 3. **Code Generation Properties**
```rust
// Code generation should preserve semantics
proptest! {
    #[test]
    fn codegen_preserves_semantics(program in valid_zeta_program_strategy()) {
        let ast = parse_zeta(&program).unwrap();
        let mir = lower_to_mir(&ast).unwrap();
        let asm = codegen(&mir).unwrap();
        
        // Generated code should execute with same result as interpreted
        let interpreted_result = interpret(&ast);
        let compiled_result = execute(&asm);
        assert_eq!(interpreted_result, compiled_result);
    }
}
```

## Common Strategies

### Float Literal Strategy
```rust
fn float_literal_strategy() -> impl Strategy<Value = String> {
    prop_oneof![
        // Decimal notation
        r"[0-9]+\.[0-9]+",
        // Scientific notation  
        r"[0-9]+\.[0-9]+[eE][+-]?[0-9]+",
        // Integer that can be float
        r"[0-9]+",
    ]
}
```

### Type Strategy
```rust
fn any_type_strategy() -> impl Strategy<Value = Type> {
    prop_oneof![
        Just(Type::Int),
        Just(Type::Float),
        Just(Type::Bool),
        Just(Type::String),
        (any_type_strategy(), any_type_strategy())
            .prop_map(|(a, b)| Type::Function(Box::new(a), Box::new(b))),
    ]
}
```

## Best Practices

1. **Start Simple**: Test basic properties before complex ones
2. **Shrink Failures**: Use `proptest`'s shrinking to find minimal failing case
3. **Document Properties**: Each property test should document what mathematical property it verifies
4. **Combine with Example Tests**: Property tests complement, don't replace, example tests
5. **Test Invariants**: Focus on properties that should always hold

## Integration with Zeta

Add to `Cargo.toml`:
```toml
[dev-dependencies]
proptest = "1.4.0"
quickcheck = "1.0.3"
```

Create `tests/property_tests.rs`:
```rust
#[cfg(test)]
mod property_tests {
    use proptest::prelude::*;
    
    proptest! {
        // Your property tests here
    }
}
```

## Learning Resources
- [The Hypothesis documentation](https://hypothesis.readthedocs.io/) (Python, but concepts transfer)
- [Proptest book](https://altsysrq.github.io/proptest-book/intro.html)
- [QuickCheck paper](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)

---

**Property-based testing verifies that your code behaves according to mathematical laws, not just specific examples. It's essential for compiler correctness.**