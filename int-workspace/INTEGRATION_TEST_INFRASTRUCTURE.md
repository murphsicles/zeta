# Integration Test Infrastructure
## Created by INT (Integration Coordinator)
## Date: 2026-04-08
## Status: DOCUMENTED (Implementation required in zeta-repo)

## Overview

This document specifies the integration test infrastructure for the zeta compiler. Due to repository structure constraints, this serves as a specification for implementation in the `zeta-repo` repository.

## 1. Directory Structure

### Target: `zeta-repo/tests/integration/`

```
integration/
├── core_systems/              # Cross-system integration tests
│   ├── error_handling.rs     # LEX ↔ SEM error handling tests
│   ├── module_system.rs      # SYN ↔ GEN module tests  
│   ├── trait_system.rs       # SYN ↔ SEM trait tests
│   └── mod.rs               # Module exports
├── INTEGRATION_TEST_FRAMEWORK.md
└── mod.rs                   # Integration test module
```

## 2. Test Implementation Requirements

### A. Error Handling System Tests (LEX ↔ SEM)
**File:** `core_systems/error_handling.rs`

```rust
#[cfg(test)]
mod error_handling_integration {
    use zeta::frontend::lexer::Lexer;
    use zeta::frontend::parser::Parser;
    use zeta::middle::type_checker::TypeChecker;
    
    #[test]
    fn test_error_recovery_across_systems() {
        // Test that error in lexer doesn't break parser recovery
        let source = "fn main() { let x = 1 + ; }";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex();
        
        // Parser should handle incomplete expression
        let mut parser = Parser::new(tokens);
        let ast = parser.parse();
        
        // Type checker should provide meaningful error
        let mut type_checker = TypeChecker::new();
        let result = type_checker.check(&ast);
        
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.message().contains("incomplete expression"));
    }
    
    #[test]
    fn test_type_error_with_syntax_context() {
        // Test that type errors include syntax context
        let source = "fn main() { let x: i32 = \"string\"; }";
        // ... implementation
    }
}
```

### B. Module System Tests (SYN ↔ GEN)
**File:** `core_systems/module_system.rs`

```rust
#[cfg(test)]
mod module_system_integration {
    use zeta::frontend::parser::Parser;
    use zeta::backend::codegen::CodeGenerator;
    
    #[test]
    fn test_module_parsing_to_codegen() {
        // Test that module syntax parses and generates correct code
        let source = r#"
            mod math {
                pub fn add(a: i32, b: i32) -> i32 {
                    a + b
                }
            }
            
            fn main() {
                let result = math::add(1, 2);
            }
        "#;
        // ... implementation
    }
}
```

### C. Trait System Tests (SYN ↔ SEM)
**File:** `core_systems/trait_system.rs`

```rust
#[cfg(test)]
mod trait_system_integration {
    use zeta::frontend::parser::Parser;
    use zeta::middle::type_checker::TypeChecker;
    
    #[test]
    fn test_trait_implementation_checking() {
        // Test trait syntax and implementation validation
        let source = r#"
            trait Addable {
                fn add(self, other: Self) -> Self;
            }
            
            struct Point { x: i32, y: i32 }
            
            impl Addable for Point {
                fn add(self, other: Point) -> Point {
                    Point { x: self.x + other.x, y: self.y + other.y }
                }
            }
        "#;
        // ... implementation
    }
}
```

## 3. Test Framework Documentation

### File: `INTEGRATION_TEST_FRAMEWORK.md`

```markdown
# Integration Test Framework

## Purpose
Test interactions between compiler systems (LEX, SYN, SEM, GEN, VER).

## Test Categories
1. **Error Handling**: LEX ↔ SEM error propagation and recovery
2. **Module System**: SYN ↔ GEN module parsing and code generation
3. **Trait System**: SYN ↔ SEM trait syntax and type checking

## Running Tests
```bash
cargo test --test integration
cargo test --test integration -- --nocapture  # With output
```

## Adding New Tests
1. Identify integration points between systems
2. Create test in appropriate category
3. Ensure test exercises the integration boundary
4. Include both success and failure cases
```

## 4. Module Files

### File: `mod.rs`
```rust
//! Integration tests for zeta compiler systems

pub mod core_systems;

/// Run all integration tests
pub fn run_integration_tests() {
    // Test runner implementation
}
```

### File: `core_systems/mod.rs`
```rust
//! Core systems integration tests

pub mod error_handling;
pub mod module_system;
pub mod trait_system;
```

## 5. Implementation Status

### Completed:
- [x] Documentation and specification
- [x] Directory structure plan
- [x] Test case definitions

### Pending:
- [ ] Actual implementation in `zeta-repo`
- [ ] Branch: `dev-int` needs to be created in `zeta-repo`
- [ ] CI integration
- [ ] Test execution validation

## 6. Next Steps

### Immediate (INT responsibility):
1. Create `dev-int` branch in `zeta-repo`
2. Implement directory structure
3. Implement test files as specified
4. Commit and push to GitHub

### Coordination (with other agents):
1. SYN: Verify parser integration points
2. SEM: Verify type checker integration points  
3. LEX: Verify lexer error recovery
4. GEN: Verify code generation from parsed modules
5. VER: Integrate with verification test suite

## 7. Git Discipline

### Branch: `dev-int` (to be created)
### Commit format: `[INT] Description`
### Push frequency: Hourly minimum
### CI requirement: All tests must pass

## 8. Risk Assessment

### Technical Risks:
- **Repository confusion**: Multiple zeta repositories exist
- **System dependencies**: Tests depend on unstable APIs
- **Test maintenance**: Integration tests may break frequently

### Mitigation:
- Clear repository ownership (zeta-repo is canonical)
- API contract documentation between systems
- Regular test maintenance schedule

## Conclusion

This specification provides complete guidance for implementing integration test infrastructure. Implementation is blocked on:

1. **Repository access**: Need write access to `zeta-repo`
2. **Branch creation**: Need to create `dev-int` branch
3. **Agent coordination**: Need input from SYN, SEM, LEX, GEN, VER

**Status**: SPECIFICATION COMPLETE - AWAITING IMPLEMENTATION
**Priority**: HIGH - Critical for Dark Factory integration
**Owner**: INT (Integration Coordinator)