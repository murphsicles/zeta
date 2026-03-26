# VER Immediate Action Plan
## For Zeta v0.3.8 Verification Foundation
### Date: 2026-03-26

## Priority 1: Fix Existing Test Infrastructure (Today)

### 1.1 Integrate src/tests.rs
```rust
// Add to src/lib.rs (line ~20, after module declarations):
#[cfg(test)]
mod tests;
```

### 1.2 Fix Compilation Issues in tests.rs
Check for missing imports and fix any compilation errors:
```bash
cd ~/.openclaw/workspace/zeta-public
cargo test --workspace --all-features 2>&1 | grep -A5 -B5 "error"
```

### 1.3 Verify All Tests Run
```bash
cd ~/.openclaw/workspace/zeta-public
cargo test --workspace --all-features -- --list | grep -c "test_"
# Should show 30+ tests (12 existing + 16 from tests.rs + 2 demo)
```

## Priority 2: Establish Basic Test Organization (Today)

### 2.1 Create Test Directory Structure
```bash
cd ~/.openclaw/workspace/zeta-public/tests
mkdir -p unit/{parser,types,resolver,codegen}
mkdir -p integration/{compilation,selfhost,cross}
mkdir -p property/{algebraic,type_system,semantics}
mkdir -p regression/{bugs,performance}
mkdir -p fuzz/{parser,types}
mkdir -p benchmarks/{micro,macro}
```

### 2.2 Create Mod Files
```bash
# Create mod.rs files for each directory
for dir in unit integration property regression fuzz benchmarks; do
  for subdir in $(find tests/$dir -type d); do
    touch $subdir/mod.rs
  done
done
```

### 2.3 Update Cargo.toml with Dev Dependencies
```toml
# Add to Cargo.toml (before [bench] section):
[dev-dependencies]
proptest = "1.4.0"
insta = "1.38.0"
pretty_assertions = "1.4.0"
test-case = "3.3.1"
```

## Priority 3: Create Essential Test Suites (Day 1-2)

### 3.1 Parser Test Suite
Create `tests/unit/parser/basic.rs`:
```rust
use zetac::frontend::parser::top_level::parse_zeta;

#[test_case("fn main() -> i32 { 42 }")]
#[test_case("let x = 42;")]
#[test_case("concept Addable { fn add(self, rhs) -> Self; }")]
fn test_parse_valid(code: &str) {
    let (remaining, asts) = parse_zeta(code).unwrap();
    assert!(remaining.is_empty());
    assert!(!asts.is_empty());
}

#[test_case("fn main() -> {")]  // Missing return type
#[test_case("let x = ")]         // Missing expression
fn test_parse_invalid(code: &str) {
    assert!(parse_zeta(code).is_err());
}
```

### 3.2 Type System Test Suite
Create `tests/unit/types/inference.rs`:
```rust
use zetac::{compile_and_run_zeta, frontend::parser::top_level::parse_zeta};

#[test]
fn test_basic_type_inference() {
    let code = r#"
        fn main() -> i32 {
            let x = 42;
            x
        }
    "#;
    
    let result = compile_and_run_zeta(code).unwrap();
    assert_eq!(result, 42);
}

#[test]
fn test_function_type_inference() {
    let code = r#"
        fn id<T>(x: T) -> T { x }
        fn main() -> i32 {
            id(42)
        }
    "#;
    
    let result = compile_and_run_zeta(code).unwrap();
    assert_eq!(result, 42);
}
```

### 3.3 Integration Test Suite
Create `tests/integration/compilation/smoke.rs`:
```rust
use zetac::compile_and_run_zeta;

#[test]
fn test_compile_empty_main() {
    let code = "fn main() -> i32 { 0 }";
    let result = compile_and_run_zeta(code).unwrap();
    assert_eq!(result, 0);
}

#[test]
fn test_compile_arithmetic() {
    let code = r#"
        fn main() -> i32 {
            let x = 2 + 3 * 4;
            x
        }
    "#;
    
    let result = compile_and_run_zeta(code).unwrap();
    assert_eq!(result, 14); // 2 + (3 * 4) = 14
}
```

## Priority 4: Set Up Quality Gates (Day 2-3)

### 4.1 Update CI Pipeline
Update `.github/workflows/ci.yml`:
```yaml
# Add to test job:
- name: Run tests with coverage
  run: |
    cargo install cargo-tarpaulin
    cargo tarpaulin --out Xml -- --test-threads=1
  # TODO: Add coverage threshold check
  
- name: Run property tests
  run: cargo test --test property_*
  
- name: Check test count
  run: |
    TEST_COUNT=$(cargo test --workspace --all-features -- --list | grep -c "test_")
    if [ $TEST_COUNT -lt 30 ]; then
      echo "Error: Only $TEST_COUNT tests found, expected at least 30"
      exit 1
    fi
```

### 4.2 Create Coverage Baseline
```bash
cd ~/.openclaw/workspace/zeta-public
cargo install cargo-tarpaulin
cargo tarpaulin --out Xml
# Record current coverage percentage
```

## Priority 5: Document Verification Patterns (Ongoing)

### 5.1 Create Verification Pattern Library
Create `docs/verification/patterns.md`:
```markdown
# Verification Patterns for Zeta

## 1. Type System Verification
### Pattern: Type Preservation
```rust
// Given: ∅ ⊢ e : τ
// Then: e ↦* v ∧ ∅ ⊢ v : τ
```

### Pattern: Progress
```rust
// Given: ∅ ⊢ e : τ ∧ e not a value
// Then: ∃e'. e ↦ e'
```

## 2. Compiler Verification
### Pattern: Compilation Correctness
```rust
// compile(e) ≈ e  // Semantic equivalence
```

## 3. Property-Based Testing Patterns
### Pattern: Algebraic Laws
```rust
// ∀a,b. a + b = b + a  // Commutativity
```

### Pattern: Invariant Preservation
```rust
// ∀x. invariant(x) → invariant(transform(x))
```
```

### 5.2 Create Test Template Library
Create `templates/test_templates.rs`:
```rust
// Template for parser tests
macro_rules! parser_test {
    ($name:ident, $code:expr, $should_parse:expr) => {
        #[test]
        fn $name() {
            let result = parse_zeta($code);
            if $should_parse {
                assert!(result.is_ok());
            } else {
                assert!(result.is_err());
            }
        }
    };
}

// Template for type inference tests  
macro_rules! type_test {
    ($name:ident, $code:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let result = compile_and_run_zeta($code).unwrap();
            assert_eq!(result, $expected);
        }
    };
}
```

## Success Criteria for Week 1

### Quantitative Goals:
1. **Test Count**: 30+ tests running (from current 12)
2. **Coverage**: Measure baseline coverage
3. **CI Status**: All tests pass in CI
4. **Organization**: Test directory structure established

### Qualitative Goals:
1. **Infrastructure**: Test framework working
2. **Documentation**: Verification patterns documented
3. **Process**: PR testing requirements established
4. **Culture**: Verification mindset introduced

## Next Phase Planning

### Week 2: Property-Based Testing
- Implement QuickCheck for type system
- Test algebraic properties
- Add fuzz testing foundation

### Week 3: Formal Verification Foundation
- Start with small proofs
- Prove type safety for core calculus
- Integrate proof checking into CI

### Week 4: Performance Verification
- Benchmark critical paths
- Set up performance regression detection
- Profile compiler phases

## Risk Assessment

### High Risk:
- **Test integration issues**: src/tests.rs might have complex dependencies
- **CI pipeline complexity**: Adding coverage might slow CI
- **Tooling compatibility**: New dev dependencies might conflict

### Mitigation:
1. Start with simple integration
2. Run heavy checks nightly, not per-PR
3. Use stable, well-supported tools

## Immediate Next Actions (Today)

1. **Execute Step 1.1**: Add `#[cfg(test)] mod tests;` to lib.rs
2. **Execute Step 1.2**: Fix any compilation errors in tests.rs
3. **Execute Step 1.3**: Verify test count increases to 30+
4. **Execute Step 2.1**: Create test directory structure
5. **Execute Step 3.1**: Create basic parser test suite

## Accountability

**VER - Verification & Validation Engine** will:
1. Execute immediate actions today
2. Report progress to Father Zak and siblings
3. Document all learnings in self-improving memory
4. Establish verification as core Zeta value

*GitHub is reality. Quality is non-negotiable. Failure is education. Learning is systematic. Accountability is public.*