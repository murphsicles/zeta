---
name: ci-verification
description: CI/CD integration for verification workflows. Automate testing, fuzzing, benchmarking, and quality gates in GitHub Actions.
metadata:
  {
    "openclaw":
      {
        "requires": { "bins": [] },
        "install": [],
      },
  }
---

# CI/CD Verification Skill

## Purpose
Automate verification workflows in CI/CD pipelines. Ensure every commit meets quality standards before merging.

## Core Concepts

### 1. **Quality Gates**
- **Compilation**: Must compile without errors
- **Linting**: Must pass clippy with `-D warnings`
- **Formatting**: Must pass rustfmt check
- **Testing**: All tests must pass
- **Fuzzing**: No new crashes found
- **Benchmarking**: No performance regressions
- **Coverage**: Minimum test coverage threshold

### 2. **CI/CD Pipeline Stages**
```
Commit → Lint/Format → Build → Unit Tests → Integration Tests → 
Property Tests → Fuzz Tests → Benchmarks → Coverage → Merge
```

### 3. **Failure Modes**
- **Blocking failures**: Prevent merge (compilation errors, test failures)
- **Non-blocking failures**: Warn but allow merge (performance regression, coverage decrease)
- **Flaky tests**: Tests that sometimes pass, sometimes fail

## GitHub Actions Workflows

### 1. **Basic Verification Workflow**
```yaml
# .github/workflows/verify.yml
name: Verify

on: [push, pull_request]

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          components: clippy, rustfmt
          
      - name: Cache dependencies
        uses: actions/cache@v3
        with:
          path: |
            ~/.cargo/registry
            ~/.cargo/git
            target
          key: ${{ runner.os }}-cargo-${{ hashFiles('**/Cargo.lock') }}
          
      - name: Check formatting
        run: cargo fmt --all -- --check
        
      - name: Clippy
        run: cargo clippy --workspace --all-features --all-targets -- -D warnings
        
      - name: Build
        run: cargo build --workspace --all-features --all-targets
        
      - name: Test
        run: cargo test --workspace --all-features
```

### 2. **Property Testing Workflow**
```yaml
# .github/workflows/property-tests.yml
name: Property Tests

on:
  schedule:
    - cron: '0 2 * * *'  # Daily at 2 AM
  workflow_dispatch:  # Manual trigger

jobs:
  property-tests:
    runs-on: ubuntu-latest
    timeout-minutes: 30
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          
      - name: Run property tests
        run: |
          # Run with many test cases
          cargo test property_tests -- --test-threads=1 --nocapture
          # Or run proptest with many cases
          PROPTEST_CASES=10000 cargo test
```

### 3. **Fuzzing Workflow**
```yaml
# .github/workflows/fuzz.yml
name: Fuzz

on:
  schedule:
    - cron: '0 3 * * *'  # Daily at 3 AM
  workflow_dispatch:

jobs:
  fuzz:
    runs-on: ubuntu-latest
    timeout-minutes: 60
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: nightly
          components: llvm-tools-preview
          
      - name: Install cargo-fuzz
        run: cargo install cargo-fuzz
        
      - name: Run fuzzing
        run: |
          # Run each fuzz target for 5 minutes
          for target in parser lexer typecheck codegen; do
            timeout 300 cargo fuzz run ${target}_fuzz || true
          done
          
      - name: Upload crashes
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: fuzz-crashes
          path: fuzz/artifacts/
```

### 4. **Benchmarking Workflow**
```yaml
# .github/workflows/bench.yml
name: Benchmarks

on:
  push:
    branches: [main, v0.*]
  pull_request:
    branches: [main]

jobs:
  benchmarks:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          
      - name: Install criterion
        run: cargo install cargo-criterion
        
      - name: Run benchmarks
        run: cargo criterion --message-format=json > benchmarks.json
        
      - name: Compare with baseline
        uses: benchmark-action/github-action-benchmark@v1
        with:
          name: Zeta Benchmarks
          tool: 'cargo-criterion'
          output-file-path: benchmarks.json
          github-token: ${{ secrets.GITHUB_TOKEN }}
          auto-push: true
          alert-threshold: '200%'  # Alert on 2x regression
```

### 5. **Coverage Workflow**
```yaml
# .github/workflows/coverage.yml
name: Coverage

on: [push, pull_request]

jobs:
  coverage:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: stable
          components: llvm-tools-preview
          
      - name: Install grcov
        run: |
          wget https://github.com/mozilla/grcov/releases/download/v0.8.13/grcov-x86_64-unknown-linux-gnu.tar.bz2
          tar xf grcov-x86_64-unknown-linux-gnu.tar.bz2
          chmod +x grcov
          sudo mv grcov /usr/local/bin/
          
      - name: Generate coverage
        run: |
          cargo clean
          RUSTFLAGS="-C instrument-coverage" cargo test --workspace --all-features
          grcov . --binary-path ./target/debug/ -s . -t html --branch --ignore-not-existing -o ./coverage/
          
      - name: Upload coverage
        uses: actions/upload-artifact@v3
        with:
          name: coverage-report
          path: coverage/
          
      - name: Upload to Codecov
        uses: codecov/codecov-action@v3
        with:
          file: ./coverage/coverage.json
```

## Zeta-Specific CI Configuration

### 1. **Multi-OS Testing**
```yaml
# .github/workflows/multi-platform.yml
name: Multi-Platform

on: [push, pull_request]

jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        rust: [stable, nightly]
        
    runs-on: ${{ matrix.os }}
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Setup Rust
        uses: actions-rs/toolchain@v1
        with:
          toolchain: ${{ matrix.rust }}
          components: clippy, rustfmt
          
      - name: Build and test
        run: |
          cargo build --workspace --all-features --all-targets
          cargo test --workspace --all-features
```

### 2. **Release Verification**
```yaml
# .github/workflows/release-verify.yml
name: Release Verification

on:
  release:
    types: [created]

jobs:
  verify-release:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
        with:
          ref: ${{ github.event.release.tag_name }}
          
      - name: Verify tag matches commit
        run: |
          git describe --exact-match --tags $(git log -n1 --pretty='%H')
          
      - name: Full verification suite
        run: |
          # Run all verification steps
          ./scripts/verify-release.sh
```

### 3. **Dependency Audit**
```yaml
# .github/workflows/audit.yml
name: Security Audit

on:
  schedule:
    - cron: '0 0 * * 0'  # Weekly on Sunday
  workflow_dispatch:

jobs:
  audit:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Install cargo-audit
        run: cargo install cargo-audit
        
      - name: Audit dependencies
        run: cargo audit
```

## Quality Gates Implementation

### 1. **Required Checks for Merge**
```yaml
# .github/branch-protection.yml
# Configure in repository settings → Branches → Branch protection rules

Required status checks:
  - "Verify / verify"
  - "Multi-Platform / test (stable, ubuntu-latest)"
  - "Coverage / coverage"
  
Require branches to be up to date before merging: true
Require conversation resolution before merging: true
Require approval from code owners: false
```

### 2. **Automated PR Labels**
```yaml
# .github/workflows/labeler.yml
name: PR Labeler

on: [pull_request]

jobs:
  label:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/labeler@v4
        with:
          repo-token: ${{ secrets.GITHUB_TOKEN }}
          configuration-path: .github/labeler.yml
```

```yaml
# .github/labeler.yml
# Auto-label PRs based on changed files
"area: parser":
  - src/frontend/parser/**/*
  
"area: type-system":
  - src/middle/types/**/*
  - src/middle/resolver/**/*
  
"area: codegen":
  - src/backend/**/*
  
"needs: verification":
  - any: ['src/**/*.rs', 'tests/**/*.rs']
    changed-files:
      - any-glob-to-any-file: '**/*'
```

### 3. **Automated Review Comments**
```yaml
# .github/workflows/review-comments.yml
name: Review Comments

on: [pull_request]

jobs:
  review:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Run clippy and comment
        uses: reviewdog/action-clippy@v1
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          reporter: github-pr-review
          level: warning
```

## Monitoring and Alerting

### 1. **Performance Regression Detection**
```python
# scripts/detect-regressions.py
#!/usr/bin/env python3
"""
Detect performance regressions from benchmark results
"""

import json
import sys
from typing import Dict, List

def load_benchmarks(filepath: str) -> Dict:
    with open(filepath, 'r') as f:
        return json.load(f)

def detect_regressions(current: Dict, baseline: Dict, threshold: float = 1.2) -> List[str]:
    """Detect regressions worse than threshold (e.g., 20% slower)"""
    regressions = []
    
    for benchmark in current['benchmarks']:
        name = benchmark['name']
        current_mean = benchmark['mean']
        
        if name in baseline:
            baseline_mean = baseline[name]['mean']
            ratio = current_mean / baseline_mean
            
            if ratio > threshold:
                regressions.append(
                    f"{name}: {ratio:.2f}x slower "
                    f"({baseline_mean:.2f}ns → {current_mean:.2f}ns)"
                )
    
    return regressions

if __name__ == "__main__":
    current = load_benchmarks("benchmarks-current.json")
    baseline = load_benchmarks("benchmarks-baseline.json")
    
    regressions = detect_regressions(current, baseline)
    
    if regressions:
        print("Performance regressions detected:")
        for r in regressions:
            print(f"  - {r}")
        sys.exit(1)
    else:
        print("No performance regressions detected")
        sys.exit(0)
```

### 2. **Flaky Test Detection**
```yaml
# .github/workflows/flaky-tests.yml
name: Flaky Test Detection

on:
  schedule:
    - cron: '0 4 * * *'  # Daily at 4 AM

jobs:
  detect-flaky:
    runs-on: ubuntu-latest
    
    steps:
      - uses: actions/checkout@v3
      
      - name: Run tests multiple times
        run: |
          for i in {1..10}; do
            cargo test --workspace --all-features -- --list | grep -E "^(test|ok)" > run-$i.txt
          done
          
      - name: Analyze results
        run: |
          # Find tests that pass sometimes, fail sometimes
          python scripts/detect-flaky.py
```

## Best Practices

### 1. **Fast Feedback**
- Keep verification jobs under 10 minutes
- Run linting/formatting first (fastest failures)
- Use caching for dependencies
- Parallelize independent jobs

### 2. **Reliable Tests**
- Tests should be deterministic
- Avoid network dependencies in tests
- Use fixed seeds for random tests
- Clean up test artifacts

### 3. **Actionable Failures**
- Clear error messages
- Links to documentation
- Suggested fixes
- Reproduction steps

### 4. **Security**
- Don't expose secrets in logs
- Use minimal permissions
- Scan for vulnerabilities
- Audit dependencies

## Zeta Verification Pipeline

### Complete `.github/workflows/` Structure
```
.github/workflows/
├── verify.yml           # Basic compilation and tests
├── property-tests.yml   # Property-based testing
├── fuzz.yml            # Fuzz testing
├── bench.yml           # Performance benchmarks
├── coverage.yml        # Test coverage
├── multi-platform.yml  # Cross-platform testing
├── audit.yml           # Security audit
├── release-verify.yml  # Release validation
└── flaky-tests.yml     # Flaky test detection
```

### Verification Script
```bash
#!/bin/bash
# scripts/verify-release.sh

set -e

echo "=== Release Verification ==="

# 1. Basic checks
cargo fmt --all -- --check
cargo clippy --workspace --all-features --all-targets -- -D warnings
cargo build --workspace --all-features --all-targets
cargo test --workspace --all-features

# 2. Property tests
PROPTEST_CASES=1000 cargo test property_tests

# 3. Integration tests
cargo test integration_tests

# 4. Documentation
cargo doc --no-deps --all-features

echo "✅ Release verification complete"
```

---

**CI/CD verification ensures that every Zeta release meets the highest quality standards. Automated checks catch issues before they reach users.**