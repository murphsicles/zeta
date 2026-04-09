# Package Ecosystem Tests

This directory contains tests for the Zeta Package Ecosystem implementation in v0.3.36.

## Test Structure

### 1. Basic Tests (`basic_tests.rs`)
- Manifest parsing and serialization
- Version parsing and comparison
- Dependency requirement matching
- Basic dependency resolution

### 2. Integration Tests (`integration_test.rs`)
- Workspace management
- Manifest operations
- Dependency graph operations
- Version requirement edge cases

### 3. Real-World Workflow (`real_world_workflow.rs`)
- Complete development workflow demonstration
- Example usage patterns
- API demonstration

## Running Tests

```bash
# Run all package ecosystem tests
cargo test --test package_ecosystem_basic
cargo test --test package_ecosystem_integration
cargo test --test package_ecosystem_real_world

# Run specific test
cargo test --test package_ecosystem_basic test_manifest_parsing
```

## Test Coverage

The tests verify:
- ✅ Manifest parsing from TOML
- ✅ Manifest serialization to TOML
- ✅ Semantic version parsing (SemVer 2.0.0)
- ✅ Version requirement matching (^, ~, >=, etc.)
- ✅ Dependency graph building and traversal
- ✅ Cycle detection in dependencies
- ✅ Workspace discovery and management
- ✅ Real-world development workflows

## Example Usage

See `examples/package_ecosystem_demo.rs` for a complete demonstration of the package ecosystem API.

## Implementation Status

All package ecosystem features for v0.3.36 have been implemented:
- Crate manifest system (Cargo.toml style)
- Dependency management with resolution
- Zorb package manager integration
- Real-world development workflows
- Workspace support for multi-crate projects