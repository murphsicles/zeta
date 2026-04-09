# Integration Bridge Guide

## Overview

The Integration Bridge provides coordination between compiler components:
- **Parser (LEX)**: Parses source code into AST
- **Type Checker (SEM)**: Performs type checking and inference
- **Code Generator (GEN)**: Generates machine code
- **Integration Bridge (SYN)**: Coordinates all components

## Enabling Integration

Add to your `Cargo.toml`:

```toml
[features]
integration = []  # Enable integration bridge module
```

Then compile with the feature flag:

```bash
cargo build --features integration
cargo test --features integration
```

## Basic Usage

### 1. Creating a Coordination Manager

```rust
use zetac::integration::{CoordinationManager, ComponentStatus};

let mut manager = CoordinationManager::new();

// Register components
manager.register_component("parser");
manager.register_component("type_checker");
manager.register_component("codegen");
manager.register_component("integration");
```

### 2. Sending Status Updates

```rust
// Component reports its status
manager.update_status("parser", ComponentStatus::Processing, "Parsing source");
manager.update_status("parser", ComponentStatus::Complete, "AST generated");
```

### 3. Using Protocols

Each component has a dedicated protocol module:

```rust
use zetac::integration::protocols;

// Parser sends parsed AST
protocols::parser::send_parsed_ast(
    &manager,
    ast,
    generic_params,
    type_context,
);

// Type checker sends results
protocols::type_checker::send_type_check_result(
    &manager,
    ast_id,
    inferred_types,
    constraints,
    errors,
);

// Codegen requests monomorphization
protocols::codegen::request_monomorphization(
    &manager,
    generic_fn,
    type_args,
    location,
);
```

### 4. Error Handling

```rust
// Report errors
protocols::parser::report_error(&manager, "Syntax error", "main.z:10:5");

// Check for errors
let errors = manager.get_errors();
if !errors.is_empty() {
    println!("Compilation failed with {} errors", errors.len());
}
```

## Example

See `examples/integration_example.rs` for a complete example:

```bash
cargo run --example integration_example --features integration
```

## Architecture

### Data Flow

```
Source Code
    │
    ├──► Parser (LEX)
    │     │
    │     └──► AST + Generic Info
    │           │
    │           └──► Integration Bridge (SYN)
    │                 │
    │                 ├──► Type Checker (SEM)
    │                 │     │
    │                 │     └──► Type Info + Constraints
    │                 │           │
    │                 │           └──► Integration Bridge
    │                 │                 │
    │                 │                 └──► Code Generator (GEN)
    │                 │                       │
    │                 │                       └──► Machine Code
    │                 │
    │                 └──► Error Reporting
    │
    └──► User Feedback
```

### Component Responsibilities

- **Parser**: Syntax analysis, AST generation, generic syntax parsing
- **Type Checker**: Type inference, constraint solving, bounds checking
- **Code Generator**: Monomorphization, specialization, machine code generation
- **Integration Bridge**: Coordination, error aggregation, type context management

## Testing

Run integration tests:

```bash
cargo test --features integration
```

## Troubleshooting

### Common Issues

1. **"Integration feature not enabled"**
   - Solution: Add `--features integration` to cargo commands

2. **Compilation errors in integration module**
   - Check that all dependencies are properly imported
   - Verify type signatures match between components

3. **Coordination not working**
   - Ensure all components are registered
   - Check that status updates are being sent

### Debugging

Enable debug logging in coordination manager:

```rust
// The coordination manager prints status updates to stdout
// Example output:
// [COORDINATION] parser: Processing - Parsing source
// [COORDINATION] parser: Complete - AST generated
```

## Advanced Features

### Generic Type Support

The integration bridge provides enhanced generic type support:

- **Generic parameters**: Type, lifetime, and const parameters
- **Where clauses**: Additional trait bounds
- **Monomorphization**: Generic instantiation with concrete types
- **Type context**: Shared type information across components

### Error Reporting

Unified error reporting across all components:

- **Parse errors**: Syntax errors, malformed generic syntax
- **Type errors**: Type mismatches, unresolved types, bounds violations
- **Codegen errors**: Monomorphization failures, specialization issues
- **Integration errors**: Coordination failures, component miscommunication

## Performance Considerations

- **Type caching**: Shared type cache reduces parsing overhead
- **Specialization cache**: Avoids regenerating identical specializations
- **Incremental compilation**: Re-type-checks only changed nodes
- **Lazy monomorphization**: Specializes generic functions only when needed