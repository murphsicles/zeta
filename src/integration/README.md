# Integration Bridge Module

## Overview

The integration bridge provides coordination between parser, type checker, and codegen components for generic type support. It enables seamless compilation of generic code by managing type contexts, coordinating monomorphization, and providing unified error reporting.

## Architecture

```
┌─────────────┐    ┌───────────────┐    ┌─────────────┐
│   Parser    │───▶│   Type        │───▶│   Codegen   │
│   (LEX)     │    │   Checker     │    │   (GEN)     │
│             │◀───│   (SEM)       │◀───│             │
└─────────────┘    └───────────────┘    └─────────────┘
        │                  │                    │
        └──────────────────┼────────────────────┘
                           │
                    ┌───────────────┐
                    │  Integration  │
                    │  Bridge (SYN) │
                    │               │
                    │  Coordination │
                    │  Manager      │
                    └───────────────┘
```

## Components

### 1. Generic Integration (`generic_integration.rs`)
- **`GenericIntegration`**: Main coordination struct
- **`GenericParam`**: Enhanced generic parameter representation
- **`TypeContext`**: Generic parameter tracking with scopes
- **`ConstraintSet`**: Type inference constraints
- **`IntegrationError`**: Unified error type

### 2. Type Context Management (`type_context.rs`)
- **`SharedTypeContext`**: Cross-component type context
- **`TypeContextStack`**: Nested scope management
- **`ScopeGuard`**: RAII scope management
- Error conversion utilities
- Type variable utilities

### 3. Coordination Protocols (`coordination.rs`)
- **`CoordinationManager`**: Central coordination hub
- **`CoordinationMessage`**: Inter-component messages
- **Component protocols**: Namespaced interfaces for each component

## Usage

### Basic Integration Setup

```rust
use zetac::integration::{GenericIntegration, CoordinationManager};

// Create integration bridge
let mut integration = GenericIntegration::new();

// Create coordination manager
let mut manager = CoordinationManager::new();
manager.register_component("parser");
manager.register_component("type_checker");
manager.register_component("codegen");
manager.register_component("integration");
```

### Type Context Management

```rust
use zetac::integration::{TypeContextStack, GenericParam};

let mut stack = TypeContextStack::new();

// Enter new scope
{
    let _guard = stack.enter_scope();
    let context = stack.current_mut();
    
    // Add generic parameters
    let params = vec![
        GenericParam::Type {
            name: "T".to_string(),
            bounds: vec![],
        },
    ];
    context.add_generic_params(&params);
    
    // Type lookup works in this scope
    let ty = context.lookup_type("T");
}
// Scope automatically exited when guard drops
```

### Coordination Between Components

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

## Error Handling

```rust
use zetac::integration::{IntegrationError, error_conversion};

// Add errors to integration
integration.add_error(IntegrationError::TypeError("Type mismatch".to_string()));

// Convert component errors
let unify_error = UnifyError::OccursCheck;
let integration_error = error_conversion::unify_to_integration(unify_error);

// Check for errors
if integration.has_errors() {
    let errors = integration.get_errors();
    // Handle errors
}
```

## Generic Function Lifecycle

1. **Parsing**: Parser creates `GenericParam` list
2. **Registration**: Integration registers `GenericFunction`
3. **Type Checking**: Type checker infers concrete types
4. **Monomorphization**: Integration creates `ConcreteFunction`
5. **Code Generation**: Codegen produces specialized code

```rust
// Register generic function
let generic_fn = GenericFunction {
    name: "identity".to_string(),
    generic_params: vec![/* ... */],
    param_types: vec![/* ... */],
    return_type: /* ... */,
    body: ast,
    instantiations: HashMap::new(),
};

integration.register_generic_function(generic_fn);

// Get concrete function
let concrete_fn = integration.get_concrete_function("identity", &[Type::I32])?;
```

## Testing

Run the integration tests:

```rust
cargo test --test integration
```

Test files:
- `test_integration_bridge.rs`: Basic functionality tests
- `integration_demo.rs`: Full workflow demonstration

## Integration with Existing Components

### Parser (LEX) Integration
- Update parser to produce `GenericParam` instead of `Vec<String>`
- Send `ParsedAst` messages to coordination manager
- Report parser errors through `IntegrationError::ParseError`

### Type Checker (SEM) Integration
- Use `TypeContext` for generic parameter tracking
- Collect constraints during type checking
- Send `TypeCheckResult` messages with inferred types
- Report type errors through `IntegrationError::TypeError`

### Codegen (GEN) Integration
- Handle `MonomorphizationRequest` messages
- Generate specialized code for concrete types
- Send `MonomorphizationResult` messages
- Report codegen errors through `IntegrationError::MonomorphizationError`

## Performance Considerations

1. **Type Caching**: `GenericIntegration` caches concrete functions
2. **Lazy Evaluation**: Type substitutions applied only when needed
3. **Scope Stack**: Efficient nested scope management
4. **Message Queue**: Designed for asynchronous processing

## Safety Features

1. **RAII Scopes**: Automatic scope cleanup
2. **Error Propagation**: All errors captured and reported
3. **Thread Safety**: `Arc<Mutex<>>` patterns for shared state
4. **Type Safety**: Rust's type system ensures correctness

## Next Steps

1. **Component Integration**: Connect with actual parser, type checker, and codegen
2. **Performance Optimization**: Profile and optimize critical paths
3. **Error Message Improvement**: Enhance diagnostic messages
4. **Incremental Compilation**: Add support for recompiling only changed code

## See Also

- `SYN_PHASE2_COMPLETION_REPORT.md`: Detailed implementation report
- `GENERIC_INTEGRATION_ARCHITECTURE.md`: Architecture design
- `SYN_COORDINATION_PLAN.md`: Coordination strategy