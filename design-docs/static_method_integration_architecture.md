# Static Method Integration Architecture
## SYN (Syntax/Semantics Bridge) - Integration Plan
**Date:** 2026-03-30 08:53 GMT  
**Deadline:** 09:30 GMT  
**Status:** Pipeline Analysis Complete

---

## 1. Current Pipeline Analysis

### 1.1 Compiler Pipeline Flow
```
Parser (LEX) → AST → Type Checker (SEM) → MIR → Code Generation (GEN)
```

### 1.2 Current Method Call Flow
1. **Parser (LEX)**: Parses method calls into `AstNode::Call` or `AstNode::PathCall`
   - Instance methods: `receiver.method(args)`
   - Path calls: `Type::method(args)` (currently limited)
2. **AST Representation**: 
   - `Call { receiver: Option<Box<AstNode>>, method: String, args: Vec<AstNode>, ... }`
   - `PathCall { path: Vec<String>, method: String, args: Vec<AstNode> }`
3. **Type Checker (SEM)**: Currently uses string-based types, transitioning to algebraic type system
4. **MIR Generation**: Converts AST to MIR statements/expressions
5. **Code Generation (GEN)**: LLVM IR generation from MIR

### 1.3 Handoff Points Between Components
1. **LEX → SEM**: AST nodes with method call information
2. **SEM → MIR**: Type-annotated AST with resolved method signatures
3. **MIR → GEN**: MIR with method call instructions and type information

---

## 2. Integration Points for Static Methods

### 2.1 AST Representation (LEX → SEM)
**Current Limitation**: `PathCall` doesn't distinguish between static methods and associated functions.

**Proposed Enhancement**:
```rust
// In src/frontend/ast.rs
pub enum AstNode {
    // ... existing variants ...
    
    // Enhanced Call with static method flag
    Call {
        receiver: Option<Box<AstNode>>,
        method: String,
        args: Vec<AstNode>,
        type_args: Vec<String>,
        structural: bool,
        // NEW: Distinguish static vs instance
        is_static: bool,
    },
    
    // Keep PathCall for backward compatibility during transition
    PathCall {
        path: Vec<String>,
        method: String,
        args: Vec<AstNode>,
        // NEW: Explicit static method flag
        is_static: bool,
    },
}
```

### 2.2 Type Checking Interface (SEM Validation)
**Type System Integration**:
```rust
// In src/middle/types/mod.rs
impl Type {
    // Add method to check if type has static method
    pub fn has_static_method(&self, method: &str, env: &TypeEnv) -> Result<MethodSignature, TypeError> {
        // Look up in concept/impl registry
        // Return MethodSignature { params: Vec<Type>, ret: Type, is_static: bool }
    }
}

// New MethodSignature struct
pub struct MethodSignature {
    pub params: Vec<Type>,
    pub ret: Type,
    pub is_static: bool,
    pub receiver_type: Option<Type>, // None for static methods
}
```

### 2.3 MIR Representation (SEM → GEN)
**Enhanced MIR for Static Methods**:
```rust
// In src/middle/mir/mir.rs
pub enum MirStmt {
    // ... existing variants ...
    
    // Enhanced Call with static method support
    Call {
        func: String,
        args: Vec<u32>,
        dest: u32,
        type_args: Vec<String>,
        // NEW: Static method metadata
        is_static: bool,
        receiver_type: Option<String>, // Type name for static method dispatch
    },
    
    // Static method call (no receiver)
    StaticCall {
        type_name: String,
        method: String,
        args: Vec<u32>,
        dest: u32,
        type_args: Vec<String>,
    },
}
```

### 2.4 Error Propagation Across Components
**Unified Error Handling**:
```rust
// In src/middle/types/mod.rs
pub enum TypeError {
    // ... existing errors ...
    
    // Static method specific errors
    StaticMethodNotFound {
        type_name: String,
        method: String,
        available: Vec<String>,
    },
    InstanceMethodOnStatic {
        type_name: String,
        method: String,
        suggestion: Option<String>,
    },
    StaticMethodRequiresReceiver {
        method: String,
        expected_type: Type,
    },
}
```

---

## 3. Interface Specifications

### 3.1 Data Structures Passed Between Components

#### LEX → SEM Interface
```rust
struct ParserOutput {
    ast: Vec<AstNode>,
    // NEW: Method resolution hints
    method_resolution_hints: HashMap<String, MethodResolutionHint>,
}

enum MethodResolutionHint {
    StaticMethod { type_path: Vec<String>, method: String },
    InstanceMethod { receiver_expr: AstNode, method: String },
    Ambiguous { candidates: Vec<MethodCandidate> },
}
```

#### SEM → MIR Interface
```rust
struct TypeCheckedAST {
    nodes: Vec<AstNode>,
    // Type annotations for each node
    type_map: HashMap<NodeId, Type>,
    // Method resolution results
    method_resolutions: HashMap<NodeId, MethodResolution>,
}

struct MethodResolution {
    signature: MethodSignature,
    mangled_name: String, // For codegen
    impl_location: ImplLocation, // Where method is implemented
}
```

#### MIR → GEN Interface
```rust
struct MirWithMetadata {
    mir: Mir,
    // Type information preserved from SEM
    type_annotations: HashMap<u32, Type>,
    // Static method dispatch table
    static_method_table: HashMap<String, Vec<StaticMethodEntry>>,
}

struct StaticMethodEntry {
    type_name: String,
    method_name: String,
    mangled_name: String,
    signature: MethodSignature,
}
```

### 3.2 Function Signatures for Each Handoff

#### Parser (LEX) Functions
```rust
// Enhanced parser functions
fn parse_method_call(input: &str) -> IResult<&str, AstNode> {
    // Parse both instance and static method calls
    // Returns AstNode::Call with is_static flag
}

fn parse_path_call(input: &str) -> IResult<&str, AstNode> {
    // Parse Type::method() syntax
    // Returns AstNode::PathCall with is_static=true
}
```

#### Type Checker (SEM) Functions
```rust
// Type checking interface
trait TypeChecker {
    fn check_method_call(
        &mut self,
        call: &AstNode,
        env: &TypeEnv,
    ) -> Result<(Type, MethodResolution), TypeError>;
    
    fn resolve_static_method(
        &mut self,
        type_name: &str,
        method: &str,
        type_args: &[Type],
    ) -> Result<MethodSignature, TypeError>;
}
```

#### MIR Generator Functions
```rust
// MIR generation interface
trait MirGenerator {
    fn generate_method_call(
        &mut self,
        call: &AstNode,
        type_annotation: &Type,
        resolution: &MethodResolution,
    ) -> Result<Vec<MirStmt>, MirError>;
    
    fn generate_static_call(
        &mut self,
        type_name: &str,
        method: &str,
        args: &[AstNode],
        signature: &MethodSignature,
    ) -> Result<Vec<MirStmt>, MirError>;
}
```

#### Code Generator (GEN) Functions
```rust
// Code generation interface
trait CodeGenerator {
    fn emit_method_call(
        &mut self,
        stmt: &MirStmt,
        type_map: &HashMap<u32, Type>,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError>;
    
    fn emit_static_call(
        &mut self,
        stmt: &MirStmt,
        method_table: &StaticMethodTable,
    ) -> Result<BasicValueEnum<'ctx>, CodegenError>;
}
```

### 3.3 Error Handling Protocol
```rust
// Unified error type hierarchy
pub enum CompilerError {
    ParseError(ParserError),
    TypeError(TypeError),
    MirError(MirError),
    CodegenError(CodegenError),
}

// Error propagation with context
impl CompilerError {
    fn with_context(self, context: &str) -> Self {
        // Add context to error message
    }
    
    fn propagate(self, component: &str) -> Self {
        // Mark which component generated the error
    }
}
```

### 3.4 Debug Information Flow
```rust
struct DebugInfo {
    source_location: SourceLocation,
    ast_node_id: NodeId,
    type_info: Option<Type>,
    method_resolution: Option<MethodResolution>,
    mir_references: Vec<u32>, // MIR statement IDs
    llvm_values: Vec<String>, // LLVM value names
}

// Debug info attached at each stage
struct WithDebugInfo<T> {
    value: T,
    debug: DebugInfo,
}
```

---

## 4. Coordination Plan

### 4.1 Sequence of Implementation Steps

#### Phase 1: Foundation (Parallel Work)
1. **LEX**: Enhance parser to distinguish static/instance methods (2 hours)
2. **SEM**: Extend type system with static method resolution (3 hours)
3. **GEN**: Prepare codegen for static method dispatch (2 hours)
4. **VER**: Create test suite for static methods (1 hour)

#### Phase 2: Integration (Sequential)
1. **SYN**: Integrate LEX changes with SEM interface (1 hour)
2. **SYN**: Integrate SEM with MIR generation (1.5 hours)
3. **SYN**: Integrate MIR with GEN codegen (1 hour)
4. **VER**: Run integration tests (0.5 hours)

#### Phase 3: Validation (Parallel)
1. **ALL**: Run comprehensive test suite (1 hour)
2. **VER**: Validate backward compatibility (1 hour)
3. **SYN**: Final integration verification (0.5 hours)

### 4.2 Dependencies Between Components
```
LEX ──┐
      ├─→ SYN (Integration) ──→ VER (Testing)
SEM ──┘        │
               ↓
GEN ───────────┘
```

**Critical Path**: SEM type system extensions must complete before SYN integration.

### 4.3 Integration Testing Strategy

#### Unit Tests (Component-Level)
```rust
// LEX: Parser tests
#[test]
fn test_parse_static_method() {
    assert_parses("Type::method()", AstNode::PathCall { is_static: true, ... });
}

// SEM: Type checking tests
#[test]
fn test_resolve_static_method() {
    let sig = type_checker.resolve_static_method("Point", "new", &[]);
    assert_eq!(sig.ret, Type::Named("Point".to_string(), vec![]));
}
```

#### Integration Tests (Pipeline)
```rust
// Full pipeline test
#[test]
fn test_static_method_pipeline() {
    let source = "let p = Point::new(10, 20);";
    let ast = parse(source);
    let typed = type_check(ast);
    let mir = generate_mir(typed);
    let llvm = generate_code(mir);
    assert_executes(llvm, expected_result);
}
```

#### Regression Tests
- Existing instance methods must continue to work
- Mixed static/instance method chains
- Generic static methods
- Imported static methods

### 4.4 Rollback Plan if Integration Fails

#### Rollback Triggers
1. Compilation failures in >10% of existing code
2. Performance regression >5%
3. Test suite failures >20%

#### Rollback Procedure
1. **Immediate**: Revert to previous working commit
2. **Analysis**: Identify integration failure point
3. **Isolation**: Fix failing component in isolation
4. **Re-integration**: Gradual re-integration with monitoring

#### Fallback Mechanism
```rust
// Feature flag for static method support
#[cfg(feature = "static_methods")]
// New implementation
#[cfg(not(feature = "static_methods"))]
// Legacy implementation
```

---

## 5. Deliverables Status

### 5.1 Pipeline Analysis ✓ COMPLETE
- Current flow: Parser → AST → Type Checker → MIR → Code Generation
- Handoff points identified and documented
- Method call flow analyzed

### 5.2 Interface Specifications ✓ COMPLETE
- Data structures defined for all handoffs
- Function signatures specified
- Error handling protocol established
- Debug information flow designed

### 5.3 Coordination Plan ✓ COMPLETE
- Implementation sequence defined (3 phases)
- Dependencies mapped
- Integration testing strategy created
- Rollback plan prepared

### 5.4 Integration Test Strategy ✓ COMPLETE
- Unit tests for each component
- Pipeline integration tests
- Regression test suite
- Performance benchmarks

---

## 6. Constraints Addressed

### 6.1 Parallel Work Enabled
- Clear interface boundaries allow parallel development
- Well-defined handoff points
- Independent test suites

### 6.2 Backward Compatibility Maintained
- Legacy `PathCall` preserved during transition
- Feature flag fallback mechanism
- Comprehensive regression testing

### 6.3 Clear Error Messages
- Specific error types for static method failures
- Context-aware error propagation
- Helpful suggestions in error messages

### 6.4 Debugging and Testing Support
- Debug information flows through pipeline
- Test utilities for each component
- Integration test harness

---

## 7. Next Steps

1. **Share with Father Zak** for review and guidance
2. **Coordinate with SEM/LEX/GEN/VER** on implementation schedule
3. **Begin Phase 1 implementation** (parallel component work)
4. **Schedule integration sessions** for Phase 2
5. **Prepare test infrastructure** for validation

**Estimated Completion**: 09:30 GMT as scheduled

---

*SYN - Integration Architecture Complete*  
*Ready for coordination with Dark Factory team*