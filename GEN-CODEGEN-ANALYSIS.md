# GEN - Code Generation Pattern Analysis
## Detailed Analysis of Zeta's LLVM Code Generator

**Date**: 2026-03-26  
**Author**: GEN (Zeta's Generative Engine)

## 1. File Structure Analysis

### 1.1 `codegen.rs` (1,150 lines)
**Actual size**: 1,150 lines (not 46,098 as previously reported)
**Structure**:
- **Lines 1-250**: Struct definition and constructor
- **Lines 251-400**: Runtime intrinsic declarations
- **Lines 401-800**: MIR generation methods (`gen_mirs`, `gen_stmt`, `gen_expr`)
- **Lines 801-1150**: Helper methods and operator implementations

### 1.2 Key Methods Identified
1. `new()` - Constructor with intrinsic declarations
2. `gen_mirs()` - Top-level MIR generation
3. `gen_stmt()` - Statement code generation
4. `gen_expr()` - Expression code generation
5. `finalize_and_jit()` - JIT compilation

## 2. Code Generation Patterns

### 2.1 Expression Pattern Matching
Current implementation uses match statements on `MirExpr`:
```rust
match expr {
    MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, false).into(),
    MirExpr::Var(id) => self.builder.build_load(self.ptr_type, self.locals[&id], ""),
    MirExpr::BinOp(op, lhs, rhs) => { /* operator dispatch */ }
    // ... 15+ variants
}
```

**Optimization Opportunity**: Could use visitor pattern or code generation templates for better extensibility.

### 2.2 Operator Implementation Pattern
Operators are implemented via external function calls:
```rust
module.add_function("==", i64_type.fn_type(&[i64_type.into(), i64_type.into()], false), ...);
```

**Observation**: This creates runtime function calls for basic operations instead of inline LLVM IR.

### 2.3 Type Handling Pattern
Single type system: `i64_type` used for almost everything.
- Integers: `i64`
- Booleans: `i64` (0/1)
- Pointers: `ptr_type`
- No float types currently

## 3. Performance Analysis

### 3.1 Potential Bottlenecks
1. **HashMap Lookups**: `self.locals[&id]` in hot paths
2. **External Calls**: Basic operators as function calls
3. **No Caching**: Repeated type/constant creation
4. **Linear Search**: No indexed access patterns

### 3.2 Memory Usage Patterns
- `HashMap<u32, PointerValue>` for local variables
- `HashMap<String, FunctionValue>` for functions
- No arena allocation or pooling

## 4. Optimization Recommendations

### 4.1 Immediate Improvements (Low Hanging Fruit)

#### 4.1.1 Inline Operator Generation
**Current**: External function calls for `==`, `!=`, `+`, `-`, etc.
**Proposed**: Generate inline LLVM IR for basic operators:
```rust
match op {
    "+" => self.builder.build_int_add(lhs_val, rhs_val, ""),
    "-" => self.builder.build_int_sub(lhs_val, rhs_val, ""),
    // etc.
}
```

**Benefit**: Eliminate function call overhead for basic operations.

#### 4.1.2 Local Variable Indexing
**Current**: `HashMap<u32, PointerValue>` with hash lookups
**Proposed**: `Vec<PointerValue>` with `id` as index (if IDs are dense)
**Alternative**: `Vec<Option<PointerValue>>` for sparse but fast lookup

**Benefit**: O(1) array access vs O(1) hash lookup (but with less overhead).

### 4.2 Medium-term Refactoring

#### 4.2.1 Modular Code Generation
Split `codegen.rs` into:
- `expression.rs` - Expression code generation
- `statement.rs` - Statement code generation  
- `function.rs` - Function and intrinsic handling
- `types.rs` - Type system and LLVM type management

**Benefit**: Better separation of concerns, easier testing.

#### 4.2.2 Visitor Pattern for MIR
Implement visitor pattern for `Mir` traversal:
```rust
trait MirVisitor {
    fn visit_expr(&mut self, expr: &MirExpr) -> BasicValueEnum;
    fn visit_stmt(&mut self, stmt: &MirStmt);
}
```

**Benefit**: Cleaner code structure, easier to add new optimizations.

### 4.3 Advanced Optimizations

#### 4.3.1 LLVM Optimization Passes
Currently missing optimization pipeline:
```rust
let pass_manager = PassManager::create(&module);
pass_manager.add_instruction_combining_pass();
pass_manager.add_reassociate_pass();
pass_manager.add_gvn_pass();
pass_manager.add_cfg_simplification_pass();
pass_manager.run(&module);
```

**Benefit**: Better generated code quality.

#### 4.3.2 Constant Folding
Implement compile-time constant folding:
- Evaluate constant expressions at compile time
- Propagate constants through expressions
- Eliminate dead code

#### 4.3.3 Common Subexpression Elimination
Identify and reuse common subexpressions within basic blocks.

## 5. Transformation Patterns for Rust → Zeta

### 5.1 Operator Translation Table
| Rust Operator | Zeta Equivalent | Notes |
|---------------|-----------------|-------|
| `+` | `+` (inline) | Currently external call |
| `-` | `-` (inline) | Currently external call |
| `==` | `==` (inline) | Currently external call |
| `!=` | `!=` (inline) | Currently external call |
| `+=` | Not supported | P1 blocker |
| `-=` | Not supported | P1 blocker |

### 5.2 Type Translation Patterns
| Rust Type | Zeta Type | Translation Strategy |
|-----------|-----------|---------------------|
| `i32` | `i64` | Zero/sign extend |
| `i64` | `i64` | Direct mapping |
| `f32` | Not supported | P1 blocker |
| `f64` | Not supported | P1 blocker |
| `bool` | `i64` (0/1) | Convert to integer |
| `String` | `i64` (pointer) | Runtime string representation |

### 5.3 Control Flow Patterns
| Rust Pattern | Zeta Pattern | Status |
|--------------|--------------|--------|
| `if condition { ... }` | `If(cond, then, else)` | Supported |
| `while condition { ... }` | `While(cond, body)` | Supported |
| `loop { ... }` | `Loop(body)` | Supported |
| `for x in iter { ... }` | Not supported | Future work |

## 6. Implementation Plan

### Phase 1: Inline Operators (Week 1)
1. Modify `gen_binop` to generate inline LLVM IR
2. Remove external function declarations for basic operators
3. Test performance impact

### Phase 2: Local Variable Optimization (Week 2)
1. Replace `HashMap` with `Vec` for local variables
2. Add bounds checking
3. Benchmark improvement

### Phase 3: Modular Refactoring (Week 3-4)
1. Split `codegen.rs` into modules
2. Implement visitor pattern
3. Add unit tests

### Phase 4: Optimization Passes (Week 5-6)
1. Add LLVM optimization pipeline
2. Implement constant folding
3. Add common subexpression elimination

## 7. Next Actions

1. **Create prototype**: Implement inline operator generation in a branch
2. **Benchmark**: Compare performance before/after changes
3. **Coordinate with siblings**: Work with LEX on lexer integration, SYN on parser improvements
4. **Report to Father Zak**: Present findings and get guidance on priorities

---

*This is the way. Every optimization must be measured, every change must be tested.*