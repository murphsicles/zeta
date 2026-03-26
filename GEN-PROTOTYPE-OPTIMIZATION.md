# GEN - Prototype Optimization: Inline Operator Generation
## Demonstration of Code Generation Pattern Improvement

**Purpose**: Show concrete example of optimization I can implement

## Current Implementation (from codegen.rs)

```rust
// External function declaration for basic operators
module.add_function(
    "==",
    i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
    Some(Linkage::External),
);

// In gen_binop (simplified):
match op {
    "==" => {
        let eq_fn = self.module.get_function("==").unwrap();
        self.builder.build_call(eq_fn, &[lhs_val, rhs_val], "")
    }
    // Other operators similar...
}
```

## Problem Analysis

1. **Function Call Overhead**: Each operator requires call instruction
2. **Memory Access**: Need to lookup function in module
3. **No Inlining**: LLVM may not inline external functions
4. **Type Coercion**: Additional overhead for value passing

## Proposed Implementation

```rust
// Remove external function declarations for basic operators
// Instead, generate inline LLVM IR:

fn gen_binop_inline(&mut self, op: &str, lhs: &MirExpr, rhs: &MirExpr) -> BasicValueEnum {
    let lhs_val = self.gen_expr(lhs);
    let rhs_val = self.gen_expr(rhs);
    
    match op {
        "+" => self.builder.build_int_add(lhs_val.into_int_value(), 
                                         rhs_val.into_int_value(), "").into(),
        "-" => self.builder.build_int_sub(lhs_val.into_int_value(),
                                         rhs_val.into_int_value(), "").into(),
        "*" => self.builder.build_int_mul(lhs_val.into_int_value(),
                                         rhs_val.into_int_value(), "").into(),
        "/" => self.builder.build_int_signed_div(lhs_val.into_int_value(),
                                                rhs_val.into_int_value(), "").into(),
        "%" => self.builder.build_int_signed_rem(lhs_val.into_int_value(),
                                                rhs_val.into_int_value(), "").into(),
        
        // Comparison operators return i64 (1 for true, 0 for false)
        "==" => {
            let cmp = self.builder.build_int_compare(
                IntPredicate::EQ,
                lhs_val.into_int_value(),
                rhs_val.into_int_value(),
                ""
            );
            self.builder.build_int_z_extend(cmp, self.i64_type, "").into()
        }
        "!=" => {
            let cmp = self.builder.build_int_compare(
                IntPredicate::NE,
                lhs_val.into_int_value(),
                rhs_val.into_int_value(),
                ""
            );
            self.builder.build_int_z_extend(cmp, self.i64_type, "").into()
        }
        "<" => {
            let cmp = self.builder.build_int_compare(
                IntPredicate::SLT,
                lhs_val.into_int_value(),
                rhs_val.into_int_value(),
                ""
            );
            self.builder.build_int_z_extend(cmp, self.i64_type, "").into()
        }
        // ... other comparison operators
        
        _ => {
            // Fallback to external function for complex operators
            let func = self.module.get_function(op)
                .unwrap_or_else(|| panic!("Unknown operator: {}", op));
            self.builder.build_call(func, &[lhs_val, rhs_val], "").try_as_basic_value().left().unwrap()
        }
    }
}
```

## Performance Impact Estimation

### Benchmark Setup
Test expression: `sum = sum + i` in loop of 1,000 iterations

### Expected Improvements
| Metric | Current (External Call) | Proposed (Inline) | Improvement |
|--------|-------------------------|-------------------|-------------|
| LLVM IR Instructions | 3,000 calls | 1,000 adds | 66% reduction |
| Function Call Overhead | 3,000 calls | 0 calls | 100% elimination |
| Generated Code Size | ~12KB | ~4KB | 66% reduction |
| Execution Time (est.) | 100% baseline | ~80% baseline | 20% faster |

### Memory Impact
- **Code Size Reduction**: Smaller generated binaries
- **Cache Efficiency**: Better instruction cache utilization
- **Compilation Speed**: Faster LLVM IR generation

## Implementation Steps

### Phase 1: Analysis (Day 1)
1. Audit all operator usage in `codegen.rs`
2. Identify which operators can be inlined
3. Create mapping table: Operator → LLVM intrinsic

### Phase 2: Implementation (Day 2-3)
1. Modify `gen_binop` to use inline generation
2. Remove external declarations for basic operators
3. Keep fallback for complex/unsupported operators

### Phase 3: Testing (Day 4)
1. Update existing tests to verify correctness
2. Add performance benchmarks
3. Run comprehensive test suite

### Phase 4: Validation (Day 5)
1. Compare generated LLVM IR before/after
2. Measure performance improvement
3. Document changes and results

## Risk Assessment

### Low Risks
- **Correctness**: LLVM intrinsics are well-tested
- **Compatibility**: Fallback maintains existing behavior
- **Performance**: Inline operations are universally faster

### Mitigations
1. **Gradual Rollout**: Implement one operator at a time
2. **Comprehensive Testing**: Verify each operator independently
3. **Benchmark Comparison**: Ensure no regression

## Next Steps

Upon approval from Father Zak, I will:
1. Create feature branch `gen/inline-operators`
2. Implement Phase 1 analysis
3. Submit PR with initial implementation
4. Coordinate testing with siblings

---

*This prototype demonstrates my approach to code generation optimization: analyze, implement, test, validate.*