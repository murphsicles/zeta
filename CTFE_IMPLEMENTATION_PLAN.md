# CTFE IMPLEMENTATION PLAN - MISSION D
## Night Sprint to Fully Implement Compile-Time Function Execution

**Goal**: Enable `ZETA_CHAMPION_30030.z` to compile and run with CTFE, dominating the competition.

**Current Status**: CTFE Rust backend exists (`src/middle/ctfe/`), but Zeta frontend implementation incomplete.

---

## PHASE 1: ANALYSIS (30 min) - COMPLETE ✅

### Findings:
1. **Rust CTFE backend exists**:
   - `src/middle/ctfe/evaluator.rs` - Main evaluator
   - `src/middle/ctfe/evaluator_complete.rs` - Complete implementation
   - `src/middle/const_eval.rs` - Public interface

2. **Zeta frontend missing**:
   - `comptime` keyword recognized but not fully implemented
   - CTFE functions cause runtime panic
   - Need Zeta source files implementing CTFE

3. **Competition file issue**:
   - `ZETA_CHAMPION_30030.z` has `comptime fn generate_residues()`
   - Causes: "Note: Comptime function 'generate_residues' - compile-time evaluation not fully implemented yet"

---

## PHASE 2: CTFE ZETA IMPLEMENTATION (3 hours)

### Step 1: Create Zeta CTFE Module Structure
```
src/middle/ctfe/
├── ctfe.z           # Main CTFE interface
├── evaluator.z      # Zeta evaluator (calls Rust backend)
├── value.z          # ConstValue type definitions
├── context.z        # Evaluation context
└── error.z          # CTFE error handling
```

### Step 2: Implement Basic CTFE Evaluation
1. **Integer arithmetic at compile time**
   - Support `+`, `-`, `*`, `/`, `%`
   - Constant folding for binary operations
   - Literal evaluation

2. **Function evaluation**
   - `comptime fn` recognition and registration
   - Parameter substitution
   - Return value computation

3. **Array generation** (for competition)
   - Compile-time array creation
   - Loop unrolling at compile time
   - Array literal generation

### Step 3: Integration with Compiler Pipeline
1. **Hook into compilation process**
   - CTFE pass after parsing, before codegen
   - AST transformation for evaluated constants
   - Error reporting for CTFE failures

2. **Backend integration**
   - Call Rust CTFE evaluator from Zeta
   - Convert Zeta AST to Rust AST for evaluation
   - Convert Rust ConstValue back to Zeta AST

### Step 4: Test with Simple Examples
1. **Basic constant evaluation**
   ```zeta
   comptime N: i64 = 100 + 200;  // Should evaluate to 300
   ```

2. **Function evaluation**
   ```zeta
   comptime fn add(a: i64, b: i64) -> i64 {
       return a + b;
   }
   comptime SUM: i64 = add(100, 200);  // Should be 300
   ```

3. **Array generation**
   ```zeta
   comptime fn make_array() -> [5]i64 {
       let arr: [5]i64 = [];
       arr[0] = 1;
       arr[1] = 2;
       arr[2] = 3;
       arr[3] = 4;
       arr[4] = 5;
       return arr;
   }
   comptime ARR: [5]i64 = make_array();
   ```

---

## PHASE 3: COMPETITION INTEGRATION (2 hours)

### Step 1: Fix `ZETA_CHAMPION_30030.z`
1. **Implement `generate_residues()` CTFE**
   - Generate 5760 residues coprime to 30030
   - Compile-time loop unrolling
   - Array creation at compile time

2. **Optimize for competition**
   - Pre-compute all residues
   - Eliminate runtime computation
   - Generate optimal code

### Step 2: Performance Testing
1. **Benchmark before/after CTFE**
   - Measure initialization time reduction
   - Verify correctness (78498 primes)
   - Compare with `COMPETITION_FINAL_OPTIMIZED.z`

2. **Optimization passes**
   - Constant propagation
   - Dead code elimination for CTFE-only code
   - Inlining of CTFE functions

### Step 3: Documentation & Examples
1. **CTFE usage guide**
2. **Competition advantage documentation**
3. **Performance comparison report**

---

## PHASE 4: FINAL VERIFICATION (30 min)

### Success Criteria:
1. ✅ `ZETA_CHAMPION_30030.z` compiles without CTFE error
2. ✅ `generate_residues()` executes at compile time
3. ✅ Competition entry uses CTFE-generated residues
4. ✅ Performance improvement over non-CTFE version
5. ✅ Maintains correctness (78498 primes)

### Deliverables:
1. Fully functional CTFE implementation in Zeta
2. Updated competition entry using CTFE
3. Performance benchmarks
4. Documentation and examples

---

## TECHNICAL APPROACH

### 1. Leverage Existing Rust CTFE
The Rust backend already has CTFE implemented. We need to:
- Expose Rust CTFE functions to Zeta
- Create Zeta wrappers for CTFE operations
- Integrate into Zeta compilation pipeline

### 2. Zeta CTFE Architecture
```
Zeta Source (.z)
       ↓
   Parser (Rust)
       ↓
   AST with comptime nodes
       ↓
   CTFE Pass (Zeta calling Rust)
       ↓
   Transformed AST (constants evaluated)
       ↓
   Code Generation
```

### 3. Key Implementation Files
1. **`src/middle/ctfe.z`** - Main Zeta CTFE interface
2. **`src/frontend/parser/ctfe_parser.z`** - CTFE-aware parsing
3. **`src/middle/resolver/ctfe_resolver.z`** - CTFE resolution
4. **`src/backend/codegen/ctfe_codegen.z`** - CTFE code generation

### 4. Competition-Specific CTFE
For the 30030-wheel algorithm:
- Generate residues at compile time: 5760 values
- Pre-compute wheel offsets
- Optimize memory layout for cache

---

## RISK MITIGATION

### Fallback Strategy:
1. Keep `COMPETITION_FINAL_OPTIMIZED.z` as backup entry
2. Incremental CTFE implementation
3. Test each feature independently

### Testing Strategy:
1. Unit tests for each CTFE feature
2. Integration tests with competition algorithm
3. Performance regression tests

### Rollback Plan:
If CTFE implementation fails:
1. Submit `COMPETITION_FINAL_OPTIMIZED.z` (23ms performance)
2. Document CTFE as "future work"
3. Still competitive entry

---

## TIMELINE

**Phase 1: Analysis** - ✅ COMPLETE (30 min)
**Phase 2: Implementation** - 3 hours (22:45 - 01:45)
**Phase 3: Competition Integration** - 2 hours (01:45 - 03:45)
**Phase 4: Verification** - 30 min (03:45 - 04:15)

**Total**: 6 hours (complete before Roy's return)

---

## STARTING IMPLEMENTATION

Beginning with creating Zeta CTFE module structure and basic evaluation.

**First target**: Make `comptime N: i64 = 100 + 200;` evaluate to 300 at compile time.

**Second target**: Make `comptime fn add()` work with simple arithmetic.

**Final target**: Make `generate_residues()` compile and generate 5760 residues at compile time.