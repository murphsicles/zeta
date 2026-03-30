# ARCHITECTURE DISCOVERY: Dual Type Systems

**Date:** 2026-03-30  
**Discovery Context:** Static Method Support Sprint (v0.3.20)  
**Discoverer:** Father Zak during type mismatch investigation

## 🚨 THE PROBLEM

During implementation of static method support (`Point::new()`), we encountered:
- `i64 vs i32` type mismatches
- Method calls returning `0` instead of correct values
- Method mangling always using `_i64` suffix

## 🔍 ROOT CAUSE ANALYSIS

### Discovery 1: Dual Type Systems Exist
The Zeta compiler has **TWO SEPARATE TYPE SYSTEMS** that don't communicate:

1. **String-Based Type System (Legacy/Broken)**
   - Location: `resolver.rs` (`pub type Type = String`)
   - Usage: MIR generation, method mangling, specialization
   - Problem: Defaults to `"i64"` everywhere, no proper type inference

2. **Algebraic Type System (Correct/Unused)**
   - Location: `src/middle/types/` (`Type` enum)
   - Creator: SEM agent during type system implementation
   - Features: Type inference, unification, variant types, coercion
   - Problem: Not integrated with rest of compiler

### Discovery 2: Compiler Pipeline Flaw
**Current (Flawed) Pipeline:**
```
Parse → [String Types] → MIR Generation → Type Checking → Codegen
```

**Should Be:**
```
Parse → [Algebraic Types] → Type Checking → MIR Generation → Codegen
```

### Discovery 3: No Type Information Flow
- MIR generation happens BEFORE type checking
- MIR guesses types (defaults to i64)
- Type checking happens but doesn't update MIR
- Code generation uses wrong types

## 🏗️ ARCHITECTURAL IMPACT

### Technical Debt Accumulated:
1. **String type literals** (`"i64"`, `"i32"`) throughout codebase
2. **Hardcoded defaults** to i64 in MIR generation
3. **Separate type caches** that don't sync
4. **Method mangling** based on string types, not real types

### Symptoms Caused:
1. `i64 vs i32` type mismatches
2. Method returns `0` (wrong type inference)
3. Generic methods mangled as `_i64` always
4. No proper type coercion/int inference

## 🔧 THE FIX (In Progress)

**Agent:** TYPE-INTEGRATION-AGENT  
**Mission:** Integrate algebraic type system throughout compiler

### Phase 1: Update Resolver
- Change `pub type Type = String` to use `crate::middle::types::Type`
- Update all type references in resolver
- Fix compilation errors

### Phase 2: Pass Types to MIR
- Modify `resolver.lower_to_mir()` to pass type map
- Update MIR generation to use provided types
- Fix method mangling with real types

### Phase 3: Integration Testing
- Test static method compilation
- Verify i64/i32 mismatches fixed
- Ensure backward compatibility

## 📚 LESSONS LEARNED

### For Future Architecture:
1. **Single Source of Truth**: One type system, not two
2. **Pipeline Order Matters**: Type check before MIR generation
3. **Integration Testing**: Test type system integration early
4. **Documentation**: Document type system architecture

### For Agent Coordination:
1. **SEM created correct type system** but didn't integrate it
2. **Other agents used string types** without knowing about algebraic system
3. **No integration testing** between type system and rest of compiler
4. **Architecture review needed** for major subsystem changes

## 🎯 EXPECTED OUTCOMES

After TYPE-INTEGRATION-AGENT completes:

1. **✅ Static methods work correctly**: `Point::new(10, 20)` returns correct value
2. **✅ Type mismatches fixed**: No more `i64 vs i32` errors
3. **✅ Method mangling correct**: Uses actual types, not always `_i64`
4. **✅ Proper type inference**: Literals infer correct types
5. **✅ Backward compatibility**: Existing code still works

## 🔮 FUTURE WORK

1. **Type system serialization**: For caching/specialization
2. **Better error messages**: Type mismatch diagnostics
3. **Type inference improvements**: For literals, generics
4. **Integration with borrow checker**: For reference types

---
**Status:** TYPE-INTEGRATION-AGENT currently working on the fix
**Estimated Completion:** 30-60 minutes from discovery
**Impact:** Critical for static method support and future type features