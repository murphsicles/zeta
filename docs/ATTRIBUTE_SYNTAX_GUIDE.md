# Attribute Syntax Guide for PrimeZeta Compatibility

## Overview

This document describes the attribute syntax implementation in Zeta for PrimeZeta compatibility. Attributes provide metadata and directives to the compiler about code elements.

## Syntax

### Outer Attributes
```zeta
#[attribute_name]
#[attribute_name(arg1, arg2)]
#[attribute_name(key = "value")]
```

### Inner Attributes (Planned)
```zeta
#![attribute_name]  // Applies to the enclosing module or crate
```

## Supported Attributes

### 1. `#[ai_opt]` - AI Optimization Hint
**Purpose**: Indicates that the following item should be optimized using AI techniques.

**Usage**:
```zeta
#[ai_opt]
comptime fn generate_residues() -> [NUM_RESIDUES]u64 {
    // AI-optimized compile-time function
}

#[ai_opt]
comptime stepping_lut: [[NUM_RESIDUES]u16; NUM_RESIDUES] = generate_stepping_lut()
```

**Compatibility**: Directly supports PrimeZeta line 108: `#[ai_opt] // xAI Grok live optimizer`

### 2. `#[derive(...)]` - Automatic Trait Implementation
**Purpose**: Automatically implements traits for a type.

**Usage**:
```zeta
#[derive(Clone, Debug, Copy)]
struct Point {
    x: i64,
    y: i64,
}
```

### 3. `#[test]` - Test Functions
**Purpose**: Marks a function as a test case.

**Usage**:
```zeta
#[test]
fn test_addition() {
    assert_eq!(2 + 2, 4);
}
```

### 4. `#[cfg(...)]` - Conditional Compilation
**Purpose**: Includes or excludes code based on configuration.

**Usage**:
```zeta
#[cfg(target_os = "linux")]
fn linux_specific() {
    // Linux-only code
}

#[cfg(feature = "ai_optimization")]
#[ai_opt]
fn ai_optimized_function() {
    // AI-optimized code
}
```

### 5. `#[allow(...)]` - Lint Control
**Purpose**: Suppresses specific lint warnings.

**Usage**:
```zeta
#[allow(unused_variables, dead_code)]
fn example() {
    let x = 10;  // No warning for unused variable
}
```

### 6. `#[inline]` - Inlining Hint
**Purpose**: Suggests function inlining to the compiler.

**Usage**:
```zeta
#[inline]
fn fast_add(a: i64, b: i64) -> i64 {
    a + b
}

#[inline(always)]
fn always_inlined() {
    // Always inlined
}
```

### 7. `#[repr(...)]` - Representation Control
**Purpose**: Controls type representation in memory.

**Usage**:
```zeta
#[repr(u8)]
enum Status {
    Ok = 0,
    Error = 1,
}
```

## Placement Rules

Attributes can be placed before:
- Function definitions
- Struct definitions
- Enum definitions
- Constant declarations (`const` and `comptime`)
- Module declarations
- Implementation blocks
- Trait definitions

## Multiple Attributes

Multiple attributes can be applied to the same item:

```zeta
#[cfg(test)]
#[allow(unused_variables)]
#[test]
fn test_function() {
    // Test code
}
```

## PrimeZeta Compatibility

### Current Status
- ✅ `#[ai_opt]` attribute parses correctly
- ✅ All common Rust-style attributes supported
- ✅ Multiple attributes on single item
- ✅ Nested brackets in attribute arguments
- ✅ String literals in attribute arguments

### Line 108 Compatibility
PrimeZeta line 108 contains:
```zeta
#[ai_opt] // xAI Grok live optimizer
```

**Implementation**: The `#[ai_opt]` attribute is parsed and stored in the AST. While the current implementation doesn't perform AI optimizations, the syntax is fully supported for compatibility.

### Future AI Optimization Integration

The `#[ai_opt]` attribute is designed to hook into future AI optimization passes:

```zeta
// Current: Parse and store
#[ai_opt]
comptime fn optimized() -> i64 {
    // Currently just parsed
    100
}

// Future: AI optimization pass
// 1. Parse #[ai_opt] attribute
// 2. During compilation, send function to AI optimizer
// 3. Replace with optimized implementation
// 4. Compile optimized version
```

## Implementation Details

### AST Changes
The `ConstDef` AST node now includes an `attrs` field:
```rust
ConstDef {
    name: String,
    ty: String,
    value: Box<AstNode>,
    attrs: Vec<String>,      // New field
    pub_: bool,
    comptime_: bool,
}
```

### Parser Updates
1. `parse_const` function now stores attributes instead of discarding them
2. All `ConstDef` creation sites updated to include `attrs` field
3. Attribute parsing handles nested brackets and string literals

### Backward Compatibility
- Existing code without attributes continues to work
- Attributes are optional for all items
- Unrecognized attributes are stored but ignored (for forward compatibility)

## Examples

### PrimeZeta-Style Code
```zeta
// PrimeZeta v0.5.0 compatibility
const MODULUS: u64 = 30030
const NUM_RESIDUES: usize = 5760

#[ai_opt]
comptime stepping_lut: [[NUM_RESIDUES]u16; NUM_RESIDUES] = generate_stepping_lut()

#[ai_opt]
comptime fn generate_stepping_lut() -> [[NUM_RESIDUES]u16; NUM_RESIDUES] {
    var lut: [[NUM_RESIDUES]u16; NUM_RESIDUES] = [[0; NUM_RESIDUES]; NUM_RESIDUES]
    // AI-optimized computation
    lut
}
```

### Common Usage Patterns
```zeta
// Test suite
#[cfg(test)]
mod tests {
    #[test]
    #[should_panic]
    fn test_panic() {
        panic!("Expected panic");
    }
}

// Conditional compilation
#[cfg(feature = "advanced")]
#[derive(Clone, Debug, Serialize, Deserialize)]
struct AdvancedType {
    data: Vec<u8>,
}

// Optimization hints
#[inline(always)]
#[ai_opt]
fn critical_path() -> i64 {
    // Performance-critical code
    42
}
```

## Limitations and Future Work

### Current Limitations
1. **Inner attributes** (`#![attr]`) not yet implemented
2. **Attribute macros** not supported
3. **Custom attribute validation** not implemented
4. **AI optimization** not yet hooked up (syntax only)

### Planned Enhancements
1. **Inner attribute support** for module/crate-level attributes
2. **Attribute validation** with meaningful error messages
3. **AI optimization pipeline** integration
4. **Custom attribute processors** for extensibility

## Testing

Attribute parsing is tested through:
1. Unit tests for the parser combinator
2. Integration tests with various attribute patterns
3. PrimeZeta compatibility tests
4. Edge cases (nested brackets, string literals, multiple attributes)

## Conclusion

The attribute syntax implementation provides full compatibility with PrimeZeta's `#[ai_opt]` attribute and common Rust-style attributes. While AI optimization is not yet implemented, the syntax foundation is in place for future integration.

The implementation follows the "parse now, optimize later" approach recommended for compatibility with existing PrimeZeta code while allowing for future enhancements.