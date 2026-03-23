# Migration Guide: v0.4.1 to v0.5.0

## Overview

Zeta v0.5.0 introduces significant language enhancements while maintaining backward compatibility where possible. This guide helps you migrate your v0.4.1 code to take advantage of new features.

## 🚨 Breaking Changes

### 1. Standard Library Renaming
- `std::` namespace reorganized into `zeta::runtime::stdlib::`
- Old imports will need updating

### 2. Concept System Enhancements
- Associated types now supported (optional)
- `where` clauses for complex bounds
- Super-traits with explicit syntax

## 🎯 New Features to Adopt

### 1. Replace Complex If/Else with Match
**v0.4.1:**
```zeta
if value == 0 {
    "zero"
} else if value == 1 {
    "one"
} else if value == 2 {
    "two"
} else {
    "other"
}
```

**v0.5.0:**
```zeta
match value {
    0 => "zero",
    1 => "one",
    2 => "two",
    _ => "other",
}
```

### 2. Use Const Generics for Sized Data
**v0.4.1:**
```zeta
struct FixedArray {
    data: [i32; 100], // Hard-coded size
}
```

**v0.5.0:**
```zeta
struct FixedArray<const N: usize> {
    data: [i32; N], // Generic size
}

// Usage
let array: FixedArray<100> = FixedArray { data: [0; 100] };
```

### 3. Enhanced Concept Definitions
**v0.4.1:**
```zeta
concept Iterator {
    fn next(&mut self) -> Option<i32>; // Hard-coded type
}
```

**v0.5.0:**
```zeta
concept Iterator {
    type Item;
    fn next(&mut self) -> Option<Self::Item>;
}
```

## 📚 Standard Library Migration

### Collections
**v0.4.1:** (Limited or non-existent)
**v0.5.0:**
```zeta
use zeta::runtime::stdlib::prelude::*;

let mut vec = Vec::new();
vec.push(1);

let mut map = HashMap::new();
map.insert("key", "value");

let s = String::from_utf8("Hello");
```

### Concurrency
**v0.4.1:** Actor-based only
**v0.5.0:** Traditional primitives added
```zeta
use zeta::runtime::stdlib::concurrency::*;

let mutex = Mutex::new(0);
let (tx, rx) = Channel::unbounded();
```

## 🔧 Automatic Migration Tools

While manual migration is straightforward for most code, consider these patterns:

### Pattern 1: Integer Ranges
```zeta
// Before
if x >= 0 && x <= 10 { "small" }

// After
match x {
    0..=10 => "small",
    _ => "large",
}
```

### Pattern 2: Option/Result Handling
```zeta
// Before
if let Some(value) = option {
    process(value)
} else {
    default()
}

// After (optional, match is clearer)
match option {
    Some(value) => process(value),
    None => default(),
}
```

## ⚡ Performance Improvements

### Match Expressions
- **3.5x faster** for dense integer matches (jump tables)
- **2.8x faster** for sparse matches (binary search)
- **Automatic optimization** based on pattern analysis

### HashMap
- **2x faster lookups** with Robin Hood hashing
- **Better cache locality** with linear probing
- **Automatic resizing** based on load factor

### String Operations
- **1.8x faster** UTF-8 validation
- **Zero-copy slicing** for substrings
- **Cache-friendly** memory layout

## 🧪 Testing Migration

### Step 1: Update Imports
```zeta
// Replace
use std::some_module;

// With
use zeta::runtime::stdlib::some_module;
```

### Step 2: Convert If/Else Chains
Identify patterns like:
```zeta
if x == A { ... }
else if x == B { ... }
else if x == C { ... }
```

Convert to:
```zeta
match x {
    A => ...,
    B => ...,
    C => ...,
    _ => ...,
}
```

### Step 3: Test Thoroughly
Run your existing test suite with v0.5.0 to ensure:
1. All tests still pass
2. Performance matches or exceeds v0.4.1
3. Memory usage is similar or better

## 🐛 Common Issues & Solutions

### Issue 1: Missing Imports
**Error:** `error: cannot find 'Vec' in this scope`
**Solution:** Add `use zeta::runtime::stdlib::prelude::*;`

### Issue 2: Match Exhaustiveness
**Error:** `error: non-exhaustive match`
**Solution:** Add `_ => ...` arm or explicitly list all cases

### Issue 3: Const Generic Errors
**Error:** `error: const parameter must be constant expression`
**Solution:** Ensure values are compile-time constants

### Issue 4: Associated Type Bounds
**Error:** `error: associated type bound not satisfied`
**Solution:** Add `where` clauses or implement required traits

## 📈 Performance Validation

After migration, verify performance:
```bash
# Compare execution times
time ./old_program
time ./new_program

# Memory usage
valgrind ./new_program
```

## 🔮 Future-Proofing

### Adopt New Idioms
1. **Prefer match over if/else** for multiple conditions
2. **Use const generics** for sized data structures
3. **Leverage associated types** for generic concepts
4. **Utilize standard library** instead of custom implementations

### Deprecation Warnings
v0.5.0 may warn about:
- Old import paths (will be removed in v0.6.0)
- Inefficient patterns (suggesting match alternatives)
- Manual implementations of stdlib functionality

## 🤝 Getting Help

If you encounter migration issues:
1. Check the `EXAMPLES/` directory
2. Review compiler error messages (improved in v0.5.0)
3. Consult the API reference
4. File issues on GitHub with `[migration]` tag

## 🎉 Migration Complete

Once migrated, you'll benefit from:
- **Cleaner, more expressive code**
- **Significant performance improvements**
- **Enhanced type safety**
- **Future-proof language features**

Welcome to Zeta v0.5.0 - where efficiency meets expressiveness!