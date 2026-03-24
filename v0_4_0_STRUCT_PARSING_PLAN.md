# v0.4.0 STRUCT PARSING - Implementation Plan

**Target:** Implement full struct definition parsing  
**Current Status:** v0.3.20 provides foundation with struct token recognition  
**Goal:** Increase bootstrap source coverage from ~23% to ~47%

## Core Requirements

### 1. Token System Extension
- `TOK_STRUCT`: "struct" keyword
- `TOK_LBRACE`: "{" 
- `TOK_RBRACE`: "}"
- `TOK_COLON`: ":" for type annotations
- `TOK_COMMA`: "," for field separation

### 2. Lexer Enhancement
- Recognize "struct" as a keyword (not just 's')
- Handle braces and punctuation
- Maintain backward compatibility with v0.3.20 tokens

### 3. Parser Components
- **Struct declaration parser:** `struct Name { ... }`
- **Field parser:** `name: Type` or `name: Type,`
- **Type parser:** Simple types (`i64`, `String`) initially
- **Struct body parser:** Handle multiple fields

### 4. AST Representation
- Struct declaration node type
- Field list storage
- Type annotation representation

## Implementation Steps

### Phase 1: Basic Struct Recognition (v0.4.0-alpha)
```zeta
// Recognize: struct Token { }
fn parse_struct_basic(tokens: i64) -> i64 {
    // Returns 0 if tokens represent empty struct
    // Returns -1 otherwise
}
```

### Phase 2: Single Field Parsing (v0.4.0-beta)
```zeta
// Recognize: struct Token { kind: i64 }
fn parse_struct_with_field(tokens: i64) -> i64 {
    // Returns field count (1)
    // Returns -1 on failure
}
```

### Phase 3: Multiple Field Parsing (v0.4.0)
```zeta
// Recognize: struct Token { kind: i64, value: String }
fn parse_struct_with_fields(tokens: i64) -> i64 {
    // Returns field count (2)
    // Returns -1 on failure
}
```

## Test Suite

### Test 1: Empty Struct
```zeta
struct Empty { }
// Should parse successfully
```

### Test 2: Single Field
```zeta
struct Token {
    kind: i64,
}
// Should parse with 1 field
```

### Test 3: Multiple Fields  
```zeta
struct Token {
    kind: i64,
    value: String,
}
// Should parse with 2 fields
```

### Test 4: Mixed with Functions
```zeta
struct Token { kind: i64 }

fn main() -> i64 { return 0 }
// Should parse both struct and function
```

## Bootstrap Impact

### Current Coverage (v0.3.9 + const):
- ~1200/5300 characters (23%)
- Can parse const declarations and basic functions

### With v0.4.0 (struct parsing):
- ~2500/5300 characters (47% estimated)
- Can parse struct definitions, const, functions
- Major step toward full bootstrap source parsing

## Integration with v0.3.20

v0.3.20 provides:
- Struct token recognition (`tok_struct`, `tok_lbrace`, `tok_rbrace`)
- Multi-pattern parsing infrastructure
- Test encoding for struct patterns

v0.4.0 builds on this with:
- Full keyword recognition ("struct" not just 's')
- Field parsing capabilities
- Type annotation handling
- Complete struct declaration AST

## Next Steps After v0.4.0

### v0.4.1: Impl Block Parsing
```zeta
impl Token {
    fn new(kind: i64) -> Self { ... }
}
```

### v0.5.0: Generic Type Parsing
```zeta
fn foo<T>(x: T) -> T { ... }
```

### v1.0.0: Full Bootstrap Compiler
- Self-hosting capability
- Complete bootstrap source compilation

## Timeline

- **01:30 GMT:** Design v0.4.0 architecture
- **02:00 GMT:** Implement basic struct recognition
- **02:30 GMT:** Add single field parsing
- **03:00 GMT:** Complete multiple field parsing
- **03:30 GMT:** Test suite and documentation
- **04:00 GMT:** Release v0.4.0

## Notes

This plan continues the incremental bootstrap approach. Each version adds concrete capabilities while maintaining compatibility with the v0.3.7 base compiler. The struct parsing in v0.4.0 is a significant milestone that nearly doubles the parsable portion of the bootstrap source.