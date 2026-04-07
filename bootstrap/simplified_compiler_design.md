# Simplified Compiler Design for v0.3.54

## Overview
Create a simplified version of the minimal compiler that uses only Zeta syntax (no Rust-like constructs) so it can be compiled by the current Zeta compiler.

## Current Limitations
The current `tests/minimal_compiler.z` contains Rust-like syntax that the Zeta compiler cannot parse:
1. `impl` blocks for structs
2. Struct method syntax (`fn method_name(&mut self)`)
3. `self` parameter in methods

## Design Goals
1. **Remove all Rust-like syntax** - No `impl` blocks, no struct methods
2. **Use only function-based organization** - All operations as standalone functions
3. **Maintain same functionality** - Parser, AST, code generator
4. **Ensure compatibility** - All syntax must be parseable by current Zeta compiler

## Architecture Design

### Data Structures (Zeta-compatible)
Replace structs with tuples and enums:

**Current (Rust-like):**
```zeta
struct Parser {
    input: String,
    pos: usize,
}

impl Parser {
    fn new(input: String) -> Parser { ... }
    fn parse_program(&mut self) -> AstNode { ... }
}
```

**Proposed (Zeta-only):**
```zeta
// Parser state as a tuple: (input: String, pos: usize)
// Functions take state as parameter and return updated state

fn parser_new(input: String) -> (String, usize) {
    (input, 0)
}

fn parser_parse_program(state: (String, usize)) -> ((String, usize), AstNode) {
    // Parse and return updated state + result
}
```

### Function Organization
All functions will be standalone, taking state as explicit parameters:

1. **Parser functions:**
   - `parser_new(input: String) -> (String, usize)`
   - `parser_parse_program(state: (String, usize)) -> ((String, usize), AstNode)`
   - `parser_parse_function(state: (String, usize)) -> ((String, usize), AstNode)`
   - `parser_parse_expr(state: (String, usize)) -> ((String, usize), AstNode)`

2. **Code generator functions:**
   - `codegen_new() -> (String, usize)` (output string, indent level)
   - `codegen_generate(state: (String, usize), ast: AstNode) -> (String, usize)`
   - `codegen_generate_function(state: (String, usize), func: FuncDef) -> (String, usize)`

3. **Utility functions:**
   - `parser_skip_whitespace(state: (String, usize)) -> (String, usize)`
   - `parser_consume(state: (String, usize), pattern: String) -> ((String, usize), bool)`
   - `codegen_add_indent(state: (String, usize)) -> (String, usize)`

### AST Representation
Keep the same enum structure (already Zeta-compatible):
```zeta
enum AstNode {
    Program(Vec<AstNode>),
    FuncDef {
        name: String,
        params: Vec<(String, String)>, // (name, type)
        ret: String,
        body: Vec<AstNode>,
    },
    // ... other variants
}
```

### Main Compiler Function
```zeta
fn compile_zeta_to_zeta(source: String) -> String {
    // Initialize parser state
    let parser_state = parser_new(source);
    
    // Parse program
    let (parser_state, ast) = parser_parse_program(parser_state);
    
    // Initialize code generator state
    let codegen_state = codegen_new();
    
    // Generate code
    let (codegen_state, _) = codegen_generate(codegen_state, ast);
    
    // Extract output string from state
    // State is (output: String, indent: usize)
    let (output, _) = codegen_state;
    output
}
```

## Implementation Plan

### Phase 1: Create Core Functions
1. **Create parser utility functions:**
   - `parser_new`, `parser_skip_whitespace`, `parser_consume`, `parser_peek`
   - Test each function independently

2. **Create AST parsing functions:**
   - `parser_parse_primary`, `parser_parse_expr`, `parser_parse_function`
   - Build up from simple to complex

3. **Create code generator functions:**
   - `codegen_new`, `codegen_add_indent`, `codegen_generate`
   - Test with simple AST nodes

### Phase 2: Integrate and Test
1. **Create main compiler function** `compile_zeta_to_zeta`
2. **Test with simple programs** from `tests/self_compile_test.z`
3. **Iterate and fix** any compilation errors

### Phase 3: Self-Compilation Test
1. **Test simplified compiler** with itself
2. **Verify output** is valid Zeta code
3. **Document results** and success criteria

## Success Criteria
1. ✅ Simplified compiler compiles successfully with current Zeta compiler
2. ✅ Simplified compiler can parse and generate code for simple Zeta programs
3. ✅ Self-compilation test produces valid output
4. ✅ All functionality preserved from original minimal compiler

## Files to Create
1. `tests/minimal_compiler_simplified.z` - Main simplified compiler
2. `tests/test_simplified_compiler.z` - Test program for simplified compiler
3. `bootstrap/simplified_compiler_test_results.md` - Test results documentation

## Timeline
- **Today (April 3):** Complete design and create core functions
- **Tomorrow (April 4):** Integrate and test with simple programs
- **April 5:** Complete self-compilation testing
- **April 6:** Document results and plan v0.3.55

## Notes
- Start with a minimal subset and expand gradually
- Test each function independently before integration
- Keep functions small and focused on single responsibility
- Use the existing `tests/self_compile_test.z` as reference for supported syntax