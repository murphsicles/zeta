# ZETA TOOLING REQUIREMENTS - LINTER/FORMATTER

## 🏭 LESSONS FROM CI FAILURES

### What We Learned Being Linted/Formatted:
1. **Unused code detection** - `parse_if_let` function, `nom::combinator::map` import
2. **Complex type factoring** - `FuncSignature` type alias for readability
3. **String manipulation patterns** - `strip_prefix()` vs manual slicing
4. **Control flow idioms** - `if let` vs single-pattern `match`
5. **Module structure conventions** - Module-inception pattern recognition
6. **Formatting consistency** - Line length, spacing, brace placement

## 🔧 ZETA LINTER REQUIREMENTS

### Must Catch (Zeta-specific):
1. **Zeta AST patterns** - Common Zeta-specific anti-patterns
2. **Parser combinator issues** - Nom pattern optimizations
3. **Type system misuse** - Zeta type system specific checks
4. **Code generation issues** - LLVM/backend specific patterns
5. **Memory management** - Zeta's ownership system checks

### Should Teach (Educational):
1. **Zeta idioms** - Preferred ways to express patterns
2. **Performance patterns** - Zeta-specific optimizations
3. **Readability improvements** - Zeta code clarity
4. **Error handling** - Zeta error propagation patterns

### Auto-fix Capabilities:
1. **Simple transformations** - Like `if let` conversion
2. **Type factoring** - Complex type extraction
3. **Import cleanup** - Unused import removal
4. **Formatting fixes** - Basic style corrections

## 🎨 ZETA FORMATTER REQUIREMENTS

### Style Enforcement:
1. **Indentation** - Consistent (spaces/tabs, width)
2. **Line length** - Zeta-specific max line length
3. **Import ordering** - Standardized import groups
4. **Comment formatting** - Consistent comment styles
5. **Brace placement** - Zeta brace style (K&R, Allman, etc.)

### Zeta-specific Formatting:
1. **Parser combinator formatting** - Nom expression layout
2. **AST node formatting** - Consistent AST literal representation
3. **Type annotation formatting** - Zeta type syntax spacing
4. **Pattern matching formatting** - Zeta pattern layout

## 🏗️ ARCHITECTURE CONSIDERATIONS

### Built in Zeta (Self-hosting goal):
1. **Linter written in Zeta** - Lints Zeta code (including itself)
2. **Formatter written in Zeta** - Formats Zeta code (including itself)
3. **Self-hosting toolchain** - Zeta tools for Zeta development

### Integration Points:
1. **CI integration** - Like current Rust CI
2. **Editor integration** - LSP support
3. **Pre-commit hooks** - Local validation
4. **Learning mode** - Educational explanations

## 📚 LEARNING FROM RUST TOOLING

### What Works Well (Copy):
1. **`-D warnings`** - Treat warnings as errors in CI
2. **Auto-fix capability** - `cargo clippy --fix`
3. **Gradual adoption** - Allow attributes for exceptions
4. **Educational messages** - Explanation with fixes

### What to Improve (Zeta-specific):
1. **Zeta-focused checks** - Not generic Rust checks
2. **Zeta performance hints** - Zeta compiler optimization advice
3. **Zeta idiom suggestions** - Zeta community preferred patterns
4. **Zeta error message integration** - Tied to Zeta compiler errors

## 🎯 PRIORITIZATION

### Phase 1 (Post v0.3.8):
1. **Basic formatter** - Simple formatting rules
2. **Essential lints** - Critical correctness checks
3. **CI integration** - Run on Zeta code

### Phase 2 (v0.4.0):
1. **Advanced linter** - Zeta-specific patterns
2. **Auto-fix capabilities** - Automated corrections
3. **Editor integration** - LSP support

### Phase 3 (v1.0.0):
1. **Self-hosting** - Linter/formatter written in Zeta
2. **Complete toolchain** - Full Zeta development experience
3. **Community style guide** - Official Zeta style

## 🏭 CURRENT STATUS

### Learning Phase (Now):
- ✅ **Experiencing CI friction** - Learning what matters
- ✅ **Fixing violations** - Understanding the fixes
- ✅ **Documenting patterns** - Noting what to check for
- ⏳ **Building foundation** - Clean codebase to build upon

### Next Step:
**Wait for CI green checkmark → Continue v0.3.8 work → Plan Zeta tooling**

---

*Created during CI wait time*
*Date: 2026-03-26*
*Purpose: Document linter/formatter requirements based on CI learning*