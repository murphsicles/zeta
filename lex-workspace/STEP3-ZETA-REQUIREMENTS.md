# STEP 3: Zeta Language Token Requirements Analysis

## By LEX (Tokenizer Child)
## Date: 2026-03-24 06:35 GMT

## 1. Zeta Language Overview

Based on analysis of:
- `zeta_src/main.z`, `tests.z`, `plan.z`
- `src/frontend/ast.rs` (AST definitions)
- `src/frontend/parser/*.rs` (current parser)

Zeta is a **systems programming language** with:
- Rust-like syntax
- Concepts (traits), implementations, structs, enums
- Actor model concurrency
- Generic programming
- LLVM backend

## 2. Required Token Categories

### 2.1 Keywords (Reserved Words)
From parser analysis:
```rust
[
    "let", "mut", "if", "else", "for", "in", "loop", "unsafe", "return", "break",
    "continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",
    "dyn", "box", "as", "true", "false",
    // TODO: "and", "or", "not" (logical operators)
]
```

**Missing from current:**
- `match`, `where`, `where`, `pub`, `priv`, `mod`, `crate`, `super`, `self`
- `while`, `do`, `yield`, `async`, `await`
- `const`, `static`, `ref`, `move`
- `trait` (using `concept` instead)
- `union`, `macro`

### 2.2 Operators
**Current (from expr.rs):**
```rust
["+", "-", "*", "/", "%", "==", "<=", ">=", "<", ">", "!=", "&&", "||", ".."]
```

**Missing Operators:**
1. **Assignment operators:** `=`, `+=`, `-=`, `*=`, `/=`, `%=`
2. **Bitwise operators:** `&`, `|`, `^`, `~`, `<<`, `>>`, `&=`, `|=`, `^=`, `<<=`, `>>=`
3. **Comparison:** `<=>`, `=>` (for match arms)
4. **Range:** `..=`, `...`
5. **Reference:** `&`, `&mut`, `*` (dereference)
6. **Type operators:** `->`, `=>`, `::`
7. **Misc:** `?`, `!`, `.`, `,`, `;`, `:`, `@`, `$`, `#`

### 2.3 Literals
**Currently supported:**
- Integer literals: `42` (decimal only)
- String literals: `"hello"` (no escapes)
- Boolean literals: `true`, `false`

**Required additions:**
1. **Floating point:** `3.14`, `2.0e10`, `1.0f32`, `1.0f64`
2. **Integer formats:**
   - Binary: `0b1010`
   - Octal: `0o777`
   - Hexadecimal: `0xFF`, `0xff`
   - With underscores: `1_000_000`
3. **Character literals:** `'a'`, `'\n'`, `'\x20'`, `'\u{1F600}'`
4. **String variants:**
   - Raw strings: `r#"..."#`
   - Byte strings: `b"..."`
   - Character escapes: `\n`, `\t`, `\"`, `\\`, `\0`, `\r`
5. **Array literals:** `[1, 2, 3]`
6. **Tuple literals:** `(1, "hello", true)`

### 2.4 Identifiers
**Current limitations:** ASCII-only
```rust
alpha1 = ASCII alphabetic
satisfy(|c: char| c.is_alphanumeric() || c == '_')
```

**Unicode requirements:**
1. **XID_Start** (first character): Unicode property
2. **XID_Continue** (subsequent characters): Unicode property
3. **Common mathematical symbols:** `α`, `β`, `γ`, `π`, `θ`, `Σ`, `∫`
4. **Emoji identifiers:** `🚀`, `🐛`, `✅` (for test names)
5. **Non-Latin scripts:** `変数`, `αριθμός`, `מספר`

### 2.5 Whitespace and Comments
**Currently supported:**
- Line comments: `// comment`
- Block comments: `/* comment */`
- Whitespace: spaces, tabs, newlines

**Issues:**
- No nested block comments
- No documentation comments (`///`, `/** */`)
- No shebang support (`#!`)

### 2.6 Delimiters and Punctuation
**Required:**
- Parentheses: `(`, `)`
- Brackets: `[`, `]`
- Braces: `{`, `}`
- Angle brackets: `<`, `>` (for generics)
- Colon: `:` (type annotations)
- Semicolon: `;` (statement terminator)
- Comma: `,` (separator)
- Dot: `.` (member access)
- At: `@` (attributes)
- Pound: `#` (attributes)
- Dollar: `$` (macros)
- Backtick: `` ` `` (raw identifiers)

## 3. Edge Cases Analysis

### 3.1 Ambiguous Token Sequences
1. `>>` vs `> >` in generics: `Vec<Vec<i32>>`
2. `..` vs `. .` (range vs two dots)
3. `->` vs `- >` (function return)
4. `=>` vs `= >` (match arm)
5. `::` vs `: :` (path separator)
6. `&&` vs `& &` (logical AND)
7. `||` vs `| |` (logical OR)

### 3.2 Number Format Edge Cases
1. `0.` vs `0..` (float vs range start)
2. `1.2.3` (invalid, should error)
3. `0x` (incomplete hex literal)
4. `0b` (incomplete binary literal)
5. `1__2` (multiple underscores)
6. `_123` (underscore at start of number)
7. `123_` (underscore at end of number)

### 3.3 String/Character Edge Cases
1. `""` (empty string)
2. `''` (empty character - invalid)
3. `"` (unterminated string)
4. `'` (unterminated character)
5. `"\x"` (incomplete hex escape)
6. `"\u{"` (incomplete Unicode escape)
7. Raw string delimiters: `r###"..."###`

### 3.4 Identifier Edge Cases
1. `_` (underscore identifier)
2. `_1` (underscore then number)
3. `foo_bar` (snake_case)
4. `FooBar` (CamelCase)
5. `FOO_BAR` (SCREAMING_SNAKE_CASE)
6. `foo123` (identifier with numbers)
7. `foo_bar123_baz` (mixed)

### 3.5 Operator Edge Cases
1. `+ +` vs `++` (unary plus vs increment)
2. `- -` vs `--` (unary minus vs decrement)
3. `* *` vs `**` (dereference vs exponentiation)
4. `& &` vs `&&` (bitwise AND vs logical AND)
5. `| |` vs `||` (bitwise OR vs logical OR)

## 4. Current Bootstrap Pain Points

### 4.1 From Code Analysis:
1. **No float literals** - Can't represent `3.14` in Zeta source
2. **No hex/binary literals** - Can't write bit manipulation code
3. **No escape sequences** - Can't write `"Hello\nWorld"`
4. **ASCII-only identifiers** - Limits mathematical code
5. **Missing operators** - No compound assignment, bitwise ops
6. **Poor error messages** - Hard to debug lexical issues

### 4.2 Impact on Self-Hosting:
To compile itself, Zeta needs:
1. **Full numeric support** - For math libraries
2. **String escapes** - For error messages, file paths
3. **Unicode identifiers** - For mathematical constants
4. **All operators** - For low-level code generation
5. **Good diagnostics** - For compiler development

### 4.3 Test Cases That Fail:
```zeta
// Current limitations in v0.3.7:
fn math_example() -> f64 {
    let π = 3.141592653589793;  // Fails: no floats, Unicode
    let radius = 0xFF;          // Fails: no hex
    let mask = 0b1010_1010;     // Fails: no binary, underscores
    return π * radius * radius;
}

fn string_example() -> String {
    let path = "C:\\Users\\zeta";  // Fails: no escape sequences
    let msg = "Line 1\nLine 2";    // Fails: no \n
    return path + msg;
}

fn bit_ops(x: i32, y: i32) -> i32 {
    return (x & 0xFF) | (y << 8);  // Fails: no &, |, << operators
}
```

## 5. Token Position Requirements

### For Error Reporting:
Each token needs:
1. **Line number** (1-based)
2. **Column number** (1-based)
3. **Byte offset** (0-based)
4. **Source file** (path/name)

### For Macros and Codegen:
1. **Preserve whitespace** (for formatting tools)
2. **Comment preservation** (for documentation generators)
3. **Exact source mapping** (for debug info)

## 6. Performance Considerations

### Lexer Requirements:
1. **Linear time** O(n) for source length n
2. **Constant memory** O(1) lookahead
3. **No backtracking** (deterministic)
4. **Streaming capable** (for large files)
5. **Zero-copy where possible** (string slices)

### Token Stream Interface:
```rust
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,  // or &'input str for zero-copy
    pub span: Span,      // line, column, byte range
}

pub struct Lexer<'input> {
    input: &'input str,
    pos: usize,
    line: usize,
    column: usize,
}
```

## 7. Summary of Requirements

### Must Have (for bootstrap):
1. **Complete numeric literals** (int, float, hex, binary, octal)
2. **String escapes** (`\n`, `\t`, `\"`, `\\`, `\xHH`, `\u{...}`)
3. **Character literals** (`'a'`, `'\n'`)
4. **All operators** (arithmetic, bitwise, logical, assignment)
5. **Unicode identifiers** (XID_Start/XID_Continue)
6. **Position information** (line, column for errors)

### Should Have (for usability):
1. **Raw strings** (`r#"..."#`)
2. **Byte strings** (`b"..."`)
3. **Documentation comments** (`///`, `/** */`)
4. **Nested block comments**
5. **Shebang support** (`#!`)

### Nice to Have (future):
1. **Custom numeric suffixes** (`1.0f32`, `100u64`)
2. **Format strings** (interpolated)
3. **Regex literals** (`/pattern/`)
4. **Here documents** (multiline strings)

---

*Next Step: Propose First Principles lexer design*