# STEP 4: First Principles Lexer Design for Zeta

## By LEX (Tokenizer Child)
## Date: 2026-03-24 06:45 GMT

## 1. First Principles Foundation

### Core Question: "What is a token?"
**Answer:** A token is the smallest meaningful unit in a programming language, with:
- **Type** (category: keyword, identifier, literal, operator, etc.)
- **Value** (lexeme: the actual characters)
- **Position** (source location: file, line, column, byte range)

### Mathematical Model:
```
Σ = set of characters (Unicode code points)
T = set of token types (finite set)
L_t ⊆ Σ* = regular language for token type t ∈ T

Lexer: Σ* → (T × Σ* × Position)*
       (character stream → token stream with positions)
```

## 2. Ideal Zeta Lexer Design

### 2.1 Architecture Overview
```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Character     │    │   Finite        │    │   Token         │
│   Stream        │───▶│   Automaton     │───▶│   Stream        │
│   (UTF-8)       │    │   (DFA)         │    │   with Positions│
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
    Bytes/Chars           State transitions        Typed tokens
```

### 2.2 Token Categories (Complete Set)

#### 2.2.1 Keywords (Reserved Words)
```rust
pub enum Keyword {
    // Declaration
    Let, Mut, Fn, Concept, Impl, Enum, Struct, Type, Use, Extern,
    Mod, Pub, Crate, Super, Self_, // Self is Rust keyword
    
    // Control flow
    If, Else, Match, For, In, While, Loop, Break, Continue, Return,
    Where, 
    
    // Types and traits
    Dyn, Box, As, Ref, Move, Const, Static,
    
    // Boolean literals
    True, False,
    
    // Concurrency
    Async, Await, Yield, Spawn,
    
    // Unsafe
    Unsafe,
    
    // TODO: Add as needed
}
```

#### 2.2.2 Operators (Complete Set)
```rust
pub enum Operator {
    // Arithmetic
    Plus,          // +
    Minus,         // -
    Star,          // *
    Slash,         // /
    Percent,       // %
    
    // Assignment
    Eq,            // =
    PlusEq,        // +=
    MinusEq,       // -=
    StarEq,        // *=
    SlashEq,       // /=
    PercentEq,     // %=
    
    // Comparison
    EqEq,          // ==
    Ne,            // !=
    Lt,            // <
    Gt,            // >
    Le,            // <=
    Ge,            // >=
    Spaceship,     // <=> (for ordering)
    
    // Logical
    AndAnd,        // &&
    OrOr,          // ||
    Not,           // !
    
    // Bitwise
    And,           // &
    Or,            // |
    Caret,         // ^
    Tilde,         // ~
    Shl,           // <<
    Shr,           // >>
    AndEq,         // &=
    OrEq,          // |=
    CaretEq,       // ^=
    ShlEq,         // <<=
    ShrEq,         // >>=
    
    // Other
    Dot,           // .
    DotDot,        // ..
    DotDotEq,      // ..=
    DotDotDot,     // ... (deprecated range)
    Colon,         // :
    ColonColon,    // ::
    Semicolon,     // ;
    Comma,         // ,
    Arrow,         // ->
    FatArrow,      // =>
    At,            // @
    Pound,         // #
    Dollar,        // $
    Question,      // ?
    Backtick,      // `
}
```

#### 2.2.3 Literals
```rust
pub enum Literal {
    Integer {
        value: u128,           // Numeric value
        suffix: Option<String>, // e.g., "u64", "i32"
        radix: Radix,          // Decimal, Hex, Octal, Binary
    },
    Float {
        value: f64,            // Numeric value
        suffix: Option<String>, // e.g., "f32", "f64"
    },
    Char {
        value: char,           // Unicode character
    },
    String {
        value: String,         // UTF-8 string
        kind: StringKind,      // Normal, Raw, Byte
    },
    Boolean(bool),             // true or false
}
```

#### 2.2.4 Other Tokens
```rust
pub enum TokenKind {
    Keyword(Keyword),
    Operator(Operator),
    Literal(Literal),
    Identifier(String),
    Comment(String),           // With kind: Line, Block, Doc
    Whitespace,                // Significant for formatting
    Delimiter(Delimiter),      // (), [], {}, <>
    Eof,                       // End of file
    Error(LexError),           // Lexical error with details
}
```

### 2.3 Finite Automaton Design

#### 2.3.1 DFA States
```rust
pub enum LexerState {
    Start,                     // Initial state
    InIdentifier,              // [a-zA-Z_][a-zA-Z0-9_]*
    InNumber,                  // [0-9]+
    InFloat,                   // [0-9]+.[0-9]+
    InHex,                     // 0x[0-9a-fA-F]+
    InOctal,                   // 0o[0-7]+
    InBinary,                  // 0b[01]+
    InString,                  // "..." 
    InRawString,               // r#"..."#
    InChar,                    // '.' 
    InLineComment,             // // ...
    InBlockComment,            // /* ... */
    InDocComment,              /// or //!
    InOperator,                // +, -, etc.
    InWhitespace,              // spaces, tabs
    Error,                     // Invalid input
}
```

#### 2.3.2 Transition Table
For each state and input character, define next state:
- **Start state:** Based on first character
- **Identifier:** `[XID_Start]` → `InIdentifier`
- **Number:** `[0-9]` → `InNumber`
- **String:** `"` → `InString`
- **Operator:** `+`, `-`, etc. → `InOperator`
- **Comment:** `/` followed by `/` or `*` → `InLineComment` or `InBlockComment`

### 2.4 Unicode Handling

#### 2.4.1 Identifier Rules (Unicode UAX #31)
```rust
fn is_xid_start(c: char) -> bool {
    // Unicode property: XID_Start
    // Includes letters, plus some symbols like _
}

fn is_xid_continue(c: char) -> bool {
    // Unicode property: XID_Continue
    // Includes XID_Start + digits + connector punctuation
}
```

#### 2.4.2 Normalization
- Store identifiers in **NFC** (Normalization Form C)
- Allows `café` and `cafe\u{301}` to compare equal
- Important for case-insensitive comparisons if needed

### 2.5 Error Handling

#### 2.5.1 Lexical Errors
```rust
pub enum LexError {
    UnclosedString,            // "hello
    UnclosedChar,              // '
    UnclosedComment,           // /* comment
    InvalidEscape,             // \x
    InvalidUnicodeEscape,      // \u{invalid}
    InvalidNumber,             // 123abc
    InvalidFloat,              // 1.2.3
    UnexpectedCharacter,       // @$` (if not allowed)
    UnicodeError,              // Invalid UTF-8
}
```

#### 2.5.2 Error Recovery
1. **Skip invalid character** and continue
2. **Skip to next whitespace** for malformed tokens
3. **Insert error token** in stream with details
4. **Continue lexing** where possible

### 2.6 Position Tracking

#### 2.6.1 Span Structure
```rust
pub struct Span {
    pub file: PathBuf,         // Source file path
    pub start: Position,       // Start position
    pub end: Position,         // End position
    pub byte_range: Range<usize>, // Byte offsets
}

pub struct Position {
    pub line: usize,           // 1-based line number
    pub column: usize,         // 1-based column number
    pub byte_offset: usize,    // 0-based byte offset
}
```

#### 2.6.2 Line Ending Handling
- **LF:** `\n` (Unix)
- **CRLF:** `\r\n` (Windows)
- **CR:** `\r` (Mac classic)
- Count as **one line break** regardless of sequence

## 3. Specific Fixes for v0.3.7

### 3.1 Immediate Fixes (Priority 1)

#### 3.1.1 Add Float Literals
```rust
// Current: only integers
fn parse_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, num) = take_while(|c: char| c.is_digit(10)).parse(input)?;
    // ...
}

// Fix: add float support
fn parse_number(input: &str) -> IResult<&str, AstNode> {
    // Recognize: [0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
    // Return AstNode::FloatLit(f64) or AstNode::Lit(i64)
}
```

#### 3.1.2 Add String Escapes
```rust
// Current: no escapes
let (input, content) = take_while(|c: char| c != '"').parse(input)?;

// Fix: parse escape sequences
fn parse_string_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("\"")(input)?;
    let mut content = String::new();
    let mut chars = input.chars();
    // Parse with escape handling
}
```

#### 3.1.3 Add Missing Operators
Extend operator list in `parse_expr()`:
```rust
let operators = [
    // Current
    "+", "-", "*", "/", "%", "==", "<=", ">=", "<", ">", "!=", "&&", "||", "..",
    // Add
    "=", "+=", "-=", "*=", "/=", "%=",
    "&", "|", "^", "~", "<<", ">>",
    "&=", "|=", "^=", "<<=", ">>=",
    "..=",
];
```

### 3.2 Medium-term Fixes (Priority 2)

#### 3.2.1 Unicode Identifiers
Replace `alpha1` with Unicode-aware functions:
```rust
// Current: ASCII only
alt((alpha1, tag("_")))

// Fix: Unicode XID_Start
fn is_xid_start(c: char) -> bool { /* Unicode property */ }
fn is_xid_continue(c: char) -> bool { /* Unicode property */ }
```

#### 3.2.2 Hex/Octal/Binary Literals
```rust
fn parse_number(input: &str) -> IResult<&str, AstNode> {
    alt((
        parse_binary_lit,   // 0b[01]+
        parse_octal_lit,    // 0o[0-7]+
        parse_hex_lit,      // 0x[0-9a-fA-F]+
        parse_float_lit,    // [0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
        parse_int_lit,      // [0-9]+
    ))
}
```

#### 3.2.3 Character Literals
```rust
fn parse_char_lit(input: &str) -> IResult<&str, AstNode> {
    let (input, _) = tag("'")(input)?;
    let (input, ch) = parse_char_escape(input)?; // Handles '\n', '\u{...}', etc.
    let (input, _) = tag("'")(input)?;
    Ok((input, AstNode::CharLit(ch)))
}
```

### 3.3 Long-term Fixes (Priority 3)

#### 3.3.1 Separate Lexer Phase
Refactor to proper lexer:
```rust
// Current: lexerless parser
pub fn parse_zeta(input: &str) -> IResult<&str, Vec<AstNode>> {
    // Direct character parsing
}

// Future: separate lexer
pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    // Return token stream
}

pub fn parse(tokens: &[Token]) -> Result<Vec<AstNode>, ParseError> {
    // Parse from tokens
}
```

#### 3.3.2 Position Tracking
Add span information to AST:
```rust
pub struct AstNode {
    pub kind: AstKind,
    pub span: Span,  // Source location
}
```

#### 3.3.3 Error Reporting
Improve error messages:
```rust
// Current: generic parse error
format!("Parse error: {:?}", e)

// Future: detailed error
format!("{}:{}:{}: error: {}", 
    span.file, span.start.line, span.start.column, message)
```

## 4. Action Plan for v0.3.7 Improvements

### Phase 1: Immediate Bootstrap Fixes (Week 1)
1. **Add float literals** - Required for math code
2. **Add string escapes** - Required for file paths, error messages
3. **Add missing operators** - `=`, `+=`, `-=`, `&`, `|`, etc.
4. **Add character literals** - `'a'`, `'\n'`

### Phase 2: Unicode and Number Formats (Week 2)
1. **Unicode identifiers** - XID_Start/XID_Continue
2. **Hex/octal/binary literals** - `0xFF`, `0o777`, `0b1010`
3. **Underscores in numbers** - `1_000_000`
4. **Raw strings** - `r#"..."#`

### Phase 3: Separate Lexer (Week 3-4)
1. **Design token enum** - Complete token types
2. **Implement DFA lexer** - Finite automaton
3. **Add position tracking** - Line, column, byte offsets
4. **Integrate with parser** - Replace nom combinators

### Phase 4: Error Reporting (Week 5)
1. **Lexical error messages** - Clear error diagnostics
2. **Source mapping** - For debug information
3. **Error recovery** - Skip bad tokens, continue
4. **Test suite** - Comprehensive lexer tests

## 5. Implementation Strategy

### 5.1 Incremental Migration
1. **Keep current parser working** - Don't break bootstrap
2. **Add new features alongside old** - Dual support
3. **Gradually replace nom combinators** - One token at a time
4. **Test thoroughly** - Ensure no regressions

### 5.2 Testing Approach
1. **Unit tests** - Each token type
2. **Integration tests** - Full Zeta source files
3. **Property tests** - Random valid/invalid inputs
4. **Fuzz testing** - Random byte streams

### 5.3 Performance Goals
1. **Linear time** - O(n) for n characters
2. **Zero-copy where possible** - String slices
3. **Minimal allocations** - Reuse buffers
4. **Streaming capable** - Large files

## 6. First Principles Validation

### 6.1 Mathematical Correctness
- **Regular languages:** All token patterns are regular
- **Finite automaton:** Can be implemented as DFA
- **Deterministic:** No backtracking needed
- **Composition:** Lexer + Parser = complete frontend

### 6.2 Practical Requirements
- **Complete:** Handles all Zeta language constructs
- **Efficient:** Fast enough for self-hosting
- **Robust:** Good error handling and recovery
- **Maintainable:** Clean, testable code

### 6.3 Bootstrap Compatibility
- **Backward compatible:** Existing Zeta code works
- **Forward looking:** Supports future language features
- **Self-hosting ready:** Can lex Zeta compiler source

## 7. Conclusion

The v0.3.7 lexerless parser violates First Principles by:
1. **Mixing lexical and syntactic analysis**
2. **Not using finite automata** for regular languages
3. **Lacking proper token abstraction**
4. **Having incomplete token coverage**

The proposed design fixes these by:
1. **Separate lexer phase** - Characters → Tokens
2. **DFA implementation** - Efficient, correct
3. **Complete token set** - All Zeta language features
4. **Position tracking** - Good error reporting
5. **Unicode support** - Modern language requirements

**Next Steps:** Begin implementing Phase 1 fixes for immediate bootstrap needs, while designing the proper lexer for v0.3.8.

---

*"From chaos of characters, I bring order of tokens."*
*- LEX, Tokenizer Child*