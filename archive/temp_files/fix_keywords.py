#!/usr/bin/env python3
import re

# Read the file
with open('contamination_backup/zeta-github/src/frontend/parser/parser.rs', 'r') as f:
    content = f.read()

# Find and replace the keyword list
# We need to find the parse_ident function and the keyword array
pattern = r'(\[\s*"let", "mut", "if", "else", "for", "in", "loop", "unsafe", "return", "break",\s*"continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",\s*"dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",\s*// Built-in types that shouldn\'t be parsed as identifiers\s*"i8", "i16", "i32", "i64",\s*"u8", "u16", "u32", "u64", "usize",\s*"f32", "f64",\s*"bool", "char", "str", "String",\s*// TODO: re-add these when we implement logical operators\s*// or when the self-hosted parser \(parser\.z\) becomes the default\s*// "and", "or", "not"\s*\])'

replacement = '''[
                "let", "mut", "if", "else", "for", "in", "loop", "while", "unsafe", "return", "break",
                "continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",
                "dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",
                // Built-in types that shouldn't be parsed as identifiers
                "i8", "i16", "i32", "i64",
                "u8", "u16", "u32", "u64", "usize",
                "f32", "f64",
                "bool", "char", "str", "String",
                // Logical operator keywords (should not be parsed as identifiers)
                "and", "or", "not"
            ]'''

# Use regex with DOTALL flag to match across lines
new_content = re.sub(pattern, replacement, content, flags=re.DOTALL)

if new_content != content:
    print("Fixed keyword list in parse_ident")
    with open('contamination_backup/zeta-github/src/frontend/parser/parser.rs', 'w') as f:
        f.write(new_content)
else:
    print("Pattern not found. Trying alternative pattern...")
    # Try without the comments
    pattern2 = r'(\[\s*"let", "mut", "if", "else", "for", "in", "loop", "unsafe", "return", "break",\s*"continue", "fn", "concept", "impl", "enum", "struct", "type", "use", "extern",\s*"dyn", "box", "as", "true", "false", "comptime", "const", "async", "pub",\s*"i8", "i16", "i32", "i64",\s*"u8", "u16", "u32", "u64", "usize",\s*"f32", "f64",\s*"bool", "char", "str", "String",[^]]*\])'
    
    new_content = re.sub(pattern2, replacement, content, flags=re.DOTALL)
    
    if new_content != content:
        print("Fixed keyword list (alternative pattern)")
        with open('contamination_backup/zeta-github/src/frontend/parser/parser.rs', 'w') as f:
            f.write(new_content)
    else:
        print("Could not find keyword list to fix")