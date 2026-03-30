#!/usr/bin/env python3
"""Fix MIR type references from strings to Type enum."""

import re

# Read the file
with open('src/middle/mir/gen.rs', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace string type references with Type enum variants
replacements = [
    (r'"i64"\.to_string\(\)', 'Type::I64'),
    (r'"i32"\.to_string\(\)', 'Type::I32'),
    (r'"bool"\.to_string\(\)', 'Type::Bool'),
    (r'"str"\.to_string\(\)', 'Type::Str'),
    (r'"f32"\.to_string\(\)', 'Type::F32'),
    (r'"f64"\.to_string\(\)', 'Type::F64'),
    (r'"map"\.to_string\(\)', 'Type::Named("map".to_string(), vec![])'),
]

for old, new in replacements:
    content = re.sub(old, new, content)

# Also need to handle type_map.get() calls which return Option<Type> now
# These will need manual fixing

# Write back
with open('src/middle/mir/gen.rs', 'w', encoding='utf-8') as f:
    f.write(content)

print("Fixed string type references in MIR generation")