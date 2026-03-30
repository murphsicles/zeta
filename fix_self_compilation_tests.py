#!/usr/bin/env python3
"""Fix self-compilation tests to ignore file deletion errors."""

import re

with open('tests/self_compilation_test.rs', 'r', encoding='utf-8') as f:
    content = f.read()

# Replace all occurrences of fs::remove_file(...).expect(...) with let _ = fs::remove_file(...);
pattern = r'fs::remove_file\(([^)]+)\)\.expect\([^)]+\)'
replacement = r'let _ = fs::remove_file(\1);'

new_content = re.sub(pattern, replacement, content)

with open('tests/self_compilation_test.rs', 'w', encoding='utf-8') as f:
    f.write(new_content)

print("Fixed self-compilation tests")