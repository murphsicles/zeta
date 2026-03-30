#!/usr/bin/env python3
"""
Fix remaining Type::Ref calls in reference_types.rs
Adds Lifetime::Static as the second parameter
"""

import re

with open('tests/reference_types.rs', 'r') as f:
    content = f.read()

# Fix pattern 1: Type::Ref(Box::new(Type::Array(...)), Mutability::Immutable)
pattern1 = r'Type::Ref\(\s*Box::new\(Type::Array\([^)]+\)\),\s*Mutability::Immutable\s*\)'
replacement1 = r'Type::Ref(Box::new(Type::Array(\1)), Lifetime::Static, Mutability::Immutable)'

# Fix pattern 2: Type::Ref(Box::new(Type::Tuple(...)), Mutability::Mutable)
pattern2 = r'Type::Ref\(\s*Box::new\(Type::Tuple\([^)]+\)\),\s*Mutability::Mutable\s*\)'
replacement2 = r'Type::Ref(Box::new(Type::Tuple(\1)), Lifetime::Static, Mutability::Mutable)'

# Fix pattern 3: Type::Ref(Box::new(Type::Named("Option"...)), Mutability::Immutable)
pattern3 = r'Type::Ref\(\s*Box::new\(Type::Named\("Option"[^)]+\)\),\s*Mutability::Immutable\s*\)'
replacement3 = r'Type::Ref(Box::new(Type::Named("Option"\1)), Lifetime::Static, Mutability::Immutable)'

# Fix pattern 4: Type::Ref(Box::new(Type::Named("Vec"...)), Mutability::Mutable)
pattern4 = r'Type::Ref\(\s*Box::new\(Type::Named\("Vec"[^)]+\)\),\s*Mutability::Mutable\s*\)'
replacement4 = r'Type::Ref(Box::new(Type::Named("Vec"\1)), Lifetime::Static, Mutability::Mutable)'

# Actually, let me do it line by line since regex is complex
lines = content.split('\n')
fixed_lines = []

for line in lines:
    # Fix array reference
    if 'Type::Ref(Box::new(Type::Array(' in line and 'Mutability::Immutable)' in line:
        line = line.replace('Type::Ref(Box::new(Type::Array(', 'Type::Ref(Box::new(Type::Array(')
        line = line.replace('Mutability::Immutable)', 'Lifetime::Static, Mutability::Immutable)')
    
    # Fix tuple reference  
    elif 'Type::Ref(Box::new(Type::Tuple(' in line and 'Mutability::Mutable)' in line:
        line = line.replace('Mutability::Mutable)', 'Lifetime::Static, Mutability::Mutable)')
    
    # Fix Option reference
    elif 'Type::Ref(Box::new(Type::Named("Option"' in line and 'Mutability::Immutable' in line:
        line = line.replace('Mutability::Immutable', 'Lifetime::Static, Mutability::Immutable')
    
    # Fix Vec reference
    elif 'Type::Ref(Box::new(Type::Named("Vec"' in line and 'Mutability::Mutable' in line:
        line = line.replace('Mutability::Mutable', 'Lifetime::Static, Mutability::Mutable')
    
    fixed_lines.append(line)

fixed_content = '\n'.join(fixed_lines)

with open('tests/reference_types.rs', 'w') as f:
    f.write(fixed_content)

print("Fixed remaining Type::Ref calls in reference_types.rs")