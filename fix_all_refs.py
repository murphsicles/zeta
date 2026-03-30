#!/usr/bin/env python3
import os
import re

def fix_type_refs_in_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # First, add lifetime import if needed
    if "Type::Ref" in content and "use.*lifetime::Lifetime" not in content and "crate::middle::types::lifetime::Lifetime" not in content:
        # Check if we have a use statement for types
        lines = content.split('\n')
        new_lines = []
        for i, line in enumerate(lines):
            new_lines.append(line)
            # Look for use statements for types module
            if 'use crate::middle::types::' in line and 'Lifetime' not in line:
                # Add lifetime import
                new_lines.append(line.replace('use crate::middle::types::', 'use crate::middle::types::lifetime::Lifetime, '))
    
    # Fix Type::Ref calls - pattern 1: Type::Ref(Box::new(...), Mutability::...)
    pattern1 = r'Type::Ref\(\s*Box::new\(([^)]+)\)\s*,\s*(Mutability::\w+)\s*\)'
    
    def replace1(match):
        inner = match.group(1).strip()
        mutability = match.group(2).strip()
        return f'Type::Ref(Box::new({inner}), Lifetime::Static, {mutability})'
    
    content = re.sub(pattern1, replace1, content)
    
    # Fix Type::Ref calls - pattern 2: Type::Ref(..., Mutability::...) without Box::new
    # This is more complex and might need manual fixing
    pattern2 = r'Type::Ref\(\s*([^,]+),\s*(Mutability::\w+)\s*\)'
    
    def replace2(match):
        inner = match.group(1).strip()
        mutability = match.group(2).strip()
        # Check if inner already has Lifetime::Static
        if 'Lifetime::Static' in inner:
            return match.group(0)  # Already fixed
        return f'Type::Ref({inner}, Lifetime::Static, {mutability})'
    
    content = re.sub(pattern2, replace2, content)
    
    # Fix nested Type::Ref calls
    # This is complex, so we'll do multiple passes
    for _ in range(3):  # Max 3 levels of nesting
        pattern3 = r'Type::Ref\(\s*Box::new\(\s*Type::Ref\(([^)]+)\)\s*\)\s*,\s*(Mutability::\w+)\s*\)'
        if not re.search(pattern3, content):
            break
        
        def replace3(match):
            inner = match.group(1).strip()
            mutability = match.group(2).strip()
            # Check if inner already has 3 parts
            parts = [p.strip() for p in inner.split(',')]
            if len(parts) == 2:
                # Has type and mutability, need lifetime
                type_part = parts[0]
                mut_part = parts[1]
                inner_fixed = f'{type_part}, Lifetime::Static, {mut_part}'
            elif len(parts) == 3:
                # Already has lifetime
                inner_fixed = inner
            else:
                # Unknown format
                inner_fixed = inner
            
            return f'Type::Ref(Box::new(Type::Ref({inner_fixed})), Lifetime::Static, {mutability})'
        
        content = re.sub(pattern3, replace3, content)
    
    # Write back
    with open(filepath, 'w', encoding='utf-8') as f:
        f.write(content)
    
    print(f"Fixed {filepath}")

def main():
    # Fix all .rs files
    for root, dirs, files in os.walk("."):
        for file in files:
            if file.endswith(".rs"):
                filepath = os.path.join(root, file)
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.read()
                if "Type::Ref" in content:
                    fix_type_refs_in_file(filepath)

if __name__ == "__main__":
    main()