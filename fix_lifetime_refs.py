#!/usr/bin/env python3
import os
import re

def fix_file(filepath):
    with open(filepath, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Pattern to match Type::Ref calls with 2 arguments (missing lifetime)
    # Type::Ref(Box::new(...), Mutability::...)
    pattern = r'Type::Ref\(([^,]+),\s*(Mutability::\w+)\)'
    
    def replace_match(match):
        inner = match.group(1).strip()
        mutability = match.group(2).strip()
        return f'Type::Ref({inner}, Lifetime::Static, {mutability})'
    
    new_content = re.sub(pattern, replace_match, content)
    
    # Also fix nested patterns like Type::Ref(Box::new(Type::Ref(...)))
    # This needs a more complex pattern
    pattern2 = r'Type::Ref\(\s*Box::new\(\s*Type::Ref\(([^)]+)\)\s*\)\s*,\s*(Mutability::\w+)\s*\)'
    
    def replace_match2(match):
        inner_ref = match.group(1).strip()
        mutability = match.group(2).strip()
        # Check if inner_ref already has Lifetime::Static
        if 'Lifetime::Static' not in inner_ref:
            # Need to add it
            inner_parts = inner_ref.split(',')
            if len(inner_parts) == 2:
                # Has inner type and mutability, need to add lifetime
                inner_type = inner_parts[0].strip()
                inner_mut = inner_parts[1].strip()
                inner_ref_fixed = f'{inner_type}, Lifetime::Static, {inner_mut}'
            else:
                # Already has 3 parts or different format
                inner_ref_fixed = inner_ref
        else:
            inner_ref_fixed = inner_ref
        
        return f'Type::Ref(Box::new(Type::Ref({inner_ref_fixed})), Lifetime::Static, {mutability})'
    
    new_content = re.sub(pattern2, replace_match2, new_content, flags=re.DOTALL)
    
    if new_content != content:
        print(f"Fixed {filepath}")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(new_content)
        return True
    return False

def main():
    # Fix reference_types.rs
    ref_test = "tests/reference_types.rs"
    if os.path.exists(ref_test):
        fix_file(ref_test)
    
    # Fix typecheck_new.rs
    typecheck = "src/middle/resolver/typecheck_new.rs"
    if os.path.exists(typecheck):
        fix_file(typecheck)
    
    # Check for other files that might need fixing
    for root, dirs, files in os.walk("."):
        for file in files:
            if file.endswith(".rs"):
                filepath = os.path.join(root, file)
                if filepath not in [ref_test, typecheck]:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        content = f.read()
                    if "Type::Ref" in content:
                        print(f"Checking {filepath} for Type::Ref calls...")
                        fix_file(filepath)

if __name__ == "__main__":
    main()