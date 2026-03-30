#!/usr/bin/env python3
import re

# Read the test file
with open('tests/borrow_checking.rs', 'r') as f:
    content = f.read()

# Simple string to Type conversion mapping
type_mapping = {
    '"i32"': 'Type::I32',
    '"i64"': 'Type::I64',
    '"str"': 'Type::Str',
    '"bool"': 'Type::Bool',
}

# Function to convert string type to Type enum
def convert_type(match):
    type_str = match.group(1)
    
    # Handle reference types: &T, &mut T
    if type_str.startswith('&'):
        is_mut = type_str.startswith('&mut ')
        if is_mut:
            inner = type_str[5:]  # Remove "&mut "
            mutability = 'Mutability::Mutable'
        else:
            inner = type_str[1:]  # Remove "&"
            mutability = 'Mutability::Immutable'
        
        # Convert inner type
        if inner in type_mapping:
            inner_type = type_mapping[f'"{inner}"']
        else:
            inner_type = f'Type::Named("{inner}".to_string(), vec![])'
        
        return f'Type::Ref(Box::new({inner_type}), Lifetime::Static, {mutability})'
    
    # Handle simple types
    if f'"{type_str}"' in type_mapping:
        return type_mapping[f'"{type_str}"']
    
    # Default to Named type for unknown types
    return f'Type::Named("{type_str}".to_string(), vec![])'

# Replace all ".to_string()" calls with Type enum variants
# Pattern: "type".to_string()
pattern = r'"([^"]+)"\.to_string\(\)'

def replace_match(match):
    full_match = match.group(0)
    # Check if this is in a declare call
    context = content[max(0, match.start()-100):match.end()]
    if '.declare(' in context:
        return convert_type(match)
    return full_match

# Apply replacement
new_content = re.sub(pattern, replace_match, content)

# Write back
with open('tests/borrow_checking.rs', 'w') as f:
    f.write(new_content)

print("Updated borrow checking tests")