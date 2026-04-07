#!/usr/bin/env python3
import re

with open('tests/borrow_checking.rs', 'r') as f:
    content = f.read()

# Fix variable names that were incorrectly converted to Type::Named
# Pattern: checker.declare(Type::Named("var".to_string(), vec![]), ...
pattern = r'checker\.declare\(Type::Named\("([^"]+)"\.to_string\(\), vec!\[\]\),'
replacement = r'checker.declare("\1".to_string(),'

content = re.sub(pattern, replacement, content)

# Also fix standalone Type::Named for variable names
pattern2 = r'Type::Named\("([^"]+)"\.to_string\(\), vec!\[\]\)'
# But only when it's a parameter to declare
# Actually, let's just fix all remaining ones
lines = content.split('\n')
fixed_lines = []
for line in lines:
    if 'Type::Named("' in line and '.to_string(), vec![])' in line:
        # Extract the variable name
        match = re.search(r'Type::Named\("([^"]+)"\.to_string\(\), vec!\[\]\)', line)
        if match:
            var_name = match.group(1)
            line = line.replace(f'Type::Named("{var_name}".to_string(), vec![])', f'"{var_name}".to_string()')
    fixed_lines.append(line)

with open('tests/borrow_checking.rs', 'w') as f:
    f.write('\n'.join(fixed_lines))

print("Fixed remaining issues")