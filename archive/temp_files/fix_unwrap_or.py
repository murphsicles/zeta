import re

with open('src/frontend/parser/expr.rs', 'r') as f:
    content = f.read()

# Pattern to find skip_ws_and_comments0(...).unwrap_or((remaining_input, ()))
pattern1 = r'let\s*\(([ij]),\s*_\)\s*=\s*skip_ws_and_comments0\(([^)]+)\)\.unwrap_or\(\((\2),\s*\(\)\)\);'

# First pass: fix the pattern where variable names match
def replace_match1(match):
    var1 = match.group(1)  # i or j
    var2 = match.group(2)  # remaining_input or similar
    var3 = match.group(3)  # should be same as var2
    
    # Check if var2 and var3 are the same
    if var2 == var3:
        return f'let {var1} = match skip_ws_and_comments0({var2}) {{\n                Ok(({var1}, _)) => {var1},\n                Err(_) => {var2},\n            }};'
    else:
        # If they don't match, we need to handle this differently
        return match.group(0)

# Apply first replacement
new_content = re.sub(pattern1, replace_match1, content, flags=re.MULTILINE)

# Second pattern: when we have a different variable name
pattern2 = r'let\s*\(([ij]),\s*_\)\s*=\s*skip_ws_and_comments0\(([^)]+)\)\.unwrap_or\(\(([^,]+),\s*\(\)\)\);'

def replace_match2(match):
    var1 = match.group(1)  # i or j
    var2 = match.group(2)  # input variable
    var3 = match.group(3)  # fallback variable
    
    return f'let {var1} = match skip_ws_and_comments0({var2}) {{\n                Ok(({var1}, _)) => {var1},\n                Err(_) => {var3},\n            }};'

new_content = re.sub(pattern2, replace_match2, new_content, flags=re.MULTILINE)

# Write back
with open('src/frontend/parser/expr.rs', 'w') as f:
    f.write(new_content)

print("Fixed unwrap_or patterns in expr.rs")