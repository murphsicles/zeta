import re

with open('src/middle/const_eval.rs', 'r') as f:
    content = f.read()

# Find the eval_const_expr method
method_start = content.find('pub fn eval_const_expr')
if method_start == -1:
    print("Method not found")
    exit(1)

# Find the match expression
match_start = content.find('match expr {', method_start)
if match_start == -1:
    print("Match expression not found")
    exit(1)

# Find the end of the match expression (approximate)
# Look for the next '}' at the same indentation level
indent = 0
i = match_start
while i < len(content) and content[i] != '\n':
    i += 1
i += 1
while i < len(content) and content[i] == ' ':
    indent += 1
    i += 1

# Now find the matching }
brace_count = 0
i = match_start
while i < len(content):
    if content[i] == '{':
        brace_count += 1
    elif content[i] == '}':
        brace_count -= 1
        if brace_count == 0:
            match_end = i + 1
            break
    i += 1

match_expr = content[match_start:match_end]
print(f"Found match expression of length {len(match_expr)}")

# Find all arms that return ConstValue:: directly
pattern = r'(\s+)([A-Z][a-zA-Z]*\s*\{[^}]+\}\s*=>\s*)(ConstValue::[A-Z][a-zA-Z]*\([^)]+\))'
matches = re.findall(pattern, match_expr, re.DOTALL)

for indent, before, const_value in matches:
    print(f"Found: {const_value}")
    # Check if it's already wrapped in Ok()
    if not const_value.startswith('Ok('):
        print(f"  Needs fixing")