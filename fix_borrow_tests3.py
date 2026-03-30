#!/usr/bin/env python3
import re

# Read the test file
with open('tests/borrow_checking.rs', 'r') as f:
    content = f.read()

# Manually fix the file - easier to do it manually
# Let me just read and fix line by line
lines = content.split('\n')
fixed_lines = []

for line in lines:
    # Fix Type::Named("i32".to_string(), vec![]) to Type::I32
    line = line.replace('Type::Named("i32".to_string(), vec![])', 'Type::I32')
    line = line.replace('Type::Named("i64".to_string(), vec![])', 'Type::I64')
    line = line.replace('Type::Named("str".to_string(), vec![])', 'Type::Str')
    line = line.replace('Type::Named("bool".to_string(), vec![])', 'Type::Bool')
    
    # Fix Type::Ref with inner Type::Named
    line = line.replace('Type::Ref(Box::new(Type::Named("i32".to_string(), vec![])),', 'Type::Ref(Box::new(Type::I32),')
    line = line.replace('Type::Ref(Box::new(Type::Named("str".to_string(), vec![])),', 'Type::Ref(Box::new(Type::Str),')
    line = line.replace('Type::Ref(Box::new(Type::Named("i64".to_string(), vec![])),', 'Type::Ref(Box::new(Type::I64),')
    
    fixed_lines.append(line)

# Write back
with open('tests/borrow_checking.rs', 'w') as f:
    f.write('\n'.join(fixed_lines))

print("Fixed borrow checking tests")