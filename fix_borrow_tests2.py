#!/usr/bin/env python3
import re

# Read the test file
with open('tests/borrow_checking.rs', 'r') as f:
    content = f.read()

# Fix the pattern: we need to match .declare(..., ..., "type".to_string())
# But not match variable names
# Better approach: manually fix known patterns

# List of replacements
replacements = [
    ('checker.declare("x".to_string(), BorrowState::Owned, Type::Named("i32".to_string(), vec![]));', 
     'checker.declare("x".to_string(), BorrowState::Owned, Type::I32);'),
    
    ('checker.declare("y".to_string(), BorrowState::Borrowed, Type::Ref(Box::new(Type::Named("i32".to_string(), vec![])), Lifetime::Static, Mutability::Immutable));',
     'checker.declare("y".to_string(), BorrowState::Borrowed, Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Immutable));'),
    
    ('checker.declare("z".to_string(), BorrowState::Consumed, Type::Named("i32".to_string(), vec![]));',
     'checker.declare("z".to_string(), BorrowState::Consumed, Type::I32);'),
    
    ('checker.declare("s".to_string(), BorrowState::Borrowed, Type::Ref(Box::new(Type::Named("str".to_string(), vec![])), Lifetime::Static, Mutability::Immutable));',
     'checker.declare("s".to_string(), BorrowState::Borrowed, Type::Ref(Box::new(Type::Str), Lifetime::Static, Mutability::Immutable));'),
]

for old, new in replacements:
    content = content.replace(old, new)

# Also fix any remaining Type::Named("type".to_string(), vec![]) patterns
# for simple types
simple_types = ['i32', 'i64', 'str', 'bool']
for t in simple_types:
    pattern = f'Type::Named("{t}".to_string\(\), vec!\[\]\)'
    replacement = f'Type::{t.upper()}'
    content = re.sub(pattern, replacement, content)

# Write back
with open('tests/borrow_checking.rs', 'w') as f:
    f.write(content)

print("Fixed borrow checking tests")