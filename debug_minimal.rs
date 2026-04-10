// Simple debug to test parse_param directly
fn main() {
    println!("Testing parse_param with 'a: i64'");
    
    // We need to import the actual parse_param function
    // But first let me think about what could be wrong
    
    // Actually, I just had a thought! What if the issue is that
    // parse_type is failing because of something else?
    
    // Let me check if i64 is actually in the builtin_types list
    // In parse_type, builtin_types includes:
    // tag("i8"), tag("i16"), tag("i32"), tag("i64"), ...
    
    // So i64 should be there.
    
    // Wait! I just realized something else. In parse_type,
    // after builtin_types, it tries parse_type_path.
    // parse_type_path calls parse_path, which parses identifiers
    // separated by "::".
    
    // What if "i64" is being parsed by parse_type_path instead of
    // the builtin_types? That shouldn't matter, parse_type_path
    // should return "i64".
    
    // But wait, parse_type_path also handles generic arguments!
    // It calls parse_type_args which expects "<...>".
    // "i64" doesn't have "<...>", so that's fine.
    
    // Actually, I think I need to debug this step by step.
    // Let me add some debug prints to the actual code.
    
    println!("Need to add debug prints to parse_type to see what's happening");
}