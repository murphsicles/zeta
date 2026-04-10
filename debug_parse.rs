fn main() {
    // Let me trace through what happens when we parse "a: i64"
    
    // In parse_param:
    // 1. Try parse_self: checks for "self", "&self", "&mut self" not followed by ":"
    //    "a" is not "self", so fails
    // 2. Try parse_regular: (ws(parse_ident), ws(tag(":")), ws(parse_type))
    
    // parse_ident should parse "a" successfully
    // tag(":") should parse ":" successfully
    // parse_type should parse "i64"
    
    // But wait! In parse_type, the order is:
    // 1. tag("_") - fails
    // 2. parse_tuple_type - fails (doesn't start with "(")
    // 3. parse_fn_type - let's check this!
    
    // parse_fn_type:
    // 1. opt(delimited(ws(tag("extern")), ws(tag("\"C\"")), ws(tag("fn")))) - fails
    // 2. ws(tag("fn")) - fails ("i64" doesn't start with "fn")
    // So parse_fn_type should fail without consuming input
    
    // Then parse_type continues:
    // 4. parse_array_type - fails (doesn't start with "[")
    // 5. parse_pointer_type - fails (doesn't start with "*")
    // 6. preceded(ws(tag("dyn")), ws(parse_type_path)) - fails (doesn't start with "dyn")
    // 7. parse_simd_type - fails
    // 8. parse_lt_type - fails
    // 9. Then builtin_types: includes i64!
    
    // So it should work... unless there's a bug in parse_fn_type
    
    // Actually, wait! I just realized something. In parse_fn_type:
    // let (input, _) = ws(tag("fn")).parse(input)?;
    
    // What if ws(tag("fn")) consumes something? Let me check the ws function:
    // ws = delimited(multispace0, f, multispace0)
    
    // So ws(tag("fn")) will skip whitespace, then try to match "fn"
    // If there's no whitespace, it tries to match "fn" directly
    // "i64" doesn't start with "fn", so it fails
    
    // But what if the issue is with backtracking? In nom, when an alternative
    // in `alt` fails after consuming input, the whole `alt` fails.
    
    // Let me check if parse_fn_type has any parser that could consume "i64"
    // before failing. The first parser is:
    // opt(delimited(ws(tag("extern")), ws(tag("\"C\"")), ws(tag("fn"))))
    
    // ws(tag("extern")) will skip whitespace, then try to match "extern"
    // "i64" doesn't start with "extern", so it fails
    // opt() means it's optional, so failure is OK
    
    // Then: let (input, _) = ws(tag("fn")).parse(input)?;
    // ws(tag("fn")) skips whitespace, tries to match "fn"
    // "i64" doesn't start with "fn", so parse_fn_type fails
    
    // But did it consume any input? ws() uses multispace0 which can match
    // zero whitespace. So it doesn't consume "i64".
    
    // Hmm, maybe the issue is elsewhere. Let me check if parse_type_path
    // could be consuming something.
    
    println!("Debug analysis:");
    println!("1. parse_param should work for 'a: i64'");
    println!("2. parse_ident('a') -> Ok");
    println!("3. tag(':') -> Ok");
    println!("4. parse_type('i64') -> should match builtin_types");
    println!("5. But parse_type tries parse_fn_type first");
    println!("6. parse_fn_type should fail without consuming input");
    println!("7. Then parse_type should try builtin_types and match i64");
    println!("8. Unless there's a bug in parse_fn_type or alt() behavior");
    
    // Actually, I just thought of something else! What if the issue is
    // with how the alternatives are ordered in parse_type?
    // The order is: special_types, builtin_types, parse_type_path
    
    // builtin_types includes i64, but it comes AFTER special_types
    // special_types includes parse_fn_type which comes before builtin_types
    
    // But parse_fn_type should fail quickly on "i64"...
    
    // Unless... what if parse_fn_type is somehow matching "i64"?
    // No, that doesn't make sense.
    
    // Wait! I need to check if there's whitespace handling issue.
    // When parse_type is called from parse_param, it's wrapped in ws():
    // ws(parse_type)
    
    // So the input to parse_type might have leading whitespace already skipped.
    // But that shouldn't matter.
    
    // Let me think about the actual error message. The user says:
    // "Parser fails on `:` in function parameters."
    
    // Maybe the issue is with the colon itself? Let me check:
    // In parse_regular: (ws(parse_ident), ws(tag(":")), ws(parse_type))
    
    // What if ws(tag(":")) is failing? No, ":" should match.
    
    // Actually, I wonder if the issue is with the peek(not(ws(tag(":"))))
    // in parse_self. What if that's interfering somehow?
    
    // No, parse_self only matches "self", "&self", "&mut self".
    
    // I think I need to actually run the parser and see what error we get.
}