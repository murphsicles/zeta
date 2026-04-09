fn main() {
    // Let's trace through the parse_type function manually
    // The issue is in parse_array_type when called from parse_type
    
    // Looking at parse_array_type function:
    // 1. It first tries to parse PrimeZeta style: [N]T
    // 2. If that fails, it tries Zeta style: [T] or [T; N]
    
    // The problem might be in how parse_type is called from parse_fn_type
    // or how parse_fn_type is called from parse_func
    
    // In parse_func (top_level.rs line 81):
    // let (input, ret_opt) = opt(preceded(ws(tag("->")), ws(parse_type))).parse(input)?;
    
    // In parse_fn_type (parser.rs line 182):
    // let (input, _) = ws(tag("->")).parse(input)?;
    // let (input, ret) = ws(parse_type).parse(input)?;
    
    // Both call parse_type with ws() wrapper
    
    // Let me check the parse_array_type function more carefully
    // It has two syntaxes:
    // 1. PrimeZeta: [N]T (size before type)
    // 2. Zeta: [T] or [T; N] (type before size)
    
    // The issue might be with whitespace handling or precedence
    // When parse_type is called, it tries different alternatives in order:
    // 1. tag("_")
    // 2. parse_tuple_type
    // 3. parse_fn_type
    // 4. parse_array_type
    // 5. parse_pointer_type
    // 6. etc...
    
    // So when we have "[u64; 10]", parse_type should match parse_array_type
    
    // But wait! Look at the order in parse_type:
    // alt((
    //     tag("_").map(|_| "_".to_string()),
    //     parse_tuple_type,
    //     parse_fn_type,        // <-- This comes BEFORE parse_array_type!
    //     parse_array_type,     // <-- This is 4th
    //     parse_pointer_type,
    //     // ...
    // ))
    
    // So when we have "[u64; 10]" as a return type, parse_type might be trying
    // parse_fn_type first! Because parse_fn_type starts with checking for
    // "extern" or "fn", but if it doesn't find those, it might fail.
    
    // Actually, looking at parse_fn_type:
    // It starts with: opt(delimited(ws(tag("extern")), ws(tag("\"C\"")), ws(tag("fn"))))
    // Then: let (input, _) = ws(tag("fn")).parse(input)?;
    
    // So parse_fn_type expects "fn" keyword. If the input is "[u64; 10]",
    // it won't start with "fn", so it should fail quickly.
    
    // But wait! There's another issue: parse_type is wrapped in ws() when called
    // from parse_func and parse_fn_type. The ws() parser skips whitespace and comments.
    
    // Let me check if there's an issue with how parse_array_type handles
    // the ws() wrapper or if there's a precedence issue.
    
    println!("Debug analysis:");
    println!("1. parse_type is called from parse_func with ws() wrapper");
    println!("2. parse_type tries alternatives in order");
    println!("3. parse_array_type is 4th alternative");
    println!("4. parse_fn_type is 3rd alternative and expects 'fn' keyword");
    println!("5. The issue might be in parse_array_type's internal whitespace handling");
    println!("6. Or there might be an issue with how it handles the dual syntax");
    
    // Let me check the actual error. The user says:
    // "comptime fn generate_residues() -> [u64; NUM_RESIDUES] fails"
    
    // NUM_RESIDUES is an identifier, not a number. Let me check if
    // parse_array_type handles identifiers for size properly.
    
    // In parse_array_type, for Zeta style:
    // size_opt uses: alt((digit1.map(...), parse_ident))
    
    // For PrimeZeta style:
    // size uses: alt((digit1.map(...), parse_ident))
    
    // So identifiers should work for size.
    
    // The issue might be something else. Let me trace through the actual parsing.
}