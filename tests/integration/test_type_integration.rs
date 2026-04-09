use zetac::frontend::ast::AstNode;
use zetac::middle::resolver::resolver::Resolver;

fn main() {
    println!("Testing type system integration...");

    let resolver = Resolver::new();

    // Test infer_type returns Type enum, not String
    let lit = AstNode::Lit(42);
    let ty = resolver.infer_type(&lit);

    println!("Type of literal 42: {:?}", ty);
    println!("Type display name: {}", ty.display_name());

    // Check it's Type::I64, not "i64".to_string()
    match ty {
        zetac::middle::types::Type::I64 => println!("✓ Correctly returns Type::I64"),
        _ => println!("✗ Expected Type::I64, got {:?}", ty),
    }

    // Test string literal
    let str_lit = AstNode::StringLit("hello".to_string());
    let str_ty = resolver.infer_type(&str_lit);

    println!("Type of string literal: {:?}", str_ty);
    match str_ty {
        zetac::middle::types::Type::Str => println!("✓ Correctly returns Type::Str"),
        _ => println!("✗ Expected Type::Str, got {:?}", str_ty),
    }
}
