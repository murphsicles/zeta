use zetac::frontend::parser::parse_program;
use zetac::middle::types::Type;
use zetac::backend::codegen::LLVMCodegen;
use inkwell::context::Context;

fn main() {
    println!("Testing SIMD compilation...");
    
    // Test SIMD type in type system
    let element_type = Box::new(Type::I32);
    let vector_type = Type::Vector(element_type, 4);
    println!("Created vector type: {}", vector_type.display_name());
    
    // Try to parse a simple SIMD program
    let code = r#"
        fn main() -> u64 {
            // Simple test - not using SIMD yet
            return 42
        }
    "#;
    
    match parse_program(code) {
        Ok((remaining, ast)) => {
            if remaining.is_empty() {
                println!("Successfully parsed program!");
                println!("AST: {:?}", ast);
                
                // Try to compile it
                let context = Context::create();
                let module = context.create_module("test");
                let codegen = LLVMCodegen::new(&context, module);
                
                // This would normally compile the AST
                println!("Codegen created successfully");
            } else {
                println!("Didn't consume all input. Remaining: '{}'", remaining);
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
    
    // Test parsing SIMD type syntax
    println!("\nTesting SIMD type parsing...");
    let simd_code = "u64x8";
    
    // We need to check if the parser can parse SIMD types
    // This would require calling parse_type or similar
    println!("Note: Need to test parse_simd_type function directly");
}