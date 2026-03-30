use src::middle::types::Type;
use src::backend::codegen::LLVMCodegen;
use inkwell::context::Context;

#[test]
fn test_type_mangling() {
    // Test basic type mangling
    let ty_i32 = Type::I32;
    assert_eq!(ty_i32.mangled_name(), "i32");
    
    let ty_i64 = Type::I64;
    assert_eq!(ty_i64.mangled_name(), "i64");
    
    let ty_vec_i32 = Type::Named("Vec".to_string(), vec![Type::I32]);
    assert_eq!(ty_vec_i32.mangled_name(), "Vec_i32");
    
    let ty_option_box_i32 = Type::Named(
        "Option".to_string(), 
        vec![Type::Named("Box".to_string(), vec![Type::I32])]
    );
    assert_eq!(ty_option_box_i32.mangled_name(), "Option_Box_i32");
    
    let ty_hashmap_string_i32 = Type::Named(
        "HashMap".to_string(), 
        vec![Type::Str, Type::I32]
    );
    assert_eq!(ty_hashmap_string_i32.mangled_name(), "HashMap_str_i32");
    
    println!("✅ Type mangling tests passed!");
}

#[test]
fn test_function_name_mangling() {
    let context = Context::create();
    let codegen = LLVMCodegen::new(&context, "test");
    
    // Test function name mangling
    let mangled = codegen.mangle_function_name("vec_new", &[Type::I32]);
    assert_eq!(mangled, "vec_new_inst_i32");
    
    let mangled = codegen.mangle_function_name("Vec::new", &[Type::I32]);
    assert_eq!(mangled, "Vec::new_inst_i32");
    
    let mangled = codegen.mangle_function_name("option_some", &[Type::Named("Box".to_string(), vec![Type::I32])]);
    assert_eq!(mangled, "option_some_inst_Option_Box_i32");
    
    println!("✅ Function name mangling tests passed!");
}

fn main() {
    println!("Running generic codegen tests...");
    
    test_type_mangling();
    test_function_name_mangling();
    
    println!("\n🎉 All generic codegen tests passed!");
}