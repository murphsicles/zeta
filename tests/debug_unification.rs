use zetac::middle::types::*;

fn main() {
    let mut subst = Substitution::new();
    
    println!("Testing Usize vs Usize:");
    match subst.unify(&Type::Usize, &Type::Usize) {
        Ok(_) => println!("  ✓ Usize unifies with Usize"),
        Err(e) => println!("  ✗ Usize doesn't unify with Usize: {}", e),
    }
    
    println!("\nTesting Usize vs I64:");
    match subst.unify(&Type::Usize, &Type::I64) {
        Ok(_) => println!("  ✓ Usize unifies with I64 (BUG!)"),
        Err(e) => println!("  ✗ Usize doesn't unify with I64: {}", e),
    }
    
    println!("\nTesting I64 vs I64:");
    match subst.unify(&Type::I64, &Type::I64) {
        Ok(_) => println!("  ✓ I64 unifies with I64"),
        Err(e) => println!("  ✗ I64 doesn't unify with I64: {}", e),
    }
    
    println!("\nTesting U64 vs U64:");
    match subst.unify(&Type::U64, &Type::U64) {
        Ok(_) => println!("  ✓ U64 unifies with U64"),
        Err(e) => println!("  ✗ U64 doesn't unify with U64: {}", e),
    }
    
    println!("\nTesting all primitive types with themselves:");
    let types = vec![
        ("I8", Type::I8),
        ("I16", Type::I16),
        ("I32", Type::I32),
        ("I64", Type::I64),
        ("U8", Type::U8),
        ("U16", Type::U16),
        ("U32", Type::U32),
        ("U64", Type::U64),
        ("Usize", Type::Usize),
        ("F32", Type::F32),
        ("F64", Type::F64),
        ("Bool", Type::Bool),
        ("Char", Type::Char),
        ("Str", Type::Str),
        ("Range", Type::Range),
    ];
    
    for (name, ty) in types {
        let mut subst = Substitution::new();
        match subst.unify(&ty, &ty) {
            Ok(_) => println!("  ✓ {} unifies with itself", name),
            Err(e) => println!("  ✗ {} doesn't unify with itself: {}", name, e),
        }
    }
}