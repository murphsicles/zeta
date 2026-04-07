fn main() {
    // Test 1: Check that [bool; limit] parses to [bool; 0]
    let ty = zetac::middle::types::Type::from_string("[bool; limit]");
    println!("[bool; limit] -> {:?}", ty);
    
    // Test 2: Check that [bool; 10] parses to [bool; 10]
    let ty2 = zetac::middle::types::Type::from_string("[bool; 10]");
    println!("[bool; 10] -> {:?}", ty2);
    
    // Test 3: Check unification
    let mut subst = zetac::middle::types::Substitution::new();
    match subst.unify(&ty, &ty2) {
        Ok(_) => println!("✅ [bool; limit] unifies with [bool; 10]"),
        Err(e) => println!("❌ Unification failed: {:?}", e),
    }
}