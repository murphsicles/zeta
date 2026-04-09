use zetac::middle::types::Type;

fn main() {
    println!("Testing Type::from_string with variable array sizes");
    
    // Test 1: Fixed size array
    let ty1 = Type::from_string("[bool; 10]");
    println!("[bool; 10] -> {:?}", ty1);
    
    // Test 2: Variable size array
    let ty2 = Type::from_string("[bool; limit]");
    println!("[bool; limit] -> {:?}", ty2);
    
    // Test 3: Check if they unify
    let mut subst = zetac::middle::types::Substitution::new();
    match subst.unify(&ty1, &ty2) {
        Ok(_) => println!("✅ [bool; 10] unifies with [bool; limit]"),
        Err(e) => println!("❌ Unification failed: {:?}", e),
    }
    
    // Test 4: Check the other direction
    match subst.unify(&ty2, &ty1) {
        Ok(_) => println!("✅ [bool; limit] unifies with [bool; 10]"),
        Err(e) => println!("❌ Unification failed: {:?}", e),
    }
    
    // Test 5: Two variable-sized arrays
    let ty3 = Type::from_string("[bool; n]");
    println!("[bool; n] -> {:?}", ty3);
    
    match subst.unify(&ty2, &ty3) {
        Ok(_) => println!("✅ [bool; limit] unifies with [bool; n]"),
        Err(e) => println!("❌ Unification failed: {:?}", e),
    }
}