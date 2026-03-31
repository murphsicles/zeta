use zetac::middle::resolver::new_resolver::InferContext;
use zetac::middle::types::{Type, TypeVar};

fn main() {
    println!("Testing simple constraint system");
    
    let mut ctx = InferContext::new();
    
    // Test 1: Simple equality constraint
    println!("\nTest 1: Simple equality constraint");
    let t1 = Type::I32;
    let t2 = Type::I32;
    ctx.constrain_eq(t1, t2);
    
    match ctx.solve() {
        Ok(_) => println!("✓ Constraint solved successfully"),
        Err(errors) => println!("✗ Constraint solving failed: {:?}", errors),
    }
    
    // Test 2: Type variable unification
    println!("\nTest 2: Type variable unification");
    let mut ctx2 = InferContext::new();
    let tv1 = Type::Variable(TypeVar::fresh());
    let tv2 = Type::Variable(TypeVar::fresh());
    
    // Constrain tv1 == i32
    ctx2.constrain_eq(tv1, Type::I32);
    // Constrain tv1 == tv2
    ctx2.constrain_eq(tv1, tv2);
    
    match ctx2.solve() {
        Ok(_) => {
            println!("✓ Type variable constraints solved");
            // Check if tv2 is now i32
            let applied = ctx2.substitution.apply(&tv2);
            println!("  tv2 after substitution: {:?}", applied);
        }
        Err(errors) => println!("✗ Type variable constraint solving failed: {:?}", errors),
    }
    
    // Test 3: Inconsistent constraint
    println!("\nTest 3: Inconsistent constraint (i32 == bool)");
    let mut ctx3 = InferContext::new();
    ctx3.constrain_eq(Type::I32, Type::Bool);
    
    match ctx3.solve() {
        Ok(_) => println!("✗ Should have failed but didn't"),
        Err(errors) => println!("✓ Correctly failed with errors: {:?}", errors),
    }
}