use zetac::middle::types::{Substitution, Type, TypeVar};

fn main() {
    println!("Testing generic function arity checking...");

    // Test 1: Basic arity check
    let substitution = Substitution::new();

    // Create a generic function type: fn<T>(T) -> T
    let t_var = Type::Variable(TypeVar::fresh());
    let generic_func = Type::Function(vec![t_var.clone()], Box::new(t_var.clone()));

    // Try to instantiate with wrong number of type arguments
    let type_args = vec![Type::I32, Type::I64]; // Wrong: 2 args instead of 1

    // This should fail with arity error
    match substitution.instantiate_generic_with_bounds(
        &generic_func,
        &type_args,
        &Default::default(),
    ) {
        Ok(_) => println!("ERROR: Should have failed with arity mismatch"),
        Err(e) => println!("✓ Correctly failed with: {}", e),
    }

    // Test 2: Correct arity
    let type_args_correct = vec![Type::I32]; // Correct: 1 arg
    match substitution.instantiate_generic_with_bounds(
        &generic_func,
        &type_args_correct,
        &Default::default(),
    ) {
        Ok(ty) => println!("✓ Correctly instantiated to: {}", ty.display_name()),
        Err(e) => println!("ERROR: Should have succeeded: {}", e),
    }

    // Test 3: Named type arity check
    let option_t = Type::Named("Option".to_string(), vec![t_var.clone()]);

    // Wrong arity for Option<T>
    let wrong_args = vec![Type::I32, Type::I64]; // 2 args for 1 parameter
    match substitution.instantiate_generic_with_bounds(&option_t, &wrong_args, &Default::default())
    {
        Ok(_) => println!("ERROR: Should have failed with arity mismatch for Option"),
        Err(e) => println!("✓ Correctly failed with: {}", e),
    }

    // Correct arity for Option<T>
    let correct_args = vec![Type::I32]; // 1 arg for 1 parameter
    match substitution.instantiate_generic_with_bounds(
        &option_t,
        &correct_args,
        &Default::default(),
    ) {
        Ok(ty) => println!("✓ Correctly instantiated Option to: {}", ty.display_name()),
        Err(e) => println!("ERROR: Should have succeeded: {}", e),
    }

    println!("\nTest complete!");
}
