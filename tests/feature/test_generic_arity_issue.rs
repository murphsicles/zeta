// Test for generic function arity issue
use zetac::middle::types::{GenericContext, Substitution, Type, TypeVar};

fn main() {
    println!("Testing generic function arity issue...");

    let substitution = Substitution::new();
    let context = GenericContext::new();

    // Create a generic function type: fn<T>(T) -> T
    // This is represented as Function(vec![Variable(T)], Box::new(Variable(T)))
    let t_var = Type::Variable(TypeVar(0));
    let generic_func = Type::Function(vec![t_var.clone()], Box::new(t_var.clone()));

    println!("Generic function type: {}", generic_func.display_name());

    // Try to instantiate with 2 type arguments (wrong)
    let type_args_wrong = vec![Type::I32, Type::I64];

    match substitution.instantiate_generic_with_bounds(&generic_func, &type_args_wrong, &context) {
        Ok(ty) => {
            println!(
                "ERROR: Instantiated with wrong arity: {}",
                ty.display_name()
            );
            println!("This should have failed!");
        }
        Err(e) => {
            println!("✓ Failed as expected: {}", e);
        }
    }

    // Try to instantiate with 1 type argument (correct)
    let type_args_correct = vec![Type::I32];

    match substitution.instantiate_generic_with_bounds(&generic_func, &type_args_correct, &context)
    {
        Ok(ty) => {
            println!("✓ Instantiated correctly: {}", ty.display_name());
            // Should be: fn(i32) -> i32
            if let Type::Function(params, ret) = &ty {
                println!("  Params: {:?}", params);
                println!("  Return: {:?}", *ret);
            }
        }
        Err(e) => {
            println!("ERROR: Should have succeeded: {}", e);
        }
    }

    // Test with a generic type that has explicit type parameters
    // Option<T> with T as type variable
    let option_t = Type::Named("Option".to_string(), vec![t_var.clone()]);

    println!("\nTesting Option<T> arity...");
    println!("Option<T> type: {}", option_t.display_name());

    // Wrong arity
    match substitution.instantiate_generic_with_bounds(&option_t, &type_args_wrong, &context) {
        Ok(_) => println!("ERROR: Option should fail with wrong arity"),
        Err(e) => println!("✓ Option failed correctly: {}", e),
    }

    // Correct arity
    match substitution.instantiate_generic_with_bounds(&option_t, &type_args_correct, &context) {
        Ok(ty) => println!("✓ Option instantiated: {}", ty.display_name()),
        Err(e) => println!("ERROR: Option should succeed: {}", e),
    }
}
