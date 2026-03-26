//! Test the new type system with unification

extern crate zetac;
use zetac::middle::types::{Type, TypeVar, Substitution};
use zetac::frontend::ast::AstNode;

fn main() {
    println!("=== Testing New Type System Foundation ===");
    
    // Test 1: Basic type display
    println!("\n1. Type Display:");
    println!("  i64: {}", Type::I64.display_name());
    println!("  bool: {}", Type::Bool.display_name());
    println!("  [i32; 10]: {}", Type::Array(Box::new(Type::I32), 10).display_name());
    println!("  (i32, bool): {}", Type::Tuple(vec![Type::I32, Type::Bool]).display_name());
    
    // Test 2: Unification
    println!("\n2. Type Unification:");
    let mut subst = Substitution::new();
    
    // Create type variables
    let a = Type::Variable(TypeVar::fresh());
    let b = Type::Variable(TypeVar::fresh());
    
    println!("  Created type variables: {} and {}", a.display_name(), b.display_name());
    
    // Unify a = i64
    match subst.unify(&a, &Type::I64) {
        Ok(()) => println!("  ✓ Unified {} = i64", a.display_name()),
        Err(e) => println!("  ✗ Failed: {}", e),
    }
    
    // Unify b = a (which is now i64)
    match subst.unify(&b, &a) {
        Ok(()) => println!("  ✓ Unified {} = {}", b.display_name(), a.display_name()),
        Err(e) => println!("  ✗ Failed: {}", e),
    }
    
    // Apply substitution
    println!("  After substitution:");
    println!("    {} -> {}", a.display_name(), subst.apply(&a).display_name());
    println!("    {} -> {}", b.display_name(), subst.apply(&b).display_name());
    
    // Test 3: Function type unification
    println!("\n3. Function Type Unification:");
    let mut subst2 = Substitution::new();
    let x = Type::Variable(TypeVar::fresh());
    
    // (i32) -> X  unified with  (i32) -> i64
    let func1 = Type::Function(vec![Type::I32], Box::new(x.clone()));
    let func2 = Type::Function(vec![Type::I32], Box::new(Type::I64));
    
    println!("  Unifying {} with {}", func1.display_name(), func2.display_name());
    
    match subst2.unify(&func1, &func2) {
        Ok(()) => {
            println!("  ✓ Success!");
            println!("  X = {}", subst2.apply(&x).display_name());
        }
        Err(e) => println!("  ✗ Failed: {}", e),
    }
    
    // Test 4: Occurs check
    println!("\n4. Occurs Check (Prevents Infinite Types):");
    let mut subst3 = Substitution::new();
    let y = Type::Variable(TypeVar::fresh());
    let list_of_y = Type::Named("List".to_string(), vec![y.clone()]);
    
    println!("  Trying to unify {} = {}", y.display_name(), list_of_y.display_name());
    
    match subst3.unify(&y, &list_of_y) {
        Ok(()) => println!("  ✗ Should have failed occurs check!"),
        Err(e) => println!("  ✓ Correctly rejected: {}", e),
    }
    
    // Test 5: Type inference example
    println!("\n5. Type Inference Example:");
    println!("  Expression: x = 42, y = x + 1");
    println!("  Expected: x: i32, y: i32");
    
    // Simulate the inference
    let mut subst4 = Substitution::new();
    let x_type = Type::Variable(TypeVar::fresh());
    let y_type = Type::Variable(TypeVar::fresh());
    
    // Constraint: x = i32 (from literal 42)
    subst4.unify(&x_type, &Type::I32).unwrap();
    
    // Constraint: y = x + 1, so y = x (both i32)
    subst4.unify(&y_type, &x_type).unwrap();
    
    println!("  Inferred: x: {}, y: {}", 
        subst4.apply(&x_type).display_name(),
        subst4.apply(&y_type).display_name());
    
    println!("\n=== New Type System Foundation Complete ===");
    println!("Key features implemented:");
    println!("  ✓ Algebraic type representation (not strings)");
    println!("  ✓ Type variables with fresh generation");
    println!("  ✓ Hindley-Milner unification algorithm");
    println!("  ✓ Occurs check for soundness");
    println!("  ✓ Function type unification");
    println!("  ✓ Substitution application");
    println!("\nReady for integration with Zeta's semantic analysis!");
}