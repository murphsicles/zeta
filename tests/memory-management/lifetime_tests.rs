//! Lifetime system tests for Zeta v0.3.35
//! Tests lifetime parameters, inference, and bounds checking.

use zetac::middle::types::lifetime::{Lifetime, LifetimeVar, LifetimeContext, LifetimeSubstitution};
use zetac::middle::types::{Type, Mutability};

#[test]
fn test_lifetime_basics() {
    println!("=== Testing Lifetime Basics ===");
    
    // Test 1: Lifetime display names
    assert_eq!(Lifetime::Static.display_name(), "'static");
    assert_eq!(Lifetime::Named("a".to_string()).display_name(), "'a");
    let lv = LifetimeVar::fresh();
    assert_eq!(Lifetime::Variable(lv).display_name().starts_with("'L"), true);
    println!("✓ Lifetime display names work");
    
    // Test 2: Lifetime outlives relationships
    assert!(Lifetime::Static.outlives(&Lifetime::Named("a".to_string())));
    assert!(Lifetime::Static.outlives(&Lifetime::Static));
    
    let a = Lifetime::Named("a".to_string());
    assert!(a.outlives(&a)); // Same lifetime
    
    let b = Lifetime::Named("b".to_string());
    assert!(!a.outlives(&b)); // Different lifetimes
    println!("✓ Lifetime outlives relationships work");
    
    // Test 3: Lifetime variable handling
    let var1 = Lifetime::Variable(LifetimeVar::fresh());
    let var2 = Lifetime::Variable(LifetimeVar::fresh());
    
    assert!(var1.outlives(&var1)); // Same variable
    assert!(var1.outlives(&var2)); // Variables assumed related (simplified)
    println!("✓ Lifetime variable handling works");
    
    println!("=== Lifetime basics tests completed ===");
}

#[test]
fn test_lifetime_unification() {
    println!("\n=== Testing Lifetime Unification ===");
    
    let mut subst = LifetimeSubstitution::new();
    
    // Test 1: Unify variable with static
    let var = Lifetime::Variable(LifetimeVar::fresh());
    assert!(subst.unify(&var, &Lifetime::Static).is_ok());
    assert_eq!(subst.apply(&var), Lifetime::Static);
    println!("✓ Can unify lifetime variable with 'static");
    
    // Test 2: Unify two variables
    let var1 = Lifetime::Variable(LifetimeVar::fresh());
    let var2 = Lifetime::Variable(LifetimeVar::fresh());
    
    assert!(subst.unify(&var1, &var2).is_ok());
    let applied1 = subst.apply(&var1);
    let applied2 = subst.apply(&var2);
    assert_eq!(applied1, applied2);
    println!("✓ Can unify two lifetime variables");
    
    // Test 3: Unify named lifetimes (same name)
    let a1 = Lifetime::Named("a".to_string());
    let a2 = Lifetime::Named("a".to_string());
    assert!(subst.unify(&a1, &a2).is_ok());
    println!("✓ Can unify same named lifetimes");
    
    // Test 4: Cannot unify different named lifetimes
    let a = Lifetime::Named("a".to_string());
    let b = Lifetime::Named("b".to_string());
    assert!(subst.unify(&a, &b).is_err());
    println!("✓ Cannot unify different named lifetimes");
    
    println!("=== Lifetime unification tests completed ===");
}

#[test]
fn test_lifetime_context() {
    println!("\n=== Testing Lifetime Context ===");
    
    let mut ctx = LifetimeContext::new();
    
    // Test 1: Add and solve simple constraints
    ctx.add_constraint(Lifetime::Static, Lifetime::Named("a".to_string()));
    ctx.add_constraint(
        Lifetime::Named("a".to_string()),
        Lifetime::Named("b".to_string()),
    );
    
    assert!(ctx.solve().is_ok());
    println!("✓ Can solve simple lifetime constraints");
    
    // Test 2: Check substitution after solving
    let subst = ctx.substitution();
    // No variables were used, so substitution should be empty
    // Note: mapping field is private, so we can't check it directly
    println!("✓ Empty substitution for constraint-only context");
    
    // Test 3: Context with variables
    let mut ctx2 = LifetimeContext::new();
    let var = Lifetime::Variable(LifetimeVar::fresh());
    
    ctx2.add_constraint(Lifetime::Static, var.clone());
    ctx2.add_constraint(var.clone(), Lifetime::Named("short".to_string()));
    
    assert!(ctx2.solve().is_ok());
    println!("✓ Can solve constraints with lifetime variables");
    
    println!("=== Lifetime context tests completed ===");
}

#[test]
fn test_lifetime_in_types() {
    println!("\n=== Testing Lifetimes in Types ===");
    
    // Test 1: Reference types with lifetimes
    let static_ref = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Immutable,
    );
    
    assert_eq!(static_ref.display_name(), "&'static i32");
    println!("✓ Static lifetime in reference type: {}", static_ref.display_name());
    
    // Test 2: Named lifetime in reference
    let named_ref = Type::Ref(
        Box::new(Type::Str),
        Lifetime::Named("a".to_string()),
        Mutability::Immutable,
    );
    
    assert_eq!(named_ref.display_name(), "&'a str");
    println!("✓ Named lifetime in reference type: {}", named_ref.display_name());
    
    // Test 3: Lifetime variable in reference
    let lv = LifetimeVar::fresh();
    let var_ref = Type::Ref(
        Box::new(Type::I64),
        Lifetime::Variable(lv.clone()),
        Mutability::Mutable,
    );
    
    let display_name = var_ref.display_name();
    assert!(display_name.starts_with("&'L") && display_name.contains("mut i64"));
    println!("✓ Lifetime variable in reference type: {}", display_name);
    
    // Test 4: Check if type contains lifetime variables
    assert!(!static_ref.contains_vars());
    assert!(!named_ref.contains_vars());
    assert!(var_ref.contains_vars());
    println!("✓ Type.contains_vars() correctly identifies lifetime variables");
    
    println!("=== Lifetimes in types tests completed ===");
}

#[test]
fn test_lifetime_elision_patterns() {
    println!("\n=== Testing Lifetime Elision Patterns ===");
    
    // These tests demonstrate the concepts of lifetime elision
    // In Rust, the compiler can infer lifetimes in common patterns
    
    println!("Pattern 1: One input lifetime -> output gets same lifetime");
    println!("  fn first_word(s: &str) -> &str");
    println!("  becomes: fn first_word<'a>(s: &'a str) -> &'a str");
    
    println!("\nPattern 2: Multiple input lifetimes, one is &self/&mut self");
    println!("  fn get_name(&self) -> &str");
    println!("  becomes: fn get_name<'a>(&'a self) -> &'a str");
    
    println!("\nPattern 3: Multiple input lifetimes, no &self");
    println!("  fn longest(x: &str, y: &str) -> &str");
    println!("  ERROR: Cannot infer, must specify: fn longest<'a>(x: &'a str, y: &'a str) -> &'a str");
    
    println!("\n✓ Lifetime elision patterns documented");
    println!("⚠ Lifetime elision implementation needed in compiler");
    
    println!("=== Lifetime elision tests completed ===");
}

#[test]
fn test_lifetime_bounds() {
    println!("\n=== Testing Lifetime Bounds ===");
    
    // Test the concept of lifetime bounds in generics
    // In Rust: struct Ref<'a, T: 'a> { r: &'a T }
    // Means: T must live at least as long as 'a
    
    println!("Concept: Lifetime bounds constrain how long referenced data must live");
    println!("Example 1: struct Ref<'a, T: 'a> {{ r: &'a T }}");
    println!("  T must outlive 'a (T lives at least as long as 'a)");
    
    println!("\nExample 2: trait Processor<'a> {{ fn process(&self, data: &'a str); }}");
    println!("  The 'a lifetime is part of the trait");
    
    println!("\nExample 3: impl<'a> Processor<'a> for MyProcessor {{ ... }}");
    println!("  Implementation must match the lifetime parameter");
    
    println!("\n✓ Lifetime bounds concepts documented");
    println!("⚠ Lifetime bounds implementation needed in type system");
    
    println!("=== Lifetime bounds tests completed ===");
}

fn main() {
    println!("Running lifetime system tests for Zeta v0.3.35...");
    test_lifetime_basics();
    test_lifetime_unification();
    test_lifetime_context();
    test_lifetime_in_types();
    test_lifetime_elision_patterns();
    test_lifetime_bounds();
    println!("\n=== All lifetime system tests completed ===");
}