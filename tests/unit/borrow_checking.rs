// Test for borrow checking with reference types
use zetac::frontend::ast::AstNode;
use zetac::frontend::borrow::{BorrowChecker, BorrowState};
use zetac::middle::resolver::resolver::{Resolver, Type};
use zetac::middle::types::Mutability;
use zetac::middle::types::lifetime::Lifetime;

#[test]
fn test_borrow_checker_basics() {
    println!("=== Testing Borrow Checker Basics ===");

    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();

    // Test 1: Declare a variable
    checker.declare("x".to_string(), BorrowState::Owned, Type::I32);
    println!("✓ Variable 'x' declared as Owned");

    // Test 2: Check variable usage
    let var_node = AstNode::Var("x".to_string());
    assert!(checker.check(&var_node, &resolver));
    println!("✓ Variable 'x' can be used when Owned");

    // Test 3: Declare a borrowed variable
    checker.declare(
        "y".to_string(),
        BorrowState::Borrowed,
        Type::Ref(Box::new(Type::I32), Lifetime::Static, Mutability::Immutable),
    );
    println!("✓ Variable 'y' declared as Borrowed");

    let var_y = AstNode::Var("y".to_string());
    assert!(checker.check(&var_y, &resolver));
    println!("✓ Borrowed variable 'y' can be used");

    // Test 4: Declare a consumed variable
    checker.declare("z".to_string(), BorrowState::Consumed, Type::I32);
    println!("✓ Variable 'z' declared as Consumed");

    let var_z = AstNode::Var("z".to_string());
    assert!(!checker.check(&var_z, &resolver));
    println!("✓ Consumed variable 'z' cannot be used");

    println!("=== All basic borrow checker tests passed ===");
}

#[test]
fn test_reference_type_borrow_checking() {
    println!("\n=== Testing Reference Type Borrow Checking ===");

    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();

    // Note: The current borrow checker doesn't understand reference types
    // specifically, but we can test the basic functionality

    // Test 1: &str reference can be used multiple times (immutable borrow)
    checker.declare(
        "s".to_string(),
        BorrowState::Borrowed,
        Type::Ref(Box::new(Type::Str), Lifetime::Static, Mutability::Immutable),
    );
    println!("✓ &str reference declared as Borrowed");

    let var_s = AstNode::Var("s".to_string());
    assert!(checker.check(&var_s, &resolver));
    println!("✓ &str reference can be used");

    // Test 2: &mut i64 reference (mutable borrow)
    checker.declare(
        "m".to_string(),
        BorrowState::MutBorrowed,
        Type::Ref(Box::new(Type::I64), Lifetime::Static, Mutability::Mutable),
    );
    println!("✓ &mut i64 reference declared as MutBorrowed");

    let var_m = AstNode::Var("m".to_string());
    assert!(checker.check(&var_m, &resolver));
    println!("✓ &mut i64 reference can be used");

    // Test 3: Assignment to mutable reference
    let assign = AstNode::Assign(
        Box::new(AstNode::Var("m".to_string())),
        Box::new(AstNode::Lit(42)),
    );
    // This should work because m is MutBorrowed
    assert!(checker.check(&assign, &resolver));
    println!("✓ Can assign to mutable reference");

    println!("=== All reference type borrow checking tests passed ===");
}

#[test]
fn test_borrow_checker_with_ast() {
    println!("\n=== Testing Borrow Checker with AST Nodes ===");

    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();

    // Declare some variables
    checker.declare("a".to_string(), BorrowState::Owned, Type::I32);
    checker.declare("b".to_string(), BorrowState::Owned, Type::I32);

    // Test binary operation
    let binop = AstNode::BinaryOp {
        op: "+".to_string(),
        left: Box::new(AstNode::Var("a".to_string())),
        right: Box::new(AstNode::Var("b".to_string())),
    };

    assert!(checker.check(&binop, &resolver));
    println!("✓ Binary operation with owned variables works");

    // Test assignment
    let assign = AstNode::Assign(
        Box::new(AstNode::Var("c".to_string())),
        Box::new(AstNode::Lit(42)),
    );

    assert!(checker.check(&assign, &resolver));
    println!("✓ Assignment creates new variable");

    // Check that c was declared
    let var_c = AstNode::Var("c".to_string());
    assert!(checker.check(&var_c, &resolver));
    println!("✓ New variable 'c' can be used after assignment");

    println!("=== All AST-based borrow checker tests passed ===");
}

fn main() {
    println!("Running borrow checking tests...");
    test_borrow_checker_basics();
    test_reference_type_borrow_checking();
    test_borrow_checker_with_ast();
    println!("\n=== All borrow checking tests completed successfully ===");
}
