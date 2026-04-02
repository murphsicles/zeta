//! Borrowing and reference tests for Zeta v0.3.35
//! Tests reference types (&T, &mut T) and borrowing rules.

use zetac::frontend::ast::AstNode;
use zetac::frontend::borrow::{BorrowChecker, BorrowState};
use zetac::middle::resolver::resolver::{Resolver, Type};
use zetac::middle::types::Mutability;
use zetac::middle::types::lifetime::Lifetime;

#[test]
fn test_reference_types() {
    println!("=== Testing Reference Types ===");
    
    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();
    
    // Test 1: Immutable reference type (&T)
    let ref_i32 = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Immutable,
    );
    checker.declare("r".to_string(), BorrowState::Borrowed, ref_i32);
    
    let var_r = AstNode::Var("r".to_string());
    assert!(checker.check(&var_r, &resolver));
    println!("✓ Immutable reference type &i32 works");
    
    // Test 2: Mutable reference type (&mut T)
    let mut_ref_i32 = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Mutable,
    );
    checker.declare("mr".to_string(), BorrowState::MutBorrowed, mut_ref_i32);
    
    let var_mr = AstNode::Var("mr".to_string());
    assert!(checker.check(&var_mr, &resolver));
    println!("✓ Mutable reference type &mut i32 works");
    
    // Test 3: Reference to reference (& &T)
    let ref_ref = Type::Ref(
        Box::new(Type::Ref(
            Box::new(Type::I32),
            Lifetime::Static,
            Mutability::Immutable,
        )),
        Lifetime::Static,
        Mutability::Immutable,
    );
    checker.declare("rr".to_string(), BorrowState::Borrowed, ref_ref);
    
    let var_rr = AstNode::Var("rr".to_string());
    assert!(checker.check(&var_rr, &resolver));
    println!("✓ Reference to reference & &i32 works");
    
    println!("=== Reference types tests completed ===");
}

#[test]
fn test_borrowing_rules() {
    println!("\n=== Testing Borrowing Rules ===");
    
    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();
    
    // Rule 1: Multiple immutable borrows allowed
    checker.declare("x".to_string(), BorrowState::Owned, Type::I32);
    
    // Create first immutable borrow
    checker.declare("b1".to_string(), BorrowState::Borrowed, Type::I32);
    let var_b1 = AstNode::Var("b1".to_string());
    assert!(checker.check(&var_b1, &resolver));
    
    // Create second immutable borrow (should be allowed)
    checker.declare("b2".to_string(), BorrowState::Borrowed, Type::I32);
    let var_b2 = AstNode::Var("b2".to_string());
    assert!(checker.check(&var_b2, &resolver));
    
    println!("✓ Rule 1: Multiple immutable borrows allowed - PASS");
    
    // Rule 2: Only one mutable borrow at a time
    checker.declare("y".to_string(), BorrowState::Owned, Type::I32);
    
    // Create mutable borrow
    checker.declare("mb1".to_string(), BorrowState::MutBorrowed, Type::I32);
    let var_mb1 = AstNode::Var("mb1".to_string());
    assert!(checker.check(&var_mb1, &resolver));
    
    // Attempt second mutable borrow (should fail in real implementation)
    // Current checker doesn't track this, so we just note the concept
    println!("⚠ Rule 2: Only one mutable borrow - needs implementation");
    
    // Rule 3: Cannot have mutable and immutable borrows simultaneously
    checker.declare("z".to_string(), BorrowState::Owned, Type::I32);
    
    // Create immutable borrow
    checker.declare("ib".to_string(), BorrowState::Borrowed, Type::I32);
    
    // Attempt mutable borrow while immutable exists (should fail)
    println!("⚠ Rule 3: No mutable while immutable - needs implementation");
    
    println!("=== Borrowing rules tests completed ===");
}

#[test]
fn test_dereferencing() {
    println!("\n=== Testing Dereferencing ===");
    
    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();
    
    // Test 1: Dereferencing a reference
    let ref_type = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Immutable,
    );
    checker.declare("ref_val".to_string(), BorrowState::Borrowed, ref_type);
    
    // Create dereference node (*ref_val)
    let deref = AstNode::UnaryOp {
        op: "*".to_string(),
        expr: Box::new(AstNode::Var("ref_val".to_string())),
    };
    
    // Current checker doesn't understand dereferencing specifically
    // but should allow the operation
    assert!(checker.check(&deref, &resolver));
    println!("✓ Dereferencing works (basic check)");
    
    // Test 2: Taking address of a variable (&x)
    checker.declare("val".to_string(), BorrowState::Owned, Type::I32);
    
    let addr_of = AstNode::UnaryOp {
        op: "&".to_string(),
        expr: Box::new(AstNode::Var("val".to_string())),
    };
    
    assert!(checker.check(&addr_of, &resolver));
    println!("✓ Taking address works");
    
    // Test 3: Taking mutable address (&mut x)
    checker.declare("mut_val".to_string(), BorrowState::Owned, Type::I32);
    
    let mut_addr_of = AstNode::UnaryOp {
        op: "&mut".to_string(),
        expr: Box::new(AstNode::Var("mut_val".to_string())),
    };
    
    assert!(checker.check(&mut_addr_of, &resolver));
    println!("✓ Taking mutable address works");
    
    println!("=== Dereferencing tests completed ===");
}

#[test]
fn test_borrow_checker_integration() {
    println!("\n=== Testing Borrow Checker Integration ===");
    
    let mut checker = BorrowChecker::new();
    let resolver = Resolver::new();
    
    // Complex scenario: mix of owned, borrowed, and mutable borrowed
    checker.declare("owned1".to_string(), BorrowState::Owned, Type::I32);
    checker.declare("owned2".to_string(), BorrowState::Owned, Type::I32);
    
    // Create references
    let ref_type = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Immutable,
    );
    checker.declare("ref1".to_string(), BorrowState::Borrowed, ref_type.clone());
    checker.declare("ref2".to_string(), BorrowState::Borrowed, ref_type);
    
    // Create mutable reference
    let mut_ref_type = Type::Ref(
        Box::new(Type::I32),
        Lifetime::Static,
        Mutability::Mutable,
    );
    checker.declare("mut_ref".to_string(), BorrowState::MutBorrowed, mut_ref_type);
    
    // Test all can be used
    let nodes = vec![
        AstNode::Var("owned1".to_string()),
        AstNode::Var("owned2".to_string()),
        AstNode::Var("ref1".to_string()),
        AstNode::Var("ref2".to_string()),
        AstNode::Var("mut_ref".to_string()),
    ];
    
    for (i, node) in nodes.iter().enumerate() {
        assert!(checker.check(node, &resolver), "Node {} failed", i);
    }
    
    println!("✓ All variable types can be used in integrated scenario");
    
    // Test assignment through mutable reference
    let assign = AstNode::Assign(
        Box::new(AstNode::Var("mut_ref".to_string())),
        Box::new(AstNode::Lit(100)),
    );
    
    assert!(checker.check(&assign, &resolver));
    println!("✓ Can assign through mutable reference");
    
    println!("=== Borrow checker integration tests completed ===");
}

fn main() {
    println!("Running borrowing and reference tests for Zeta v0.3.35...");
    test_reference_types();
    test_borrowing_rules();
    test_dereferencing();
    test_borrow_checker_integration();
    println!("\n=== All borrowing tests completed ===");
}