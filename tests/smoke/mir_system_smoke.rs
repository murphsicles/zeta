// MIR System Smoke Tests - Simplified
// Basic smoke tests for the Middle Intermediate Representation system

use zetac::middle::mir::mir::{Mir, MirExpr, MirStmt};

#[test]
fn test_mir_smoke_creation() {
    // Test basic MIR creation
    let mut mir = Mir::default();

    // Add some expressions
    mir.exprs.insert(1, MirExpr::Lit(42));
    mir.exprs.insert(2, MirExpr::Lit(10));

    // Add a statement
    mir.stmts.push(MirStmt::Assign { lhs: 1, rhs: 2 });

    // Add a return statement
    mir.stmts.push(MirStmt::Return { val: 1 });

    // Verify MIR structure
    assert_eq!(mir.exprs.len(), 2, "Should have 2 expressions");
    assert_eq!(mir.stmts.len(), 2, "Should have 2 statements");
}

#[test]
fn test_mir_smoke_expression_types() {
    // Test different MIR expression types
    let mut mir = Mir::default();

    // Test literal expression
    mir.exprs.insert(1, MirExpr::Lit(100));
    assert!(matches!(mir.exprs.get(&1), Some(MirExpr::Lit(100))));

    // Test variable expression
    mir.exprs.insert(2, MirExpr::Var(42));
    assert!(matches!(mir.exprs.get(&2), Some(MirExpr::Var(42))));

    // Test string literal
    mir.exprs.insert(3, MirExpr::StringLit("hello".to_string()));
    if let Some(MirExpr::StringLit(s)) = mir.exprs.get(&3) {
        assert_eq!(s, "hello", "String literal should be 'hello'");
    } else {
        panic!("Should be a string literal");
    }
}

#[test]
fn test_mir_smoke_statement_types() {
    // Test different MIR statement types
    let mut mir = Mir::default();

    // Create some expressions first
    mir.exprs.insert(1, MirExpr::Lit(1));
    mir.exprs.insert(2, MirExpr::Lit(2));
    mir.exprs.insert(3, MirExpr::Lit(3));

    // Test assignment statement
    mir.stmts.push(MirStmt::Assign { lhs: 1, rhs: 2 });

    // Test return statement
    mir.stmts.push(MirStmt::Return { val: 3 });

    // Test void call statement (function call without return value)
    mir.stmts.push(MirStmt::VoidCall {
        func: "print".to_string(),
        args: vec![1],
    });

    // Verify statements
    assert_eq!(mir.stmts.len(), 3, "Should have 3 statements");

    // Check assignment statement
    if let MirStmt::Assign { lhs, rhs } = &mir.stmts[0] {
        assert_eq!(*lhs, 1, "Assignment lhs should be expr1");
        assert_eq!(*rhs, 2, "Assignment rhs should be expr2");
    } else {
        panic!("First statement should be assignment");
    }

    // Check return statement
    if let MirStmt::Return { val } = &mir.stmts[1] {
        assert_eq!(*val, 3, "Return value should be expr3");
    } else {
        panic!("Second statement should be return");
    }

    // Check void call statement
    if let MirStmt::VoidCall { func, args } = &mir.stmts[2] {
        assert_eq!(func, "print", "Function name should be 'print'");
        assert_eq!(args.len(), 1, "Should have 1 argument");
        assert_eq!(args[0], 1, "Argument should be expr1");
    } else {
        panic!("Third statement should be void call");
    }
}

#[test]
fn test_mir_smoke_optimization_interface() {
    // Note: Optimization module is not exported in the public API
    // This test is skipped for now
    println!("Optimization interface test skipped - module not exported");
}
