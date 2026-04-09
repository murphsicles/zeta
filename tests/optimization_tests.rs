//! Tests for optimization passes

use zetac::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use zetac::middle::optimization::{strength_reduction, algebraic_simplification, dead_code_elimination, constant_folding, common_subexpression_elimination};

#[test]
fn test_strength_reduction_basic() {
    // Test: x * 8 should be optimized to x << 3
    let mut mir = Mir {
        stmts: vec![
            MirStmt::Assign { lhs: 1, rhs: 2 }, // x = some value
            MirStmt::SemiringFold {
                op: SemiringOp::Mul,
                values: vec![1, 3], // x * 8
                result: 4,
            },
            MirStmt::Return { val: 4 },
        ],
        exprs: vec![
            (1, MirExpr::Var(100)), // x (variable)
            (2, MirExpr::Lit(42)),  // x's value
            (3, MirExpr::Lit(8)),   // 8 (power of two)
            (4, MirExpr::Lit(0)),   // Result placeholder
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    // Run strength reduction
    strength_reduction(&mut mir);

    // Check that the SemiringFold was replaced
    // (In our implementation, it becomes a self-assignment that DCE will clean up)
    let has_semiring_fold = mir.stmts.iter().any(|stmt| {
        matches!(stmt, MirStmt::SemiringFold { .. })
    });
    
    // The optimization should have replaced the SemiringFold
    assert!(!has_semiring_fold, "SemiringFold should have been replaced");
    
    // Check that we have a BinaryOp with "<<"
    let has_shift = mir.exprs.values().any(|expr| {
        if let MirExpr::BinaryOp { op, .. } = expr {
            op == "<<"
        } else {
            false
        }
    });
    
    assert!(has_shift, "Should have created a shift operation");
}

#[test]
fn test_algebraic_simplification_add_zero() {
    // Test: x + 0 should be optimized to x
    let mut mir = Mir {
        stmts: vec![
            MirStmt::Assign { lhs: 1, rhs: 2 }, // x = some value
            MirStmt::SemiringFold {
                op: SemiringOp::Add,
                values: vec![1, 3], // x + 0
                result: 4,
            },
            MirStmt::Return { val: 4 },
        ],
        exprs: vec![
            (1, MirExpr::Var(100)), // x (variable)
            (2, MirExpr::Lit(42)),  // x's value
            (3, MirExpr::Lit(0)),   // 0
            (4, MirExpr::Lit(0)),   // Result placeholder
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    // Run algebraic simplification
    algebraic_simplification(&mut mir);

    // Check that result expression is now the same as x (expression 1)
    // In our implementation, the result should point to expression 1
    // (or be a copy of it)
    let result_is_x = match mir.exprs.get(&4) {
        Some(MirExpr::Var(var_id)) => *var_id == 100,
        _ => false,
    };
    
    assert!(result_is_x, "x + 0 should simplify to x");
}

#[test]
fn test_algebraic_simplification_mul_one() {
    // Test: x * 1 should be optimized to x
    let mut mir = Mir {
        stmts: vec![
            MirStmt::Assign { lhs: 1, rhs: 2 }, // x = some value
            MirStmt::SemiringFold {
                op: SemiringOp::Mul,
                values: vec![1, 3], // x * 1
                result: 4,
            },
            MirStmt::Return { val: 4 },
        ],
        exprs: vec![
            (1, MirExpr::Var(100)), // x (variable)
            (2, MirExpr::Lit(42)),  // x's value
            (3, MirExpr::Lit(1)),   // 1
            (4, MirExpr::Lit(0)),   // Result placeholder
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    // Run algebraic simplification
    algebraic_simplification(&mut mir);

    // Check that result expression is now the same as x (expression 1)
    let result_is_x = match mir.exprs.get(&4) {
        Some(MirExpr::Var(var_id)) => *var_id == 100,
        _ => false,
    };
    
    assert!(result_is_x, "x * 1 should simplify to x");
}

#[test]
fn test_algebraic_simplification_mul_zero() {
    // Test: x * 0 should be optimized to 0
    let mut mir = Mir {
        stmts: vec![
            MirStmt::Assign { lhs: 1, rhs: 2 }, // x = some value
            MirStmt::SemiringFold {
                op: SemiringOp::Mul,
                values: vec![1, 3], // x * 0
                result: 4,
            },
            MirStmt::Return { val: 4 },
        ],
        exprs: vec![
            (1, MirExpr::Var(100)), // x (variable)
            (2, MirExpr::Lit(42)),  // x's value
            (3, MirExpr::Lit(0)),   // 0
            (4, MirExpr::Lit(0)),   // Result placeholder
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    // Run algebraic simplification
    algebraic_simplification(&mut mir);

    // Check that result expression is now 0
    let result_is_zero = match mir.exprs.get(&4) {
        Some(MirExpr::Lit(value)) => *value == 0,
        _ => false,
    };
    
    assert!(result_is_zero, "x * 0 should simplify to 0");
}

#[test]
fn test_dead_code_elimination() {
    let mut mir = Mir {
        stmts: vec![
            MirStmt::Assign { lhs: 1, rhs: 2 }, // Dead assignment
            MirStmt::Assign { lhs: 3, rhs: 4 }, // Used assignment
            MirStmt::Return { val: 3 },
        ],
        exprs: vec![
            (1, MirExpr::Lit(42)),  // Dead expression
            (2, MirExpr::Lit(10)),  // Dead expression
            (3, MirExpr::Lit(99)),  // Used expression
            (4, MirExpr::Lit(20)),  // Used expression
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    dead_code_elimination(&mut mir);

    // Only expressions 3 and 4 should remain (1 and 2 are dead)
    assert_eq!(mir.exprs.len(), 2);
    assert!(mir.exprs.contains_key(&3));
    assert!(mir.exprs.contains_key(&4));

    // Only the return statement and assignment to 3 should remain
    assert_eq!(mir.stmts.len(), 2);
}

#[test]
fn test_constant_folding() {
    let mut mir = Mir {
        stmts: vec![MirStmt::SemiringFold {
            op: SemiringOp::Add,
            values: vec![1, 2, 3],
            result: 4,
        }],
        exprs: vec![
            (1, MirExpr::Lit(10)),
            (2, MirExpr::Lit(20)),
            (3, MirExpr::Lit(30)),
            (4, MirExpr::Lit(0)), // Will be replaced
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    constant_folding(&mut mir);

    // Result should be constant folded to 60
    if let MirExpr::Lit(value) = mir.exprs[&4] {
        assert_eq!(value, 60);
    } else {
        panic!("Expected Lit(60)");
    }
}

#[test]
fn test_common_subexpression_elimination() {
    let mut mir = Mir {
        stmts: vec![
            MirStmt::Assign { lhs: 1, rhs: 2 },
            MirStmt::Assign { lhs: 3, rhs: 4 }, // Same as expression 2
            MirStmt::Return { val: 1 },
        ],
        exprs: vec![
            (1, MirExpr::Lit(0)), // Will be replaced
            (2, MirExpr::Lit(42)),
            (3, MirExpr::Lit(0)), // Will be replaced
            (4, MirExpr::Lit(42)), // Duplicate of expression 2
        ]
        .into_iter()
        .collect(),
        ..Default::default()
    };

    common_subexpression_elimination(&mut mir);

    // Expression 4 should have been eliminated (duplicate of 2)
    // Both assignments should now use expression 2
    assert!(!mir.exprs.contains_key(&4), "Duplicate expression should be removed");
    
    // Check that assignment to 3 now uses expression 2
    let stmt_uses_expr2 = mir.stmts.iter().any(|stmt| {
        if let MirStmt::Assign { rhs, .. } = stmt {
            *rhs == 2
        } else {
            false
        }
    });
    
    assert!(stmt_uses_expr2, "Should use expression 2 instead of duplicate");
}