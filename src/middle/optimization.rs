// src/middle/optimization.rs
//! Optimization passes for MIR

use super::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use std::collections::HashMap;

/// Optimization level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OptLevel {
    /// No optimizations
    O0,
    /// Basic optimizations
    O1,
    /// Aggressive optimizations
    O2,
    /// Maximum optimizations
    O3,
}

/// Dead code elimination pass
pub fn dead_code_elimination(mir: &mut Mir) {
    let mut used = HashMap::new();

    // First pass: mark all expressions that are directly used
    // (in returns, as arguments, etc.)
    for stmt in &mir.stmts {
        match stmt {
            MirStmt::Assign { lhs: _, rhs: _ } => {
                // Don't mark rhs yet - we'll handle it in the second pass
                // after we know which variables are used
            }
            MirStmt::Call { args, dest: _, .. } => {
                for arg in args {
                    mark_expr_used(*arg, &mut used, &mir.exprs);
                }
            }
            MirStmt::VoidCall { args, .. } => {
                for arg in args {
                    mark_expr_used(*arg, &mut used, &mir.exprs);
                }
            }
            MirStmt::Return { val } => {
                mark_expr_used(*val, &mut used, &mir.exprs);
            }
            MirStmt::SemiringFold { values, result, .. } => {
                used.insert(*result, true);
                for val in values {
                    mark_expr_used(*val, &mut used, &mir.exprs);
                }
            }
            MirStmt::ParamInit { param_id, .. } => {
                used.insert(*param_id, true);
            }
            MirStmt::Consume { id } => {
                mark_expr_used(*id, &mut used, &mir.exprs);
            }
            MirStmt::StructNew { fields, dest, .. } => {
                used.insert(*dest, true);
                for (_, expr_id) in fields {
                    mark_expr_used(*expr_id, &mut used, &mir.exprs);
                }
            }
            MirStmt::If {
                cond,
                then,
                else_,
                dest: _,
            } => {
                mark_expr_used(*cond, &mut used, &mir.exprs);
                // Recursively process nested statements
                let mut nested_mir = Mir {
                    stmts: then.clone(),
                    ..Default::default()
                };
                dead_code_elimination(&mut nested_mir);

                let mut nested_mir_else = Mir {
                    stmts: else_.clone(),
                    ..Default::default()
                };
                dead_code_elimination(&mut nested_mir_else);
            }
            MirStmt::TryProp {
                expr_id,
                ok_dest: _,
                err_dest: _,
            } => {
                mark_expr_used(*expr_id, &mut used, &mir.exprs);
            }
            MirStmt::DictInsert {
                map_id,
                key_id,
                val_id,
            } => {
                mark_expr_used(*map_id, &mut used, &mir.exprs);
                mark_expr_used(*key_id, &mut used, &mir.exprs);
                mark_expr_used(*val_id, &mut used, &mir.exprs);
            }
            MirStmt::DictGet {
                map_id,
                key_id,
                dest: _,
            } => {
                mark_expr_used(*map_id, &mut used, &mir.exprs);
                mark_expr_used(*key_id, &mut used, &mir.exprs);
            }
            MirStmt::MapNew { dest: _ } => {
                // Nothing to mark
            }
            MirStmt::For {
                iterator,
                pattern: _,
                body,
            } => {
                mark_expr_used(*iterator, &mut used, &mir.exprs);
                // Recursively process nested statements in the loop body
                let mut nested_mir = Mir {
                    stmts: body.clone(),
                    ..Default::default()
                };
                dead_code_elimination(&mut nested_mir);
            }
        }
    }

    // Second pass: propagate usage through assignments
    // We need to iterate backwards because usage flows from later statements to earlier ones
    for stmt in mir.stmts.iter().rev() {
        if let MirStmt::Assign { lhs, rhs } = stmt {
            // If the lhs is used, then mark the rhs as used
            if used.contains_key(lhs) {
                mark_expr_used(*rhs, &mut used, &mir.exprs);
            }
        }
    }

    // Remove unused expressions
    mir.exprs.retain(|id, _| used.contains_key(id));

    // Remove dead assignments
    mir.stmts.retain(|stmt| match stmt {
        MirStmt::Assign { lhs, .. } => used.contains_key(lhs),
        _ => true,
    });
}

fn mark_expr_used(id: u32, used: &mut HashMap<u32, bool>, exprs: &HashMap<u32, MirExpr>) {
    used.insert(id, true);

    if let Some(expr) = exprs.get(&id) {
        match expr {
            MirExpr::Var(var_id) => {
                used.insert(*var_id, true);
            }
            MirExpr::FString(parts) => {
                for part in parts {
                    used.insert(*part, true);
                }
            }
            MirExpr::TimingOwned(inner_id) => {
                used.insert(*inner_id, true);
            }
            _ => {}
        }
    }
}

/// Constant folding pass
pub fn constant_folding(mir: &mut Mir) {
    let mut const_values = HashMap::new();

    // First pass: collect constant values
    for (id, expr) in &mir.exprs {
        match expr {
            MirExpr::Lit(value) => {
                const_values.insert(*id, *value);
            }
            MirExpr::ConstEval(value) => {
                const_values.insert(*id, *value);
            }
            _ => {}
        }
    }

    // Second pass: fold constants in statements
    for stmt in &mut mir.stmts {
        if let MirStmt::SemiringFold { op, values, result } = stmt {
            // Try to fold if all values are constants
            let mut all_const = true;
            let mut folded_value: i64 = match op {
                SemiringOp::Add => 0,
                SemiringOp::Mul => 1,
            };

            for val in values.iter() {
                if let Some(const_val) = const_values.get(val) {
                    folded_value = match op {
                        SemiringOp::Add => folded_value.wrapping_add(*const_val),
                        SemiringOp::Mul => folded_value.wrapping_mul(*const_val),
                    };
                } else {
                    all_const = false;
                    break;
                }
            }

            if all_const {
                // Replace with constant expression
                mir.exprs.insert(*result, MirExpr::Lit(folded_value));
                // Mark the statement for removal (will be handled by DCE)
                *stmt = MirStmt::Assign {
                    lhs: *result,
                    rhs: *result, // Self-assignment, will be removed
                };
            }
        }
    }
}

/// Run all optimizations at the specified level
pub fn optimize(mir: &mut Mir, level: OptLevel) {
    match level {
        OptLevel::O0 => {
            // No optimizations
        }
        OptLevel::O1 => {
            // Basic optimizations
            dead_code_elimination(mir);
            constant_folding(mir);
            dead_code_elimination(mir); // Clean up after constant folding
        }
        OptLevel::O2 => {
            // Aggressive optimizations (O1 plus more)
            dead_code_elimination(mir);
            constant_folding(mir);
            // Add more passes here as needed
            dead_code_elimination(mir);
        }
        OptLevel::O3 => {
            // Maximum optimizations (O2 plus even more)
            dead_code_elimination(mir);
            constant_folding(mir);
            // Run multiple iterations for maximum effect
            for _ in 0..3 {
                dead_code_elimination(mir);
                constant_folding(mir);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::mir::mir::{Mir, MirExpr, MirStmt};

    #[test]
    fn test_dead_code_elimination() {
        let mut mir = Mir {
            stmts: vec![
                MirStmt::Assign { lhs: 1, rhs: 2 },
                MirStmt::Assign { lhs: 3, rhs: 4 },
                MirStmt::Return { val: 1 },
            ],
            exprs: vec![
                (1, MirExpr::Lit(42)),
                (2, MirExpr::Lit(10)),
                (3, MirExpr::Lit(99)),
                (4, MirExpr::Lit(20)),
            ]
            .into_iter()
            .collect(),
            ..Default::default()
        };

        dead_code_elimination(&mut mir);

        // Only expression 1 and 2 should remain (3 and 4 are dead)
        assert_eq!(mir.exprs.len(), 2);
        assert!(mir.exprs.contains_key(&1));
        assert!(mir.exprs.contains_key(&2));

        // Only the return statement and assignment to 1 should remain
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
}
