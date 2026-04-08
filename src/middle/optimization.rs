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
                var_id: _,
            } => {
                mark_expr_used(*iterator, &mut used, &mir.exprs);
                // Recursively process nested statements in the loop body
                let mut nested_mir = Mir {
                    stmts: body.clone(),
                    ..Default::default()
                };
                dead_code_elimination(&mut nested_mir);
            }
            MirStmt::While {
                cond,
                body,
            } => {
                mark_expr_used(*cond, &mut used, &mir.exprs);
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
            MirExpr::StackArray { elements, .. } => {
                for element_id in elements {
                    used.insert(*element_id, true);
                }
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

/// Common subexpression elimination
pub fn common_subexpression_elimination(mir: &mut Mir) {
    let mut expression_map = HashMap::new(); // expression hash -> expression id
    let mut to_remove = Vec::new();
    
    // First pass: identify duplicate expressions
    for (id, expr) in &mir.exprs {
        // Create a hashable representation of the expression
        let expr_hash = match expr {
            MirExpr::Lit(value) => format!("Lit({})", value),
            MirExpr::Var(var_id) => format!("Var({})", var_id),
            MirExpr::ConstEval(value) => format!("ConstEval({})", value),
            MirExpr::FString(parts) => {
                let parts_str = parts.iter()
                    .map(|p| p.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("FString([{}])", parts_str)
            }
            MirExpr::TimingOwned(inner_id) => format!("TimingOwned({})", inner_id),
            MirExpr::StringLit(s) => format!("StringLit({})", s),
            MirExpr::Struct { variant, fields, .. } => {
                let fields_str = fields.iter()
                    .map(|(name, id)| format!("{}:{}", name, id))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("Struct{} {{{}}}", variant, fields_str)
            }
            MirExpr::FieldAccess { base, field } => format!("FieldAccess({}.{})", base, field),
            MirExpr::As { expr, target_type } => format!("As({} as {:?})", expr, target_type),
            MirExpr::Range { start, end } => {
                format!("Range({}-{})", start, end)
            }
            MirExpr::BinaryOp { op, left, right } => {
                format!("BinaryOp({} {} {})", left, op, right)
            }
            MirExpr::StackArray { elements, size } => {
                let elements_str = elements.iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(",");
                format!("StackArray(size={}, elements=[{}])", size, elements_str)
            }
        };
        
        // Check if we've seen this expression before
        if let Some(&existing_id) = expression_map.get(&expr_hash) {
            // Replace all uses of this expression with the existing one
            for stmt in &mut mir.stmts {
                match stmt {
                    MirStmt::Assign { lhs: _, rhs } if *rhs == *id => {
                        *rhs = existing_id;
                    }
                    MirStmt::Call { args, dest: _, .. } => {
                        for arg in args {
                            if *arg == *id {
                                *arg = existing_id;
                            }
                        }
                    }
                    MirStmt::VoidCall { args, .. } => {
                        for arg in args {
                            if *arg == *id {
                                *arg = existing_id;
                            }
                        }
                    }
                    MirStmt::Return { val } if *val == *id => {
                        *val = existing_id;
                    }
                    MirStmt::SemiringFold { values, result: _, .. } => {
                        for val in values {
                            if *val == *id {
                                *val = existing_id;
                            }
                        }
                    }
                    MirStmt::Consume { id: expr_id } if *expr_id == *id => {
                        *expr_id = existing_id;
                    }
                    MirStmt::StructNew { fields, dest: _, .. } => {
                        for (_, expr_id) in fields {
                            if *expr_id == *id {
                                *expr_id = existing_id;
                            }
                        }
                    }
                    MirStmt::If { cond, then: _, else_: _, dest: _ } if *cond == *id => {
                        *cond = existing_id;
                    }
                    MirStmt::TryProp { expr_id, ok_dest: _, err_dest: _ } if *expr_id == *id => {
                        *expr_id = existing_id;
                    }
                    MirStmt::DictInsert { map_id, key_id, val_id } => {
                        if *map_id == *id {
                            *map_id = existing_id;
                        }
                        if *key_id == *id {
                            *key_id = existing_id;
                        }
                        if *val_id == *id {
                            *val_id = existing_id;
                        }
                    }
                    MirStmt::DictGet { map_id, key_id, dest: _ } => {
                        if *map_id == *id {
                            *map_id = existing_id;
                        }
                        if *key_id == *id {
                            *key_id = existing_id;
                        }
                    }
                    MirStmt::For { iterator, pattern: _, body: _, var_id: _ } if *iterator == *id => {
                        *iterator = existing_id;
                    }
                    _ => {}
                }
            }
            
            // Mark for removal
            to_remove.push(*id);
        } else {
            // First time seeing this expression
            expression_map.insert(expr_hash, *id);
        }
    }
    
    // Remove duplicate expressions
    for id in to_remove {
        mir.exprs.remove(&id);
    }
}

/// Helper function to check if a number is a power of two
fn is_power_of_two(n: i64) -> bool {
    n > 0 && (n & (n - 1)) == 0
}

/// Helper function to get log2 of a power of two number
fn log2_power_of_two(n: i64) -> u32 {
    n.trailing_zeros()
}

/// Strength reduction optimization
pub fn strength_reduction(mir: &mut Mir) {
    // Look for multiplication by powers of two in SemiringFold operations
    for stmt in &mut mir.stmts {
        if let MirStmt::SemiringFold { op, values, result } = stmt {
            if *op == SemiringOp::Mul && values.len() == 2 {
                // Check if either operand is a constant power of two
                for i in 0..2 {
                    let other_idx = 1 - i;
                    if let Some(MirExpr::Lit(value)) = mir.exprs.get(&values[i]) {
                        if is_power_of_two(*value) {
                            // Found multiplication by power of two
                            // x * 2^n can be replaced with x << n
                            // where n = log2(value)
                            let n = log2_power_of_two(*value);
                            
                            // Get the other operand (the non-constant one)
                            let other_operand = values[other_idx];
                            
                            // Create a shift left expression
                            // We need to create a new expression ID for the shift
                            // For now, we'll reuse the result ID
                            // In a real implementation, we'd need to generate a new ID
                            
                            // Create shift amount expression (constant n)
                            let shift_amount_id = *result + 1000; // Temporary ID
                            mir.exprs.insert(shift_amount_id, MirExpr::Lit(n as i64));
                            
                            // Create shift operation expression
                            // Note: We're using BinaryOp with "<<" as the operator
                            // This assumes the codegen handles "<<" as a shift operator
                            mir.exprs.insert(*result, MirExpr::BinaryOp {
                                op: "<<".to_string(),
                                left: other_operand,
                                right: shift_amount_id,
                            });
                            
                            // Mark the SemiringFold statement for removal
                            // We'll replace it with an assignment that will be cleaned up by DCE
                            *stmt = MirStmt::Assign {
                                lhs: *result,
                                rhs: *result, // Self-assignment, will be removed by DCE
                            };
                            
                            // We found and replaced one optimization, break out of the loop
                            break;
                        }
                    }
                }
            }
        }
    }
}

/// Algebraic simplification
pub fn algebraic_simplification(mir: &mut Mir) {
    // Look for algebraic identities in SemiringFold operations
    for stmt in &mut mir.stmts {
        if let MirStmt::SemiringFold { op, values, result } = stmt {
            match op {
                SemiringOp::Add => {
                    // x + 0 = x, 0 + x = x
                    // Check for addition with zero
                    let mut new_values = Vec::new();
                    let mut has_zero = false;
                    
                    for &val_id in values.iter() {
                        if let Some(MirExpr::Lit(value)) = mir.exprs.get(&val_id) {
                            if *value == 0 {
                                // Skip zero operand
                                has_zero = true;
                                continue;
                            }
                        }
                        new_values.push(val_id);
                    }
                    
                    // If we removed zeros and have at least one operand left
                    if has_zero && !new_values.is_empty() {
                        if new_values.len() == 1 {
                            // Single operand remaining: replace SemiringFold with that operand
                            mir.exprs.insert(*result, mir.exprs[&new_values[0]].clone());
                            // Mark statement for removal (self-assignment that DCE will clean up)
                            *stmt = MirStmt::Assign {
                                lhs: *result,
                                rhs: *result,
                            };
                        } else {
                            // Multiple operands remaining: update values list
                            *values = new_values;
                        }
                    }
                }
                SemiringOp::Mul => {
                    // x * 0 = 0, x * 1 = x, 0 * x = 0, 1 * x = x
                    let mut new_values = Vec::new();
                    let mut has_zero = false;
                    let mut has_one = false;
                    
                    for &val_id in values.iter() {
                        if let Some(MirExpr::Lit(value)) = mir.exprs.get(&val_id) {
                            if *value == 0 {
                                // Multiplication by zero - result is zero
                                has_zero = true;
                                break; // Once we have zero, result is zero
                            } else if *value == 1 {
                                // Multiplication by one - skip this operand
                                has_one = true;
                                continue;
                            }
                        }
                        new_values.push(val_id);
                    }
                    
                    if has_zero {
                        // Result is zero
                        mir.exprs.insert(*result, MirExpr::Lit(0));
                        // Mark statement for removal
                        *stmt = MirStmt::Assign {
                            lhs: *result,
                            rhs: *result,
                        };
                    } else if has_one {
                        if new_values.is_empty() {
                            // All operands were ones, result is 1
                            mir.exprs.insert(*result, MirExpr::Lit(1));
                            *stmt = MirStmt::Assign {
                                lhs: *result,
                                rhs: *result,
                            };
                        } else if new_values.len() == 1 {
                            // Single non-one operand remaining
                            mir.exprs.insert(*result, mir.exprs[&new_values[0]].clone());
                            *stmt = MirStmt::Assign {
                                lhs: *result,
                                rhs: *result,
                            };
                        } else {
                            // Multiple operands remaining, update values list
                            *values = new_values;
                        }
                    }
                }
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
            algebraic_simplification(mir);
            dead_code_elimination(mir); // Clean up after constant folding
        }
        OptLevel::O2 => {
            // Aggressive optimizations (O1 plus more)
            dead_code_elimination(mir);
            constant_folding(mir);
            algebraic_simplification(mir);
            common_subexpression_elimination(mir);
            strength_reduction(mir);
            dead_code_elimination(mir); // Clean up
        }
        OptLevel::O3 => {
            // Maximum optimizations (O2 plus even more)
            // Run multiple iterations for maximum effect
            for _ in 0..3 {
                dead_code_elimination(mir);
                constant_folding(mir);
                algebraic_simplification(mir);
                common_subexpression_elimination(mir);
                strength_reduction(mir);
            }
            dead_code_elimination(mir); // Final cleanup
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
