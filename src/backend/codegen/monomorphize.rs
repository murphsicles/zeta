//! Monomorphization implementation for generic functions
//!
//! This module handles type substitution and instantiation of generic functions.

use crate::middle::mir::mir::{Mir, MirExpr, MirStmt};
use crate::middle::types::{Substitution, Type, TypeVar};
use std::collections::{HashMap, HashSet};

/// Create a substitution from type variables to concrete types
///
/// # Arguments
/// * `type_vars` - List of type variables (e.g., [TypeVar(0), TypeVar(1)])
/// * `type_args` - List of concrete types (e.g., [Type::I32, Type::String])
///
/// # Returns
/// A substitution mapping type variables to concrete types
pub fn create_substitution(type_vars: &[TypeVar], type_args: &[Type]) -> Substitution {
    let mut substitution = Substitution::new();

    for (type_var, concrete_type) in type_vars.iter().zip(type_args.iter()) {
        substitution
            .mapping
            .insert(type_var.clone(), concrete_type.clone());
    }

    substitution
}

/// Apply type substitution to a type (wrapper around Substitution::apply)
///
/// # Arguments
/// * `ty` - The type to substitute
/// * `substitution` - Substitution mapping type variables to concrete types
///
/// # Returns
/// The type with all type variables substituted
pub fn substitute_type(ty: &Type, substitution: &Substitution) -> Type {
    substitution.apply(ty)
}

/// Apply type substitution to a MIR expression
///
/// # Arguments
/// * `expr` - The MIR expression
/// * `substitution` - Substitution mapping type variables to concrete types
///
/// # Returns
/// The expression with types substituted (if applicable)
pub fn substitute_expr(expr: &MirExpr, substitution: &Substitution) -> MirExpr {
    // Most expressions don't contain type information directly
    // Type information is stored separately in the type_map
    expr.clone()
}

/// Apply type substitution to a MIR statement
///
/// # Arguments
/// * `stmt` - The MIR statement
/// * `substitution` - Substitution mapping type variables to concrete types
///
/// # Returns
/// The statement with types substituted
pub fn substitute_stmt(stmt: &MirStmt, substitution: &Substitution) -> MirStmt {
    match stmt {
        MirStmt::Assign { lhs, rhs } => MirStmt::Assign {
            lhs: *lhs,
            rhs: *rhs,
        },
        MirStmt::Call {
            func,
            args,
            dest,
            type_args,
        } => {
            // Substitute type arguments in the call
            let substituted_type_args: Vec<Type> = type_args
                .iter()
                .map(|ty| substitute_type(ty, substitution))
                .collect();

            MirStmt::Call {
                func: func.clone(),
                args: args.clone(),
                dest: *dest,
                type_args: substituted_type_args,
            }
        }
        MirStmt::VoidCall { func, args } => MirStmt::VoidCall {
            func: func.clone(),
            args: args.clone(),
        },
        MirStmt::Return { val } => MirStmt::Return { val: *val },
        // TODO: Handle other MIR statement variants
        _ => todo!("MIR statement variant not yet implemented in substitute_stmt"),
    }
}

/// Apply type substitution to an entire MIR
///
/// # Arguments
/// * `mir` - The MIR to substitute
/// * `substitution` - Substitution mapping type variables to concrete types
///
/// # Returns
/// A new MIR with all types substituted
pub fn substitute_mir(mir: &Mir, substitution: &Substitution) -> Mir {
    let mut new_mir = mir.clone();

    // Substitute types in type_map
    let mut new_type_map = HashMap::new();
    for (id, ty) in &mir.type_map {
        new_type_map.insert(*id, substitute_type(ty, substitution));
    }
    new_mir.type_map = new_type_map;

    // Substitute types in statements
    new_mir.stmts = mir
        .stmts
        .iter()
        .map(|stmt| substitute_stmt(stmt, substitution))
        .collect();

    // Note: We don't substitute expressions directly since they don't contain type info
    // The type information is in type_map which we already substituted

    new_mir
}

/// Extract type variables from a generic function
///
/// This extracts type variables from the MIR's type map.
///
/// # Arguments
/// * `mir` - The MIR of a generic function
///
/// # Returns
/// List of type variables found in the MIR
pub fn extract_type_vars(mir: &Mir) -> Vec<TypeVar> {
    use std::collections::HashSet;
    
    let mut type_vars = HashSet::new();
    
    // Extract type variables from type map
    for ty in mir.type_map.values() {
        extract_type_vars_from_type(ty, &mut type_vars);
    }
    
    // Convert to vector and sort for deterministic output
    let mut result: Vec<TypeVar> = type_vars.into_iter().collect();
    result.sort_by_key(|tv| tv.0);
    result
}

/// Helper function to extract type variables from a type
fn extract_type_vars_from_type(ty: &Type, type_vars: &mut HashSet<TypeVar>) {
    match ty {
        Type::Variable(var) => {
            type_vars.insert(var.clone());
        }
        Type::Array(inner, _) => {
            extract_type_vars_from_type(inner, type_vars);
        }
        Type::Slice(inner) => {
            extract_type_vars_from_type(inner, type_vars);
        }
        Type::Tuple(types) => {
            for t in types {
                extract_type_vars_from_type(t, type_vars);
            }
        }
        Type::Ptr(inner) => {
            extract_type_vars_from_type(inner, type_vars);
        }
        Type::Ref(inner, _, _) => {
            extract_type_vars_from_type(inner, type_vars);
        }
        Type::Named(_, args) => {
            for arg in args {
                extract_type_vars_from_type(arg, type_vars);
            }
        }
        Type::Function(params, ret) => {
            for param in params {
                extract_type_vars_from_type(param, type_vars);
            }
            extract_type_vars_from_type(ret, type_vars);
        }
        _ => {} // Primitive types don't contain type variables
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::{Substitution, Type, TypeVar};
    use crate::middle::mir::mir::{Mir, MirExpr, MirStmt};

    #[test]
    fn test_create_substitution() {
        let type_vars = vec![TypeVar(0), TypeVar(1)];
        let type_args = vec![Type::I32, Type::Named("String".to_string(), vec![])];

        let substitution = create_substitution(&type_vars, &type_args);

        assert_eq!(substitution.mapping.get(&TypeVar(0)), Some(&Type::I32));
        assert_eq!(
            substitution.mapping.get(&TypeVar(1)),
            Some(&Type::Named("String".to_string(), vec![]))
        );
    }

    #[test]
    fn test_substitute_type() {
        let mut substitution = Substitution::new();
        substitution.mapping.insert(TypeVar(0), Type::I32);

        // Test substituting TypeVar(0) with i32
        let var_0 = Type::Variable(TypeVar(0));
        let result = substitute_type(&var_0, &substitution);
        assert_eq!(result, Type::I32);

        // Test that other type variables are not substituted
        let var_1 = Type::Variable(TypeVar(1));
        let result = substitute_type(&var_1, &substitution);
        assert_eq!(result, Type::Variable(TypeVar(1)));
    }

    #[test]
    fn test_substitute_type_recursive() {
        let mut substitution = Substitution::new();
        substitution.mapping.insert(TypeVar(0), Type::I32);

        // Test substituting Option<Vec<T>> with T=i32
        let option_vec_t = Type::Named(
            "Option".to_string(),
            vec![Type::Named(
                "Vec".to_string(),
                vec![Type::Variable(TypeVar(0))],
            )],
        );

        let result = substitute_type(&option_vec_t, &substitution);

        // Expected: Option<Vec<i32>>
        let expected = Type::Named(
            "Option".to_string(),
            vec![Type::Named("Vec".to_string(), vec![Type::I32])],
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_extract_type_vars() {
        // Create a simple MIR with type variables
        let mut mir = Mir {
            name: None,
            param_indices: vec![],
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
            type_map: HashMap::new(),
        };
        
        // Add some types with variables
        mir.type_map.insert(1, Type::Variable(TypeVar(0)));
        mir.type_map.insert(2, Type::I32);
        mir.type_map.insert(3, Type::Named("Vec".to_string(), vec![Type::Variable(TypeVar(1))]));
        mir.type_map.insert(4, Type::Function(
            vec![Type::Variable(TypeVar(0))],
            Box::new(Type::Variable(TypeVar(1)))
        ));
        
        let type_vars = extract_type_vars(&mir);
        
        // Should find TypeVar(0) and TypeVar(1)
        assert_eq!(type_vars.len(), 2);
        assert!(type_vars.contains(&TypeVar(0)));
        assert!(type_vars.contains(&TypeVar(1)));
        
        // Should be sorted
        assert_eq!(type_vars[0], TypeVar(0));
        assert_eq!(type_vars[1], TypeVar(1));
    }
    
    #[test]
    fn test_extract_type_vars_no_vars() {
        // MIR with no type variables
        let mut mir = Mir {
            name: None,
            param_indices: vec![],
            stmts: vec![],
            exprs: HashMap::new(),
            ctfe_consts: HashMap::new(),
            type_map: HashMap::new(),
        };
        
        mir.type_map.insert(1, Type::I32);
        mir.type_map.insert(2, Type::Bool);
        mir.type_map.insert(3, Type::Named("String".to_string(), vec![]));
        
        let type_vars = extract_type_vars(&mir);
        assert!(type_vars.is_empty());
    }
}
