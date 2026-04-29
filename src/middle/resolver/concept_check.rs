// src/middle/resolver/concept_check.rs
//! Concept satisfaction checking for Stepanov Elements of Programming.
//! Verifies that type arguments satisfy concept bounds on generic functions.

use crate::middle::types::{Substitution, Type};

/// Result of concept checking
#[derive(Debug, Clone)]
pub struct ConceptCheckResult {
    pub passed: bool,
    pub errors: Vec<String>,
}

/// Check if a type satisfies the given concept name
pub fn check_concept_satisfaction(ty: &Type, concept_name: &str) -> bool {
    let subst = Substitution::new();
    let concept = match concept_name {
        "Regular" | "regular" => Type::Regular,
        "TotallyOrdered" | "totally_ordered" | "TotallyOrdered" => Type::TotallyOrdered,
        "Semigroup" | "semigroup" => Type::Semigroup,
        "Monoid" | "monoid" => Type::Monoid,
        "Group" | "group" => Type::Group,
        "Ring" | "ring" => Type::Ring,
        "InputIterator" | "input_iterator" | "InputIterator" => Type::InputIterator,
        "ForwardIterator" | "forward_iterator" | "ForwardIterator" => Type::ForwardIterator,
        "BidirectionalIterator" | "bidirectional_iterator" | "BidirectionalIterator" => {
            Type::BidirectionalIterator
        }
        "RandomAccessIterator" | "random_access_iterator" | "RandomAccessIterator" => {
            Type::RandomAccessIterator
        }
        _ => return true, // Unknown concepts are assumed to be satisfied
    };
    subst.satisfies_concept(ty, &concept)
}

/// Check all concept bounds for a set of type arguments
pub fn check_concept_bounds(
    type_args: &[(String, Type)], // (param_name, param_type) pairs
    where_clauses: &[(String, Vec<String>)], // (param_name, [concept_names])
) -> ConceptCheckResult {
    let mut errors = Vec::new();

    for (param_name, concept_names) in where_clauses {
        // Find the type argument for this parameter
        let param_type = type_args.iter().find(|(name, _)| name == param_name);

        if let Some((_, ty)) = param_type {
            for concept_name in concept_names {
                if !check_concept_satisfaction(ty, concept_name) {
                    errors.push(format!(
                        "Type '{}' does not satisfy concept '{}' (parameter: {})",
                        ty.display_name(),
                        concept_name,
                        param_name
                    ));
                }
            }
        } else {
            // Parameter not found in type_args - skip (might be inferred later)
        }
    }

    ConceptCheckResult {
        passed: errors.is_empty(),
        errors,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::middle::types::Type;

    #[test]
    fn test_concept_satisfaction() {
        // i64 satisfies Regular
        assert!(check_concept_satisfaction(&Type::I64, "Regular"));

        // i64 satisfies TotallyOrdered
        assert!(check_concept_satisfaction(&Type::I64, "TotallyOrdered"));

        // i64 satisfies Semigroup
        assert!(check_concept_satisfaction(&Type::I64, "Semigroup"));

        // bool satisfies Regular
        assert!(check_concept_satisfaction(&Type::Bool, "Regular"));

        // Tuple of regular types satisfies Regular
        let tuple = Type::Tuple(vec![Type::I32, Type::Bool]);
        assert!(check_concept_satisfaction(&tuple, "Regular"));
    }
}
