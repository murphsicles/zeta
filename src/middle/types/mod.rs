//! # Type System Foundation
//!
//! Algebraic type representation and unification for Zeta.
//! Replaces string-based types with proper type algebra.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU32, Ordering};

/// Type variable for inference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeVar(u32);

impl TypeVar {
    /// Generate a fresh type variable
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        TypeVar(COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

/// Mutability qualifier for references
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Mutability {
    Immutable,
    Mutable,
}

/// Algebraic type representation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    // Primitive numeric types
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,

    // Other primitives
    Bool,
    Char,
    Str,

    // Compound types
    Array(Box<Type>, usize),    // [T; N]
    Slice(Box<Type>),           // [T]
    Tuple(Vec<Type>),           // (T1, T2, ...)
    Ptr(Box<Type>),             // *T
    Ref(Box<Type>, Mutability), // &T, &mut T

    // Named types (structs, enums, concepts)
    Named(String, Vec<Type>), // Name<T1, T2, ...>

    // Function types
    Function(Vec<Type>, Box<Type>), // (T1, T2, ...) -> R

    // Type variables (for inference)
    Variable(TypeVar),

    // Error type (when inference fails)
    Error,
}

impl Type {
    /// Check if type contains any type variables
    pub fn contains_vars(&self) -> bool {
        match self {
            Type::Variable(_) => true,
            Type::Array(inner, _) => inner.contains_vars(),
            Type::Slice(inner) => inner.contains_vars(),
            Type::Tuple(types) => types.iter().any(|t| t.contains_vars()),
            Type::Ptr(inner) => inner.contains_vars(),
            Type::Ref(inner, _) => inner.contains_vars(),
            Type::Named(_, args) => args.iter().any(|t| t.contains_vars()),
            Type::Function(params, ret) => {
                params.iter().any(|t| t.contains_vars()) || ret.contains_vars()
            }
            _ => false,
        }
    }

    /// Get display name for type
    pub fn display_name(&self) -> String {
        match self {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "str".to_string(),
            Type::Array(inner, size) => format!("[{}; {}]", inner.display_name(), size),
            Type::Slice(inner) => format!("[{}]", inner.display_name()),
            Type::Tuple(types) => {
                let inner = types
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({})", inner)
            }
            Type::Ptr(inner) => format!("*{}", inner.display_name()),
            Type::Ref(inner, Mutability::Immutable) => format!("&{}", inner.display_name()),
            Type::Ref(inner, Mutability::Mutable) => format!("&mut {}", inner.display_name()),
            Type::Named(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let args_str = args
                        .iter()
                        .map(|t| t.display_name())
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{}<{}>", name, args_str)
                }
            }
            Type::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|t| t.display_name())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({}) -> {}", params_str, ret.display_name())
            }
            Type::Variable(var) => format!("T{}", var.0),
            Type::Error => "<?>".to_string(),
        }
    }
}

/// Substitution mapping type variables to types
#[derive(Debug, Clone, Default)]
pub struct Substitution {
    mapping: HashMap<TypeVar, Type>,
}

impl Substitution {
    /// Create empty substitution
    pub fn new() -> Self {
        Substitution {
            mapping: HashMap::new(),
        }
    }

    /// Apply substitution to a type
    pub fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Variable(var) => self
                .mapping
                .get(var)
                .cloned()
                .unwrap_or(Type::Variable(var.clone())),
            Type::Array(inner, size) => Type::Array(Box::new(self.apply(inner)), *size),
            Type::Slice(inner) => Type::Slice(Box::new(self.apply(inner))),
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.apply(t)).collect()),
            Type::Ptr(inner) => Type::Ptr(Box::new(self.apply(inner))),
            Type::Ref(inner, mutability) => Type::Ref(Box::new(self.apply(inner)), *mutability),
            Type::Named(name, args) => {
                Type::Named(name.clone(), args.iter().map(|t| self.apply(t)).collect())
            }
            Type::Function(params, ret) => Type::Function(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            _ => ty.clone(),
        }
    }

    /// Check if a type variable occurs in a type
    fn occurs_check(&self, var: &TypeVar, ty: &Type) -> bool {
        let ty = self.apply(ty);
        match &ty {
            Type::Variable(v) => v == var,
            Type::Array(inner, _) => self.occurs_check(var, inner),
            Type::Slice(inner) => self.occurs_check(var, inner),
            Type::Tuple(types) => types.iter().any(|t| self.occurs_check(var, t)),
            Type::Ptr(inner) => self.occurs_check(var, inner),
            Type::Ref(inner, _) => self.occurs_check(var, inner),
            Type::Named(_, args) => args.iter().any(|t| self.occurs_check(var, t)),
            Type::Function(params, ret) => {
                params.iter().any(|p| self.occurs_check(var, p)) || self.occurs_check(var, ret)
            }
            _ => false,
        }
    }
}

/// Unification errors
#[derive(Debug, Clone)]
pub enum UnifyError {
    Mismatch(Type, Type),
    OccursCheck(TypeVar, Type),
    ArityMismatch(usize, usize),
}

impl std::fmt::Display for UnifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnifyError::Mismatch(t1, t2) => write!(
                f,
                "Type mismatch: {} vs {}",
                t1.display_name(),
                t2.display_name()
            ),
            UnifyError::OccursCheck(var, ty) => write!(
                f,
                "Occurs check failed: T{} occurs in {}",
                var.0,
                ty.display_name()
            ),
            UnifyError::ArityMismatch(expected, actual) => write!(
                f,
                "Arity mismatch: expected {} arguments, got {}",
                expected, actual
            ),
        }
    }
}

impl Substitution {
    /// Unify two types, updating substitution
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), UnifyError> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);

        match (&t1, &t2) {
            // Same type
            (Type::I64, Type::I64) => Ok(()),
            (Type::I32, Type::I32) => Ok(()),
            (Type::Bool, Type::Bool) => Ok(()),
            (Type::Str, Type::Str) => Ok(()),
            (Type::F32, Type::F32) => Ok(()),
            (Type::F64, Type::F64) => Ok(()),

            // Type variable cases
            (Type::Variable(a), Type::Variable(b)) if a == b => Ok(()),
            (Type::Variable(a), _) => {
                if self.occurs_check(a, &t2) {
                    Err(UnifyError::OccursCheck(a.clone(), t2))
                } else {
                    self.mapping.insert(a.clone(), t2);
                    Ok(())
                }
            }
            (_, Type::Variable(_b)) => self.unify(&t2, &t1),

            // Array types
            (Type::Array(inner1, size1), Type::Array(inner2, size2)) => {
                if size1 != size2 {
                    return Err(UnifyError::Mismatch(t1, t2));
                }
                self.unify(inner1, inner2)
            }

            // Tuple types
            (Type::Tuple(types1), Type::Tuple(types2)) => {
                if types1.len() != types2.len() {
                    return Err(UnifyError::ArityMismatch(types1.len(), types2.len()));
                }
                for (t1, t2) in types1.iter().zip(types2) {
                    self.unify(t1, t2)?;
                }
                Ok(())
            }

            // Function types
            (Type::Function(params1, ret1), Type::Function(params2, ret2)) => {
                if params1.len() != params2.len() {
                    return Err(UnifyError::ArityMismatch(params1.len(), params2.len()));
                }
                for (p1, p2) in params1.iter().zip(params2) {
                    self.unify(p1, p2)?;
                }
                self.unify(ret1, ret2)
            }

            // Mismatch
            _ => Err(UnifyError::Mismatch(t1, t2)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_display() {
        assert_eq!(Type::I64.display_name(), "i64");
        assert_eq!(Type::Bool.display_name(), "bool");
        assert_eq!(Type::Str.display_name(), "str");

        let array = Type::Array(Box::new(Type::I32), 10);
        assert_eq!(array.display_name(), "[i32; 10]");

        let tuple = Type::Tuple(vec![Type::I32, Type::Bool]);
        assert_eq!(tuple.display_name(), "(i32, bool)");

        let func = Type::Function(vec![Type::I32, Type::I32], Box::new(Type::I32));
        assert_eq!(func.display_name(), "(i32, i32) -> i32");
    }

    #[test]
    fn test_unify_primitives() {
        let mut subst = Substitution::new();
        assert!(subst.unify(&Type::I64, &Type::I64).is_ok());
        assert!(subst.unify(&Type::I64, &Type::Bool).is_err());
    }

    #[test]
    fn test_unify_variables() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());
        let b = Type::Variable(TypeVar::fresh());

        // a = i64
        assert!(subst.unify(&a, &Type::I64).is_ok());
        assert_eq!(subst.apply(&a), Type::I64);

        // b = a (which is now i64)
        assert!(subst.unify(&b, &a).is_ok());
        assert_eq!(subst.apply(&b), Type::I64);
    }

    #[test]
    fn test_occurs_check() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());
        let list = Type::Named("List".to_string(), vec![a.clone()]);

        // a = List<a> should fail occurs check
        assert!(subst.unify(&a, &list).is_err());
    }

    #[test]
    fn test_unify_functions() {
        let mut subst = Substitution::new();
        let a = Type::Variable(TypeVar::fresh());

        // (i32) -> T  unified with  (i32) -> i64
        let func1 = Type::Function(vec![Type::I32], Box::new(a.clone()));
        let func2 = Type::Function(vec![Type::I32], Box::new(Type::I64));

        assert!(subst.unify(&func1, &func2).is_ok());
        assert_eq!(subst.apply(&a), Type::I64);
    }
}
