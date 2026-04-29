//! Unified Type Checking Interface
//!
//! Provides a unified facade for type checking that can route to
//! either the old or new type system implementations.
//!
//! This is the first step in resolving the architecture conflict
//! between old and new type systems.

use crate::frontend::ast::AstNode;
use crate::middle::types::{Substitution, Type, UnifyError};

use super::resolver::Resolver;

/// Unified type checking result
#[derive(Debug)]
pub enum TypeCheckResult {
    /// Type checking succeeded with substitution
    Success(Substitution),
    /// Type checking failed with errors
    Failure(Vec<UnifyError>),
    /// Fallback to old system needed
    Fallback,
}

/// Unified type checking trait
pub trait UnifiedTypeCheck {
    /// Perform type checking on AST nodes
    fn typecheck_unified(&mut self, asts: &[AstNode]) -> TypeCheckResult;

    /// Convert string type to algebraic Type
    fn parse_type_string(&self, s: &str) -> Result<Type, String>;

    /// Convert algebraic Type to string representation
    fn type_to_string(&self, ty: &Type) -> String;
}

/// Type checking strategy
#[derive(Debug, Clone, Copy)]
pub enum TypeCheckStrategy {
    /// Use new type system (algebraic types, unification)
    NewSystem,
    /// Use old type system (string-based, simple)
    OldSystem,
    /// Auto-select based on capabilities
    Auto,
}

/// Unified type checker that routes to appropriate implementation.
///
/// This standalone checker creates an internal Resolver to perform
/// type checking, and routes between new and old systems as needed.
pub struct UnifiedTypeChecker {
    strategy: TypeCheckStrategy,
    resolver: Resolver,
}

impl UnifiedTypeChecker {
    /// Create a new unified type checker with specified strategy
    pub fn new(strategy: TypeCheckStrategy) -> Self {
        Self {
            strategy,
            resolver: Resolver::new(),
        }
    }

    /// Create with auto-selection strategy (default)
    pub fn auto() -> Self {
        Self::new(TypeCheckStrategy::Auto)
    }

    /// Get a mutable reference to the internal resolver
    pub fn resolver(&mut self) -> &mut Resolver {
        &mut self.resolver
    }

    /// Register top-level AST definitions before type checking
    pub fn register(&mut self, ast: AstNode) {
        self.resolver.register(ast);
    }
}

impl UnifiedTypeCheck for UnifiedTypeChecker {
    fn typecheck_unified(&mut self, asts: &[AstNode]) -> TypeCheckResult {
        match self.strategy {
            TypeCheckStrategy::NewSystem => {
                // Try new system first using the Resolver's typecheck_new
                use super::typecheck_new::NewTypeCheck;

                match self.resolver.typecheck_new(asts) {
                    Ok(sub) => TypeCheckResult::Success(sub),
                    Err(errors) => TypeCheckResult::Failure(errors),
                }
            }
            TypeCheckStrategy::OldSystem => {
                // Use old system - the Resolver's typecheck integrates both
                // but here we skip the new system entirely
                let mut ok = true;
                for ast in asts {
                    if !self.resolver.check_node(ast) {
                        ok = false;
                    }
                }
                if ok {
                    TypeCheckResult::Success(Substitution::new())
                } else {
                    TypeCheckResult::Failure(vec![])
                }
            }
            TypeCheckStrategy::Auto => {
                // Auto-select: try new, fallback to old
                // This mimics current behavior in typecheck.rs
                use super::typecheck_new::NewTypeCheck;

                match self.resolver.typecheck_new(asts) {
                    Ok(sub) => TypeCheckResult::Success(sub),
                    Err(errors) => {
                        let has_type_mismatch = errors
                            .iter()
                            .any(|e| matches!(e, UnifyError::Mismatch(_, _)));

                        if has_type_mismatch {
                            TypeCheckResult::Failure(errors)
                        } else {
                            // Fallback to old system
                            let mut ok = true;
                            for ast in asts {
                                if !self.resolver.check_node(ast) {
                                    ok = false;
                                }
                            }
                            if ok {
                                TypeCheckResult::Success(Substitution::new())
                            } else {
                                TypeCheckResult::Failure(errors)
                            }
                        }
                    }
                }
            }
        }
    }

    fn parse_type_string(&self, s: &str) -> Result<Type, String> {
        // Use Type's built-in parser
        Ok(Type::from_string(s))
    }

    fn type_to_string(&self, ty: &Type) -> String {
        match ty {
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Usize => "usize".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Char => "char".to_string(),
            Type::Str => "str".to_string(),
            Type::Range => "Range".to_string(),
            Type::Error => "!error".to_string(),
            Type::V4I64 => "v4i64".to_string(),
            Type::I32x4 => "i32x4".to_string(),
            Type::I64x2 => "i64x2".to_string(),
            Type::F32x4 => "f32x4".to_string(),
            Type::Tuple(types) => {
                let inner: Vec<String> = types.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})", inner.join(", "))
            }
            Type::Array(inner, size) => {
                format!("[{}; {}]", self.type_to_string(inner), size)
            }
            Type::Slice(inner) => {
                format!("[{}]", self.type_to_string(inner))
            }
            Type::DynamicArray(inner) => {
                format!("[dynamic]{}", self.type_to_string(inner))
            }
            Type::Ref(inner, _, mutability) => {
                let prefix = match mutability {
                    crate::middle::types::Mutability::Mutable => "&mut ",
                    crate::middle::types::Mutability::Immutable => "&",
                };
                format!("{}{}", prefix, self.type_to_string(inner))
            }
            Type::Ptr(inner, mutability) => {
                let prefix = match mutability {
                    crate::middle::types::Mutability::Mutable => "*mut ",
                    crate::middle::types::Mutability::Immutable => "*const ",
                };
                format!("{}{}", prefix, self.type_to_string(inner))
            }
            Type::Named(name, args) => {
                if args.is_empty() {
                    name.clone()
                } else {
                    let inner: Vec<String> = args.iter().map(|t| self.type_to_string(t)).collect();
                    format!("{}<{}>", name, inner.join(", "))
                }
            }
            Type::Function(params, ret) => {
                let param_strs: Vec<String> =
                    params.iter().map(|t| self.type_to_string(t)).collect();
                format!(
                    "({}) -> {}",
                    param_strs.join(", "),
                    self.type_to_string(ret)
                )
            }
            Type::Variable(var) => format!("_{}", var.0),
            Type::AsyncFunction(params, ret) => {
                let param_strs: Vec<String> =
                    params.iter().map(|t| self.type_to_string(t)).collect();
                format!(
                    "async ({}) -> {}",
                    param_strs.join(", "),
                    self.type_to_string(ret)
                )
            }
            Type::TraitObject(name) => format!("dyn {}", name),
            Type::Vector(inner, size) => {
                format!("Vector<{}, {}>", self.type_to_string(inner), size)
            }
            Type::Identity(id) => {
                let caps: Vec<String> = id.capabilities.iter().map(|c| c.to_string()).collect();
                let caps_str = if caps.is_empty() {
                    String::new()
                } else {
                    format!("[{}]", caps.join(", "))
                };
                if let Some(ref val) = id.value {
                    format!("identity(\"{}\"){}", val, caps_str)
                } else {
                    format!("identity{}", caps_str)
                }
            }
            Type::Constructor(name, args, kind) => {
                let arg_strs: Vec<String> = args.iter().map(|t| self.type_to_string(t)).collect();
                format!("{}<{}>::{:?}", name, arg_strs.join(", "), kind)
            }
            Type::PartialApplication(ctor, args) => {
                let arg_strs: Vec<String> = args.iter().map(|t| self.type_to_string(t)).collect();
                format!("({})<{}>", self.type_to_string(ctor), arg_strs.join(", "))
            }
        }
    }
}

/// Integration with existing Resolver
impl UnifiedTypeCheck for super::resolver::Resolver {
    fn typecheck_unified(&mut self, asts: &[AstNode]) -> TypeCheckResult {
        // Use the existing hybrid approach from typecheck.rs
        use super::typecheck_new::NewTypeCheck;

        match self.typecheck_new(asts) {
            Ok(sub) => TypeCheckResult::Success(sub),
            Err(errors) => {
                // Check if we should fallback
                let has_type_mismatch = errors
                    .iter()
                    .any(|e| matches!(e, UnifyError::Mismatch(_, _)));

                if has_type_mismatch {
                    TypeCheckResult::Failure(errors)
                } else {
                    TypeCheckResult::Fallback
                }
            }
        }
    }

    fn parse_type_string(&self, s: &str) -> Result<Type, String> {
        use super::typecheck_new::NewTypeCheck;
        Ok(self.string_to_type(s))
    }

    fn type_to_string(&self, ty: &Type) -> String {
        use super::typecheck_new::NewTypeCheck;
        NewTypeCheck::type_to_string(self, ty)
    }
}
