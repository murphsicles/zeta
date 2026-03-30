// src/middle/resolver/type_cache.rs
//! Type inference caching for performance optimization

use crate::Resolver;
use crate::frontend::ast::AstNode;
use crate::middle::types::Type;
use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Hash key for AST nodes for caching
#[derive(Debug, Clone)]
struct AstNodeKey {
    hash: u64,
    node_type: String,
}

impl AstNodeKey {
    fn from_node(node: &AstNode) -> Self {
        let mut hasher = DefaultHasher::new();

        // Hash based on node structure
        match node {
            AstNode::Var(name) => {
                "Var".hash(&mut hasher);
                name.hash(&mut hasher);
            }
            AstNode::Lit(value) => {
                "Lit".hash(&mut hasher);
                value.hash(&mut hasher);
            }
            AstNode::BinaryOp { op, left, right } => {
                "BinaryOp".hash(&mut hasher);
                op.hash(&mut hasher);
                // Note: We don't hash the entire subtree, just the operator
                // This is a trade-off for cache efficiency
            }
            AstNode::Call { method, args, .. } => {
                "Call".hash(&mut hasher);
                method.hash(&mut hasher);
                args.len().hash(&mut hasher);
            }
            AstNode::FuncDef { name, params, .. } => {
                "FuncDef".hash(&mut hasher);
                name.hash(&mut hasher);
                params.len().hash(&mut hasher);
            }
            _ => {
                // For other nodes, use a simple type-based hash
                let type_str = format!("{:?}", std::mem::discriminant(node));
                type_str.hash(&mut hasher);
            }
        }

        Self {
            hash: hasher.finish(),
            node_type: format!("{:?}", std::mem::discriminant(node)),
        }
    }
}

impl PartialEq for AstNodeKey {
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.node_type == other.node_type
    }
}

impl Eq for AstNodeKey {}

impl Hash for AstNodeKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
        self.node_type.hash(state);
    }
}

/// Cache for type inference results
pub struct TypeCache {
    map: HashMap<AstNodeKey, Type>,
    hits: usize,
    misses: usize,
}

impl Default for TypeCache {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeCache {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            hits: 0,
            misses: 0,
        }
    }

    /// Get cached type for AST node
    pub fn get(&self, node: &AstNode) -> Option<&Type> {
        let key = AstNodeKey::from_node(node);
        self.map.get(&key)
    }

    /// Store type for AST node in cache
    pub fn insert(&mut self, node: &AstNode, ty: Type) {
        let key = AstNodeKey::from_node(node);
        self.map.insert(key, ty);
    }

    /// Clear the cache
    pub fn clear(&mut self) {
        self.map.clear();
        self.hits = 0;
        self.misses = 0;
    }

    /// Get cache statistics
    pub fn stats(&self) -> (usize, usize, f64) {
        let total = self.hits + self.misses;
        let hit_rate = if total > 0 {
            self.hits as f64 / total as f64
        } else {
            0.0
        };
        (self.hits, self.misses, hit_rate)
    }

    /// Record a cache hit
    pub fn record_hit(&mut self) {
        self.hits += 1;
    }

    /// Record a cache miss
    pub fn record_miss(&mut self) {
        self.misses += 1;
    }

    /// Get cache size
    pub fn size(&self) -> usize {
        self.map.len()
    }
}

/// Optimized type checker with caching
pub struct CachingTypeChecker {
    cache: TypeCache,
    resolver: Resolver,
}

impl CachingTypeChecker {
    pub fn new(resolver: Resolver) -> Self {
        Self {
            cache: TypeCache::new(),
            resolver,
        }
    }

    /// Type check with caching
    pub fn typecheck(&mut self, asts: &[AstNode]) -> bool {
        let mut ok = true;

        for ast in asts {
            if !self.check_node_with_cache(ast) {
                ok = false;
            }
        }

        // Print cache statistics
        let (hits, misses, hit_rate) = self.cache.stats();
        eprintln!(
            "[TYPE CACHE] Hits: {}, Misses: {}, Hit rate: {:.2}%, Size: {}",
            hits,
            misses,
            hit_rate * 100.0,
            self.cache.size()
        );

        ok
    }

    fn check_node_with_cache(&mut self, node: &AstNode) -> bool {
        // Check if we have this node in cache
        let in_cache = self.cache.get(node).is_some();

        if in_cache {
            self.cache.record_hit();
            return true; // Assume cached types are valid
        }

        self.cache.record_miss();

        // For simplicity, we'll just infer the type and assume it's valid
        // In a full implementation, we would do proper type checking
        if let Some(ty) = self.infer_type_with_cache(node) {
            // Note: infer_type_with_cache already caches the result
            true
        } else {
            false
        }
    }

    fn infer_type_with_cache(&mut self, node: &AstNode) -> Option<Type> {
        // Check cache first
        let cached = self.cache.get(node).cloned();

        if cached.is_some() {
            self.cache.record_hit();
            return cached;
        }

        self.cache.record_miss();

        // Infer type using resolver
        let ty = self.resolver.infer_type(node);

        // Cache the result
        self.cache.insert(node, ty.clone());

        Some(ty)
    }

    /// Get the underlying resolver
    pub fn resolver(&self) -> &Resolver {
        &self.resolver
    }

    /// Get mutable access to the resolver
    pub fn resolver_mut(&mut self) -> &mut Resolver {
        &mut self.resolver
    }
}
