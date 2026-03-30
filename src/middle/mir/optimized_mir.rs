// src/middle/mir/optimized_mir.rs
//! Optimized MIR data structures for better performance

use super::mir::Mir;
use crate::middle::types::Type;
use std::collections::HashMap;

/// Optimized MIR structure using Vec instead of HashMap for expression storage
#[derive(Debug, Clone, Default)]
pub struct OptimizedMir {
    pub name: Option<String>,
    pub param_indices: Vec<(String, u32)>,
    pub stmts: Vec<MirStmt>,

    // Optimized: Use Vec with Option for sparse but sequential ID access
    // IDs are sequential u32 starting from 1, so Vec index = ID
    pub exprs: Vec<Option<MirExpr>>,

    // Optimized: Same pattern for constants
    pub ctfe_consts: Vec<Option<i64>>,

    // Optimized: Same pattern for types
    pub type_map: Vec<Option<Type>>,
}

impl OptimizedMir {
    /// Create a new OptimizedMir with pre-allocated capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            name: None,
            param_indices: Vec::new(),
            stmts: Vec::new(),
            exprs: vec![None; capacity], // Pre-allocate with None
            ctfe_consts: vec![None; capacity],
            type_map: vec![None; capacity],
        }
    }

    /// Convert from regular Mir to OptimizedMir
    pub fn from_mir(mir: Mir) -> Self {
        let max_id = mir
            .exprs
            .keys()
            .chain(mir.ctfe_consts.keys())
            .chain(mir.type_map.keys())
            .copied()
            .max()
            .unwrap_or(0) as usize;

        let capacity = max_id + 1; // +1 because IDs start from 1

        let mut result = Self::new(capacity);
        result.name = mir.name;
        result.param_indices = mir.param_indices;
        result.stmts = mir.stmts;

        // Fill expressions
        for (id, expr) in mir.exprs {
            if (id as usize) < capacity {
                result.exprs[id as usize] = Some(expr);
            }
        }

        // Fill constants
        for (id, constant) in mir.ctfe_consts {
            if (id as usize) < capacity {
                result.ctfe_consts[id as usize] = Some(constant);
            }
        }

        // Fill types
        for (id, ty) in mir.type_map {
            if (id as usize) < capacity {
                result.type_map[id as usize] = Some(ty);
            }
        }

        result
    }

    /// Convert back to regular Mir
    pub fn to_mir(self) -> Mir {
        let mut exprs = HashMap::new();
        let mut ctfe_consts = HashMap::new();
        let mut type_map = HashMap::new();

        for (id, expr) in self.exprs.into_iter().enumerate() {
            if let Some(expr) = expr {
                exprs.insert(id as u32, expr);
            }
        }

        for (id, constant) in self.ctfe_consts.into_iter().enumerate() {
            if let Some(constant) = constant {
                ctfe_consts.insert(id as u32, constant);
            }
        }

        for (id, ty) in self.type_map.into_iter().enumerate() {
            if let Some(ty) = ty {
                type_map.insert(id as u32, ty);
            }
        }

        Mir {
            name: self.name,
            param_indices: self.param_indices,
            stmts: self.stmts,
            exprs,
            ctfe_consts,
            type_map,
        }
    }

    /// Get expression by ID (O(1) array access)
    pub fn get_expr(&self, id: u32) -> Option<&MirExpr> {
        self.exprs.get(id as usize).and_then(|opt| opt.as_ref())
    }

    /// Get constant by ID (O(1) array access)
    pub fn get_constant(&self, id: u32) -> Option<i64> {
        self.ctfe_consts.get(id as usize).and_then(|opt| *opt)
    }

    /// Get type by ID (O(1) array access)
    pub fn get_type(&self, id: u32) -> Option<&Type> {
        self.type_map.get(id as usize).and_then(|opt| opt.as_ref())
    }

    /// Set expression by ID
    pub fn set_expr(&mut self, id: u32, expr: MirExpr) {
        if id as usize >= self.exprs.len() {
            self.exprs.resize(id as usize + 1, None);
            self.ctfe_consts.resize(id as usize + 1, None);
            self.type_map.resize(id as usize + 1, None);
        }
        self.exprs[id as usize] = Some(expr);
    }

    /// Set constant by ID
    pub fn set_constant(&mut self, id: u32, constant: i64) {
        if id as usize >= self.ctfe_consts.len() {
            self.exprs.resize(id as usize + 1, None);
            self.ctfe_consts.resize(id as usize + 1, None);
            self.type_map.resize(id as usize + 1, None);
        }
        self.ctfe_consts[id as usize] = Some(constant);
    }

    /// Set type by ID
    pub fn set_type(&mut self, id: u32, ty: Type) {
        if id as usize >= self.type_map.len() {
            self.exprs.resize(id as usize + 1, None);
            self.ctfe_consts.resize(id as usize + 1, None);
            self.type_map.resize(id as usize + 1, None);
        }
        self.type_map[id as usize] = Some(ty);
    }
}

// Re-export the Mir types for compatibility
pub use super::mir::{MirExpr, MirStmt, SemiringOp};
