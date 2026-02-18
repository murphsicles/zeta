// src/backend/codegen/codegen.rs
//! Complete LLVM code generator for Zeta.
//!
//! This file contains the entire codegen pipeline in one module for maximum simplicity
//! and performance. It translates MIR directly into LLVM IR, handles parameter passing,
//! monomorphized calls, and all runtime intrinsics.
//!
//! This is the heart of Zeta's execution engine and will be the foundation for
//! the self-hosted compiler.

use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{IntType, PointerType, VectorType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, PointerValue,
};
use std::collections::HashMap;

use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use inkwell::IntPredicate;

/// The complete LLVM code generator for Zeta.
pub struct LLVMCodegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub i64_type: IntType<'ctx>,
    pub vec4_i64_type: VectorType<'ctx>,
    pub ptr_type: PointerType<'ctx>,
    pub locals: HashMap<u32, PointerValue<'ctx>>,
    pub fns: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Creates a new code generator and declares all runtime intrinsics.
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let vec4_i64_type = i64_type.vec_type(4);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let void_type = context.void_type();

        // Declare all host/runtime functions (these are provided by the Zeta runtime)
        module.add_function(
            "datetime_now",
            i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "free",
            void_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "channel_send",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "channel_recv",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "spawn",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "http_get",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "tls_handshake",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );

        module.add_function(
            "result_make_ok",
            ptr_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "result_make_err",
            ptr_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "result_is_ok",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "result_get_data",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "result_free",
            void_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );

        module.add_function(
            "map_new",
            ptr_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_insert",
            void_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_get",
            i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_free",
            void_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );

        module.add_function(
            "host_str_concat",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_to_lowercase",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_to_uppercase",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_trim",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_len",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_starts_with",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_ends_with",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_contains",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_replace",
            i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        module.add_function(
            "scheduler::init_runtime",
            void_type.fn_type(&[], false),
            Some(Linkage::External),
        );

        Self {
            context,
            module,
            builder,
            i64_type,
            vec4_i64_type,
            ptr_type,
            locals: HashMap::new(),
            fns: HashMap::new(),
        }
    }
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Generates LLVM IR for a list of MIR functions.
    /// First pre-declares all functions (to support forward references and recursion),
    /// then emits the body of each.
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        // Pre-declare
        for mir in mirs {
            let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
            let param_types: Vec<_> = (0..mir.param_indices.len())
                .map(|_| self.i64_type.into())
                .collect();
            let fn_type = self.i64_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(&fn_name, fn_type, None);
            self.fns.insert(fn_name.clone(), fn_val);
        }

        for mir in mirs {
            self.gen_fn(mir);
        }
    }

    fn gen_fn(&mut self, mir: &Mir) {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let fn_val = self.get_function(&fn_name);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.locals.clear();

        let all_ids = self.collect_all_local_ids(mir);
        for &id in &all_ids {
            let alloca = self
                .builder
                .build_alloca(self.i64_type, &format!("local_{}", id))
                .unwrap();
            self.locals.insert(id, alloca);
        }

        // Initialize parameters from LLVM arguments
        for (i, _) in mir.param_indices.iter().enumerate() {
            if let Some(param_val) = fn_val.get_nth_param(i as u32) {
                if let Some(&alloca) = self.locals.get(&(i as u32 + 1)) {
                    self.builder.build_store(alloca, param_val).unwrap();
                }
            }
        }

        for stmt in &mir.stmts {
            self.gen_stmt(stmt, &mir.exprs);
        }

        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder
                .build_return(Some(&self.i64_type.const_zero()))
                .unwrap();
        }
    }

    fn collect_all_local_ids(&self, mir: &Mir) -> std::collections::HashSet<u32> {
        let mut ids = std::collections::HashSet::new();
        for (_, id) in &mir.param_indices {
            ids.insert(*id);
        }
        for &id in mir.exprs.keys() {
            ids.insert(id);
        }
        for stmt in &mir.stmts {
            self.collect_ids_from_stmt_safe(stmt, &mut ids, &mir.exprs);
        }
        ids
    }

    fn collect_ids_from_stmt_safe(
        &self,
        stmt: &MirStmt,
        ids: &mut std::collections::HashSet<u32>,
        exprs: &HashMap<u32, MirExpr>,
    ) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                ids.insert(*lhs);
                if let Some(e) = exprs.get(rhs) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::Call { args, dest, .. } => {
                for &arg_id in args {
                    if let Some(e) = exprs.get(&arg_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
                ids.insert(*dest);
            }
            MirStmt::VoidCall { args, .. } => {
                for &arg_id in args {
                    if let Some(e) = exprs.get(&arg_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirStmt::Return { val } => {
                if let Some(e) = exprs.get(val) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::SemiringFold { values, result, .. } => {
                for &val_id in values {
                    if let Some(e) = exprs.get(&val_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
                ids.insert(*result);
            }
            MirStmt::TryProp {
                expr_id,
                ok_dest,
                err_dest,
            } => {
                if let Some(e) = exprs.get(expr_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                ids.insert(*ok_dest);
                ids.insert(*err_dest);
            }
            MirStmt::MapNew { dest } => {
                ids.insert(*dest);
            }
            MirStmt::DictInsert {
                map_id,
                key_id,
                val_id,
            } => {
                ids.insert(*map_id);
                if let Some(e) = exprs.get(key_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                if let Some(e) = exprs.get(val_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::DictGet {
                map_id,
                key_id,
                dest,
            } => {
                ids.insert(*map_id);
                if let Some(e) = exprs.get(key_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                ids.insert(*dest);
            }
            MirStmt::If { cond, then, else_ } => {
                if let Some(e) = exprs.get(cond) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                for s in then {
                    self.collect_ids_from_stmt_safe(s, ids, exprs);
                }
                for s in else_ {
                    self.collect_ids_from_stmt_safe(s, ids, exprs);
                }
            }
            MirStmt::ParamInit { param_id, .. } => {
                ids.insert(*param_id);
            }
            _ => {}
        }
    }

    fn collect_ids_from_expr_safe(
        &self,
        expr: &MirExpr,
        ids: &mut std::collections::HashSet<u32>,
        exprs: &HashMap<u32, MirExpr>,
    ) {
        match expr {
            MirExpr::Var(id) => {
                ids.insert(*id);
            }
            MirExpr::FString(inner_ids) => {
                for &id in inner_ids {
                    if let Some(e) = exprs.get(&id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirExpr::TimingOwned(inner_id) => {
                ids.insert(*inner_id);
            }
            _ => {}
        }
    }

    fn get_function(&self, name: &str) -> FunctionValue<'ctx> {
        if let Some(&f) = self.fns.get(name) {
            return f;
        }
        let mangled = if name.contains('_') {
            name.to_string()
        } else {
            format!("{}_i64", name)
        };
        if let Some(&f) = self.fns.get(&mangled) {
            return f;
        }
        let base = name.split('_').next().unwrap_or(name);
        if let Some(&f) = self.fns.get(base) {
            return f;
        }
        if name == "add" {
            if let Some(&f) = self.fns.get("add_i64") {
                return f;
            }
        }
        if name == "add_i64" {
            if let Some(&f) = self.fns.get("add") {
                return f;
            }
        }
        panic!("CRITICAL: Missing function '{}'", name);
    }

    fn gen_stmt(&mut self, stmt: &MirStmt, exprs: &HashMap<u32, MirExpr>) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr_safe(rhs, exprs);
                let alloca = *self.locals.get(lhs).unwrap();
                self.builder.build_store(alloca, val).unwrap();
            }
            MirStmt::Call {
                func, args, dest, ..
            } => {
                let callee = self.get_function(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.gen_expr_safe(&id, exprs).into())
                    .collect();
                let call = self
                    .builder
                    .build_call(callee, &arg_vals, &format!("call_{dest}"))
                    .unwrap();
                if let Some(val) = Self::call_site_to_basic_value(call) {
                    let alloca = *self.locals.get(dest).unwrap();
                    self.builder.build_store(alloca, val).unwrap();
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee = self.get_function(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.gen_expr_safe(&id, exprs).into())
                    .collect();
                let _ = self
                    .builder
                    .build_call(callee, &arg_vals, "void_call")
                    .unwrap();
            }
            MirStmt::Return { val } => {
                let ret_val = self.gen_expr_safe(val, exprs);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                if values.is_empty() {
                    let alloca = *self.locals.get(result).unwrap();
                    self.builder
                        .build_store(alloca, self.i64_type.const_zero())
                        .unwrap();
                    return;
                }
                let mut acc = self.gen_expr_safe(&values[0], exprs);
                for &val_id in &values[1..] {
                    let val = self.gen_expr_safe(&val_id, exprs);
                    acc = match op {
                        SemiringOp::Add => self
                            .builder
                            .build_int_add(acc.into_int_value(), val.into_int_value(), "fold_add")
                            .unwrap()
                            .into(),
                        SemiringOp::Mul => self
                            .builder
                            .build_int_mul(acc.into_int_value(), val.into_int_value(), "fold_mul")
                            .unwrap()
                            .into(),
                    };
                }
                let alloca = *self.locals.get(result).unwrap();
                self.builder.build_store(alloca, acc).unwrap();
            }
            MirStmt::TryProp {
                expr_id,
                ok_dest,
                err_dest,
            } => {
                let expr_val = self.gen_expr_safe(expr_id, exprs);
                let is_ok = self
                    .builder
                    .build_call(
                        self.get_function("result_is_ok"),
                        &[expr_val.into()],
                        "is_ok",
                    )
                    .unwrap();
                let is_ok_val = Self::call_site_to_basic_value(is_ok)
                    .unwrap()
                    .into_int_value();
                let is_ok_i1 = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        is_ok_val,
                        self.i64_type.const_zero(),
                        "is_ok_i1",
                    )
                    .unwrap();

                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let ok_bb = self.context.append_basic_block(parent_fn, "prop_ok");
                let err_bb = self.context.append_basic_block(parent_fn, "prop_err");
                let cont_bb = self.context.append_basic_block(parent_fn, "prop_cont");

                self.builder
                    .build_conditional_branch(is_ok_i1, ok_bb, err_bb)
                    .unwrap();

                self.builder.position_at_end(ok_bb);
                let data = self
                    .builder
                    .build_call(
                        self.get_function("result_get_data"),
                        &[expr_val.into()],
                        "get_data",
                    )
                    .unwrap();
                let data_val = Self::call_site_to_basic_value(data).unwrap();
                let ok_alloca = *self.locals.get(ok_dest).unwrap();
                self.builder.build_store(ok_alloca, data_val).unwrap();
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(err_bb);
                let err_alloca = *self.locals.get(err_dest).unwrap();
                self.builder.build_store(err_alloca, expr_val).unwrap();
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(cont_bb);
            }
            MirStmt::MapNew { dest } => {
                let call = self
                    .builder
                    .build_call(self.get_function("map_new"), &[], "map_new")
                    .unwrap();
                let ptr = Self::call_site_to_basic_value(call).unwrap();
                let ptr_i64 = self
                    .builder
                    .build_ptr_to_int(ptr.into_pointer_value(), self.i64_type, "map_ptr_i64")
                    .unwrap();
                let alloca = *self.locals.get(dest).unwrap();
                self.builder.build_store(alloca, ptr_i64).unwrap();
            }
            MirStmt::DictInsert {
                map_id,
                key_id,
                val_id,
            } => {
                let map_i64 = self.load_local(*map_id);
                let map_ptr = self
                    .builder
                    .build_int_to_ptr(map_i64.into_int_value(), self.ptr_type, "map_ptr")
                    .unwrap();
                let key_val = self.gen_expr_safe(key_id, exprs);
                let val_val = self.gen_expr_safe(val_id, exprs);
                let _ = self.builder.build_call(
                    self.get_function("map_insert"),
                    &[map_ptr.into(), key_val.into(), val_val.into()],
                    "dict_insert",
                );
            }
            MirStmt::DictGet {
                map_id,
                key_id,
                dest,
            } => {
                let map_i64 = self.load_local(*map_id);
                let map_ptr = self
                    .builder
                    .build_int_to_ptr(map_i64.into_int_value(), self.ptr_type, "map_ptr")
                    .unwrap();
                let key_val = self.gen_expr_safe(key_id, exprs);
                let call = self
                    .builder
                    .build_call(
                        self.get_function("map_get"),
                        &[map_ptr.into(), key_val.into()],
                        "dict_get",
                    )
                    .unwrap();
                let val = Self::call_site_to_basic_value(call).unwrap();
                let alloca = *self.locals.get(dest).unwrap();
                self.builder.build_store(alloca, val).unwrap();
            }
            MirStmt::If { cond, then, else_ } => {
                let cond_i64 = self.gen_expr_safe(cond, exprs).into_int_value();
                let cond_i1 = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        cond_i64,
                        self.i64_type.const_zero(),
                        "cond_i1",
                    )
                    .unwrap();

                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_bb = self.context.append_basic_block(parent_fn, "then");
                let else_bb = self.context.append_basic_block(parent_fn, "else");
                let merge_bb = self.context.append_basic_block(parent_fn, "merge");

                self.builder
                    .build_conditional_branch(cond_i1, then_bb, else_bb)
                    .unwrap();

                self.builder.position_at_end(then_bb);
                for s in then {
                    self.gen_stmt(s, exprs);
                }
                self.builder.build_unconditional_branch(merge_bb).unwrap();

                self.builder.position_at_end(else_bb);
                for s in else_ {
                    self.gen_stmt(s, exprs);
                }
                self.builder.build_unconditional_branch(merge_bb).unwrap();

                self.builder.position_at_end(merge_bb);
            }
            MirStmt::ParamInit { .. } => {} // handled at entry
            _ => {}
        }
    }

    fn gen_expr_safe(&self, id: &u32, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
        if let Some(expr) = exprs.get(id) {
            self.gen_expr(expr, exprs)
        } else {
            self.i64_type.const_zero().into()
        }
    }

    fn gen_expr(&self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::StringLit(s) => {
                let global = self.module.add_global(
                    self.context.i8_type().array_type(s.len() as u32 + 1),
                    None,
                    "str_lit",
                );
                global.set_linkage(inkwell::module::Linkage::Private);
                global.set_constant(true);
                let mut bytes = s.as_bytes().to_vec();
                bytes.push(0);
                let values: Vec<_> = bytes
                    .iter()
                    .map(|&b| self.context.i8_type().const_int(b as u64, false))
                    .collect();
                global.set_initializer(&self.context.i8_type().const_array(&values));
                let gptr = global.as_pointer_value();
                self.builder
                    .build_ptr_to_int(gptr, self.i64_type, "str_ptr_i64")
                    .unwrap()
                    .into()
            }
            MirExpr::FString(ids) => {
                if ids.is_empty() {
                    return self.i64_type.const_int(0, false).into();
                }
                let mut res = self.gen_expr(&exprs[&ids[0]], exprs);
                for &id in &ids[1..] {
                    let next = self.gen_expr(&exprs[&id], exprs);
                    let call = self
                        .builder
                        .build_call(
                            self.module.get_function("host_str_concat").unwrap(),
                            &[res.into(), next.into()],
                            "fconcat",
                        )
                        .unwrap();
                    res = Self::call_site_to_basic_value(call).unwrap();
                }
                res
            }
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::TimingOwned(inner_id) => {
                let ptr = *self.locals.get(inner_id).unwrap();
                self.builder
                    .build_load(self.i64_type, ptr, "timing_load")
                    .unwrap()
            }
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = *self.locals.get(&id).unwrap();
        self.builder
            .build_load(self.i64_type, ptr, &format!("load_{id}"))
            .unwrap()
    }

    fn call_site_to_basic_value(call: CallSiteValue<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match call.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(basic) => Some(basic),
            _ => None,
        }
    }
}
