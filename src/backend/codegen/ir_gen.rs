// src/backend/codegen/ir_gen.rs
use super::codegen::LLVMCodegen;
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
};
use std::collections::HashMap;

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            self.gen_fn(mir);
        }
    }

    fn gen_fn(&mut self, mir: &Mir) {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let param_types: Vec<_> = (0..mir.param_indices.len())
            .map(|_| self.i64_type.into())
            .collect();
        let fn_type = self.i64_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(&fn_name, fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.locals.clear();

        for (i, (_, arg_index)) in mir.param_indices.iter().enumerate() {
            let param_val = fn_val.get_nth_param(i as u32).unwrap();
            let alloca = self.builder.build_alloca(self.i64_type, &format!("param_{arg_index}")).unwrap();
            self.builder.build_store(alloca, param_val).unwrap();
            self.locals.insert(*arg_index, alloca);
        }

        for stmt in &mir.stmts {
            self.gen_stmt(stmt, &mir.exprs);
        }

        if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
            self.builder.build_return(Some(&self.i64_type.const_zero())).unwrap();
        }

        self.fns.insert(fn_name, fn_val);
    }

    fn gen_stmt(&mut self, stmt: &MirStmt, exprs: &HashMap<u32, MirExpr>) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(&exprs[rhs], exprs);
                let alloca = self.builder.build_alloca(self.i64_type, &format!("local_{lhs}")).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.locals.insert(*lhs, alloca);
            }
            MirStmt::Call { func, args, dest, type_args: _ } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.gen_expr(&exprs[&id], exprs).into()).collect();
                let call = self.builder.build_call(callee, &arg_vals, &format!("call_{dest}")).unwrap();
                if let Some(val) = Self::call_site_to_basic_value(call) {
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("dest_{dest}")).unwrap();
                    self.builder.build_store(alloca, val).unwrap();
                    self.locals.insert(*dest, alloca);
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.gen_expr(&exprs[&id], exprs).into()).collect();
                let _ = self.builder.build_call(callee, &arg_vals, "void_call").unwrap();
            }
            MirStmt::Return { val } => {
                let ret_val = self.gen_expr(&exprs[val], exprs);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                let mut acc: BasicValueEnum<'ctx> = self.i64_type.const_zero().into();
                for &val_id in values {
                    let val = self.gen_expr(&exprs[&val_id], exprs);
                    acc = match op {
                        SemiringOp::Add => self.builder.build_int_add(acc.into_int_value(), val.into_int_value(), "fold_add").unwrap().into(),
                        SemiringOp::Mul => self.builder.build_int_mul(acc.into_int_value(), val.into_int_value(), "fold_mul").unwrap().into(),
                    };
                }
                let alloca = self.builder.build_alloca(self.i64_type, &format!("fold_{result}")).unwrap();
                self.builder.build_store(alloca, acc).unwrap();
                self.locals.insert(*result, alloca);
            }
            MirStmt::TryProp { expr_id, ok_dest, err_dest } => {
                let expr_val = self.gen_expr(&exprs[expr_id], exprs);
                let is_ok = self.builder.build_call(
                    self.get_callee("result_is_ok"),
                    &[expr_val.into()],
                    "is_ok",
                ).unwrap();
                let is_ok_val = Self::call_site_to_basic_value(is_ok).unwrap().into_int_value();

                let parent_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let ok_bb = self.context.append_basic_block(parent_fn, "prop_ok");
                let err_bb = self.context.append_basic_block(parent_fn, "prop_err");
                let cont_bb = self.context.append_basic_block(parent_fn, "prop_cont");

                self.builder.build_conditional_branch(is_ok_val, ok_bb, err_bb).unwrap();

                self.builder.position_at_end(ok_bb);
                let data = self.builder.build_call(
                    self.get_callee("result_get_data"),
                    &[expr_val.into()],
                    "get_data",
                ).unwrap();
                let data_val = Self::call_site_to_basic_value(data).unwrap();
                let ok_alloca = self.builder.build_alloca(self.i64_type, &format!("ok_{ok_dest}")).unwrap();
                self.builder.build_store(ok_alloca, data_val).unwrap();
                self.locals.insert(*ok_dest, ok_alloca);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(err_bb);
                let err_alloca = self.builder.build_alloca(self.i64_type, &format!("err_{err_dest}")).unwrap();
                self.builder.build_store(err_alloca, expr_val).unwrap();
                self.locals.insert(*err_dest, err_alloca);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                self.builder.position_at_end(cont_bb);
            }
            MirStmt::MapNew { dest } => {
                let call = self.builder.build_call(self.get_callee("map_new"), &[], "map_new").unwrap();
                let ptr = Self::call_site_to_basic_value(call).unwrap();
                let alloca = self.builder.build_alloca(self.ptr_type, &format!("map_{dest}")).unwrap();
                self.builder.build_store(alloca, ptr).unwrap();
                self.locals.insert(*dest, alloca);
            }
            MirStmt::DictInsert { map_id, key_id, val_id } => {
                let map_ptr = self.load_local(*map_id);
                let key_val = self.gen_expr(&exprs[key_id], exprs);
                let val_val = self.gen_expr(&exprs[val_id], exprs);
                let _ = self.builder.build_call(
                    self.get_callee("map_insert"),
                    &[map_ptr.into(), key_val.into(), val_val.into()],
                    "dict_insert",
                );
            }
            MirStmt::DictGet { map_id, key_id, dest } => {
                let map_ptr = self.load_local(*map_id);
                let key_val = self.gen_expr(&exprs[key_id], exprs);
                let call = self.builder.build_call(
                    self.get_callee("map_get"),
                    &[map_ptr.into(), key_val.into()],
                    "dict_get",
                ).unwrap();
                let val = Self::call_site_to_basic_value(call).unwrap();
                let alloca = self.builder.build_alloca(self.i64_type, &format!("get_{dest}")).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.locals.insert(*dest, alloca);
            }
            MirStmt::If { cond, then, else_ } => {
                let cond_val = self.gen_expr(&exprs[cond], exprs).into_int_value();
                let parent_fn = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let then_bb = self.context.append_basic_block(parent_fn, "then");
                let else_bb = self.context.append_basic_block(parent_fn, "else");
                let merge_bb = self.context.append_basic_block(parent_fn, "merge");

                self.builder.build_conditional_branch(cond_val, then_bb, else_bb).unwrap();

                self.builder.position_at_end(then_bb);
                for s in then { self.gen_stmt(s, exprs); }
                self.builder.build_unconditional_branch(merge_bb).unwrap();

                self.builder.position_at_end(else_bb);
                for s in else_ { self.gen_stmt(s, exprs); }
                self.builder.build_unconditional_branch(merge_bb).unwrap();

                self.builder.position_at_end(merge_bb);
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
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
                let values: Vec<_> = bytes.iter().map(|&b| self.context.i8_type().const_int(b as u64, false)).collect();
                global.set_initializer(&self.context.i8_type().const_array(&values));
                global.as_pointer_value().into()
            }
            MirExpr::FString(ids) => {
                if ids.is_empty() {
                    return self.i64_type.const_int(0, false).into();
                }
                let mut res = self.gen_expr(&exprs[&ids[0]], exprs);
                for &id in &ids[1..] {
                    let next = self.gen_expr(&exprs[&id], exprs);
                    let call = self.builder.build_call(
                        self.module.get_function("str_concat").unwrap(),
                        &[
                            res.into(),
                            self.i64_type.const_int(u64::MAX, false).into(),
                            next.into(),
                            self.i64_type.const_int(u64::MAX, false).into(),
                        ],
                        "fconcat",
                    ).unwrap();
                    res = Self::call_site_to_basic_value(call).unwrap();
                }
                res
            }
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::TimingOwned(inner_id) => {
                let ptr = self.locals[inner_id];
                let load = self.builder.build_load(self.i64_type, ptr, "timing_load").unwrap();
                if let Some(inst) = load.as_instruction_value() {
                    let tbaa_kind = self.context.get_kind_id("tbaa");
                    inst.set_metadata(self.tbaa_const_time, tbaa_kind).unwrap();
                }
                load
            }
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals[&id];
        self.builder.build_load(self.i64_type, ptr, &format!("load_{id}")).unwrap()
    }

    fn call_site_to_basic_value(call: CallSiteValue<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        if let Some(basic) = call.try_as_basic_value() {
            Some(basic)
        } else {
            None
        }
    }

    fn get_callee(&self, name: &str) -> FunctionValue<'ctx> {
        self.module.get_function(name).unwrap_or_else(|| {
            let fn_type = self.i64_type.fn_type(&[self.i64_type.into(); 2], false);
            self.module.add_function(name, fn_type, None)
        })
    }
}
