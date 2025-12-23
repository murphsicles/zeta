// src/backend/codegen/ir_gen.rs
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};
use inkwell::values::{FunctionValue};
use std::collections::HashMap;
use inkwell::values::BasicValue;
use super::codegen::LLVMCodegen;

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        for mir in mirs {
            self.gen_fn(mir);
        }
    }

    fn gen_fn(&mut self, mir: &Mir) {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let param_types: Vec<BasicMetadataTypeEnum> = (0..mir.param_indices.len())
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
            MirStmt::Call { func, args, dest, type_args } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.load_local(id).into()).collect();
                let call = self.builder.build_call(callee, &arg_vals, &format!("call_{dest}")).unwrap();
                if let Some(basic_val) = call.try_as_basic_value().left() {
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("call_dest_{dest}")).unwrap();
                    self.builder.build_store(alloca, basic_val).unwrap();
                    self.locals.insert(*dest, alloca);
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee = self.get_callee(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.load_local(id).into()).collect();
                self.builder.build_call(callee, &arg_vals, "").unwrap();
            }
            MirStmt::Return { val } => {
                let ret_val = self.load_local(*val);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                let mut acc = self.i64_type.const_zero();
                for &id in values {
                    let v = self.load_local(id).into_int_value();
                    acc = match op {
                        SemiringOp::Add => self.builder.build_int_add(acc, v, "add").unwrap(),
                        SemiringOp::Mul => self.builder.build_int_mul(acc, v, "mul").unwrap(),
                    };
                }
                let alloca = self.builder.build_alloca(self.i64_type, &format!("fold_{result}")).unwrap();
                self.builder.build_store(alloca, acc).unwrap();
                self.locals.insert(*result, alloca);
            }
            MirStmt::ParamInit { param_id, arg_index } => {
                let param = self.locals[param_id];
                let arg = self.locals[arg_index];
                let load = self.builder.build_load(self.i64_type, arg, "param_load").unwrap();
                self.builder.build_store(param, load).unwrap();
            }
            MirStmt::Consume { id } => {
                let ptr = self.locals[id];
                let load = self.builder.build_load(self.i64_type, ptr, "consume_load").unwrap();
                let free_fn = self.module.get_function("free").unwrap();
                self.builder.build_call(free_fn, &[load.into()], "consume_free").unwrap();
            }
            MirStmt::If { cond, then, else_ } => {
                let cond_val = self.load_local(*cond).into_int_value();
                let then_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "then");
                let else_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "else");
                let merge_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "ifcont");
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb).unwrap();
                self.builder.position_at_end(then_bb);
                for stmt in then {
                    self.gen_stmt(stmt, exprs);
                }
                if then_bb.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                self.builder.position_at_end(else_bb);
                for stmt in else_ {
                    self.gen_stmt(stmt, exprs);
                }
                if else_bb.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                self.builder.position_at_end(merge_bb);
            }
        }
    }

    fn gen_expr(&mut self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::StringLit(s) => {
                let global = self.module.add_global(self.context.i8_type().array_type(s.len() as u32 + 1), None, "str_lit");
                let bytes = s.as_bytes().to_vec();
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
                    let concat_fn = self.get_callee("str_concat");
                    let call = self.builder.build_call(concat_fn, &[res.into(), next.into()], "fconcat").unwrap();
                    if let Some(basic_val) = call.try_as_basic_value().left() {
                        res = basic_val;
                    }
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

    fn get_callee(&self, name: &str) -> FunctionValue<'ctx> {
        if let Some(fn_val) = self.module.get_function(name) {
            fn_val
        } else {
            let fn_type = self.i64_type.fn_type(&[self.i64_type.into(), self.i64_type.into()], false);
            self.module.add_function(name, fn_type, None)
        }
    }
}
