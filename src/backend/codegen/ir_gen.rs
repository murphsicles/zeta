// src/backend/codegen/ir_gen.rs
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::middle::specialization::{MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, IntValue};
use inkwell::module::Linkage;
use inkwell::values::{FunctionValue, VectorValue};
use std::collections::HashMap;
use inkwell::values::BasicValue;
use inkwell::Either;
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
                let callee = if !type_args.is_empty() {
                    let key = MonoKey { func_name: func.clone(), type_args: type_args.clone() };
                    if let Some(val) = lookup_specialization(&key) {
                        val.llvm_func_name
                    } else {
                        let mangled = key.mangle();
                        if is_cache_safe(&key) {
                            record_specialization(key.clone(), MonoValue { llvm_func_name: mangled.clone() });
                        }
                        mangled
                    }
                } else {
                    func.clone()
                };
                let callee_fn = self.get_callee(&callee);
                let args_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.load_local(id).into()).collect();
                let call = self.builder.build_call(callee_fn, &args_vals, "call").unwrap();
                if let Either::Left(basic_val) = call.try_as_basic_value() {
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("call_{dest}")).unwrap();
                    self.builder.build_store(alloca, basic_val).unwrap();
                    self.locals.insert(*dest, alloca);
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee_fn = self.get_callee(func);
                let args_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.load_local(id).into()).collect();
                self.builder.build_call(callee_fn, &args_vals, "").unwrap();
            }
            MirStmt::Return { val } => {
                let ret_val = self.load_local(*val);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            MirStmt::SemiringFold { op: SemiringOp::Add, values, result } => {
                let mut vec_acc: VectorValue<'ctx> = self.vec4_i64_type.const_zero();
                let chunk_size = 4;
                for chunk in values.chunks(chunk_size) {
                    let mut vec_vals: Vec<IntValue<'ctx>> = chunk.iter().map(|&id| self.load_local(id).into_int_value()).collect();
                    if vec_vals.len() < chunk_size {
                        vec_vals.resize(chunk_size, self.i64_type.const_zero());
                    }
                    let mut vec_right: VectorValue<'ctx> = self.vec4_i64_type.const_zero();
                    vec_right = self.builder.build_insert_element(vec_right, vec_vals[0], self.i64_type.const_int(0, false), "").unwrap();
                    vec_right = self.builder.build_insert_element(vec_right, vec_vals[1], self.i64_type.const_int(1, false), "").unwrap();
                    vec_right = self.builder.build_insert_element(vec_right, vec_vals[2], self.i64_type.const_int(2, false), "").unwrap();
                    vec_right = self.builder.build_insert_element(vec_right, vec_vals[3], self.i64_type.const_int(3, false), "").unwrap();
                    vec_acc = self.builder.build_int_add(vec_acc, vec_right, "vec_add").unwrap();
                }
                let mut acc = self.builder.build_extract_element(vec_acc, self.i64_type.const_int(0, false), "reduce0").unwrap().into_int_value();
                acc = self.builder.build_int_add(acc, self.builder.build_extract_element(vec_acc, self.i64_type.const_int(1, false), "reduce1").unwrap().into_int_value(), "").unwrap();
                acc = self.builder.build_int_add(acc, self.builder.build_extract_element(vec_acc, self.i64_type.const_int(2, false), "reduce2").unwrap().into_int_value(), "").unwrap();
                acc = self.builder.build_int_add(acc, self.builder.build_extract_element(vec_acc, self.i64_type.const_int(3, false), "reduce3").unwrap().into_int_value(), "").unwrap();
                let alloca = self.builder.build_alloca(self.i64_type, &format!("vec_fold_{result}")).unwrap();
                self.builder.build_store(alloca, acc).unwrap();
                self.locals.insert(*result, alloca);
            }
            MirStmt::SemiringFold { op: SemiringOp::Mul, values, result } => {
                let mut acc = self.i64_type.const_int(1, false);
                for &val_id in values {
                    let val = self.load_local(val_id).into_int_value();
                    acc = self.builder.build_int_mul(acc, val, "mul").unwrap();
                }
                let alloca = self.builder.build_alloca(self.i64_type, &format!("fold_{result}")).unwrap();
                self.builder.build_store(alloca, acc).unwrap();
                self.locals.insert(*result, alloca);
            }
            MirStmt::ParamInit { param_id, arg_index } => {
                let param_val = self.builder.build_alloca(self.i64_type, &format!("init_param_{param_id}")).unwrap();
                let arg_val = self.i64_type.const_int(*arg_index as u64, false);
                self.builder.build_store(param_val, arg_val).unwrap();
                self.locals.insert(*param_id, param_val);
            }
            MirStmt::Consume { id } => {
                let ptr = self.locals.remove(id);
                if let Some(p) = ptr {
                    self.builder.build_free(p).unwrap();
                }
            }
            MirStmt::If { cond, then, else_ } => {
                let cond_val = self.gen_expr(&exprs[cond], exprs).into_int_value();
                let then_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "then");
                let else_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "else");
                let merge_bb = self.context.append_basic_block(self.builder.get_insert_block().unwrap().get_parent().unwrap(), "merge");
                self.builder.build_conditional_branch(cond_val, then_bb, else_bb).unwrap();
                self.builder.position_at_end(then_bb);
                for stmt in then {
                    self.gen_stmt(stmt, exprs);
                }
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                self.builder.position_at_end(else_bb);
                for stmt in else_ {
                    self.gen_stmt(stmt, exprs);
                }
                self.builder.build_unconditional_branch(merge_bb).unwrap();
                self.builder.position_at_end(merge_bb);
            }
        }
    }

    fn gen_expr(&self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::StringLit(s) => {
                let mut bytes = s.as_bytes().to_vec();
                bytes.push(0);
                let array_ty = self.context.i8_type().array_type(bytes.len() as u32);
                let global = self.module.add_global(array_ty, None, ".str");
                global.set_linkage(Linkage::Private);
                global.set_constant(true);
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
                    if let Either::Left(basic_val) = call.try_as_basic_value() {
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
