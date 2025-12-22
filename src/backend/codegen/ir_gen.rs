// src/backend/codegen/ir_gen.rs
use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::middle::specialization::{MonoKey, MonoValue, is_cache_safe, lookup_specialization, record_specialization};
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};
use std::collections::HashMap;
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
            self.locals.insert(*arg_index as u32, alloca);
        }
        for stmt in &mir.stmts {
            self.gen_stmt(stmt, &mir.exprs);
        }
        self.fns.insert(fn_name, fn_val);
    }

    fn gen_stmt(&mut self, stmt: &MirStmt, exprs: &HashMap<u32, MirExpr>) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr(rhs, exprs);
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
                        let cache_safe = type_args.iter().all(|t| is_cache_safe(t));
                        record_specialization(key, MonoValue { llvm_func_name: mangled.clone(), cache_safe });
                        mangled
                    }
                } else {
                    func.clone()
                };
                let callee_fn = self.get_callee(&callee);
                let arg_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.load_local(id).into()).collect();
                let call = self.builder.build_call(callee_fn, &arg_vals, "call").unwrap();
                if let Some(basic_val) = call.try_as_basic_value().left() {
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("call_{dest}")).unwrap();
                    self.builder.build_store(alloca, basic_val).unwrap();
                    self.locals.insert(*dest, alloca);
                }
            }
            MirStmt::VoidCall { func, args } => {
                let callee = func.clone();
                let callee_fn = self.get_callee(&callee);
                let arg_vals: Vec<BasicMetadataValueEnum> = args.iter().map(|&id| self.load_local(id).into()).collect();
                self.builder.build_call(callee_fn, &arg_vals, "void_call").unwrap();
            }
            MirStmt::Return { val } => {
                let ret_val = self.load_local(*val);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                if values.len() >= 4 {
                    let mut vec_acc = self.vec4_i64_type.const_zero();
                    for i in 0..values.len() / 4 {
                        let vec_vals: Vec<_> = (0..4).map(|j| self.load_local(values[i * 4 + j])).collect();
                        let mut vec_right = self.vec4_i64_type.const_zero();
                        vec_right = self.builder.build_insert_element(vec_right, vec_vals[0], 0u64, "").unwrap();
                        vec_right = self.builder.build_insert_element(vec_right, vec_vals[1], 1u64, "").unwrap();
                        vec_right = self.builder.build_insert_element(vec_right, vec_vals[2], 2u64, "").unwrap();
                        vec_right = self.builder.build_insert_element(vec_right, vec_vals[3], 3u64, "").unwrap();
                        vec_acc = match op {
                            SemiringOp::Add => self.builder.build_int_add(vec_acc, vec_right, "vec_fold_add").unwrap(),
                            SemiringOp::Mul => self.builder.build_int_mul(vec_acc, vec_right, "vec_fold_mul").unwrap(),
                        };
                    }
                    let mut acc = self.builder.build_extract_element(vec_acc, 0u64, "reduce0").unwrap().into_int_value();
                    acc = self.builder.build_int_add(acc, self.builder.build_extract_element(vec_acc, 1u64, "reduce1").unwrap().into_int_value(), "").unwrap();
                    acc = self.builder.build_int_add(acc, self.builder.build_extract_element(vec_acc, 2u64, "reduce2").unwrap().into_int_value(), "").unwrap();
                    acc = self.builder.build_int_add(acc, self.builder.build_extract_element(vec_acc, 3u64, "reduce3").unwrap().into_int_value(), "").unwrap();
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("vec_fold_{result}")).unwrap();
                    self.builder.build_store(alloca, acc).unwrap();
                    self.locals.insert(*result, alloca);
                } else {
                    let mut acc = self.load_local(values[0]);
                    for &v in &values[1..] {
                        let right = self.load_local(v);
                        acc = match op {
                            SemiringOp::Add => self.builder.build_int_add(acc.into_int_value(), right.into_int_value(), "fold_add").unwrap().into(),
                            SemiringOp::Mul => self.builder.build_int_mul(acc.into_int_value(), right.into_int_value(), "fold_mul").unwrap().into(),
                        };
                    }
                    let alloca = self.builder.build_alloca(self.i64_type, &format!("fold_{result}")).unwrap();
                    self.builder.build_store(alloca, acc).unwrap();
                    self.locals.insert(*result, alloca);
                }
            }
            MirStmt::ParamInit { .. } => {}
            MirStmt::Consume { .. } => {}
            MirStmt::If { cond, then, else_ } => {
                let cond_val = self.load_local(*cond);
                let parent = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let merge_bb = self.context.append_basic_block(parent, "merge");
                self.builder.build_conditional_branch(cond_val.into_int_value(), then_bb, else_bb).unwrap();
                self.builder.position_at_end(then_bb);
                for stmt in then {
                    self.gen_stmt(stmt, exprs);
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                self.builder.position_at_end(else_bb);
                for stmt in else_ {
                    self.gen_stmt(stmt, exprs);
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                self.builder.position_at_end(merge_bb);
            }
        }
    }

    fn gen_expr(&self, expr: &MirExpr, _exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
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
                let mut res = self.gen_expr(&MirExpr::Var(ids[0]), _exprs);
                for &id in &ids[1..] {
                    let next = self.gen_expr(&MirExpr::Var(id), _exprs);
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
                    inst.set_metadata(self.tbaa_const_time, 0).unwrap();
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
