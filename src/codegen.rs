// src/codegen.rs
use crate::mir::{Mir, MirStmt, MirExpr, SemiringOp};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::BasicValueEnum;
use inkwell::types::VectorType;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: inkwell::types::IntType<'ctx>,
    i32x4_type: VectorType<'ctx>,
    locals: HashMap<u32, inkwell::values::PointerValue<'ctx>>,
    enable_simd: bool,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i32x4_type = i32_type.vec_type(4);

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            i32x4_type,
            locals: HashMap::new(),
            enable_simd: true,
        }
    }

    pub fn gen_mir(&mut self, mir: &Mir) {
        // Create a dummy function just so we can emit instructions
        let fn_type = self.i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }

        // If we never emitted a return, add one (fallback)
        if !mir.stmts.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
            self.builder.build_return(Some(&self.i32_type.const_zero())).unwrap();
        }
    }

    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let value = self.gen_expr(rhs);
                let ptr = self.locals.entry(*lhs).or_insert_with(|| {
                    self.builder.build_alloca(self.i32_type, &format!("local_{}", lhs)).unwrap()
                });
                self.builder.build_store(*ptr, value).unwrap();
            }
            MirStmt::Call { func: _, args, dest } => {
                // For now we only support i32.add(i32) intrinsic
                let lhs = self.load_local(args[0]);
                let rhs = self.load_local(args[1]);
                let sum = self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add_tmp").unwrap();
                let ptr = self.locals.entry(*dest).or_insert_with(|| {
                    self.builder.build_alloca(self.i32_type, &format!("call_result_{}", dest)).unwrap()
                });
                self.builder.build_store(*ptr, sum).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                let mut acc = self.load_local(values[0]);

                if values.len() >= 4 && self.enable_simd {
                    for chunk in values[1..].chunks(4) {
                        let mut elems = vec![];
                        for &v in chunk {
                            elems.push(self.load_local(v).into_int_value());
                        }
                        while elems.len() < 4 {
                            elems.push(self.i32_type.const_zero());
                        }
                        let vec = VectorType::const_vector(&elems);
                        acc = match op {
                            SemiringOp::Add => self
                                .builder
                                .build_int_add(acc.into_vector_value().unwrap_or(vec), vec, "fold_add_simd")
                                .unwrap()
                                .into(),
                            SemiringOp::Mul => self
                                .builder
                                .build_int_mul(acc.into_vector_value().unwrap_or(vec), vec, "fold_mul_simd")
                                .unwrap()
                                .into(),
                        };
                    }
                } else {
                    for &v in &values[1..] {
                        let rhs = self.load_local(v);
                        acc = match op {
                            SemiringOp::Add => self
                                .builder
                                .build_int_add(acc.into_int_value(), rhs.into_int_value(), "fold_add")
                                .unwrap()
                                .into(),
                            SemiringOp::Mul => self
                                .builder
                                .build_int_mul(acc.into_int_value(), rhs.into_int_value(), "fold_mul")
                                .unwrap()
                                .into(),
                        };
                    }
                }

                let ptr = self.locals.entry(*result).or_insert_with(|| {
                    self.builder.build_alloca(self.i32_type, &format!("fold_result_{}", result)).unwrap()
                });
                self.builder.build_store(*ptr, acc).unwrap();
            }
            MirStmt::Return { val } => {
                let v = self.load_local(*val);
                self.builder.build_return(Some(&v)).unwrap();
            }
            _ => {}
        }
    }

    fn gen_expr(&self, expr: &MirExpr) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i32_type.const_int(*n as u64, false).into(),
            MirExpr::ConstEval(v) => self.i32_type.const_int(*v as u64, false).into(),
            _ => self.i32_type.const_zero().into(),
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        if let Some(&ptr) = self.locals.get(&id) {
            self.builder.build_load(self.i32_type, ptr, &format!("load_{}", id)).unwrap()
        } else {
            // Temporary alloca for values that were never assigned (should not happen)
            let ptr = self.builder.build_alloca(self.i32_type, &format!("tmp_{}", id)).unwrap();
            self.builder.build_load(self.i32_type, ptr, &format!("tmp_load_{}", id)).unwrap()
        }
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }
}
