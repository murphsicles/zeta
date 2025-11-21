// src/codegen.rs
use crate::mir::{Mir, MirStmt, MirExpr, SemiringOp};
use inkwell::context::Context;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::module::{Module, Linkage};
use inkwell::builder::Builder;
use inkwell::values::{PointerValue, IntValue, BasicValueEnum};
use inkwell::types::{IntType, PointerType};
use inkwell::OptimizationLevel;
use inkwell::AddressSpace;
use std::collections::HashMap;
use std::time::{SystemTime, UNIX_EPOCH};
use std::error::Error;
use std::ffi::CString;

extern "C" fn host_datetime_now() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as i64
}

extern "C" fn host_http_get(url_ptr: *const u8, url_len: usize) -> *mut u8 {
    use std::slice;
    let url_bytes = unsafe { slice::from_raw_parts(url_ptr, url_len) };
    let url = match std::str::from_utf8(url_bytes) {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let client = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(10))
        .build()
        .unwrap();

    match client.get(url).send() {
        Ok(resp) if resp.status().is_success() => {
            let bytes = resp.bytes().unwrap_or_default();
            let len = bytes.len();
            let ptr = unsafe { libc::malloc(len + 1) as *mut u8 };
            if ptr.is_null() { return std::ptr::null_mut(); }
            unsafe {
                std::ptr::copy_nonnull::NonNull::new_unchecked(ptr).copy_from_nonoverlapping(bytes.as_ptr(), len);
                *ptr.add(len) = 0;
            }
            ptr
        }
        _ => std::ptr::null_mut(),
    }
}

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i64_type: IntType<'ctx>,
    ptr_type: PointerType<'ctx>,
    locals: HashMap<u32, PointerValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let ptr_type = context.i8_type().ptr_type(AddressSpace::default());

        let datetime_ty = i64_type.fn_type(&[], false);
        module.add_function("datetime_now", datetime_ty, Some(Linkage::External));

        let http_sig = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        module.add_function("http_get", http_sig, Some(Linkage::External));

        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i64_type,
            ptr_type,
            locals: HashMap::new(),
        }
    }

    pub fn gen_mir(&mut self, mir: &Mir) {
        let fn_type = self.i64_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        for stmt in &mir.stmts {
            self.gen_stmt(stmt);
        }

        if !mir.stmts.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
            self.builder.build_return(Some(&self.i64_type.const_zero())).unwrap();
        }
    }

    fn gen_stmt(&mut self, stmt: &MirStmt) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let value = self.gen_expr(rhs);
                let ptr = self.alloc_local(*lhs);
                self.builder.build_store(ptr, value).unwrap();
            }
            MirStmt::Call { func, args, dest } if func == "datetime_now" => {
                let call = self.builder.build_call(self.module.get_function("datetime_now").unwrap(), &[], "dt").unwrap();
                let val = call.try_as_basic_value().left().unwrap();
                let ptr = self.alloc_local(*dest);
                self.builder.build_store(ptr, val).unwrap();
            }
            MirStmt::Call { func, args, dest } if func == "http_get" => {
                let url_local = args[0];
                let url_ptr = self.builder.build_load(self.ptr_type, self.locals[&url_local], "url_ptr").unwrap().into_pointer_value();
                let url_len = self.builder.build_load(self.i64_type, self.locals[&args[1]], "url_len").unwrap().into_int_value();

                let call = self.builder.build_call(
                    self.module.get_function("http_get").unwrap(),
                    &[url_ptr.into(), url_len.into()],
                    "raw_str",
                ).unwrap();

                let raw_ptr = call.try_as_basic_value().left().unwrap().into_pointer_value();
                let ptr = self.alloc_local(*dest);
                self.builder.build_store(ptr, raw_ptr).unwrap();
            }
            MirStmt::Call { func, args, dest } => {
                let lhs = self.load_local(args[0]);
                let rhs = args.get(1).map_or(self.i64_type.const_zero().into(), |&id| self.load_local(id));
                let result: IntValue = match func.as_str() {
                    "add" => self.builder.build_int_add(lhs.into_int_value(), rhs.into_int_value(), "add_tmp").unwrap(),
                    "mul" => self.builder.build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "mul_tmp").unwrap(),
                    _ => unimplemented!("call {}", func),
                };
                let ptr = self.alloc_local(*dest);
                self.builder.build_store(ptr, result).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                let mut acc = self.load_local(values[0]);
                for &v in &values[1..] {
                    let rhs = self.load_local(v);
                    acc = match op {
                        SemiringOp::Add => self.builder.build_int_add(acc.into_int_value(), rhs.into_int_value(), "fold_add").unwrap().into(),
                        SemiringOp::Mul => self.builder.build_int_mul(acc.into_int_value(), rhs.into_int_value(), "fold_mul").unwrap().into(),
                    };
                }
                let ptr = self.alloc_local(*result);
                self.builder.build_store(ptr, acc).unwrap();
            }
            MirStmt::Return { val } => {
                let v = self.load_local(*val);
                self.builder.build_return(Some(&v)).unwrap();
            }
            _ => unimplemented!("stmt {:?}", stmt),
        }
    }

    fn gen_expr(&self, expr: &MirExpr) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = self.locals[&id];
        match ptr.get_type().get_element_type().into_int_type().get_bit_width() {
            64 => self.builder.build_load(self.i64_type, ptr, &format!("load_{}", id)).unwrap(),
            _ => self.builder.build_load(self.ptr_type, ptr, &format!("load_{}", id)).unwrap(),
        }
    }

    fn alloc_local(&mut self, id: u32) -> PointerValue<'ctx> {
        *self.locals.entry(id).or_insert_with(|| {
            self.builder.build_alloca(self.i64_type, &format!("local_{}", id)).unwrap()
        })
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        
        ee.add_global_mapping(&self.module.get_function("datetime_now").unwrap(), host_datetime_now as usize);
        ee.add_global_mapping(&self.module.get_function("http_get").unwrap(), host_http_get as usize);
        
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }
}
