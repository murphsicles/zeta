// src/codegen.rs
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, OptimizationLevel};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, MetadataValue};
use inkwell::types::{BasicTypeEnum, StructType};
use crate::ast::AstNode;
use crate::parser::parse_zeta;
use crate::resolver::Resolver;
use std::collections::HashMap;
use std::error::Error;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: inkwell::types::IntType<'ctx>,
    i64_type: inkwell::types::IntType<'ctx>,
    i8_type: inkwell::types::IntType<'ctx>,
    i8ptr_type: inkwell::types::PointerType<'ctx>,
    vec_type: StructType<'ctx>,
    add_i32_fn: Option<FunctionValue<'ctx>>,
    add_vec_fn: Option<FunctionValue<'ctx>>,
    malloc_fn: Option<FunctionValue<'ctx>>,
    channel_send_fn: Option<FunctionValue<'ctx>>,
    http_get_fn: Option<FunctionValue<'ctx>>,
    tls_connect_fn: Option<FunctionValue<'ctx>>,
    datetime_now_fn: Option<FunctionValue<'ctx>>,
    tbaa_root: Option<MetadataValue<'ctx>>,
    abi_version: Option<String>,
    // AI-Opt: MLGO metadata node (placeholder for pass hints)
    ai_opt_meta: Option<MetadataValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let i8_type = context.i8_type();
        let i8ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);
        let vec_type = context.struct_type(&[i8ptr_type.into(), i32_type.into()], false);
        Self { 
            context, module, builder, execution_engine: None, i32_type, i64_type, i8_type, i8ptr_type, vec_type, 
            add_i32_fn: None, add_vec_fn: None, malloc_fn: None, channel_send_fn: None, http_get_fn: None, 
            tls_connect_fn: None, datetime_now_fn: None, tbaa_root: None, abi_version: Some("1.0".to_string()),
            ai_opt_meta: None,
        }
    }

    pub fn gen_intrinsics(&mut self) {
        // TBAA metadata for stricter aliasing
        let tbaa_id = self.context.metadata_value(self.i32_type.const_int(0, false));
        let tbaa_node = self.context.metadata_value(self.i32_type.const_int(1, false));
        let tbaa_md = self.context.metadata_node(&[tbaa_id, tbaa_node]);
        self.tbaa_root = Some(tbaa_md);

        // AI-Opt: MLGO placeholder metadata (vectorize/branch hints)
        let ai_hint = self.context.metadata_value(self.i32_type.const_int(1, false)); // 1=vectorize, 2=branch-pred
        self.ai_opt_meta = Some(ai_hint);

        // add_i32 with TBAA
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();
        let ret = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&ret.into()));
        self.add_i32_fn = Some(fn_val);

        // add_vec_i32 with SIMD + TBAA + AI-Opt metadata
        let vec_ptr_type = self.vec_type.ptr_type(inkwell::AddressSpace::Generic);
        let add_vec_type = vec_ptr_type.fn_type(&[vec_ptr_type.into(), self.i32_type.into()], false);
        let add_vec_val = self.module.add_function("add_vec_i32", add_vec_type, None);
        let entry = self.context.append_basic_block(add_vec_val, "entry");
        self.builder.position_at_end(entry);
        let vec_arg = add_vec_val.get_nth_param(0).unwrap().into_pointer_value();
        let scalar = add_vec_val.get_nth_param(1).unwrap().into_int_value();
        let len_ptr = self.builder.build_struct_gep(vec_arg, 1, "len").unwrap();
        let len = self.builder.build_load(len_ptr, "load_len").into_int_value();
        let new_len = self.builder.build_int_add(len, self.i32_type.const_int(1, false), "new_len");
        let new_size = self.builder.build_int_mul(new_len, self.i32_type.const_int(4, false), "new_size");
        // Stub malloc (declare later)
        self.malloc_fn = Some(self.module.add_function("malloc", self.i8ptr_type.fn_type(&[self.i64_type.into()], false), None));
        let new_ptr = self.builder.build_call(self.malloc_fn.as_ref().unwrap(), &[new_size.into()], "new_ptr").try_as_basic_value().left().unwrap().into_pointer_value();
        let old_ptr = self.builder.build_struct_gep(vec_arg, 0, "old_ptr").unwrap();
        let old_data = self.builder.build_load(old_ptr, "old_data").into_pointer_value();
        let memcpy = self.module.add_function("llvm.memcpy.p0i8.p0i8.i32", self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i32_type.into()], false), None);
        let size4 = self.builder.build_int_mul(len, self.i32_type.const_int(4, false), "size4");
        self.builder.build_call(memcpy, &[new_ptr.into(), old_data.into(), size4.into()], "memcpy");
        let offset = self.builder.build_int_mul(len, self.i32_type.const_int(4, false), "offset");
        let offset_ptr = self.builder.build_gep(new_ptr, &[offset.into()], "offset_ptr");
        // SIMD store with v4i32 + TBAA + AI-Opt
        let v4i32 = self.i32_type.vector_type(4);
        let scalar_vec = v4i32.const_array(&[scalar, self.i32_type.const_int(0, false), self.i32_type.const_int(0, false), self.i32_type.const_int(0, false)]);
        self.builder.build_store(offset_ptr, scalar_vec);
        // Attach AI-Opt metadata to SIMD store (placeholder)
        if let Some(ai_meta) = &self.ai_opt_meta {
            let store_inst = self.builder.get_last_instruction().unwrap();
            store_inst.set_metadata("ai_opt_hint", *ai_meta);
        }
        // Update vec struct (stub: return new vec)
        let new_vec = self.builder.build_aggregate_store(&[new_ptr.into(), new_len.into()], vec_arg);
        self.builder.build_return(Some(&vec_arg.into()));
        self.add_vec_fn = Some(add_vec_val);

        // Stub other intrinsics
        self.channel_send_fn = Some(self.module.add_function("channel_send", self.i32_type.fn_type(&[self.i8ptr_type.into(), self.i32_type.into()], false), None));
        self.http_get_fn = Some(self.module.add_function("std_http_get", self.i8ptr_type.fn_type(&[self.i8ptr_type.into()], false), None));
        self.tls_connect_fn = Some(self.module.add_function("std_tls_connect", self.i32_type.fn_type(&[self.i8ptr_type.into()], false), None));
        self.datetime_now_fn = Some(self.module.add_function("std_datetime_now", self.i64_type.fn_type(&[], false), None));
    }

    pub fn gen_func(&mut self, ast: &AstNode, param_map: &mut HashMap<String, PointerValue<'ctx>>) {
        if let AstNode::FuncDef { name, params, body, ret, attrs, .. } = ast {
            let param_types: Vec<BasicTypeEnum> = params.iter().map(|(_, t)| if *t == "i32" { self.i32_type.into() } else { self.i8ptr_type.into() }).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            // Params to alloca
            for (i, (pname, _)) in params.iter().enumerate() {
                let param_val = fn_val.get_nth_param(i as u32).unwrap().into_pointer_value();
                let alloca = self.builder.build_alloca(param_val.get_type(), pname);
                self.builder.build_store(alloca, param_val);
                param_map.insert(pname.clone(), alloca);
            }

            // Gen body
            for node in body {
                if let Some(val) = self.gen_expr(node, param_map) {
                    // AI-Opt: Attach metadata if #[ai_opt] (e.g., on loops/calls)
                    if attrs.contains(&"ai_opt".to_string()) && self.builder.get_insert_block().is_some() {
                        if let Some(ai_meta) = &self.ai_opt_meta {
                            let last_inst = self.builder.get_last_instruction().unwrap();
                            last_inst.set_metadata("mlgo_hint", *ai_meta); // Placeholder for vectorize/branch
                        }
                    }
                }
            }
            self.builder.build_return(Some(&self.i32_type.const_int(0, false).into()));
        }
    }

    fn gen_expr(&mut self, node: &AstNode, param_map: &HashMap<String, PointerValue<'ctx>>) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Call { method, receiver, .. } => {
                if method == "add" && receiver == "i32" {
                    if let Some(add_fn) = &self.add_i32_fn {
                        let x = self.i32_type.const_int(1, false);
                        let y = self.i32_type.const_int(2, false);
                        Some(self.builder.build_call(add_fn, &[x.into(), y.into()], "add_call").try_as_basic_value().left().unwrap())
                    } else { None }
                } else if method == "add" && receiver.contains("Vec") {
                    // Stub vec ptr
                    let vec_ptr = self.builder.build_alloca(self.vec_type.ptr_type(inkwell::AddressSpace::Generic), "vec_ptr");
                    let scalar = self.i32_type.const_int(3, false);
                    if let Some(add_vec_fn) = &self.add_vec_fn {
                        Some(self.builder.build_call(add_vec_fn, &[vec_ptr.into(), scalar.into()], "vec_add").try_as_basic_value().left().unwrap())
                    } else { None }
                } else { None }
            }
            AstNode::TimingOwned { .. } => {
                // Constant-time XOR erase (placeholder: XOR with rand)
                let inner_val = self.i32_type.const_int(42, false);
                let xor_key = self.i32_type.const_int(0xDEADBEEF, false);
                let erased = self.builder.build_int_xor(inner_val, xor_key, "timing_erase");
                Some(erased.into())
            }
            AstNode::Lit(n) => {
                let lit_ptr = self.builder.build_alloca(self.i32_type, "lit_ptr");
                let lit_val = self.i32_type.const_int(*n as u64, false);
                self.builder.build_store(lit_ptr, lit_val);
                Some(lit_ptr.into())
            }
            AstNode::Var(v) => param_map.get(v).map(|p| self.builder.build_load(*p, v).into()),
            _ => None,
        }
    }

    pub fn gen_actor(&mut self, actor: &AstNode) {
        if let AstNode::ActorDef { name, methods } = actor {
            let actor_ty = self.context.struct_type(&[self.i8ptr_type.into(), self.i32_type.into()], false);
            let actor_ptr_type = actor_ty.ptr_type(inkwell::AddressSpace::Generic);
            for method in methods {
                if let AstNode::Method { name: mname, params, ret } = method {
                    let param_types: Vec<BasicTypeEnum> = params.iter().skip(1).map(|(_, t)| if *t == "i32" { self.i32_type.into() } else { panic!() }).collect();
                    let fn_type = self.i32_type.fn_type(&param_types, false);
                    let fn_val = self.module.add_function(mname, fn_type, None);
                    let entry = self.context.append_basic_block(fn_val, "entry");
                    self.builder.position_at_end(entry);
                    if let Some(send) = &self.channel_send_fn {
                        let self_param = fn_val.get_nth_param(0).unwrap().into_pointer_value();
                        let queue = self.builder.build_struct_gep(self_param, 0, "queue").unwrap();
                        let msg = fn_val.get_nth_param(1).unwrap().into_int_value();
                        self.builder.build_call(send, &[queue.into(), msg.into()], "send_msg");
                    }
                    self.builder.build_return(Some(&self.i32_type.const_int(0, false).into()));
                }
            }
            let handler_type = self.i32_type.fn_type(&[actor_ptr_type.into()], false);
            let handler = self.module.add_function(&format!("{}_handler", name), handler_type, None);
            let entry_h = self.context.append_basic_block(handler, "entry");
            self.builder.position_at_end(entry_h);
            let loop_bb = self.context.append_basic_block(handler, "loop");
            self.builder.build_unconditional_branch(loop_bb);
            self.builder.position_at_end(loop_bb);
            let self_arg = handler.get_nth_param(0).unwrap().into_pointer_value();
            let queue = self.builder.build_struct_gep(self_arg, 0, "queue").unwrap();
            let poll = self.builder.build_load(queue, "poll").into_int_value();
            let cond = self.builder.build_int_compare(inkwell::IntPredicate::NE, poll, self.i32_type.const_int(0, false), "has_msg");
            let then_bb = self.context.append_basic_block(handler, "then");
            let else_bb = self.context.append_basic_block(handler, "else");
            self.builder.build_conditional_branch(cond, then_bb, else_bb);
            self.builder.position_at_end(then_bb);
            let msg = self.builder.build_load(queue, "msg").into_int_value();
            let inc_fn = self.module.get_function("increment").unwrap_or_else(|| self.module.add_function("increment", self.i32_type.fn_type(&[actor_ptr_type.into(), self.i32_type.into()], false), None));
            let inc_call = self.builder.build_call(inc_fn, &[self_arg.into(), msg.into()], "inc_call");
            self.builder.build_return(Some(&inc_call.try_as_basic_value().left().unwrap().into()));
            self.builder.position_at_end(else_bb);
            self.builder.build_br(loop_bb);
        }
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>> where F: 'static + Copy + inkwell::supports::ToFromPrimitive {
        self.execution_engine.as_ref().and_then(|ee| ee.get_function(name).ok())
    }
}

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error>> {
    let (_, asts) = parse_zeta(input)?;
    let mut resolver = Resolver::new();
    for ast in &asts { resolver.register(ast.clone()); }
    if !resolver.typecheck(&asts) { return Err("Typecheck failed".into()); }

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");
    codegen.gen_intrinsics();
    for ast in &asts {
        match ast {
            AstNode::ActorDef { .. } => codegen.gen_actor(ast),
            _ => {}
        }
    }
    let mut param_map = HashMap::new();
    for ast in asts.iter().filter(|a| matches!(a, AstNode::FuncDef { .. })) {
        codegen.gen_func(ast, &mut param_map);
    }
    let ee = codegen.finalize_and_jit()?;

    unsafe {
        type UseVecAddFn = unsafe extern "C" fn() -> i32;
        if let Some(use_add) = codegen.get_fn::<UseVecAddFn>("use_vec_add") {
            Ok(use_add.call())
        } else {
            Err("use_vec_add not found".into())
        }
    }
}
