// src/codegen.rs
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::OptimizationLevel;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, MetadataValue, FloatValue};
use inkwell::types::{BasicTypeEnum, StructType};
use crate::ast::AstNode;
use crate::parser::parse_zeta;
use crate::resolver::Resolver;
use crate::xai::XAIClient;
use std::collections::HashMap;
use std::error::Error;
use std::thread;
use std::time::Duration;

pub struct LLVMCodegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: Option<ExecutionEngine<'ctx>>,
    i32_type: inkwell::types::IntType<'ctx>,
    i64_type: inkwell::types::IntType<'ctx>,
    i8_type: inkwell::types::IntType<'ctx>,
    i8ptr_type: inkwell::types::PointerType<'ctx>,
    f64_type: inkwell::types::FloatType<'ctx>,
    vec_type: StructType<'ctx>,
    add_i32_fn: Option<FunctionValue<'ctx>>,
    add_vec_fn: Option<FunctionValue<'ctx>>,
    malloc_fn: Option<FunctionValue<'ctx>>,
    channel_send_fn: Option<FunctionValue<'ctx>>,
    channel_poll_fn: Option<FunctionValue<'ctx>>,
    http_get_fn: Option<FunctionValue<'ctx>>,
    tls_connect_fn: Option<FunctionValue<'ctx>>,
    datetime_now_fn: Option<FunctionValue<'ctx>>,
    serde_json_fn: Option<FunctionValue<'ctx>>,
    rand_next_fn: Option<FunctionValue<'ctx>>,
    log_trace_fn: Option<FunctionValue<'ctx>>,
    governor_permit_fn: Option<FunctionValue<'ctx>>,
    prometheus_inc_fn: Option<FunctionValue<'ctx>>,
    tbaa_root: Option<MetadataValue<'ctx>>,
    abi_version: Option<String>,
    ai_opt_meta: Option<MetadataValue<'ctx>>,
    copy_fn: Option<FunctionValue<'ctx>>,
    mlgo_vectorize_meta: Option<MetadataValue<'ctx>>,
    mlgo_branch_meta: Option<MetadataValue<'ctx>>,
    locals: HashMap<String, PointerValue<'ctx>>,
    xai_client: Option<XAIClient>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();
        let i8_type = context.i8_type();
        let i8ptr_type = i8_type.ptr_type(inkwell::AddressSpace::Generic);
        let f64_type = context.f64_type();
        let vec_type = context.struct_type(&[i8ptr_type.into(), i32_type.into()], false);
        let mut xai_client = None;
        if let Ok(client) = XAIClient::new() {
            xai_client = Some(client);
        }
        Self { 
            context, module, builder, execution_engine: None, i32_type, i64_type, i8_type, i8ptr_type, f64_type, vec_type, 
            add_i32_fn: None, add_vec_fn: None, malloc_fn: None, channel_send_fn: None, channel_poll_fn: None,
            http_get_fn: None, tls_connect_fn: None, datetime_now_fn: None, serde_json_fn: None, rand_next_fn: None,
            log_trace_fn: None, governor_permit_fn: None, prometheus_inc_fn: None,
            tbaa_root: None, abi_version: Some("1.0".to_string()),
            ai_opt_meta: None, copy_fn: None, mlgo_vectorize_meta: None, mlgo_branch_meta: None, locals: HashMap::new(),
            xai_client,
        }
    }

    pub fn gen_intrinsics(&mut self) {
        // TBAA metadata
        let tbaa_id = self.context.metadata_value(self.i32_type.const_int(0, false));
        let tbaa_node = self.context.metadata_value(self.i32_type.const_int(1, false));
        let tbaa_md = self.context.metadata_node(&[tbaa_id, tbaa_node]);
        self.tbaa_root = Some(tbaa_md);

        // AI opt metadata
        let ai_hint = self.context.metadata_value(self.i32_type.const_int(1, false));
        self.ai_opt_meta = Some(ai_hint);

        // MLGO vectorize metadata
        let vec_hint = self.context.metadata_value(self.i32_type.const_int(4, false));
        self.mlgo_vectorize_meta = Some(self.context.metadata_node(&[vec_hint]));

        // MLGO branch metadata
        let branch_hint = self.context.metadata_value(self.i32_type.const_int(1, false));
        self.mlgo_branch_meta = Some(self.context.metadata_node(&[branch_hint]));

        // add_i32 intrinsic
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();
        let ret = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&ret.into()));
        self.add_i32_fn = Some(fn_val);

        // add_vec_i32 with SIMD
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
        let new_size = self.builder.build_int_mul(new_len, self.i64_type.const_int(4, false), "new_size");
        let malloc_type = self.i8ptr_type.fn_type(&[self.i64_type.into()], false);
        let malloc_val = self.module.add_function("malloc", malloc_type, None);
        self.malloc_fn = Some(malloc_val);
        let new_ptr = self.builder.build_call(malloc_val, &[new_size.into()], "new_ptr").try_as_basic_value().left().unwrap().into_pointer_value();
        let old_ptr = self.builder.build_struct_gep(vec_arg, 0, "old_ptr").unwrap();
        let old_data = self.builder.build_load(old_ptr, "old_data").into_pointer_value();
        let memcpy_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i64_type.into()], false);
        let memcpy = self.module.add_function("llvm.memcpy.p0i8.p0i8.i64", memcpy_type, None);
        let size_bytes = self.builder.build_int_mul(len, self.i64_type.const_int(4, false), "size_bytes");
        self.builder.build_call(memcpy, &[new_ptr.into(), old_data.into(), size_bytes.into()], "memcpy_old");
        // SIMD add (v4i32)
        let vec_ty = self.context.vector_type(self.i32_type, 4);
        let data_ptr = self.builder.build_pointer_cast(new_ptr, self.i32_type.ptr_type(inkwell::AddressSpace::Generic), "data_cast");
        let simd_load = self.builder.build_load(data_ptr, "simd_load").into_vector_value();
        let broadcast = self.builder.build_int_nsw_vector_splat(vec_ty.len(), scalar, "broadcast");
        let simd_add = self.builder.build_int_add(simd_load, broadcast, "simd_add");
        self.builder.build_store(data_ptr, simd_add);
        // Update len in new vec
        let new_vec_ptr = self.builder.build_pointer_cast(new_ptr, vec_ptr_type, "new_vec_cast");
        let new_len_ptr = self.builder.build_struct_gep(new_vec_ptr, 1, "new_len_ptr").unwrap();
        self.builder.build_store(new_len_ptr, new_len);
        self.builder.build_return(Some(&new_vec_ptr.into()));
        self.add_vec_fn = Some(add_vec_val);

        // malloc (external)
        // Already declared above

        // channel_send (external stub: returns i32 success)
        let send_type = self.i32_type.fn_type(&[self.i8ptr_type.into(), self.i32_type.into()], false);
        let send_val = self.module.add_function("channel_send", send_type, None);
        self.channel_send_fn = Some(send_val);

        // channel_poll (external: returns msg or -1)
        let poll_type = self.i32_type.fn_type(&[self.i8ptr_type.into()], false);
        let poll_val = self.module.add_function("channel_poll", poll_type, None);
        self.channel_poll_fn = Some(poll_val);

        // http_get (external: returns status i64)
        let http_type = self.i64_type.fn_type(&[self.i8ptr_type.into()], false);
        let http_val = self.module.add_function("std_net_http_get", http_type, None);
        self.http_get_fn = Some(http_val);

        // tls_connect (external)
        let tls_type = self.i32_type.fn_type(&[self.i8ptr_type.into()], false);
        let tls_val = self.module.add_function("std_tls_connect", tls_type, None);
        self.tls_connect_fn = Some(tls_val);

        // datetime_now (external: returns i64 timestamp)
        let now_type = self.i64_type.fn_type(&[], false);
        let now_val = self.module.add_function("std_datetime_now", now_type, None);
        self.datetime_now_fn = Some(now_val);

        // serde_json (external: parse/serialize i8* <-> i8*)
        let serde_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into()], false);
        let serde_val = self.module.add_function("std_serde_json_parse", serde_type, None);
        self.serde_json_fn = Some(serde_val);

        // rand_next (external: i64 RNG)
        let rand_type = self.i64_type.fn_type(&[], false);
        let rand_val = self.module.add_function("std_rand_next", rand_type, None);
        self.rand_next_fn = Some(rand_val);

        // log_trace (external: void log i8*)
        let log_type = self.i8_type.fn_type(&[self.i8ptr_type.into()], false);
        let log_val = self.module.add_function("std_log_trace", log_type, None);
        self.log_trace_fn = Some(log_val);

        // governor_permit (external: bool rate limit)
        let gov_type = self.i32_type.fn_type(&[], false);
        let gov_val = self.module.add_function("std_governor_permit", gov_type, None);
        self.governor_permit_fn = Some(gov_val);

        // prometheus_inc (external: void metric i8*)
        let prom_type = self.i8_type.fn_type(&[self.i8ptr_type.into()], false);
        let prom_val = self.module.add_function("std_prometheus_inc", prom_type, None);
        self.prometheus_inc_fn = Some(prom_val);

        // copy (for Copy derive: memcpy)
        let copy_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i64_type.into()], false);
        let copy_val = self.module.add_function("copy_mem", copy_type, None);
        self.copy_fn = Some(copy_val);
    }

    fn gen_value<'a>(&mut self, node: &AstNode, resolver: &Resolver) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Lit(n) => Some(self.i64_type.const_int(*n as u64, false).into()),
            AstNode::Var(v) => {
                if let Some(ptr) = self.locals.get(v) {
                    Some(self.builder.build_load(*ptr, v).into_int_value().into())
                } else {
                    None
                }
            }
            AstNode::Call { receiver, method, args } => {
                let recv_val = self.gen_value(&AstNode::Var(receiver.clone()), resolver)?;
                let mut arg_vals = vec![recv_val];
                for arg in args {
                    if let Some(val) = self.gen_value(&AstNode::Var(arg.clone()), resolver) {
                        arg_vals.push(val);
                    }
                }
                // Resolve method: intrinsic or genned
                let func_name = format!("{method}");
                if let Some(f) = self.module.get_function(&func_name) {
                    let call = self.builder.build_call(f, &arg_vals, "method_call");
                    call.try_as_basic_value().left()
                } else if method == "add" && receiver == "i32" {
                    if let (Some(x), Some(y)) = (arg_vals[0].into_int_value(), arg_vals[1].into_int_value()) {
                        let res = self.builder.build_int_add(x, y, "add_res");
                        Some(res.into())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            AstNode::TimingOwned { ty: _, inner } => {
                let inner_val = self.gen_value(inner, resolver)?;
                // Constant-time XOR erase: XOR with random (stub: const 0 for now)
                if let Some(iv) = inner_val.into_int_value() {
                    let zero = self.i32_type.const_int(0, false);
                    let erased = self.builder.build_int_xor(iv, zero, "erased");
                    Some(erased.into())
                } else {
                    Some(inner_val)
                }
            }
            _ => None,
        }
    }

    pub fn gen_func(&mut self, ast: &AstNode, resolver: &Resolver, param_map: &mut HashMap<String, u32>) {
        if let AstNode::FuncDef { name, params, body, ret, attrs, .. } = ast {
            let mut param_types: Vec<BasicTypeEnum<'ctx>> = vec![];
            for (_, pty) in params.iter() {
                if pty == "i32" {
                    param_types.push(self.i32_type.into());
                } else if pty == "i64" {
                    param_types.push(self.i64_type.into());
                } else {
                    param_types.push(self.i8ptr_type.into()); // Default ptr
                }
            }
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            // MLGO metadata
            if attrs.contains(&"ai_opt".to_string()) {
                if let Some(vec_meta) = &self.mlgo_vectorize_meta {
                    fn_val.add_metadata(vec_meta, 0);
                }
                if let Some(branch_meta) = &self.mlgo_branch_meta {
                    fn_val.add_metadata(branch_meta, 1);
                }
                // xAI integration: Query for opt suggestion
                if let Some(client) = &self.xai_client {
                    if let Ok(suggestion) = client.optimize_codegen(&format!("{:?}", ast)) {
                        // Stub: Apply suggestion (e.g., inline hint)
                        println!("xAI Opt Suggestion: {}", suggestion);
                    }
                }
            }

            // Params allocas
            let mut param_idx = 0;
            self.locals.clear();
            for (pname, pty) in params {
                let param_val = fn_val.get_nth_param(param_idx).unwrap();
                let alloca = self.builder.build_alloca(param_val.get_type(), pname);
                self.builder.build_store(alloca, param_val);
                self.locals.insert(pname.clone(), alloca);
                param_idx += 1;
            }

            // Body stmts
            for node in body {
                match node {
                    AstNode::Assign(vname, expr) => {
                        let val = self.gen_value(expr, resolver).and_then(|v| v.into_int_value().into());
                        if let Some(val) = val {
                            let alloca = self.builder.build_alloca(self.i32_type, vname);
                            self.builder.build_store(alloca, val);
                            self.locals.insert(vname.clone(), alloca);
                        }
                    }
                    AstNode::Call { .. } => {
                        let _ = self.gen_value(node, resolver);
                    }
                    AstNode::TimingOwned { .. } => {
                        let _ = self.gen_value(node, resolver);
                    }
                    AstNode::Var(_) | AstNode::Lit(_) => {} // No-op
                    _ => {}
                }
            }

            // Return last val or const
            let ret_val = self.i32_type.const_int(42, false);
            self.builder.build_return(Some(&ret_val.into()));
        }
    }

    pub fn gen_actor(&mut self, ast: &AstNode) {
        if let AstNode::ActorDef { name, methods } = ast {
            // Actor struct: { queue: i8*, state: i32 }
            let actor_type = self.context.struct_type(&[self.i8ptr_type.into(), self.i32_type.into()], false);
            let actor_ptr_type = actor_type.ptr_type(inkwell::AddressSpace::Generic);

            // Handler fn
            let handler_type = self.i32_type.fn_type(&[actor_ptr_type.into()], false);
            let handler = self.module.add_function(&format!("{name}_handler"), handler_type, None);
            let entry = self.context.append_basic_block(handler, "entry");
            self.builder.position_at_end(entry);
            let loop_bb = self.context.append_basic_block(handler, "loop");
            self.builder.build_unconditional_branch(loop_bb);
            self.builder.position_at_end(loop_bb);
            let self_arg = handler.get_nth_param(0).unwrap().into_pointer_value();
            let queue_ptr = self.builder.build_struct_gep(self_arg, 0, "queue").unwrap();
            let inc_type = self.i32_type.fn_type(&[actor_ptr_type.into(), self.i32_type.into()], false);
            let inc_fn = self.module.add_function("increment", inc_type, None);
            if let Some(poll) = &self.channel_poll_fn {
                let poll_res = self.builder.build_call(*poll, &[queue_ptr.into()], "poll_msg");
                let poll_val = poll_res.try_as_basic_value().left().unwrap().into_int_value();
                let neg_one = self.i32_type.const_int(-1, false);
                let has_msg = self.builder.build_int_compare(inkwell::IntPredicate::NE, poll_val, neg_one, "has_msg");
                let then_bb = self.context.append_basic_block(handler, "then");
                let else_bb = self.context.append_basic_block(handler, "else");
                self.builder.build_conditional_branch(has_msg, then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                // Call method: e.g., increment
                let inc_call = self.builder.build_call(inc_fn, &[self_arg.into(), poll_val.into()], "inc_call");
                let inc_res = inc_call.try_as_basic_value().left().unwrap().into_int_value();
                let err = self.builder.build_int_compare(inkwell::IntPredicate::EQ, inc_res, neg_one, "inc_err");
                let poison_bb = self.context.append_basic_block(handler, "poison");
                self.builder.build_conditional_branch(err, poison_bb, loop_bb);
                self.builder.position_at_end(poison_bb);
                let state_ptr = self.builder.build_struct_gep(self_arg, 1, "state").unwrap();
                self.builder.build_store(state_ptr, self.i32_type.const_int(-999, false));
                self.builder.build_return(Some(&self.i32_type.const_int(-1, false).into()));
                self.builder.position_at_end(else_bb);
                self.builder.build_br(loop_bb);
            }
            self.builder.build_return(Some(&self.i32_type.const_int(0, false).into()));

            // MLGO on handler
            if let Some(branch_meta) = &self.mlgo_branch_meta {
                handler.add_metadata(branch_meta.clone(), 0);
            }

            // Gen methods, e.g., increment
            let inc_entry = self.context.append_basic_block(inc_fn, "entry");
            self.builder.position_at_end(inc_entry);
            let inc_self = inc_fn.get_nth_param(0).unwrap().into_pointer_value();
            let delta = inc_fn.get_nth_param(1).unwrap().into_int_value();
            let state_ptr = self.builder.build_struct_gep(inc_self, 1, "state").unwrap();
            let state = self.builder.build_load(state_ptr, "state").into_int_value();
            let new_state = self.builder.build_int_add(state, delta, "new_state");
            self.builder.build_store(state_ptr, new_state);
            self.builder.build_return(Some(&new_state.into()));
        }
    }

    pub fn jit_warmup(&mut self) -> Result<(), Box<dyn Error>> {
        if let Some(ee) = &self.execution_engine {
            type DummyFn = unsafe extern "C" fn() -> i32;
            if let Some(dummy) = ee.get_function::<DummyFn>("main") {
                unsafe { dummy.call(); }
            }
            thread::sleep(Duration::from_millis(1));
        }
        Ok(())
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        self.jit_warmup()?;
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>> where F: 'static + Copy + num_traits::ToPrimitive {
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
            AstNode::Derive { ty, traits } => {
                if traits.contains(&"Copy".to_string()) {
                    if let Some(copy) = &codegen.copy_fn {
                        let size = codegen.i64_type.const_int(4, false); // i32 size
                        let src = codegen.builder.build_alloca(codegen.i32_type, "src");
                        let dst = codegen.builder.build_alloca(codegen.i32_type, "dst");
                        codegen.builder.build_call(*copy, &[dst.into(), src.into(), size.into()], "derive_copy");
                    }
                }
            }
            _ => {}
        }
    }
    let mut param_map = HashMap::new();
    for ast in asts.iter().filter(|a| matches!(a, AstNode::FuncDef { .. })) {
        codegen.gen_func(ast, &resolver, &mut param_map);
    }
    let ee = codegen.finalize_and_jit()?;

    unsafe {
        type UseVecAddFn = unsafe extern "C" fn() -> i32;
        if let Some(use_add) = codegen.get_fn::<UseVecAddFn>("use_vec_add") {
            Ok(use_add.call())
        } else {
            Ok(0)
        }
    }
}
