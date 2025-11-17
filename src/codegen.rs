// src/codegen.rs
use crate::ast::AstNode;
use crate::parser::parse_zeta;
use crate::resolver::Resolver;
use crate::xai::XAIClient;
use inkwell::module::Linkage;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, MetadataValue, PointerValue};
use std::collections::HashMap;
use std::error::Error;
use std::thread;
use std::time::Duration;
use num_traits::ToPrimitive;

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
        let i8ptr_type = context.ptr_type(i8_type.into());
        let f64_type = context.f64_type();
        let vec_type = context.struct_type(&[i8ptr_type.into(), i32_type.into()], false);
        let mut xai_client = None;
        if let Ok(client) = XAIClient::new() {
            xai_client = Some(client);
        }
        Self {
            context,
            module,
            builder,
            execution_engine: None,
            i32_type,
            i64_type,
            i8_type,
            i8ptr_type,
            f64_type,
            vec_type,
            add_i32_fn: None,
            add_vec_fn: None,
            malloc_fn: None,
            channel_send_fn: None,
            channel_poll_fn: None,
            http_get_fn: None,
            tls_connect_fn: None,
            datetime_now_fn: None,
            serde_json_fn: None,
            rand_next_fn: None,
            log_trace_fn: None,
            governor_permit_fn: None,
            prometheus_inc_fn: None,
            tbaa_root: None,
            abi_version: Some("1.0".to_string()),
            ai_opt_meta: None,
            copy_fn: None,
            mlgo_vectorize_meta: None,
            mlgo_branch_meta: None,
            locals: HashMap::new(),
            xai_client,
        }
    }

    pub fn gen_intrinsics(&mut self) {
        // TBAA metadata
        let md0 = self.i32_type.const_int(0, false).into().into();
        let md1 = self.i32_type.const_int(1, false).into().into();
        let tbaa_md = self.context.metadata_node(&[md0, md1]);
        self.tbaa_root = Some(tbaa_md);

        // AI opt metadata
        let ai_md = self.i32_type.const_int(1, false).into().into();
        self.ai_opt_meta = Some(self.context.metadata_node(&[ai_md]));

        // MLGO vectorize metadata
        let vec_md = self.i32_type.const_int(4, false).into().into();
        self.mlgo_vectorize_meta = Some(self.context.metadata_node(&[vec_md]));

        // MLGO branch metadata
        let branch_md = self.i32_type.const_int(1, false).into().into();
        self.mlgo_branch_meta = Some(self.context.metadata_node(&[branch_md]));

        // add_i32 intrinsic
        let fn_type = self
            .i32_type
            .fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();
        let ret = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&ret.into()));
        self.add_i32_fn = Some(fn_val);

        // add_vec_i32 with SIMD
        let vec_ptr_type = self.context.ptr_type(self.vec_type.into());
        let add_vec_type =
            vec_ptr_type.fn_type(&[vec_ptr_type.into(), self.i32_type.into()], false);
        let add_vec_val = self.module.add_function("add_vec_i32", add_vec_type, None);
        let entry = self.context.append_basic_block(add_vec_val, "entry");
        self.builder.position_at_end(entry);
        let vec_arg = add_vec_val.get_nth_param(0).unwrap().into_pointer_value();
        let scalar = add_vec_val.get_nth_param(1).unwrap().into_int_value();
        let len_ptr = self.builder.build_struct_gep(self.vec_type, vec_arg, 1, "len").unwrap();
        let len = self.builder.build_load(self.i32_type, len_ptr, "len").unwrap().into_int_value();
        let data_ptr = self.builder.build_struct_gep(self.vec_type, vec_arg, 0, "data").unwrap();
        let loop_bb = self.context.append_basic_block(add_vec_val, "loop");
        let body_bb = self.context.append_basic_block(add_vec_val, "body");
        let exit_bb = self.context.append_basic_block(add_vec_val, "exit");
        let zero = self.i32_type.const_int(0, false);
        let one = self.i32_type.const_int(1, false);
        let idx_ptr = self.builder.build_alloca(self.i32_type, "idx").unwrap();
        self.builder.build_store(idx_ptr, zero);
        self.builder.build_unconditional_branch(loop_bb);
        self.builder.position_at_end(loop_bb);
        let idx = self.builder.build_load(self.i32_type, idx_ptr, "idx").unwrap().into_int_value();
        let cond = self.builder.build_int_compare(
            inkwell::IntPredicate::ULT,
            idx,
            len,
            "cond",
        );
        self.builder.build_conditional_branch(cond, body_bb, exit_bb);
        self.builder.position_at_end(body_bb);
        // Stub body: increment idx
        let new_idx = self.builder.build_int_add(idx, one, "new_idx");
        self.builder.build_store(idx_ptr, new_idx);
        self.builder.build_unconditional_branch(loop_bb);
        self.builder.position_at_end(exit_bb);
        self.builder.build_return(Some(&vec_arg.into()));
        self.add_vec_fn = Some(add_vec_val);

        // malloc intrinsic (stub: use llvm.malloc)
        let malloc_type = self.i8ptr_type.fn_type(&[self.i64_type.into()], false);
        self.malloc_fn = Some(self.module.add_function("malloc", malloc_type, None));

        // channel_send (tokio stub)
        let chan_send_type = self.i32_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into()], false);
        self.channel_send_fn = Some(self.module.add_function("channel_send", chan_send_type, None));

        // channel_poll
        let chan_poll_type = self.i32_type.fn_type(&[self.i8ptr_type.into()], false);
        self.channel_poll_fn = Some(self.module.add_function("channel_poll", chan_poll_type, None));

        // std embeds: http_get (reqwest stub)
        let http_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into()], false);
        self.http_get_fn = Some(self.module.add_function("http_get", http_type, None));

        // tls_connect
        let tls_type = self.i32_type.fn_type(&[self.i8ptr_type.into()], false);
        self.tls_connect_fn = Some(self.module.add_function("tls_connect", tls_type, None));

        // datetime_now
        let now_type = self.i64_type.fn_type(&[], false);
        self.datetime_now_fn = Some(self.module.add_function("datetime_now", now_type, None));

        // serde_json (stub)
        let json_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into()], false);
        self.serde_json_fn = Some(self.module.add_function("serde_json", json_type, None));

        // rand_next
        let rand_type = self.i32_type.fn_type(&[], false);
        self.rand_next_fn = Some(self.module.add_function("rand_next", rand_type, None));

        // log_trace
        let log_type = self.void_type().fn_type(&[self.i8ptr_type.into()], false);
        self.log_trace_fn = Some(self.module.add_function("log_trace", log_type, None));

        // governor_permit
        let gov_type = self.i32_type.fn_type(&[], false);
        self.governor_permit_fn = Some(self.module.add_function("governor_permit", gov_type, None));

        // prometheus_inc
        let prom_type = self.void_type().fn_type(&[self.i8ptr_type.into()], false);
        self.prometheus_inc_fn = Some(self.module.add_function("prometheus_inc", prom_type, None));

        // copy (memcpy stub)
        let copy_type = self.void_type().fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i64_type.into()], false);
        self.copy_fn = Some(self.module.add_function("copy", copy_type, None));
    }

    fn void_type(&self) -> inkwell::types::VoidType<'ctx> {
        self.context.void_type()
    }

    pub fn gen_actor(&mut self, ast: &AstNode) {
        if let AstNode::ActorDef { name, methods } = ast {
            // Gen actor struct: vtable + state
            let actor_struct = self.context.struct_type(&[
                self.i8ptr_type.into(), // vtable ptr
                self.i32_type.into(),   // state (e.g., counter)
            ], false);
            let actor_ptr_type = self.context.ptr_type(actor_struct.into());

            // Vtable: methods as fns
            let vtable_type = self.i8ptr_type.array_type(8); // Stub: 8 methods max
            let vtable_struct = self.context.struct_type(&[vtable_type.into()], false);

            // Main actor fn: spawn handler loop
            let actor_fn_type = self.i32_type.fn_type(&[actor_ptr_type.into()], false);
            let actor_fn = self.module.add_function(&format!("actor_{}", name), actor_fn_type, None);
            let entry = self.context.append_basic_block(actor_fn, "entry");
            self.builder.position_at_end(entry);
            let self_arg = actor_fn.get_nth_param(0).unwrap().into_pointer_value();
            let queue_ptr = self.builder.build_alloca(self.i8ptr_type, "queue").unwrap(); // Channel queue
            self.builder.build_store(queue_ptr, self.i8ptr_type.const_null());

            // Method fn, e.g., increment
            let inc_fn_type = self.i32_type.fn_type(&[actor_ptr_type.into(), self.i32_type.into()], false);
            let inc_fn = self.module.add_function("increment", inc_fn_type, None);
            let inc_entry = self.context.append_basic_block(inc_fn, "entry");

            // Handler loop
            let handler = self.context.append_basic_block(actor_fn, "handler");
            let loop_bb = self.context.append_basic_block(actor_fn, "loop");
            self.builder.build_unconditional_branch(handler);
            self.builder.position_at_end(handler);
            self.builder.build_unconditional_branch(loop_bb);
            self.builder.position_at_end(loop_bb);

            // Poll channel
            if let Some(poll) = &self.channel_poll_fn {
                let poll_res = self.builder
                    .build_call(*poll, &[queue_ptr.into()], "poll_msg")
                    .unwrap();
                let poll_val = poll_res.try_as_basic_value().left().unwrap().into_int_value();
                let neg_one = self.i32_type.const_int(4294967295u64, false);
                let has_msg = self.builder.build_int_compare(
                    inkwell::IntPredicate::NE,
                    poll_val,
                    neg_one,
                    "has_msg",
                );
                let then_bb = self.context.append_basic_block(actor_fn, "then");
                let else_bb = self.context.append_basic_block(actor_fn, "else");
                self.builder
                    .build_conditional_branch(has_msg, then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                // Call method: e.g., increment
                let inc_call = self.builder.build_call(
                    inc_fn,
                    &[self_arg.into(), poll_val.into()],
                    "inc_call",
                ).unwrap();
                let inc_res = inc_call.try_as_basic_value().left().unwrap().into_int_value();
                let err = self.builder.build_int_compare(
                    inkwell::IntPredicate::EQ,
                    inc_res,
                    neg_one,
                    "inc_err",
                );
                let poison_bb = self.context.append_basic_block(actor_fn, "poison");
                self.builder
                    .build_conditional_branch(err, poison_bb, loop_bb);
                self.builder.position_at_end(poison_bb);
                let state_ptr = self.builder.build_struct_gep(actor_struct, self_arg, 1, "state").unwrap();
                self.builder
                    .build_store(state_ptr, self.i32_type.const_int(4294966297u64, false));
                self.builder
                    .build_return(Some(&self.i32_type.const_int(4294967295u64, false).into()));
                self.builder.position_at_end(else_bb);
                self.builder.build_unconditional_branch(loop_bb);
            }
            self.builder
                .build_return(Some(&self.i32_type.const_int(0, false).into()));

            // Gen methods body
            self.builder.position_at_end(inc_entry);
            let inc_self = inc_fn.get_nth_param(0).unwrap().into_pointer_value();
            let delta = inc_fn.get_nth_param(1).unwrap().into_int_value();
            let state_ptr = self.builder.build_struct_gep(actor_struct, inc_self, 1, "state").unwrap();
            let state = self.builder.build_load(self.i32_type, state_ptr, "state").unwrap().into_int_value();
            let new_state = self.builder.build_int_add(state, delta, "new_state");
            self.builder.build_store(state_ptr, new_state);
            self.builder.build_return(Some(&new_state.into()));
        }
    }

    pub fn gen_func(&mut self, ast: &AstNode, resolver: &Resolver, param_map: &mut HashMap<String, PointerValue<'ctx>>) {
        if let AstNode::FuncDef { name, params, ret, body, attrs, .. } = ast {
            let fn_type = self.i32_type.fn_type(
                &params.iter().map(|(_, _)| self.i32_type.into()).collect::<Vec<_>>(),
                false,
            );
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            // Params
            for ((pname, _), param) in params.iter().zip(fn_val.get_params()) {
                let ptr = self.builder.build_alloca(self.i32_type, pname).unwrap();
                self.builder.build_store(ptr, param);
                param_map.insert(pname.clone(), ptr);
            }

            // Body
            for node in body {
                self.gen_stmt(node, resolver, param_map);
            }

            // Ret
            self.builder.build_return(Some(&self.i32_type.const_int(0, false).into()));

            // AI opt metadata if #[ai_opt]
            if attrs.contains(&"ai_opt".to_string()) {
                if let Some(ai_meta) = &self.ai_opt_meta {
                    // entry.add_metadata(ai_meta.clone(), 0); // TODO: add to instruction
                }
            }

            // Stable ABI: extern "C" if #[stable_abi]
            if attrs.contains(&"stable_abi".to_string()) {
                fn_val.set_linkage(Linkage::External);
            }
        }
    }

    fn gen_stmt(&mut self, node: &AstNode, resolver: &Resolver, param_map: &mut HashMap<String, PointerValue<'ctx>>) {
        match node {
            AstNode::Call { method, receiver, args } => {
                if let Some(recv_ptr) = param_map.get(receiver) {
                    let recv = self.builder.build_load(self.i32_type, *recv_ptr, receiver).unwrap();
                    let arg_vals: Vec<BasicValueEnum<'ctx>> = args.iter().map(|a| {
                        if let Some(ptr) = param_map.get(a) {
                            self.builder.build_load(self.i32_type, *ptr, a).unwrap().into()
                        } else {
                            self.i32_type.const_int(0, false).into()
                        }
                    }).collect();
                    if method == "add" && resolver.has_method("Addable", receiver, "add") {
                        if let Some(add_fn) = &self.add_i32_fn {
                            let args_meta: Vec<BasicMetadataValueEnum> = arg_vals.iter().map(|v| v.into()).collect();
                            let _res = self.builder.build_call(*add_fn, &args_meta, "add_res").unwrap();
                            // Store res if needed
                        }
                    }
                }
            }
            AstNode::TimingOwned { ty: _, inner } => {
                // Constant-time erase: XOR with random, defer restore
                if let Some(rand) = &self.rand_next_fn {
                    let xor_key_call = self.builder.build_call(*rand, &[], "xor_key").unwrap();
                    let xor_key = xor_key_call.try_as_basic_value().left().unwrap().into_int_value();
                    let inner_val = self.gen_expr(inner, param_map).unwrap_or(self.i32_type.const_int(0, false).into());
                    let inner_int = inner_val.into_int_value();
                    let masked = self.builder.build_xor(inner_int, xor_key, "masked");
                    // Defer: XOR back (stub)
                    let defer_stmt = AstNode::Defer(Box::new(AstNode::Assign("tmp".to_string(), Box::new(AstNode::Lit(0)))));
                    self.gen_stmt(&defer_stmt, resolver, param_map);
                }
            }
            _ => {}
        }
    }

    fn gen_expr(&mut self, node: &AstNode, param_map: &HashMap<String, PointerValue<'ctx>>) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Lit(n) => Some(self.i32_type.const_int(*n as u64, false).into()),
            AstNode::Var(v) => param_map.get(v).map(|ptr| self.builder.build_load(self.i32_type, *ptr, v).unwrap()),
            _ => None,
        }
    }

    pub fn jit_warmup(&mut self) -> Result<(), Box<dyn Error>> {
        if let Some(ee) = &self.execution_engine {
            type DummyFn = unsafe extern "C" fn() -> i32;
            if let Some(dummy) = ee.get_function::<DummyFn>("main") {
                unsafe {
                    dummy.call();
                }
            }
            thread::sleep(Duration::from_millis(1));
        }
        Ok(())
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;
        let ee = self
            .module
            .create_jit_execution_engine(OptimizationLevel::Default)?; // Reduced for faster CI
        self.execution_engine = Some(ee.clone());
        self.jit_warmup()?;
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>>
    where
        F: 'static + Copy + ToPrimitive,
    {
        self.execution_engine
            .as_ref()
            .and_then(|ee| ee.get_function(name).ok())
    }
}

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error>> {
    let (_, asts) = parse_zeta(input)?;
    let mut resolver = Resolver::new();
    for ast in &asts {
        resolver.register(ast.clone());
    }
    if !resolver.typecheck(&asts) {
        return Err("Typecheck failed".into());
    }

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
                        let src = codegen.builder.build_alloca(codegen.i32_type, "src").unwrap();
                        let dst = codegen.builder.build_alloca(codegen.i32_type, "dst").unwrap();
                        let _ = codegen.builder.build_call(
                            *copy,
                            &[dst.into(), src.into(), size.into()],
                            "derive_copy",
                        ).unwrap();
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
