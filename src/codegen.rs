// src/codegen.rs
use crate::ast::AstNode;
use crate::parser::parse_zeta;
use crate::resolver::Resolver;
use crate::xai::XAIClient;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum, StructType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, MetadataValue, PointerValue};
use inkwell::AddressSpace;
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
        let i8ptr_type = context.ptr_type(AddressSpace::default());
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
        let tbaa_id = self.context.metadata_node(&[self.i32_type.const_int(0, false).into()]);
        let tbaa_node = self.context.metadata_node(&[self.i32_type.const_int(1, false).into()]);
        let tbaa_md = self.context.metadata_node(&[tbaa_id.into(), tbaa_node.into()]);
        self.tbaa_root = Some(tbaa_md);

        // AI opt metadata
        let ai_hint = self.context.metadata_node(&[self.i32_type.const_int(1, false).into()]);
        self.ai_opt_meta = Some(ai_hint);

        // MLGO vectorize metadata
        let vec_hint = self.context.metadata_node(&[self.i32_type.const_int(4, false).into()]);
        self.mlgo_vectorize_meta = Some(vec_hint);

        // MLGO branch metadata
        let branch_hint = self.context.metadata_node(&[self.i32_type.const_int(1, false).into()]);
        self.mlgo_branch_meta = Some(branch_hint);

        // add_i32 intrinsic
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();
        let ret = self.builder.build_int_add(x, y, "sum");
        self.builder.build_return(Some(&ret));
        self.add_i32_fn = Some(fn_val);

        // add_vec_i32 with SIMD
        let vec_ptr_type = self.context.ptr_type(AddressSpace::default());
        let add_vec_type = vec_ptr_type.fn_type(&[vec_ptr_type.into(), self.i32_type.into()], false);
        let add_vec_val = self.module.add_function("add_vec_i32", add_vec_type, None);
        let entry = self.context.append_basic_block(add_vec_val, "entry");
        self.builder.position_at_end(entry);
        let vec_arg = add_vec_val.get_nth_param(0).unwrap().into_pointer_value();
        let scalar = add_vec_val.get_nth_param(1).unwrap().into_int_value();
        let len_ptr = self.builder.build_struct_gep(vec_arg, self.i32_type, 1, "len").unwrap();
        let len = self.builder.build_load(len_ptr, "load_len").unwrap().into_int_value();
        let old_ptr = self.builder.build_struct_gep(vec_arg, self.i8ptr_type, 0, "old_ptr").unwrap();
        let old_data = self.builder.build_load(old_ptr, "old_data").unwrap().into_pointer_value();
        let size = self.builder.build_int_mul(scalar, len, "size");
        let new_size = self.builder.build_int_add(size, self.i64_type.const_int(1, false), "new_size");
        let malloc_val = self.module.add_function("malloc", self.i8ptr_type.fn_type(&[self.i64_type.into()], false), None);
        let new_ptr = self.builder.build_call(malloc_val, &[new_size.into()], "new_ptr").unwrap().try_as_basic_value().left().unwrap().into_pointer_value();
        // Stub memcpy
        let copy_val = self.module.add_function("memcpy", self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i64_type.into()], false), None);
        self.builder.build_call(copy_val, &[new_ptr.into(), old_data.into(), size.into()], "copy");
        // SIMD add
        let vec_ty = self.context.vector_type(self.i32_type.into(), 4);
        let data_ptr = self.builder.build_pointer_cast(new_ptr, self.context.ptr_type(AddressSpace::default()), "data_ptr").unwrap();
        let simd_load = self.builder.build_load(data_ptr, "simd_load").unwrap().into_vector_value();
        let broadcast = self.builder.build_int_splat(vec_ty, scalar, "broadcast").unwrap();
        let simd_add = self.builder.build_int_add(simd_load, broadcast, "simd_add").unwrap();
        self.builder.build_store(data_ptr, simd_add);
        let new_vec_ptr = self.builder.build_pointer_cast(new_ptr, vec_ptr_type, "new_vec").unwrap();
        let new_len_ptr = self.builder.build_struct_gep(new_vec_ptr, self.i32_type, 1, "new_len_ptr").unwrap();
        self.builder.build_store(new_len_ptr, len);
        self.builder.build_return(Some(&new_vec_ptr.into()));
        self.add_vec_fn = Some(add_vec_val);
    }

    pub fn gen_actor(&mut self, ast: &AstNode) {
        if let AstNode::ActorDef { name, methods } = ast {
            let actor_type = self.context.struct_type(&[self.i8ptr_type.into(), self.i32_type.into()], false); // queue, state
            let actor_ptr_type = self.context.ptr_type(AddressSpace::default());
            let handler_type = actor_ptr_type.fn_type(&[actor_ptr_type.into()], false);
            let handler = self.module.add_function(&format!("{}_handler", name), handler_type, None);
            let entry = self.context.append_basic_block(handler, "entry");
            self.builder.position_at_end(entry);
            let self_arg = handler.get_nth_param(0).unwrap().into_pointer_value();
            let loop_bb = self.context.append_basic_block(handler, "loop");
            self.builder.build_unconditional_branch(loop_bb);
            self.builder.position_at_end(loop_bb);
            let queue_ptr = self.builder.build_struct_gep(self_arg, self.i8ptr_type, 0, "queue").unwrap();
            let poll = self.channel_poll_fn.unwrap();
            let poll_res = self.builder.build_call(poll, &[queue_ptr.into()], "poll_msg").unwrap();
            let poll_val = poll_res.try_as_basic_value().left().unwrap().into_int_value();
            let neg_one = self.i32_type.const_all_ones();
            let has_msg = self.builder.build_int_compare(inkwell::IntPredicate::NE, poll_val, neg_one, "has_msg");
            let then_bb = self.context.append_basic_block(handler, "then");
            let else_bb = self.context.append_basic_block(handler, "else");
            self.builder.build_conditional_branch(has_msg, then_bb, else_bb);
            self.builder.position_at_end(then_bb);
            // Call method: e.g., increment
            let inc_fn = self.module.add_function("increment", self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false), None);
            let inc_call = self.builder.build_call(inc_fn, &[self_arg.into(), poll_val.into()], "inc_call").unwrap();
            let inc_res = inc_call.try_as_basic_value().left().unwrap().into_int_value();
            let err = self.builder.build_int_compare(inkwell::IntPredicate::EQ, inc_res, neg_one, "inc_err");
            let poison_bb = self.context.append_basic_block(handler, "poison");
            self.builder.build_conditional_branch(err, poison_bb, loop_bb);
            self.builder.position_at_end(poison_bb);
            let state_ptr = self.builder.build_struct_gep(self_arg, self.i32_type, 1, "state").unwrap();
            self.builder.build_store(state_ptr, self.i32_type.const_int_from_string("-999", 10).unwrap());
            self.builder.build_return(Some(&self.i32_type.const_all_ones().into()));
            self.builder.position_at_end(else_bb);
            self.builder.build_unconditional_branch(loop_bb);
            self.builder.position_at_end(entry);
            self.builder.build_return(Some(&self.i32_type.const_int(0, false).into()));

            // MLGO on handler
            if let Some(branch_meta) = &self.mlgo_branch_meta {
                entry.get_first_instruction().unwrap().set_metadata(branch_meta.clone(), 0);
            }

            // Gen methods, e.g., increment
            let inc_entry = self.context.append_basic_block(inc_fn, "entry");
            self.builder.position_at_end(inc_entry);
            let inc_self = inc_fn.get_nth_param(0).unwrap().into_pointer_value();
            let delta = inc_fn.get_nth_param(1).unwrap().into_int_value();
            let state_ptr = self.builder.build_struct_gep(inc_self, self.i32_type, 1, "state").unwrap();
            let state = self.builder.build_load(state_ptr, "state").unwrap().into_int_value();
            let new_state = self.builder.build_int_add(state, delta, "new_state");
            self.builder.build_store(state_ptr, new_state);
            self.builder.build_return(Some(&new_state));
        }
    }

    pub fn jit_warmup(&mut self) -> Result<(), Box<dyn Error>> {
        if let Some(ee) = &self.execution_engine {
            type DummyFn = unsafe extern "C" fn() -> i32;
            if let Ok(dummy) = ee.get_function::<DummyFn>("main") {
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
        F: 'static + Copy,
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
                        codegen.builder.build_call(
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
        if let Ok(use_add) = ee.get_function::<UseVecAddFn>("use_vec_add") {
            Ok(use_add.call())
        } else {
            Ok(0)
        }
    }
}
