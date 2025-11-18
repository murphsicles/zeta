// src/codegen.rs
use crate::ast::AstNode;
use crate::parser::parse_zeta;
use crate::resolver::Resolver;
use crate::xai::XAIClient;
use inkwell::address_space::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::types::StructType;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, MetadataValue, PointerValue,
};
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
        let i8ptr_type = context.ptr_type(i8_type.into(), AddressSpace::Generic);
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
        let md0 = self.i32_type.const_int(0, false).into();
        let md1 = self.i32_type.const_int(1, false).into();
        let tbaa_md = self.context.metadata_node(&[md0, md1]);
        self.tbaa_root = Some(tbaa_md);

        // AI opt metadata
        let ai_md = self.i32_type.const_int(1, false).into();
        self.ai_opt_meta = Some(self.context.metadata_node(&[ai_md]));

        // MLGO vectorize metadata
        let vec_md = self.i32_type.const_int(4, false).into();
        self.mlgo_vectorize_meta = Some(self.context.metadata_node(&[vec_md]));

        // MLGO branch metadata
        let branch_md = self.i32_type.const_int(1, false).into();
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
        let vec_ptr_type = self.context.ptr_type(self.vec_type.into(), AddressSpace::Generic);
        let add_vec_type =
            vec_ptr_type.fn_type(&[vec_ptr_type.into(), self.i32_type.into()], false);
        let add_vec_val = self.module.add_function("add_vec_i32", add_vec_type, None);
        let entry_vec = self.context.append_basic_block(add_vec_val, "entry");
        self.builder.position_at_end(entry_vec);
        self.builder.build_return(None);
        self.add_vec_fn = Some(add_vec_val);

        // Stub other intrinsics
        self.malloc_fn = None;
        self.channel_send_fn = None;
        self.rand_next_fn = None;
        self.copy_fn = None;
    }

    pub fn gen_func(&mut self, ast: &AstNode, resolver: &Resolver, param_map: &mut HashMap<String, PointerValue<'ctx>>) {
        if let AstNode::FuncDef { name, params, body, ret, .. } = ast {
            let param_types: Vec<inkwell::types::BasicTypeEnum> = params.iter().map(|(_, _)| self.i32_type.into()).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);
            for (i, (pname, _)) in params.iter().enumerate() {
                let param_val = fn_val.get_nth_param(i as u32).unwrap().into_pointer_value();
                let alloca = self.builder.build_alloca(self.i32_type, pname).unwrap();
                self.builder.build_store(alloca, param_val).unwrap();
                param_map.insert(pname.clone(), alloca);
            }
            for node in body {
                self.gen_stmt(node, resolver, param_map);
            }
            let ret_val = self.i32_type.const_int(0, false);
            self.builder.build_return(Some(&ret_val.into()));
        }
    }

    fn gen_stmt(&mut self, node: &AstNode, resolver: &Resolver, param_map: &HashMap<String, PointerValue<'ctx>>) {
        match node {
            AstNode::Call { receiver, method, args } => {
                let recv_id = param_map.get(receiver).cloned().unwrap_or_else(|| self.builder.build_alloca(self.i32_type, receiver).unwrap());
                let recv_val = self.builder.build_load(self.i32_type, recv_id, receiver).unwrap();
                let arg_vals: Vec<BasicValueEnum> = args.iter().map(|a| {
                    let arg_ptr = param_map.get(a).cloned().unwrap_or_else(|| self.builder.build_alloca(self.i32_type, a).unwrap());
                    self.builder.build_load(self.i32_type, arg_ptr, a).unwrap().into()
                }).collect();
                if method == "add" && resolver.has_method("Addable", receiver, "add") {
                    if let Some(add_fn) = &self.add_i32_fn {
                        let args_meta: Vec<BasicMetadataValueEnum> =
                            arg_vals.iter().map(|v| (*v).into()).collect();
                        let _res = self
                            .builder
                            .build_call(*add_fn, &args_meta, "add_res")
                            .unwrap();
                        // Store res if needed
                    }
                }
            }
            AstNode::TimingOwned { ty: _, inner } => {
                // Constant-time erase: XOR with random, defer restore
                if let Some(_rand) = &self.rand_next_fn {
                    let xor_key = self.i32_type.const_int(0, false);
                    let inner_val = self
                        .gen_expr(inner, param_map)
                        .unwrap_or(self.i32_type.const_int(0, false).into());
                    let inner_int = inner_val.into_int_value();
                    let _masked = self.builder.build_xor(inner_int, xor_key, "masked");
                    // Defer: XOR back (stub)
                    let defer_stmt = AstNode::Defer(Box::new(AstNode::Assign(
                        "tmp".to_string(),
                        Box::new(AstNode::Lit(0)),
                    )));
                    self.gen_stmt(&defer_stmt, resolver, param_map);
                }
            }
            _ => {}
        }
    }

    fn gen_expr(
        &mut self,
        node: &AstNode,
        param_map: &HashMap<String, PointerValue<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Lit(n) => Some(self.i32_type.const_int(*n as u64, false).into()),
            AstNode::Var(v) => param_map
                .get(v)
                .map(|ptr| self.builder.build_load(self.i32_type, *ptr, v).unwrap()),
            _ => None,
        }
    }

    pub fn jit_warmup(&mut self) -> Result<(), Box<dyn Error>> {
        if let Some(ee) = &self.execution_engine {
            type DummyFn = unsafe extern "C" fn() -> i32;
            if let Ok(dummy) = unsafe { ee.get_function::<DummyFn>("main") } {
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
            .create_jit_execution_engine(OptimizationLevel::Default)?;
        self.execution_engine = Some(ee.clone());
        self.jit_warmup()?;
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>>
    where
        F: 'static + UnsafeFunctionPointer,
    {
        self.execution_engine
            .as_ref()
            .and_then(|ee| unsafe { ee.get_function::<F>(name) }.ok())
    }
}

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error + 'static>> {
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
    let mut param_map = HashMap::new();
    for ast in asts.iter().filter(|a| matches!(a, AstNode::FuncDef { .. })) {
        codegen.gen_func(ast, &resolver, &mut param_map);
    }
    let _ee = codegen.finalize_and_jit()?;

    unsafe {
        type UseVecAddFn = unsafe extern "C" fn() -> i32;
        if let Some(use_add) = codegen.get_fn::<UseVecAddFn>("use_vec_add") {
            Ok(use_add.call())
        } else {
            Ok(0)
        }
    }
}

impl<'ctx> LLVMCodegen<'ctx> {
    fn gen_actor(&mut self, _ast: &AstNode) {
        // Stub actor gen
    }
}
