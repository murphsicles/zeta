use crate::ast::AstNode;
use crate::parser::parse_zeta;
use crate::resolver::Resolver;
use crate::xai::XAIClient;
use inkwell::OptimizationLevel;
use inkwell::address_space::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, UnsafeFunctionPointer};
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::types::StructType;
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, IntValue, PointerValue,
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
    tbaa_root: Option<inkwell::metadata::MetadataValue<'ctx>>,
    abi_version: Option<String>,
    ai_opt_meta: Option<inkwell::metadata::MetadataValue<'ctx>>,
    copy_fn: Option<FunctionValue<'ctx>>,
    mlgo_vectorize_meta: Option<inkwell::metadata::MetadataValue<'ctx>>,
    mlgo_branch_meta: Option<inkwell::metadata::MetadataValue<'ctx>>,
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
        let vec_type = context.opaque_struct_type("vec_i32");
        let mut xai_client = None;
        if XAIClient::new().is_ok() {
            xai_client = XAIClient::new().ok();
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
        // TBAA root
        let tbaa_root = self.context.metadata_node(&[]);
        self.tbaa_root = Some(tbaa_root);

        // MLGO hints
        self.mlgo_vectorize_meta = Some(self.context.metadata_node(&[self.i32_type.const_int(1, false).into()]));
        self.mlgo_branch_meta = Some(self.context.metadata_node(&[self.i32_type.const_int(1, false).into()]));

        // add_i32
        let fn_type = self.i32_type.fn_type(&[self.i32_type.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        let x = fn_val.get_nth_param(0).unwrap().into_int_value();
        let y = fn_val.get_nth_param(1).unwrap().into_int_value();
        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        self.builder.build_return(Some(&sum)).unwrap();
        self.add_i32_fn = Some(fn_val);

        // add_vec_i32 stub (SIMD-ready)
        let vec_ptr = self.i8ptr_type;
        let fn_type = vec_ptr.fn_type(&[vec_ptr.into(), self.i32_type.into()], false);
        let fn_val = self.module.add_function("add_vec_i32", fn_type, None);
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.builder.build_return(None).unwrap();
        self.add_vec_fn = Some(fn_val);
    }

    pub fn gen_func(
        &mut self,
        ast: &AstNode,
        _resolver: &Resolver,
        locals: &mut HashMap<String, PointerValue<'ctx>>,
    ) {
        if let AstNode::FuncDef { name, params, body, .. } = ast {
            let param_types: Vec<_> = params.iter().map(|_| self.i32_type.into()).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let fn_val = self.module.add_function(name, fn_type, None);
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);

            for (i, (pname, _)) in params.iter().enumerate() {
                let param = fn_val.get_nth_param(i as u32).unwrap();
                let alloca = self.builder.build_alloca(self.i32_type, pname).unwrap();
                self.builder.build_store(alloca, param).unwrap();
                locals.insert(pname.clone(), alloca);
            }

            for stmt in body {
                self.gen_stmt(stmt, locals);
            }

            let zero = self.i32_type.const_int(0, false);
            self.builder.build_return(Some(&zero)).unwrap();
        }
    }

    fn gen_stmt(&mut self, node: &AstNode, locals: &HashMap<String, PointerValue<'ctx>>) {
        if let AstNode::Call { receiver, method, args } = node {
            if method == "add" {
                if let Some(add_fn) = self.add_i32_fn {
                    let recv = self.load_local(receiver, locals);
                    let arg = if args.is_empty() { recv } else { self.load_local(&args[0], locals) };
                    let call = self.builder.build_call(add_fn, &[recv.into(), arg.into()], "addcall").unwrap();
                    if let Some(res) = call.try_as_basic_value().left() {
                        if let Some(store_ptr) = locals.get(receiver) {
                            self.builder.build_store(*store_ptr, res).unwrap();
                        }
                    }
                }
            }
        }
    }

    fn load_local(&self, name: &str, locals: &HashMap<String, PointerValue<'ctx>>) -> IntValue<'ctx> {
        let ptr = locals.get(name).copied().unwrap_or_else(|| {
            self.builder.build_alloca(self.i32_type, name).unwrap()
        });
        self.builder.build_load(self.i32_type, ptr, name).unwrap().into_int_value()
    }

    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn Error>> {
        self.module.verify().map_err(|e| e.to_string())?;

        let pass_builder = PassManagerBuilder::create();
        pass_builder.set_optimization_level(OptimizationLevel::Aggressive);
        let fpm = PassManager::create(());
        pass_builder.populate_module_pass_manager(&fpm);
        fpm.run_on(&self.module);

        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        self.execution_engine = Some(ee.clone());
        Ok(ee)
    }

    pub fn get_fn<F>(&self, name: &str) -> Option<JitFunction<F>>
    where
        F: UnsafeFunctionPointer,
    {
        self.execution_engine.as_ref()?.get_function(name).ok()
    }
}

pub fn compile_and_run_zeta(input: &str) -> Result<i32, Box<dyn Error + 'static>> {
    let (_, asts) = parse_zeta(input)?;
    let resolver = Resolver::new(); // registration stubbed for now

    let context = Context::create();
    let mut codegen = LLVMCodegen::new(&context, "zeta");
    codegen.gen_intrinsics();

    let mut locals = HashMap::new();
    for ast in asts.iter().filter(|a| matches!(a, AstNode::FuncDef { .. })) {
        codegen.gen_func(ast, &resolver, &mut locals);
    }

    let ee = codegen.finalize_and_jit()?;

    type MainFn = unsafe extern "C" fn() -> i32;
    unsafe {
        if let Ok(main) = ee.get_function::<MainFn>("main") {
            Ok(main.call())
        } else {
            Ok(0)
        }
    }
}
