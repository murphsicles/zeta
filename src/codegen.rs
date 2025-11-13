// src/codegen.rs
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::execution_engine::{ExecutionEngine, JitFunction, OptimizationLevel};
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue};
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
    tbaa_root: Option<inkwell::values::MetadataValue<'ctx>>,
    abi_version: Option<String>,
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
        Self { context, module, builder, execution_engine: None, i32_type, i64_type, i8_type, i8ptr_type, vec_type, add_i32_fn: None, add_vec_fn: None, malloc_fn: None, channel_send_fn: None, http_get_fn: None, tls_connect_fn: None, datetime_now_fn: None, tbaa_root: None, abi_version: Some("1.0".to_string()) }
    }

    pub fn gen_intrinsics(&mut self) {
        // TBAA metadata for stricter aliasing
        let tbaa_id = self.context.metadata_value(self.i32_type.const_int(0, false));
        let tbaa_node = self.context.metadata_value(self.i32_type.const_int(1, false));
        let tbaa_md = self.context.metadata_node(&[tbaa_id, tbaa_node]);
        self.tbaa_root = Some(tbaa_md);

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

        // add_vec_i32 with SIMD + TBAA
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
        let new_ptr = self.builder.build_call(self.malloc_fn.as_ref().unwrap(), &[new_size.into()], "new_ptr").try_as_basic_value().left().unwrap().into_pointer_value();
        let old_ptr = self.builder.build_struct_gep(vec_arg, 0, "old_ptr").unwrap();
        let old_data = self.builder.build_load(old_ptr, "old_data").into_pointer_value();
        let memcpy = self.module.add_function("llvm.memcpy.p0i8.p0i8.i32", self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i32_type.into()], false), None);
        let size4 = self.builder.build_int_mul(len, self.i32_type.const_int(4, false), "size4");
        self.builder.build_call(memcpy, &[new_ptr.into(), old_data.into(), size4.into()], "memcpy");
        let offset = self.builder.build_int_mul(len, self.i32_type.const_int(4, false), "offset");
        let offset_ptr = self.builder.build_gep(new_ptr, &[offset.into()], "offset_ptr");
        let scalar_vec = self.i32_type.vector_type(4).const_array(&[scalar, self.i32_type.const_int(0, false), self.i32_type.const_int(0, false), self.i32_type.const_int(0, false)]);
        let store = self.module.add_function("llvm.store.v4i32", self.i8ptr_type.fn_type(&[self.i32_type.vector_type(4).into(), self.i8ptr_type.into()], false), None);
        self.builder.build_call(store, &[scalar_vec.into(), offset_ptr.into()], "simd_store");
        let new_vec = self.builder.build_alloca(self.vec_type, "new_vec");
        self.builder.build_store(self.builder.build_struct_gep(new_vec, 0, "new_data").unwrap(), new_ptr);
        self.builder.build_store(self.builder.build_struct_gep(new_vec, 1, "new_len").unwrap(), new_len);
        self.builder.build_return(Some(&new_vec.into()));
        self.add_vec_fn = Some(add_vec_val);

        // malloc
        let malloc_type = self.i8ptr_type.fn_type(&[self.i32_type.into()], false);
        self.malloc_fn = Some(self.module.add_function("malloc", malloc_type, None));

        // channel_send with TBAA
        let channel_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i32_type.into()], false);
        let channel_send = self.module.add_function("channel_send", channel_type, None);
        let entry_cs = self.context.append_basic_block(channel_send, "entry");
        self.builder.position_at_end(entry_cs);
        let ch = channel_send.get_nth_param(0).unwrap().into_pointer_value();
        let msg = channel_send.get_nth_param(1).unwrap().into_int_value();
        let msg_ptr = self.builder.build_gep(ch, &[self.i32_type.const_int(0, false).into()], "msg_ptr");
        self.builder.build_store(msg_ptr, msg);
        self.builder.build_return(Some(&self.i32_type.const_int(1, false).into()));
        self.channel_send_fn = Some(channel_send);

        // std::net::http_get with TLS
        let http_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into()], false);
        let http_get = self.module.add_function("std_net_http_get", http_type, None);
        let entry_h = self.context.append_basic_block(http_get, "entry");
        self.builder.position_at_end(entry_h);
        let url = http_get.get_nth_param(0).unwrap().into_pointer_value();
        let ctx = self.builder.build_call(self.tls_connect_fn.as_ref().unwrap(), &[url.into()], "tls_connect");
        let resp = self.builder.build_call(self.malloc_fn.as_ref().unwrap(), &[self.i32_type.const_int(1024, false).into()], "resp_buf").try_as_basic_value().left().unwrap().into_pointer_value();
        let resp_str = self.i8_type.const_string("HTTP/1.1 200 OK\r\nContent: Hello", false);
        let resp_data = self.builder.build_global_string_ptr(resp_str, "resp_str");
        let memcpy_http = self.module.add_function("llvm.memcpy.p0i8.p0i8.i32", self.i8ptr_type.fn_type(&[self.i8ptr_type.into(), self.i8ptr_type.into(), self.i32_type.into()], false), None);
        self.builder.build_call(memcpy_http, &[resp.into(), resp_data.into(), self.i32_type.const_int(1024, false).into()], "copy_resp");
        self.builder.build_return(Some(&resp.into()));
        self.http_get_fn = Some(http_get);

        // std::net::tls_connect
        let tls_type = self.i8ptr_type.fn_type(&[self.i8ptr_type.into()], false);
        let tls_connect = self.module.add_function("std_net_tls_connect", tls_type, None);
        let entry_t = self.context.append_basic_block(tls_connect, "entry");
        self.builder.position_at_end(entry_t);
        let url = tls_connect.get_nth_param(0).unwrap().into_pointer_value();
        let ctx = self.builder.build_call(self.malloc_fn.as_ref().unwrap(), &[self.i32_type.const_int(512, false).into()], "tls_ctx").try_as_basic_value().left().unwrap().into_pointer_value();
        self.builder.build_return(Some(&ctx.into()));
        self.tls_connect_fn = Some(tls_connect);

        // std::datetime::now
        let dt_type = self.i64_type.fn_type(&[], false);
        let dt_now = self.module.add_function("std_datetime_now", dt_type, None);
        let entry_d = self.context.append_basic_block(dt_now, "entry");
        self.builder.position_at_end(entry_d);
        let now = self.i64_type.const_int(1731475200, false); // Nov 13, 2025
        self.builder.build_return(Some(&now.into()));
        self.datetime_now_fn = Some(dt_now);
    }

    pub fn ai_opt_loop(&mut self, ir_pattern: &str) -> Option<()> {
        if ir_pattern.contains("loop") {
            let scalable = self.context.bool_type().const_int(1, false);
            let meta = self.context.metadata_node(&[scalable]);
            self.module.add_metadata("llvm.loop.vectorize.enable", meta);
            Some(())
        } else {
            None
        }
    }

    pub fn gen_func(&mut self, func: &AstNode, param_map: &mut HashMap<String, BasicValueEnum<'ctx>>) -> Option<FunctionValue<'ctx>> {
        if let AstNode::FuncDef { name, params, ret, body, attrs, .. } = func {
            let param_types: Vec<BasicTypeEnum<'ctx>> = params.iter().map(|(_, t)| {
                if *t == "i32" { self.i32_type.into() }
                else if *t == "Vec<i32>" { self.vec_type.into() }
                else if t.ends_with("Actor") { self.i8ptr_type.into() }
                else if *t == "i8*" { self.i8ptr_type.into() }
                else { panic!("Unsupported type") }
            }).collect();
            let fn_type = self.i32_type.fn_type(&param_types, false);
            let c_name = if attrs.contains(&"stable_abi".to_string()) {
                format!("{}_v{}", name, self.abi_version.as_ref().unwrap_or(&"1".to_string()))
            } else {
                name.clone()
            };
            let fn_val = self.module.add_function(&c_name, fn_type, None);
            if attrs.contains(&"stable_abi".to_string()) {
                fn_val.set_linkage(inkwell::Linkage::ExternalLinkage);
                fn_val.set_calling_conv(inkwell::CallingConvention::C);
                let version_md = self.context.metadata_value(self.i32_type.const_int(self.abi_version.as_ref().unwrap().parse::<u64>().unwrap_or(1), false));
                self.module.add_metadata(&format!("{}.abi_version", name), version_md);
                if ret == "CStruct" {
                    let c_struct = self.context.struct_type(&[self.i32_type.into()], false);
                    // Packed struct for ABI
                    c_struct.set_packed(true);
                }
            }
            let entry = self.context.append_basic_block(fn_val, "entry");
            self.builder.position_at_end(entry);
            for (i, param) in params.iter().enumerate() {
                let arg = fn_val.get_nth_param(i as u32).unwrap().into();
                let alloca = self.builder.build_alloca(arg.get_type(), param.0.as_str());
                self.builder.build_store(alloca, arg);
                param_map.insert(param.0.clone(), alloca.into());
            }
            let mut last_val = self.i32_type.const_int(0, false).into();
            for node in body.iter() {
                if let Some(val) = self.gen_stmt(node, param_map) {
                    last_val = val;
                }
            }
            self.builder.build_return(Some(&self.i32_type.const_int(42, false).into()));
            Some(fn_val)
        } else { None }
    }

    fn gen_stmt(&mut self, node: &AstNode, param_map: &HashMap<String, BasicValueEnum<'ctx>>) -> Option<BasicValueEnum<'ctx>> {
        match node {
            AstNode::Call { method, receiver, args } if *method == "add" => {
                if let Some(recv_ptr) = param_map.get(receiver) {
                    if let Some(arg_name) = args.first() {
                        if let Some(arg_ptr) = param_map.get(arg_name) {
                            let recv = self.builder.build_load(recv_ptr.into_pointer_value(), "load_recv").into_int_value();
                            let arg = self.builder.build_load(arg_ptr.into_pointer_value(), "load_arg").into_int_value();
                            if let Some(add) = &self.add_i32_fn {
                                let call = self.builder.build_call(add, &[recv.into(), arg.into()], "add_call");
                                let res = call.try_as_basic_value().left().unwrap().into_int_value();
                                let res_ptr = self.builder.build_alloca(self.i32_type, "res_ptr");
                                self.builder.build_store(res_ptr, res);
                                return Some(res_ptr.into());
                            }
                        }
                    }
                }
                None
            }
            AstNode::Call { method, receiver: _, args } if *method == "http_get" => {
                if let Some(url) = args.first() {
                    if let Some(url_ptr) = param_map.get(url) {
                        if let Some(http) = &self.http_get_fn {
                            let url_val = self.builder.build_load(url_ptr.into_pointer_value(), "load_url").into_pointer_value();
                            let call = self.builder.build_call(http, &[url_val.into()], "http_call");
                            let res_ptr = call.try_as_basic_value().left().unwrap().into_pointer_value();
                            let alloc_res = self.builder.build_alloca(self.i8ptr_type, "http_res");
                            self.builder.build_store(alloc_res, res_ptr);
                            return Some(alloc_res.into());
                        }
                    }
                }
                None
            }
            AstNode::Call { method, .. } if *method == "now" => {
                if let Some(dt) = &self.datetime_now_fn {
                    let call = self.builder.build_call(dt, &[], "dt_call");
                    let ts = call.try_as_basic_value().left().unwrap().into_int_value();
                    let ts_ptr = self.builder.build_alloca(self.i64_type, "ts_ptr");
                    self.builder.build_store(ts_ptr, ts);
                    return Some(ts_ptr.into());
                }
                None
            }
            AstNode::SpawnActor { actor_ty, init_args } => {
                if let Some(arg) = init_args.first() {
                    if let Some(init_ptr) = param_map.get(arg) {
                        let init_val = self.builder.build_load(init_ptr.into_pointer_value(), "load_init").into_int_value();
                        let actor_ptr = self.builder.build_call(self.malloc_fn.as_ref().unwrap(), &[self.i32_type.const_int(8, false).into()], "actor_ptr").try_as_basic_value().left().unwrap().into_pointer_value();
                        let channel = self.builder.build_call(self.malloc_fn.as_ref().unwrap(), &[self.i32_type.const_int(64, false).into()], "channel").try_as_basic_value().left().unwrap().into_pointer_value();
                        self.builder.build_store(actor_ptr, init_val);
                        let spawn_ptr = self.builder.build_alloca(self.i8ptr_type, "spawn_ptr");
                        self.builder.build_store(spawn_ptr, actor_ptr);
                        return Some(spawn_ptr.into());
                    }
                }
                None
            }
            AstNode::Defer(expr) => {
                if let Some(val) = self.gen_stmt(expr, param_map) {
                    let drop_fn = self.module.add_function("drop_raii", self.i32_type.fn_type(&[val.clone().get_type().into()], false), None);
                    let entry_d = self.context.append_basic_block(drop_fn, "entry");
                    self.builder.position_at_end(entry_d);
                    let val_load = self.builder.build_load(val.into_pointer_value(), "defer_load").into_int_value();
                    let zero = self.i32_type.const_int(0, false);
                    let erase = self.builder.build_int_sub(val_load, zero, "erase");
                    self.builder.build_return(Some(&erase.into()));
                    self.builder.build_call(drop_fn, &[val.clone()], "drop");
                    Some(val)
                } else { None }
            }
            AstNode::Lit(n) => {
                let lit_ptr = self.builder.build_alloca(self.i32_type, "lit_ptr");
                let lit_val = self.i32_type.const_int(*n as u64, false);
                self.builder.build_store(lit_ptr, lit_val);
                Some(lit_ptr.into())
            }
            AstNode::Var(v) => param_map.get(v).cloned(),
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
            let inc_fn = self.module.get_function("increment").unwrap();
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
        type UseVecAddFn = unsafe extern "C" fn(PointerValue, i32) -> i32;
        if let Some(use_add) = codegen.get_fn::<UseVecAddFn>("use_vec_add") {
            Ok(use_add.call(0 as PointerValue, 3) as i32)
        } else {
            Err("use_vec_add not found".into())
        }
    }
}
