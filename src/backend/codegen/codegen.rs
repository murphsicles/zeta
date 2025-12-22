// src/backend/codegen/codegen.rs
//! Manages LLVM code generation for a module.
use inkwell::AddressSpace;
use inkwell::attributes::AttributeLoc;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, IntType, PointerType, VectorType};
use inkwell::values::{FunctionValue, PointerValue, MetadataValue};
use std::collections::HashMap;

pub struct LLVMCodegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    i64_type: IntType<'ctx>,
    vec4_i64_type: VectorType<'ctx>,
    #[allow(dead_code)]
    ptr_type: PointerType<'ctx>,
    locals: HashMap<u32, PointerValue<'ctx>>,
    tbaa_const_time: MetadataValue<'ctx>,
    fns: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let vec4_i64_type = i64_type.vec_type(4);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let char_ptr_type = context.ptr_type(AddressSpace::default());
        let void_type = context.void_type();
        let i64_fn_type = i64_type.fn_type(&[], false);
        module.add_function("datetime_now", i64_fn_type, Some(Linkage::External));
        let free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("free", free_type, Some(Linkage::External));
        let send_type = void_type.fn_type(&[i64_type.into(), i64_type.into()], false);
        module.add_function("channel_send", send_type, Some(Linkage::External));
        let recv_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("channel_recv", recv_type, Some(Linkage::External));
        let spawn_type = i64_type.fn_type(&[i64_type.into()], false);
        module.add_function("spawn", spawn_type, Some(Linkage::External));
        let http_type = i64_type.fn_type(&[char_ptr_type.into()], false);
        module.add_function("http_get", http_type, Some(Linkage::External));
        let tls_type = i64_type.fn_type(&[char_ptr_type.into()], false);
        module.add_function("tls_handshake", tls_type, Some(Linkage::External));
        let result_make_type = ptr_type.fn_type(&[i64_type.into()], false);
        module.add_function("result_make_ok", result_make_type, Some(Linkage::External));
        module.add_function("result_make_err", result_make_type, Some(Linkage::External));
        let result_is_ok_type = i64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("result_is_ok", result_is_ok_type, Some(Linkage::External));
        let result_get_data_type = i64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("result_get_data", result_get_data_type, Some(Linkage::External));
        let result_free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("result_free", result_free_type, Some(Linkage::External));
        let map_new_type = ptr_type.fn_type(&[], false);
        module.add_function("map_new", map_new_type, Some(Linkage::External));
        let map_insert_type = void_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into()], false);
        module.add_function("map_insert", map_insert_type, Some(Linkage::External));
        let map_get_type = i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
        module.add_function("map_get", map_get_type, Some(Linkage::External));
        let map_free_type = void_type.fn_type(&[ptr_type.into()], false);
        module.add_function("map_free", map_free_type, Some(Linkage::External));
        let str_concat_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into(), ptr_type.into(), i64_type.into()], false);
        module.add_function("str_concat", str_concat_type, Some(Linkage::External));
        let tbaa_const_time = context.metadata_string("const_time");
        Self {
            context,
            module,
            builder,
            i64_type,
            vec4_i64_type,
            ptr_type,
            locals: HashMap::new(),
            tbaa_const_time,
            fns: HashMap::new(),
        }
    }
}
