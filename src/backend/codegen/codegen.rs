// src/backend/codegen/codegen.rs
use inkwell::AddressSpace;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{IntType, PointerType, VectorType};
use inkwell::values::{FunctionValue, MetadataValue, PointerValue};
use std::collections::HashMap;

pub struct LLVMCodegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub i64_type: IntType<'ctx>,
    pub vec4_i64_type: VectorType<'ctx>,
    pub ptr_type: PointerType<'ctx>,
    pub locals: HashMap<u32, PointerValue<'ctx>>,
    pub tbaa_const_time: MetadataValue<'ctx>,
    pub fns: HashMap<String, FunctionValue<'ctx>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let vec4_i64_type = i64_type.vec_type(4);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let void_type = context.void_type();

        // External host functions
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

        let http_type = i64_type.fn_type(&[ptr_type.into()], false);
        module.add_function("http_get", http_type, Some(Linkage::External));

        let tls_type = i64_type.fn_type(&[ptr_type.into()], false);
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

        // Pure LLVM str_concat implementation (no host dependency)
        let str_concat_type = ptr_type.fn_type(
            &[
                ptr_type.into(),  // a
                i64_type.into(),  // a_len
                ptr_type.into(),  // b
                i64_type.into(),  // b_len
            ],
            false,
        );
        let str_concat_fn = module.add_function("str_concat", str_concat_type, None);
        let entry = context.append_basic_block(str_concat_fn, "entry");
        builder.position_at_end(entry);

        let a_ptr = str_concat_fn.get_first_param().unwrap().into_pointer_value();
        let a_len = str_concat_fn.get_nth_param(1).unwrap().into_int_value();
        let b_ptr = str_concat_fn.get_nth_param(2).unwrap().into_pointer_value();
        let b_len = str_concat_fn.get_nth_param(3).unwrap().into_int_value();

        let total_len = builder.build_int_add(a_len, b_len, "total_len").unwrap();
        let malloc_size = builder.build_int_add(total_len, i64_type.const_int(1, false), "malloc_size").unwrap();

        let malloc_call = builder.build_call(
            module.get_function("malloc").unwrap_or_else(|| {
                let malloc_ty = ptr_type.fn_type(&[i64_type.into()], false);
                module.add_function("malloc", malloc_ty, Some(Linkage::External))
            }),
            &[malloc_size.into()],
            "malloc_call",
        ).unwrap();

        let dest_ptr = malloc_call
            .try_as_basic_value()
            .expect_basic("malloc should return a value")
            .into_pointer_value();

        let _memcpy_a = builder.build_memcpy(dest_ptr, 1, a_ptr, 1, a_len).unwrap();
        let dest_after_a = builder.build_gep(i64_type, dest_ptr, &[a_len], "dest_after_a").unwrap();
        let _memcpy_b = builder.build_memcpy(dest_after_a, 1, b_ptr, 1, b_len).unwrap();

        let null_term = builder.build_gep(i64_type, dest_ptr, &[total_len], "null_pos").unwrap();
        builder.build_store(null_term, context.i8_type().const_zero()).unwrap();

        builder.build_return(Some(&dest_ptr)).unwrap();

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
