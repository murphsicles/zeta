// src/backend/codegen/codegen.rs
//! Complete LLVM code generator for Zeta.
//!
//! This file contains the entire codegen pipeline in one module for maximum simplicity
//! and performance. It translates MIR directly into LLVM IR, handles parameter passing,
//! monomorphized calls, and all runtime intrinsics.
//!
//! This is the heart of Zeta's execution engine and will be the foundation for
//! the self-hosted compiler.
//!
//! UPDATED FOR PRINTLN SUPPORT (March 2026) - Grok + Zeta team

use crate::middle::mir::mir::{Mir, MirExpr, MirStmt, SemiringOp};
use crate::middle::types::{Substitution, Type, TypeVar};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{IntType, PointerType, VectorType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, CallSiteValue, FunctionValue, PointerValue,
};
use std::collections::HashMap;

/// The complete LLVM code generator for Zeta.
pub struct LLVMCodegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub i64_type: IntType<'ctx>,
    pub f64_type: inkwell::types::FloatType<'ctx>,
    pub vec4_i64_type: VectorType<'ctx>,
    pub ptr_type: PointerType<'ctx>,
    pub locals: HashMap<u32, PointerValue<'ctx>>,
    pub fns: HashMap<String, FunctionValue<'ctx>>,

    // NEW: Caches for monomorphized functions and types
    pub specialized_fns: HashMap<String, FunctionValue<'ctx>>,
    pub specialized_types: HashMap<String, inkwell::types::StructType<'ctx>>,

    // NEW: Map from generic function names to their MIR definitions
    pub generic_defs: HashMap<String, crate::middle::mir::mir::Mir>,
    
    // Current type map for the function being compiled
    pub current_type_map: Option<std::collections::HashMap<u32, crate::middle::types::Type>>,
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Creates a new code generator and declares all runtime intrinsics.
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let i64_type = context.i64_type();
        let f64_type = context.f64_type();
        let vec4_i64_type = i64_type.vec_type(4);
        let ptr_type = context.ptr_type(AddressSpace::default());
        let void_type = context.void_type();

        // Declare all host/runtime functions (these are provided by the Zeta runtime)
        module.add_function(
            "datetime_now",
            i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        // free is NOT declared here — user-declared extern fn free(ptr: i64) -> ()
        // provides the declaration with i64-compatible types that the codegen uses.
        module.add_function(
            "channel_send",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "channel_recv",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "spawn",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "http_get",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "tls_handshake",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_result_make_ok",
            ptr_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_result_make_err",
            ptr_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_result_is_ok",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_result_get_data",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_result_free",
            void_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        // Option runtime functions
        module.add_function(
            "option_make_some",
            ptr_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "option_make_none",
            ptr_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "option_is_some",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "option_get_data",
            i64_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "option_free",
            void_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        // Vector constructors for common SIMD types
        // Vector<u64, 8>
        module.add_function(
            "vector_make_u64x8",
            i64_type.fn_type(&[
                i64_type.into(), i64_type.into(), i64_type.into(), i64_type.into(),
                i64_type.into(), i64_type.into(), i64_type.into(), i64_type.into()
            ], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_splat_u64x8",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        // Vector<i32, 4>
        module.add_function(
            "vector_make_i32x4",
            i64_type.fn_type(&[
                i64_type.into(), i64_type.into(), i64_type.into(), i64_type.into()
            ], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_splat_i32x4",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        // Vector operation functions
        // Vector<u64, 8> operations
        module.add_function(
            "vector_add_u64x8",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_sub_u64x8",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_mul_u64x8",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_get_u64x8",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_set_u64x8",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_free_u64x8",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        // Vector<i32, 4> operations
        module.add_function(
            "vector_add_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_sub_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_mul_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_get_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_set_i32x4",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "vector_free_i32x4",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_new",
            ptr_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_insert",
            void_type.fn_type(&[ptr_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_get",
            i64_type.fn_type(&[ptr_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "map_free",
            void_type.fn_type(&[ptr_type.into()], false),
            Some(Linkage::External),
        );
        // I/O functions
        module.add_function(
            "print_i64",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "print_bool",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "print_str",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        // println(no-args) is NOT declared here — the println! macro goes to
        // println_i64, and user-declared extern fn println(msg: i64) -> ()
        // provides its own declaration.
        module.add_function(
            "println_i64",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "test_return_i64",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "flush",
            void_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        // Timing and result output for competition entries
        module.add_function(
            "get_time_us",
            i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "print_result",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        // Memory allocation functions
        module.add_function(
            "runtime_malloc",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "runtime_free",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "runtime_calloc",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "runtime_realloc",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        // Array functions
        module.add_function(
            "array_new",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_len",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        let array_get_fn = module.add_function(
            "array_get",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        #[cfg(target_os = "windows")]
        {
            // Note: Calling convention setting disabled - can't find correct API in inkwell 0.8.0
            // array_get_fn.set_call_conventions(...);
        }
        module.add_function(
            "stack_array_get",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_set",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "stack_array_set",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_push",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_free",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_set_len",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        // Sieve/simulation runtime functions
        module.add_function(
            "avx512_byte_fill",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "clear_bit",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "test_bit",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "sieve_step",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "avx512_count_bits",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "read_qword_builtin",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "write_qword_builtin",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "run_sieve",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_concat",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_to_lowercase",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_to_uppercase",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_trim",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_len",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_starts_with",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_ends_with",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_contains",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_replace",
            i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        
        // Identity-aware runtime functions (only declared when identity feature is enabled)
        #[cfg(feature = "identity")]
        {
            module.add_function(
                "identity_host_str_concat",
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_len",
                i64_type.fn_type(&[i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_to_lowercase",
                i64_type.fn_type(&[i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_to_uppercase",
                i64_type.fn_type(&[i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_trim",
                i64_type.fn_type(&[i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_starts_with",
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_ends_with",
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_contains",
                i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "identity_host_str_replace",
                i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
                Some(Linkage::External),
            );
            module.add_function(
                "init_global_identity_context",
                void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
                Some(Linkage::External),
            );
        }
        
        // Array runtime functions
        // Note: array_new already declared above (line 312)
        // module.add_function(
        //     "array_new",
        //     i64_type.fn_type(&[i64_type.into()], false),
        //     Some(Linkage::External),
        // );
        module.add_function(
            "array_push",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_len",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        let array_get_fn2 = module.add_function(
            "array_get",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        #[cfg(target_os = "windows")]
        {
            // Note: Calling convention setting disabled - can't find correct API in inkwell 0.8.0
            // array_get_fn2.set_call_conventions(...);
        }
        module.add_function(
            "stack_array_get",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_set",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "stack_array_set",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_free",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "array_set_len",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "scheduler::init_runtime",
            void_type.fn_type(&[], false),
            Some(Linkage::External),
        );

        // === RUNTIME FUNCTIONS FOR COMPILATION (v0.3.23) ===
        module.add_function(
            "clone_i64",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "is_null_i64",
            i64_type.fn_type(&[i64_type.into()], false), // Returns i64 (0 or 1) for bool
            Some(Linkage::External),
        );
        module.add_function(
            "to_string_str",
            i64_type.fn_type(&[i64_type.into()], false), // str is i64 pointer in runtime
            Some(Linkage::External),
        );
        module.add_function(
            "clone_bool",
            i64_type.fn_type(&[i64_type.into()], false), // bool is i64 in runtime
            Some(Linkage::External),
        );
        module.add_function(
            "is_null_bool",
            i64_type.fn_type(&[i64_type.into()], false), // Returns i64 (0 or 1) for bool
            Some(Linkage::External),
        );
        module.add_function(
            "to_string_i64",
            i64_type.fn_type(&[i64_type.into()], false), // Returns i64 pointer to string
            Some(Linkage::External),
        );
        module.add_function(
            "to_string_bool",
            i64_type.fn_type(&[i64_type.into()], false), // Returns i64 pointer to string
            Some(Linkage::External),
        );

        // === PRINTLN SUPPORT (the fix) ===
        let printf_type = context.i32_type().fn_type(&[ptr_type.into()], true); // variadic
        module.add_function("printf", printf_type, Some(Linkage::External));
        // println is NOT declared here — the println! macro goes to
        // println_i64, and user-declared extern fn println(msg: i64) provides
        // its own declaration.
        module.add_function(
            "println_i64",
            i64_type.fn_type(&[i64_type.into()], false), // takes i64, returns void
            Some(Linkage::External),
        );
        
        // Zeta runtime functions (temporary bridge)
        module.add_function(
            "zeta_array_get_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "zeta_array_set_i64",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "zeta_array_get_bool",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "zeta_array_set_bool",
            void_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "zeta_sieve_new",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "zeta_print_i64",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "zeta_println_i64",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );

        // === OPERATOR FUNCTION DECLARATIONS ===
        // Integer comparison operators (return i64: 1 for true, 0 for false)
        module.add_function(
            "==",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "!=",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "<",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            ">",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "<=",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            ">=",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // Arithmetic operators
        module.add_function(
            "+",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "-",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "*",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "/",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "%",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // Also declare mangled operator names for compatibility
        module.add_function(
            "eq_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "ne_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "lt_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "gt_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "le_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "ge_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "add_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "sub_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "mul_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "div_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "mod_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "and_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "or_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "not_i64",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "shl_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "shr_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "xor_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // Declare call_i64 for function calls (will be handled inline)
        module.add_function(
            "call_i64",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // Note: These functions are declared as external but will be handled inline
        // by the code generator (see is_operator and MirStmt::Call handling)

        // === SIMD INTRINSIC DECLARATIONS ===
        // Vector splat operations (return i64 pointers to heap-allocated vectors)
        module.add_function(
            "simd_splat_i32x4",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_splat_i64x2",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_splat_f32x4",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );

        // Vector arithmetic operations (take i64 pointers, return i64 pointer)
        module.add_function(
            "simd_add_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_mul_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_sub_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // Vector load/store operations
        module.add_function(
            "simd_load_i32x4",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_store_i32x4",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // Vector extract/insert operations
        module.add_function(
            "simd_extract_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_insert_i32x4",
            i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );

        // SIMD free functions
        module.add_function(
            "simd_free_i32x4",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_free_i64x2",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "simd_free_f32x4",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );

        Self {
            context,
            module,
            builder,
            i64_type,
            f64_type,
            vec4_i64_type,
            ptr_type,
            locals: HashMap::new(),
            fns: HashMap::new(),
            specialized_fns: HashMap::new(),
            specialized_types: HashMap::new(),
            generic_defs: HashMap::new(),
            current_type_map: None,
        }
    }
}

impl<'ctx> LLVMCodegen<'ctx> {
    /// Mangle a function name with type arguments for monomorphization
    fn mangle_function_name(
        &self,
        base_name: &str,
        type_args: &[crate::middle::types::Type],
    ) -> String {
        if type_args.is_empty() {
            return base_name.to_string();
        }

        let mut mangled = base_name.to_string();
        mangled.push_str("_inst");

        for ty in type_args {
            mangled.push('_');
            mangled.push_str(&ty.mangled_name());
        }

        mangled
    }
    pub fn gen_mirs(&mut self, mirs: &[Mir]) {
        // First pass: collect all functions
        for mir in mirs {
            let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());

            // Check if this is a generic function
            let is_generic = self.is_generic_function(mir);

            if is_generic {
                // Store generic definition for later instantiation
                self.generic_defs.insert(fn_name.clone(), mir.clone());
            } else {
                // Non-generic function: declare as before
                let param_types: Vec<_> = (0..mir.param_indices.len())
                    .map(|_| self.i64_type.into())
                    .collect();
                let fn_type = self.i64_type.fn_type(&param_types, false);
                let fn_val = self.module.add_function(&fn_name, fn_type, None);
                self.fns.insert(fn_name.clone(), fn_val);
            }
        }

        // Second pass: generate non-generic function bodies
        for mir in mirs {
            if !self.is_generic_function(mir) {
                self.gen_fn(mir);
            }
            // Generic functions are generated on-demand via monomorphization
        }
    }

    fn gen_fn(&mut self, mir: &Mir) {
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let fn_val = self.get_function(&fn_name);

        // If function has no body (extern/FFI declaration), mark as
        // external linkage and skip body generation — the linker will
        // resolve the symbol at link time instead of producing a stub.
        if mir.stmts.is_empty() {
            fn_val.set_linkage(Linkage::External);
            return;
        }

        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.locals.clear();
        self.current_type_map = Some(mir.type_map.clone());
        let all_ids = self.collect_all_local_ids(mir);
        for &id in &all_ids {
            let alloca = self
                .builder
                .build_alloca(self.i64_type, &"")
                .unwrap();
            self.locals.insert(id, alloca);
        }
        for (i, _) in mir.param_indices.iter().enumerate() {
            if let Some(param_val) = fn_val.get_nth_param(i as u32)
                && let Some(&alloca) = self.locals.get(&(i as u32 + 1))
            {
                self.builder.build_store(alloca, param_val).unwrap();
            }
        }
        for stmt in &mir.stmts {
            self.gen_stmt(stmt, &mir.exprs);
        }
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder
                .build_return(Some(&self.i64_type.const_zero()))
                .unwrap();
        }
    }

    fn collect_all_local_ids(&self, mir: &Mir) -> std::collections::HashSet<u32> {
        let mut ids = std::collections::HashSet::new();
        for (_, id) in &mir.param_indices {
            ids.insert(*id);
        }
        for &id in mir.exprs.keys() {
            ids.insert(id);
        }
        for stmt in &mir.stmts {
            self.collect_ids_from_stmt_safe(stmt, &mut ids, &mir.exprs);
        }
        ids
    }

    fn collect_ids_from_stmt_safe(
        &self,
        stmt: &MirStmt,
        ids: &mut std::collections::HashSet<u32>,
        exprs: &HashMap<u32, MirExpr>,
    ) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                ids.insert(*lhs);
                if let Some(e) = exprs.get(rhs) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::Call { args, dest, .. } => {
                for &arg_id in args {
                    if let Some(e) = exprs.get(&arg_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
                ids.insert(*dest);
            }
            MirStmt::VoidCall { args, .. } => {
                for &arg_id in args {
                    if let Some(e) = exprs.get(&arg_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirStmt::Return { val } => {
                if let Some(e) = exprs.get(val) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::SemiringFold { values, result, .. } => {
                for &val_id in values {
                    if let Some(e) = exprs.get(&val_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
                ids.insert(*result);
            }
            MirStmt::TryProp {
                expr_id,
                ok_dest,
                err_dest,
            } => {
                if let Some(e) = exprs.get(expr_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                ids.insert(*ok_dest);
                ids.insert(*err_dest);
            }
            MirStmt::MapNew { dest } => {
                ids.insert(*dest);
            }
            MirStmt::DictInsert {
                map_id,
                key_id,
                val_id,
            } => {
                ids.insert(*map_id);
                if let Some(e) = exprs.get(key_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                if let Some(e) = exprs.get(val_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::DictGet {
                map_id,
                key_id,
                dest,
            } => {
                ids.insert(*map_id);
                if let Some(e) = exprs.get(key_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                ids.insert(*dest);
            }
            MirStmt::If {
                cond,
                then,
                else_,
                dest,
            } => {
                if let Some(e) = exprs.get(cond) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                for s in then {
                    self.collect_ids_from_stmt_safe(s, ids, exprs);
                }
                for s in else_ {
                    self.collect_ids_from_stmt_safe(s, ids, exprs);
                }
                if let Some(dest_id) = dest {
                    ids.insert(*dest_id);
                }
            }
            MirStmt::While { cond, body } => {
                if let Some(e) = exprs.get(cond) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                for s in body {
                    self.collect_ids_from_stmt_safe(s, ids, exprs);
                }
            }
            MirStmt::For { iterator, body, .. } => {
                if let Some(e) = exprs.get(iterator) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                for s in body {
                    self.collect_ids_from_stmt_safe(s, ids, exprs);
                }
            }
            MirStmt::ParamInit { param_id, .. } => {
                ids.insert(*param_id);
            }
            MirStmt::Store { addr_id, val_id, .. } => {
                ids.insert(*addr_id);
                if let Some(e) = exprs.get(val_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            _ => {}
        }
    }

    fn collect_ids_from_expr_safe(
        &self,
        expr: &MirExpr,
        ids: &mut std::collections::HashSet<u32>,
        exprs: &HashMap<u32, MirExpr>,
    ) {
        match expr {
            MirExpr::Var(id) => {
                ids.insert(*id);
            }
            MirExpr::BinaryOp { left, right, .. } => {
                if let Some(e) = exprs.get(left) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                if let Some(e) = exprs.get(right) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirExpr::FString(inner_ids) => {
                for &id in inner_ids {
                    if let Some(e) = exprs.get(&id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirExpr::TimingOwned(inner_id) => {
                ids.insert(*inner_id);
            }
            MirExpr::Deref { addr_id, .. } => {
                if let Some(e) = exprs.get(addr_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirExpr::SemiringFold { values, .. } => {
                for &v in values {
                    if let Some(e) = exprs.get(&v) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            _ => {}
        }
    }

    /// Check if a function name is an operator that should be handled inline
    fn is_operator(&self, name: &str) -> bool {
        matches!(
            name,
            "+" | "-"
                | "*"
                | "/"
                | "%"
                | "=="
                | "!="
                | "<"
                | ">"
                | "<="
                | ">="
                | "&&"
                | "||"
                | "!"
                | "<<"
                | ">>"
                | "&"
                | "|"
                | "^"
                | "add"
                | "sub"
                | "mul"
                | "div"
                | "mod"
                | "eq"
                | "ne"
                | "lt"
                | "gt"
                | "le"
                | "ge"
                | "and"
                | "or"
                | "not"
                | "shl"
                | "shr"
                | "bitand"
                | "bitor"
                | "bitxor"
                | "add_i64"
                | "sub_i64"
                | "mul_i64"
                | "div_i64"
                | "mod_i64"
                | "eq_i64"
                | "ne_i64"
                | "lt_i64"
                | "gt_i64"
                | "le_i64"
                | "ge_i64"
                | "and_i64"
                | "or_i64"
                | "not_i64"
                | "shl_i64"
                | "shr_i64"
                | "xor_i64"
        )
    }

    /// Check if a function name is a SIMD operation
    fn is_simd_operation(&self, name: &str) -> bool {
        name.starts_with("simd_") // e.g., i32x4, f32x4
    }

    fn get_function(&self, name: &str) -> FunctionValue<'ctx> {
        if let Some(&f) = self.fns.get(name) {
            return f;
        }

        // Handle Type::method names (static methods)
        if name.contains("::") {
            let mangled = name.replace("::", "_");
            // Try the mangled name first
            if let Some(&f) = self.fns.get(&mangled) {
                return f;
            }
            // Also try the original name (functions might be compiled with :: in name)
            if let Some(&f) = self.fns.get(name) {
                return f;
            }
            // Also try just the method name (function might be compiled as just "new")
            let method_name = name.split("::").last().unwrap_or(name);
            if let Some(&f) = self.fns.get(method_name) {
                return f;
            }
        } else {
            // Try with std:: prefix (for imported stdlib functions)
            let std_name = format!("std::{}", name);
            if let Some(&f) = self.fns.get(&std_name) {
                return f;
            }
        }

        let mangled = if name.contains('_') {
            name.to_string()
        } else {
            format!("{}_i64", name)
        };
        if let Some(&f) = self.fns.get(&mangled) {
            return f;
        }
        // Check the module before doing the split('_') fallback to avoid
        // matching println_i64 to println, print_i64 to print, etc.
        if let Some(f) = self.module.get_function(name) {
            return f;
        }

        let base = name.split('_').next().unwrap_or(name);
        if let Some(&f) = self.fns.get(base) {
            return f;
        }
        if name == "add"
            && let Some(&f) = self.fns.get("add_i64")
        {
            return f;
        }
        if name == "add_i64"
            && let Some(&f) = self.fns.get("add")
        {
            return f;
        }
        // === NEW: handle println explicitly to prevent CRITICAL panic ===
        if name == "println"
            && let Some(f) = self.module.get_function("println")
        {
            return f;
        }
        // === NEW: handle call_i64 - function call dispatcher ===
        if name == "call_i64"
            && let Some(f) = self.module.get_function("call_i64")
        {
            return f;
        }
        // === NEW: handle comparison operators ===
        if (name == "==" || name == "eq" || name == "eq_i64")
            && let Some(f) = self.module.get_function("==")
        {
            return f;
        }
        if (name == "!=" || name == "ne" || name == "ne_i64")
            && let Some(f) = self.module.get_function("!=")
        {
            return f;
        }
        if (name == "<" || name == "lt" || name == "lt_i64")
            && let Some(f) = self.module.get_function("<")
        {
            return f;
        }
        if (name == ">" || name == "gt" || name == "gt_i64")
            && let Some(f) = self.module.get_function(">")
        {
            return f;
        }
        if (name == "<=" || name == "le" || name == "le_i64")
            && let Some(f) = self.module.get_function("<=")
        {
            return f;
        }
        if (name == ">=" || name == "ge" || name == "ge_i64")
            && let Some(f) = self.module.get_function(">=")
        {
            return f;
        }
        // Handle enum constructors
        if name == "Option::Some"
            && let Some(f) = self.module.get_function("option_make_some")
        {
            return f;
        }
        if name == "Option::None"
            && let Some(f) = self.module.get_function("option_make_none")
        {
            return f;
        }
        if name == "Result::Ok"
            && let Some(f) = self.module.get_function("host_result_make_ok")
        {
            return f;
        }
        if name == "Result::Err"
            && let Some(f) = self.module.get_function("host_result_make_err")
        {
            return f;
        }
        // Handle Vector::new constructor
        // Note: This is a hack - we assume Vector<u64, 8> for now
        // In a real implementation, we would need to know the actual vector type
        if name == "Vector::new" {
            // Try to find vector_make_u64x8 function (most common for Murphy's Sieve)
            if let Some(f) = self.module.get_function("vector_make_u64x8") {
                return f;
            }
            // Fall back to i32x4
            if let Some(f) = self.module.get_function("vector_make_i32x4") {
                return f;
            }
            // If not found, create a dummy function
            // Note: This is a hack to avoid crashing
            // In a real implementation, we would generate proper SIMD code
            let dummy_fn = self.module.add_function(
                "vector_make_dummy",
                self.i64_type.fn_type(&[], false),
                Some(Linkage::External),
            );
            return dummy_fn;
        }
        // Handle Vector::splat
        if name == "Vector::splat" {
            // Try to find vector_splat function
            if let Some(f) = self.module.get_function("vector_splat") {
                return f;
            }
            // Create a dummy function
            let dummy_fn = self.module.add_function(
                "vector_splat_dummy",
                self.i64_type.fn_type(&[self.i64_type.into()], false),
                Some(Linkage::External),
            );
            return dummy_fn;
        }
        // Handle string functions - map from str_* to host_str_* or identity_host_str_*
        if name.starts_with("str_") {
            #[cfg(feature = "identity")]
            {
                // Try identity-aware version first if identity feature is enabled
                let identity_host_name = format!("identity_host_{}", name);
                if let Some(f) = self.module.get_function(&identity_host_name) {
                    return f;
                }
            }
            
            // Fall back to regular host function
            let host_name = format!("host_{}", name);
            if let Some(f) = self.module.get_function(&host_name) {
                return f;
            }
        }

        // Check if it's an external function declared in the module
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        
        // Handle Vector::extract (method call, not static method)
        // Note: This is handled differently - as a method call on a vector value
        panic!("CRITICAL: Missing function '{}'", name);
    }

    /// Get function with type arguments for monomorphization
    fn get_function_with_types(
        &mut self,
        name: &str,
        type_args: &[crate::middle::types::Type],
    ) -> FunctionValue<'ctx> {
        // If no type arguments, use regular lookup
        if type_args.is_empty() {
            return self.get_function(name);
        }

        // Generate mangled name
        let mangled_name = self.mangle_function_name(name, type_args);

        // Check cache first
        if let Some(&f) = self.specialized_fns.get(&mangled_name) {
            return f;
        }

        // Check if we have a generic definition
        if let Some(generic_mir) = self.generic_defs.get(name) {
            // Clone the MIR to avoid borrowing issues
            let generic_mir_clone = generic_mir.clone();
            // Monomorphize the generic function
            let monomorphized_fn = self.monomorphize_function(&generic_mir_clone, name, type_args);
            self.specialized_fns
                .insert(mangled_name.clone(), monomorphized_fn);
            // Also add to regular functions map for future lookups
            self.fns.insert(mangled_name, monomorphized_fn);
            return monomorphized_fn;
        }

        // Fallback: try regular lookup (for non-generic functions called with empty type_args)
        self.get_function(name)
    }

    /// Check if a function is generic
    fn is_generic_function(&self, mir: &crate::middle::mir::mir::Mir) -> bool {
        // Check if function has type parameters
        // We need a better heuristic. For now, check if the function name
        // contains generic type parameters in its type map.
        // This is a temporary hack - we should store type parameters in MIR.

        // Look for type variables in the type map
        mir.type_map.values().any(|ty| match ty {
            crate::middle::types::Type::Variable(_) => true,
            crate::middle::types::Type::Named(_, args) => args
                .iter()
                .any(|arg| matches!(arg, crate::middle::types::Type::Variable(_))),
            _ => false,
        })
    }

    /// Basic monomorphization implementation
    fn monomorphize_function(
        &mut self,
        generic_mir: &crate::middle::mir::mir::Mir,
        name: &str,
        type_args: &[crate::middle::types::Type],
    ) -> FunctionValue<'ctx> {
        let mangled_name = self.mangle_function_name(name, type_args);

        // Create function with mangled name
        let param_types: Vec<_> = (0..generic_mir.param_indices.len())
            .map(|_| self.i64_type.into())
            .collect();
        let fn_type = self.i64_type.fn_type(&param_types, false);
        let fn_val = self.module.add_function(&mangled_name, fn_type, None);

        // Store the function in our maps before generating body
        // (gen_fn will look it up)
        self.fns.insert(mangled_name.clone(), fn_val);
        self.specialized_fns.insert(mangled_name.clone(), fn_val);

        // Create a substitution for type variables
        // For now, assume the generic function has type parameters TypeVar(0), TypeVar(1), etc.
        let mut substitution = Substitution::new();
        for (i, type_arg) in type_args.iter().enumerate() {
            substitution
                .mapping
                .insert(TypeVar(i as u32), type_arg.clone());
        }

        // Apply substitution to create a monomorphized MIR
        let monomorphized_mir = self.substitute_mir(generic_mir, &substitution);

        // Generate the function body
        self.gen_fn(&monomorphized_mir);

        fn_val
    }

    /// Apply type substitution to a MIR
    fn substitute_mir(&self, mir: &Mir, substitution: &Substitution) -> Mir {
        let mut new_mir = mir.clone();

        // Substitute types in type_map
        let mut new_type_map = HashMap::new();
        for (id, ty) in &mir.type_map {
            new_type_map.insert(*id, substitution.apply(ty));
        }
        new_mir.type_map = new_type_map;

        // Substitute types in statements (type arguments in calls)
        new_mir.stmts = mir
            .stmts
            .iter()
            .map(|stmt| self.substitute_stmt(stmt, substitution))
            .collect();

        new_mir
    }

    /// Apply type substitution to a statement
    fn substitute_stmt(&self, stmt: &MirStmt, substitution: &Substitution) -> MirStmt {
        match stmt {
            MirStmt::Assign { lhs, rhs } => MirStmt::Assign {
                lhs: *lhs,
                rhs: *rhs,
            },
            MirStmt::Call {
                func,
                args,
                dest,
                type_args,
            } => {
                // Substitute type arguments in the call
                let substituted_type_args: Vec<Type> =
                    type_args.iter().map(|ty| substitution.apply(ty)).collect();

                MirStmt::Call {
                    func: func.clone(),
                    args: args.clone(),
                    dest: *dest,
                    type_args: substituted_type_args,
                }
            }
            MirStmt::VoidCall { func, args } => MirStmt::VoidCall {
                func: func.clone(),
                args: args.clone(),
            },
            MirStmt::Return { val } => MirStmt::Return { val: *val },
            MirStmt::SemiringFold { op, values, result } => MirStmt::SemiringFold {
                op: *op,
                values: values.clone(),
                result: *result,
            },
            MirStmt::ParamInit {
                param_id,
                arg_index,
            } => MirStmt::ParamInit {
                param_id: *param_id,
                arg_index: *arg_index,
            },
            MirStmt::Consume { id } => MirStmt::Consume { id: *id },
            MirStmt::If {
                cond,
                then,
                else_,
                dest,
            } => {
                // Recursively substitute in then and else blocks
                let substituted_then: Vec<MirStmt> = then
                    .iter()
                    .map(|stmt| self.substitute_stmt(stmt, substitution))
                    .collect();
                let substituted_else: Vec<MirStmt> = else_
                    .iter()
                    .map(|stmt| self.substitute_stmt(stmt, substitution))
                    .collect();

                MirStmt::If {
                    cond: *cond,
                    then: substituted_then,
                    else_: substituted_else,
                    dest: *dest,
                }
            }
            MirStmt::While { cond, body } => {
                // Recursively substitute in loop body
                let substituted_body: Vec<MirStmt> = body
                    .iter()
                    .map(|stmt| self.substitute_stmt(stmt, substitution))
                    .collect();

                MirStmt::While {
                    cond: *cond,
                    body: substituted_body,
                }
            }
            MirStmt::TryProp {
                expr_id,
                ok_dest,
                err_dest,
            } => MirStmt::TryProp {
                expr_id: *expr_id,
                ok_dest: *ok_dest,
                err_dest: *err_dest,
            },
            MirStmt::DictInsert {
                map_id,
                key_id,
                val_id,
            } => MirStmt::DictInsert {
                map_id: *map_id,
                key_id: *key_id,
                val_id: *val_id,
            },
            MirStmt::DictGet {
                map_id,
                key_id,
                dest,
            } => MirStmt::DictGet {
                map_id: *map_id,
                key_id: *key_id,
                dest: *dest,
            },
            MirStmt::Store {
                addr_id,
                val_id,
                pointee_width,
            } => MirStmt::Store {
                addr_id: *addr_id,
                val_id: *val_id,
                pointee_width: *pointee_width,
            },
            // TODO: Handle other MIR statement variants
            _ => todo!(
                "MIR statement variant not yet implemented in substitute_stmt: {:?}",
                stmt
            ),
        }
    }

    fn gen_stmt(&mut self, stmt: &MirStmt, exprs: &HashMap<u32, MirExpr>) {
        match stmt {
            MirStmt::Assign { lhs, rhs } => {
                let val = self.gen_expr_safe(rhs, exprs);
                let alloca = *self.locals.get(lhs).unwrap();
                self.builder.build_store(alloca, val).unwrap();
            }
            MirStmt::Call {
                func,
                args,
                dest,
                type_args,
            } => {
                // Handle call_i64 - actual function call dispatch
                if func == "call_i64" && args.len() >= 2 {
                    // call_i64(func_ptr: i64, arg: i64) -> i64
                    // For now, use identity workaround
                    let arg_val = self.gen_expr_safe(&args[1], exprs);
                    let dest_alloca = *self.locals.get(dest).unwrap();
                    self.builder.build_store(dest_alloca, arg_val).unwrap();
                    return;
                }

                // Handle unary minus before operator check
                if args.len() == 1 && (func == "-" || func == "unary_minus") {
                    let operand = self.gen_expr_safe(&args[0], exprs);
                    let zero = self.i64_type.const_zero();
                    let result = self.builder.build_int_sub(zero, operand.into_int_value(), "neg").unwrap();
                    let alloca = *self.locals.get(dest).unwrap();
                    self.builder.build_store(alloca, result).unwrap();
                    return;
                }

                // Handle array_get and stack_array_get specially for direct memory access
                // DISABLED: Causing crashes on Windows - using runtime call instead
                // Original code commented out:
                /*
                if (func == "array_get" || func == "stack_array_get") && args.len() == 2 {
                    // Get array pointer and index
                    let array_ptr_val = self.gen_expr_safe(&args[0], exprs).into_int_value();
                    let index_val = self.gen_expr_safe(&args[1], exprs).into_int_value();
                    
                    // Convert array pointer (i64) to LLVM pointer
                    let array_ptr = self.builder.build_int_to_ptr(
                        array_ptr_val,
                        self.context.ptr_type(AddressSpace::default()),
                        "array_ptr"
                    ).unwrap();
                    
                    // Generate GEP to get element pointer
                    let elem_ptr = unsafe {
                        self.builder.build_gep(
                            self.i64_type,
                            array_ptr,
                            &[index_val],
                            "elem_ptr"
                        ).unwrap()
                    };
                    
                    // Load the value
                    let value = self.builder.build_load(self.i64_type, elem_ptr, "array_elem").unwrap();
                    
                    // Store result
                    let dest_alloca = *self.locals.get(dest).unwrap();
                    self.builder.build_store(dest_alloca, value).unwrap();
                    return;
                }
                */
                // Handle operator functions inline
                if self.is_operator(func) {
                    // Handle unary operators
                    if args.len() == 1 && func == "!" {
                        let operand = self.gen_expr_safe(&args[0], exprs);
                        // Bitwise NOT: operand ^ -1 (all 1s)
                        let result = self
                            .builder
                            .build_xor(
                                operand.into_int_value(),
                                self.i64_type.const_int(-1i64 as u64, true),
                                "bitnot",
                            )
                            .unwrap();

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                        return;
                    }
                    
                    // Handle unary minus
                    if args.len() == 1 && func == "-" {
                        let operand = self.gen_expr_safe(&args[0], exprs);
                        // Unary minus: 0 - operand
                        let zero = self.i64_type.const_zero();
                        let result = self
                            .builder
                            .build_int_sub(zero, operand.into_int_value(), "neg")
                            .unwrap();

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                        return;
                    }

                    // Handle binary operators
                    if args.len() == 2 {
                        let left = self.gen_expr_safe(&args[0], exprs);
                        let right = self.gen_expr_safe(&args[1], exprs);

                        let result = match func.as_str() {
                            // Arithmetic operators
                            "+" | "add" | "add_i64" => self
                                .builder
                                .build_int_add(left.into_int_value(), right.into_int_value(), "add")
                                .unwrap(),
                            "-" | "sub" | "sub_i64" => self
                                .builder
                                .build_int_sub(left.into_int_value(), right.into_int_value(), "sub")
                                .unwrap(),
                            "*" | "mul" | "mul_i64" => self
                                .builder
                                .build_int_mul(left.into_int_value(), right.into_int_value(), "mul")
                                .unwrap(),
                            "/" | "div" | "div_i64" => self
                                .builder
                                .build_int_signed_div(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "div",
                                )
                                .unwrap(),
                            "%" | "mod" | "mod_i64" => self
                                .builder
                                .build_int_signed_rem(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "mod",
                                )
                                .unwrap(),

                            // Bitwise operators
                            "<<" | "shl" | "shl_i64" => self
                                .builder
                                .build_left_shift(left.into_int_value(), right.into_int_value(), "shl")
                                .unwrap(),
                            ">>" | "shr" | "shr_i64" => self
                                .builder
                                .build_right_shift(left.into_int_value(), right.into_int_value(), false, "shr")
                                .unwrap(),
                            "&" | "bitand" | "and_i64" => self
                                .builder
                                .build_and(left.into_int_value(), right.into_int_value(), "and")
                                .unwrap(),
                            "|" | "bitor" | "or_i64" => self
                                .builder
                                .build_or(left.into_int_value(), right.into_int_value(), "or")
                                .unwrap(),
                            "^" | "bitxor" | "xor_i64" => self
                                .builder
                                .build_xor(left.into_int_value(), right.into_int_value(), "xor")
                                .unwrap(),

                            // Comparison operators (return i64: 1 for true, 0 for false)
                            "==" | "eq" | "eq_i64" => {
                                let cmp = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::EQ,
                                        left.into_int_value(),
                                        right.into_int_value(),
                                        "eq",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(cmp, self.i64_type, "eq_ext")
                                    .unwrap()
                            }
                            "!=" | "ne" | "ne_i64" => {
                                let cmp = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::NE,
                                        left.into_int_value(),
                                        right.into_int_value(),
                                        "ne",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(cmp, self.i64_type, "ne_ext")
                                    .unwrap()
                            }
                            "<" | "lt" | "lt_i64" => {
                                let cmp = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SLT,
                                        left.into_int_value(),
                                        right.into_int_value(),
                                        "lt",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(cmp, self.i64_type, "lt_ext")
                                    .unwrap()
                            }
                            ">" | "gt" | "gt_i64" => {
                                let cmp = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SGT,
                                        left.into_int_value(),
                                        right.into_int_value(),
                                        "gt",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(cmp, self.i64_type, "gt_ext")
                                    .unwrap()
                            }
                            "<=" | "le" | "le_i64" => {
                                let cmp = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SLE,
                                        left.into_int_value(),
                                        right.into_int_value(),
                                        "le",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(cmp, self.i64_type, "le_ext")
                                    .unwrap()
                            }
                            ">=" | "ge" | "ge_i64" => {
                                let cmp = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::SGE,
                                        left.into_int_value(),
                                        right.into_int_value(),
                                        "ge",
                                    )
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(cmp, self.i64_type, "ge_ext")
                                    .unwrap()
                            }

                            // Logical operators (treat i64 as boolean: 0=false, non-zero=true)
                            "&&" | "and" => {
                                // Convert to boolean (0 or 1) then logical AND
                                let left_bool = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::NE,
                                        left.into_int_value(),
                                        self.i64_type.const_int(0, false),
                                        "left_bool",
                                    )
                                    .unwrap();
                                let right_bool = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::NE,
                                        right.into_int_value(),
                                        self.i64_type.const_int(0, false),
                                        "right_bool",
                                    )
                                    .unwrap();
                                let bool_and = self
                                    .builder
                                    .build_and(left_bool, right_bool, "and")
                                    .unwrap();
                                self.builder
                                    .build_int_z_extend(bool_and, self.i64_type, "and_ext")
                                    .unwrap()
                            }
                            "||" | "or" => {
                                // Convert to boolean (0 or 1) then logical OR
                                let left_bool = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::NE,
                                        left.into_int_value(),
                                        self.i64_type.const_int(0, false),
                                        "left_bool",
                                    )
                                    .unwrap();
                                let right_bool = self
                                    .builder
                                    .build_int_compare(
                                        inkwell::IntPredicate::NE,
                                        right.into_int_value(),
                                        self.i64_type.const_int(0, false),
                                        "right_bool",
                                    )
                                    .unwrap();
                                let bool_or =
                                    self.builder.build_or(left_bool, right_bool, "or").unwrap();
                                self.builder
                                    .build_int_z_extend(bool_or, self.i64_type, "or_ext")
                                    .unwrap()
                            }
                            // Note: "!" (not) is unary, handled separately above

                            // Not an operator we handle inline
                            _ => {
                                let callee = self.get_function_with_types(func, type_args);
                                let arg_vals: Vec<BasicMetadataValueEnum> = args
                                    .iter()
                                    .map(|&id| self.gen_expr_safe(&id, exprs).into())
                                    .collect();
                                let call = self
                                    .builder
                                    .build_call(callee, &arg_vals, "")
                                    .unwrap();
                                // Convert BasicValueEnum to IntValue
                                let basic_val = Self::call_site_to_basic_value(call).unwrap();
                                basic_val.into_int_value()
                            }
                        };

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                    } else {
                        // Operator with wrong number of arguments, fall through to regular function call
                        let callee = self.get_function_with_types(func, type_args);
                        let arg_vals: Vec<BasicMetadataValueEnum> = args
                            .iter()
                            .map(|&id| self.gen_expr_safe(&id, exprs).into())
                            .collect();
                        let call = self
                            .builder
                            .build_call(callee, &arg_vals, "")
                            .unwrap();
                        if let Some(val) = Self::call_site_to_basic_value(call) {
                            let alloca = *self.locals.get(dest).unwrap();
                            self.builder.build_store(alloca, val).unwrap();
                        }
                    }
                } else if self.is_simd_operation(func) {
                    // Handle SIMD operations
                    let callee = self.get_function_with_types(func, type_args);
                    let arg_vals: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .map(|&id| self.gen_expr_safe(&id, exprs).into())
                        .collect();
                    let call = self
                        .builder
                        .build_call(callee, &arg_vals, "")
                        .unwrap();
                    if let Some(val) = Self::call_site_to_basic_value(call) {
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, val).unwrap();
                    }
                } else {
                    // Regular function call
                    // Handle unary minus before looking up function
                    if args.len() == 1 && (func == "-" || func == "unary_minus") {
                        let operand = self.gen_expr_safe(&args[0], exprs);
                        let zero = self.i64_type.const_zero();
                        let result = self.builder.build_int_sub(zero, operand.into_int_value(), "neg").unwrap();
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                        return;
                    }
                    let callee = self.get_function_with_types(func, type_args);

                    // Check if this is a runtime function that takes pointer arguments
                    let needs_ptr_arg = func == "option_is_some"
                        || func == "option_get_data"
                        || func == "option_free"
                        || func == "host_result_is_ok"
                        || func == "host_result_get_data"
                        || func == "host_result_free";

                    // Check if this is a runtime function that returns a pointer
                    let returns_ptr = func == "option_make_some"
                        || func == "option_make_none"
                        || func == "host_result_make_ok"
                        || func == "host_result_make_err";

                    let arg_vals: Vec<BasicMetadataValueEnum> = args
                        .iter()
                        .enumerate()
                        .map(|(i, &id)| {
                            let val = self.gen_expr_safe(&id, exprs);
                            // If this function needs pointer arguments, convert i64 to ptr
                            if needs_ptr_arg && i == 0 {
                                // First argument is the pointer
                                // Convert i64 to ptr
                                let int_val = val.into_int_value();
                                let ptr_val = self
                                    .builder
                                    .build_int_to_ptr(int_val, self.ptr_type, "inttoptr")
                                    .unwrap();
                                ptr_val.into()
                            } else {
                                val.into()
                            }
                        })
                        .collect();
                    let call = self
                        .builder
                        .build_call(callee, &arg_vals, "")
                        .unwrap();
                    if let Some(val) = Self::call_site_to_basic_value(call) {
                        // If function returns a pointer, convert it to i64
                        let final_val = if returns_ptr {
                            let ptr_val = val.into_pointer_value();
                            self.builder
                                .build_ptr_to_int(ptr_val, self.i64_type, "ptrtoint")
                                .unwrap()
                                .into()
                        } else {
                            val
                        };

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, final_val).unwrap();
                    }
                }
            }
            MirStmt::VoidCall { func, args } => {
                // DEBUG
                
                // === NEW: println(i64) support via println_i64 (bypasses get_function) ===
                if func == "println" && !args.is_empty() {
                    let val = self.gen_expr_safe(&args[0], exprs);
                    // Call Zeta runtime function println_i64 (no C dependencies)
                    if let Some(println_i64_fn) = self.module.get_function("println_i64") {
                        let _ = self
                            .builder
                            .build_call(println_i64_fn, &[val.into()], "println_call")
                            .unwrap();
                    } else {
                        // Fallback (should not happen)
                    }
                    return;
                }

                // Note: Special handling for array_set/stack_array_set removed
                // Always call runtime function to handle both stack and heap arrays
                // (Heap arrays have headers that need to be checked)

                // original code for other VoidCalls
                let callee = self.get_function(func);
                let arg_vals: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|&id| self.gen_expr_safe(&id, exprs).into())
                    .collect();
                let _ = self
                    .builder
                    .build_call(callee, &arg_vals, "void_call")
                    .unwrap();
            }
            MirStmt::Return { val } => {
                let ret_val = self.gen_expr_safe(val, exprs);
                self.builder.build_return(Some(&ret_val)).unwrap();
            }
            MirStmt::SemiringFold { op, values, result } => {
                if values.is_empty() {
                    let alloca = *self.locals.get(result).unwrap();
                    self.builder
                        .build_store(alloca, self.i64_type.const_zero())
                        .unwrap();
                    return;
                }
                let mut acc = self.gen_expr_safe(&values[0], exprs);
                for &val_id in &values[1..] {
                    let val = self.gen_expr_safe(&val_id, exprs);
                    acc = match op {
                        SemiringOp::Add => self
                            .builder
                            .build_int_add(acc.into_int_value(), val.into_int_value(), "fold_add")
                            .unwrap()
                            .into(),
                        SemiringOp::Mul => self
                            .builder
                            .build_int_mul(acc.into_int_value(), val.into_int_value(), "fold_mul")
                            .unwrap()
                            .into(),
                    };
                }
                let alloca = *self.locals.get(result).unwrap();
                self.builder.build_store(alloca, acc).unwrap();
            }
            MirStmt::TryProp {
                expr_id,
                ok_dest,
                err_dest,
            } => {
                let expr_val = self.gen_expr_safe(expr_id, exprs);
                let is_ok = self
                    .builder
                    .build_call(
                        self.get_function("host_result_is_ok"),
                        &[expr_val.into()],
                        "is_ok",
                    )
                    .unwrap();
                let is_ok_val = Self::call_site_to_basic_value(is_ok)
                    .unwrap()
                    .into_int_value();
                let is_ok_i1 = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        is_ok_val,
                        self.i64_type.const_zero(),
                        "is_ok_i1",
                    )
                    .unwrap();
                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let ok_bb = self.context.append_basic_block(parent_fn, "prop_ok");
                let err_bb = self.context.append_basic_block(parent_fn, "prop_err");
                let cont_bb = self.context.append_basic_block(parent_fn, "prop_cont");
                self.builder
                    .build_conditional_branch(is_ok_i1, ok_bb, err_bb)
                    .unwrap();
                self.builder.position_at_end(ok_bb);
                let data = self
                    .builder
                    .build_call(
                        self.get_function("host_result_get_data"),
                        &[expr_val.into()],
                        "get_data",
                    )
                    .unwrap();
                let data_val = Self::call_site_to_basic_value(data).unwrap();
                let ok_alloca = *self.locals.get(ok_dest).unwrap();
                self.builder.build_store(ok_alloca, data_val).unwrap();
                self.builder.build_unconditional_branch(cont_bb).unwrap();
                self.builder.position_at_end(err_bb);
                let err_alloca = *self.locals.get(err_dest).unwrap();
                self.builder.build_store(err_alloca, expr_val).unwrap();
                self.builder.build_unconditional_branch(cont_bb).unwrap();
                self.builder.position_at_end(cont_bb);
            }
            MirStmt::MapNew { dest } => {
                let call = self
                    .builder
                    .build_call(self.get_function("map_new"), &[], "map_new")
                    .unwrap();
                let ptr = Self::call_site_to_basic_value(call).unwrap();
                let ptr_i64 = self
                    .builder
                    .build_ptr_to_int(ptr.into_pointer_value(), self.i64_type, "map_ptr_i64")
                    .unwrap();
                let alloca = *self.locals.get(dest).unwrap();
                self.builder.build_store(alloca, ptr_i64).unwrap();
            }
            MirStmt::DictInsert {
                map_id,
                key_id,
                val_id,
            } => {
                let map_i64 = self.load_local(*map_id);
                let map_ptr = self
                    .builder
                    .build_int_to_ptr(map_i64.into_int_value(), self.ptr_type, "map_ptr")
                    .unwrap();
                let key_val = self.gen_expr_safe(key_id, exprs);
                let val_val = self.gen_expr_safe(val_id, exprs);
                let _ = self.builder.build_call(
                    self.get_function("map_insert"),
                    &[map_ptr.into(), key_val.into(), val_val.into()],
                    "dict_insert",
                );
            }
            MirStmt::DictGet {
                map_id,
                key_id,
                dest,
            } => {
                let map_i64 = self.load_local(*map_id);
                let map_ptr = self
                    .builder
                    .build_int_to_ptr(map_i64.into_int_value(), self.ptr_type, "map_ptr")
                    .unwrap();
                let key_val = self.gen_expr_safe(key_id, exprs);
                let call = self
                    .builder
                    .build_call(
                        self.get_function("map_get"),
                        &[map_ptr.into(), key_val.into()],
                        "dict_get",
                    )
                    .unwrap();
                let val = Self::call_site_to_basic_value(call).unwrap();
                let alloca = *self.locals.get(dest).unwrap();
                self.builder.build_store(alloca, val).unwrap();
            }
            MirStmt::If {
                cond,
                then,
                else_,
                dest,
            } => {
                for (i, s) in then.iter().enumerate() {
                }
                for (i, s) in else_.iter().enumerate() {
                }
                let cond_i64 = self.gen_expr_safe(cond, exprs).into_int_value();
                let cond_i1 = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        cond_i64,
                        self.i64_type.const_zero(),
                        "cond_i1",
                    )
                    .unwrap();
                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let then_bb = self.context.append_basic_block(parent_fn, "then");
                let else_bb = self.context.append_basic_block(parent_fn, "else");
                let merge_bb = self.context.append_basic_block(parent_fn, "merge");
                self.builder
                    .build_conditional_branch(cond_i1, then_bb, else_bb)
                    .unwrap();

                // Generate then block
                self.builder.position_at_end(then_bb);
                for s in then {
                    self.gen_stmt(s, exprs);
                }
                // Only branch to merge if block doesn't end with return
                if !then.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                // Generate else block
                self.builder.position_at_end(else_bb);
                for s in else_ {
                    self.gen_stmt(s, exprs);
                }
                // Only branch to merge if block doesn't end with return
                if !else_.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                // Continue at merge block
                self.builder.position_at_end(merge_bb);
                // dest is handled by assignments in the branches
            }
            MirStmt::While { cond, body } => {
                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                
                // Create basic blocks for loop
                let loop_cond_bb = self.context.append_basic_block(parent_fn, "while.cond");
                let loop_body_bb = self.context.append_basic_block(parent_fn, "while.body");
                let loop_exit_bb = self.context.append_basic_block(parent_fn, "while.exit");
                
                // Branch to condition block
                self.builder.build_unconditional_branch(loop_cond_bb).unwrap();
                
                // Generate condition block
                self.builder.position_at_end(loop_cond_bb);
                let cond_i64 = self.gen_expr_safe(cond, exprs).into_int_value();
                let cond_i1 = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        cond_i64,
                        self.i64_type.const_zero(),
                        "while.cond",
                    )
                    .unwrap();
                self.builder
                    .build_conditional_branch(cond_i1, loop_body_bb, loop_exit_bb)
                    .unwrap();
                
                // Generate loop body
                self.builder.position_at_end(loop_body_bb);
                for s in body {
                    self.gen_stmt(s, exprs);
                }
                // Branch back to condition (unless body ends with return)
                if !body.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
                    self.builder.build_unconditional_branch(loop_cond_bb).unwrap();
                }
                
                // Continue at exit block
                self.builder.position_at_end(loop_exit_bb);
            }
            
            MirStmt::For { iterator, pattern, var_id, body } => {
                // For now, implement simple range-based for loop: for i in start..end
                // We need to get the range expression
                if let Some(MirExpr::Range { start, end }) = exprs.get(iterator) {
                    let parent_fn = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    
                    // Create basic blocks for loop
                    let loop_cond_bb = self.context.append_basic_block(parent_fn, "for.cond");
                    let loop_body_bb = self.context.append_basic_block(parent_fn, "for.body");
                    let loop_exit_bb = self.context.append_basic_block(parent_fn, "for.exit");
                    
                    // Get start and end values
                    let start_val = self.gen_expr_safe(start, exprs).into_int_value();
                    let end_val = self.gen_expr_safe(end, exprs).into_int_value();
                    
                    // Get loop variable pointer from locals map
                    let loop_var_ptr = *self.locals.get(var_id).unwrap();
                    
                    // Initialize loop variable to start
                    self.builder.build_store(loop_var_ptr, start_val).unwrap();
                    
                    // Branch to condition block
                    self.builder.build_unconditional_branch(loop_cond_bb).unwrap();
                    
                    // Generate condition block
                    self.builder.position_at_end(loop_cond_bb);
                    
                    // Load current loop variable value
                    let current_val = self.builder.build_load(
                        self.i64_type,
                        loop_var_ptr,
                        ""
                    ).unwrap().into_int_value();
                    
                    // Check if current_val < end_val
                    let cond = self.builder.build_int_compare(
                        IntPredicate::SLT, // Signed less than
                        current_val,
                        end_val,
                        "for.cond"
                    ).unwrap();
                    
                    self.builder
                        .build_conditional_branch(cond, loop_body_bb, loop_exit_bb)
                        .unwrap();
                    
                    // Generate loop body
                    self.builder.position_at_end(loop_body_bb);
                    
                    // Store loop variable in local variables map for use in body
                    // We need to find the variable ID for this pattern
                    // For now, we'll just use the pointer directly
                    
                    for s in body {
                        self.gen_stmt(s, exprs);
                    }
                    
                    // Increment loop variable: i = i + 1
                    let current_val_after = self.builder.build_load(
                        self.i64_type,
                        loop_var_ptr,
                        ""
                    ).unwrap().into_int_value();
                    
                    let next_val = self.builder.build_int_add(
                        current_val_after,
                        self.i64_type.const_int(1, false),
                        ""
                    ).unwrap();
                    
                    self.builder.build_store(loop_var_ptr, next_val).unwrap();
                    
                    // Branch back to condition
                    self.builder.build_unconditional_branch(loop_cond_bb).unwrap();
                    
                    // Continue at exit block
                    self.builder.position_at_end(loop_exit_bb);
                } else {
                    // Not a range iterator - for now, just skip
                }
            }
                        MirStmt::Store { addr_id, val_id, pointee_width } => {
                // *addr = val — store val through the pointer
                let addr_i64 = self.gen_expr_safe(addr_id, exprs).into_int_value();
                let val = self.gen_expr_safe(val_id, exprs);
                let pointee_llvm_type = self.context.custom_width_int_type((*pointee_width as u32) * 8);
                let pointed_ptr_type = pointee_llvm_type.ptr_type(inkwell::AddressSpace::default());
                let ptr = self.builder.build_int_to_ptr(
                    addr_i64, pointed_ptr_type, "store_ptr",
                ).unwrap();
                // Truncate the i64 value to the pointee width before storing
                let narrowed = self.builder.build_int_truncate(
                    val.into_int_value(),
                    pointee_llvm_type,
                    "store_trunc",
                ).unwrap();
                self.builder.build_store(ptr, narrowed).unwrap();
            }            MirStmt::ParamInit { .. } => {} // handled at entry
            _ => {}
        }
    }

    fn gen_expr_safe(&mut self, id: &u32, exprs: &HashMap<u32, MirExpr>) -> BasicValueEnum<'ctx> {
        if let Some(expr) = exprs.get(id) {
            self.gen_expr(expr, exprs, Some(*id))
        } else {
            self.i64_type.const_zero().into()
        }
    }

    fn gen_expr(&mut self, expr: &MirExpr, exprs: &HashMap<u32, MirExpr>, expr_id: Option<u32>) -> BasicValueEnum<'ctx> {
        match expr {
            MirExpr::Var(id) => self.load_local(*id),
            MirExpr::Lit(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::StringLit(s) => {
                let global = self.module.add_global(
                    self.context.i8_type().array_type(s.len() as u32 + 1),
                    None,
                    "str_lit",
                );
                global.set_linkage(inkwell::module::Linkage::Private);
                global.set_constant(true);
                let mut bytes = s.as_bytes().to_vec();
                bytes.push(0);
                let values: Vec<_> = bytes
                    .iter()
                    .map(|&b| self.context.i8_type().const_int(b as u64, false))
                    .collect();
                global.set_initializer(&self.context.i8_type().const_array(&values));
                let gptr = global.as_pointer_value();
                self.builder
                    .build_ptr_to_int(gptr, self.i64_type, "str_ptr_i64")
                    .unwrap()
                    .into()
            }
            MirExpr::FString(ids) => {
                if ids.is_empty() {
                    return self.i64_type.const_int(0, false).into();
                }
                let mut res = self.gen_expr(&exprs[&ids[0]], exprs, None);
                for &id in &ids[1..] {
                    let next = self.gen_expr(&exprs[&id], exprs, None);
                    let call = self
                        .builder
                        .build_call(
                            self.module.get_function("host_str_concat").unwrap(),
                            &[res.into(), next.into()],
                            "fconcat",
                        )
                        .unwrap();
                    res = Self::call_site_to_basic_value(call).unwrap();
                }
                res
            }
            MirExpr::ConstEval(n) => self.i64_type.const_int(*n as u64, true).into(),
            MirExpr::SemiringFold { op, values } => {
                // Evaluate inline — doesn't depend on a body-side statement
                // having already stored to an alloca. This is essential for
                // while-loop conditions where the SemiringFold statement is
                // in the loop body but the condition check happens before it.
                let left = self.gen_expr(&exprs[&values[0]], exprs, None).into_int_value();
                if values.len() == 1 {
                    // Unary (e.g. unary minus)
                    match op {
                        crate::middle::mir::mir::SemiringOp::Mul | crate::middle::mir::mir::SemiringOp::Add => {
                            // Identity: return value unchanged
                            left.into()
                        }
                    }
                } else {
                    let right = self.gen_expr(&exprs[&values[1]], exprs, None).into_int_value();
                    let result = match op {
                        crate::middle::mir::mir::SemiringOp::Add => self.builder.build_int_add(left, right, "sef_add"),
                        crate::middle::mir::mir::SemiringOp::Mul => self.builder.build_int_mul(left, right, "sef_mul"),
                    };
                    result.unwrap().into()
                }
            }
            MirExpr::TimingOwned(inner_id) => {
                let ptr = *self.locals.get(inner_id).unwrap();
                self.builder
                    .build_load(self.i64_type, ptr, "timing_load")
                    .unwrap()
            }
            MirExpr::BinaryOp { op, left, right } => {
                let left_val = self.gen_expr(&exprs[left], exprs, None).into_int_value();
                let right_val = self.gen_expr(&exprs[right], exprs, None).into_int_value();
                
                match op.as_str() {
                    "<" => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SLT, left_val, right_val, "cmp_lt",
                        ).unwrap();
                        self.builder.build_int_z_extend(cmp, self.i64_type, "cmp_ext").unwrap().into()
                    }
                    ">" => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SGT, left_val, right_val, "cmp_gt",
                        ).unwrap();
                        self.builder.build_int_z_extend(cmp, self.i64_type, "cmp_ext").unwrap().into()
                    }
                    "<=" => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SLE, left_val, right_val, "cmp_le",
                        ).unwrap();
                        self.builder.build_int_z_extend(cmp, self.i64_type, "cmp_ext").unwrap().into()
                    }
                    ">=" => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::SGE, left_val, right_val, "cmp_ge",
                        ).unwrap();
                        self.builder.build_int_z_extend(cmp, self.i64_type, "cmp_ext").unwrap().into()
                    }
                    "==" => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::EQ, left_val, right_val, "cmp_eq",
                        ).unwrap();
                        self.builder.build_int_z_extend(cmp, self.i64_type, "cmp_ext").unwrap().into()
                    }
                    "!=" => {
                        let cmp = self.builder.build_int_compare(
                            IntPredicate::NE, left_val, right_val, "cmp_ne",
                        ).unwrap();
                        self.builder.build_int_z_extend(cmp, self.i64_type, "cmp_ext").unwrap().into()
                    }
                    "+" => self.builder.build_int_add(left_val, right_val, "add").unwrap().into(),
                    "-" => self.builder.build_int_sub(left_val, right_val, "sub").unwrap().into(),
                    "*" => self.builder.build_int_mul(left_val, right_val, "mul").unwrap().into(),
                    "/" => self.builder.build_int_signed_div(left_val, right_val, "div").unwrap().into(),
                    "%" => self.builder.build_int_signed_rem(left_val, right_val, "mod").unwrap().into(),
                    "&" => self.builder.build_and(left_val, right_val, "bitand").unwrap().into(),
                    "|" => self.builder.build_or(left_val, right_val, "bitor").unwrap().into(),
                    "^" => self.builder.build_xor(left_val, right_val, "bitxor").unwrap().into(),
                    "<<" => self.builder.build_left_shift(left_val, right_val, "shl").unwrap().into(),
                    ">>" => self.builder.build_right_shift(left_val, right_val, true, "shr").unwrap().into(),
                    _ => {
                        panic!("Unsupported binary operator in BinaryOp: {}", op);
                    }
                }
            }
            MirExpr::Struct { variant, fields } => {
                // Allocate struct on stack and store field values
                // Create a type key for caching
                let type_key = format!("{}_fields_{}", variant, fields.len());

                // Get or create LLVM struct type
                let struct_type = if let Some(ty) = self.specialized_types.get(&type_key) {
                    *ty
                } else {
                    // Create struct type based on field count
                    // For now, assume all fields are i64
                    let field_types: Vec<_> =
                        (0..fields.len()).map(|_| self.i64_type.into()).collect();

                    let ty = self.context.struct_type(&field_types, false);
                    self.specialized_types.insert(type_key.clone(), ty);
                    ty
                };

                // Allocate struct on stack
                let alloca = self
                    .builder
                    .build_alloca(struct_type, "struct_alloca")
                    .unwrap();

                // Store each field value
                for (i, (field_name, field_id)) in fields.iter().enumerate() {
                    let field_ptr = self
                        .builder
                        .build_struct_gep(
                            struct_type,
                            alloca,
                            i as u32,
                            "",
                        )
                        .unwrap();

                    let field_val = self.gen_expr(&exprs[field_id], exprs, None).into_int_value();
                    self.builder.build_store(field_ptr, field_val).unwrap();
                }

                // Return pointer to struct (as i64)
                // Convert pointer to integer
                let ptr_as_int = self
                    .builder
                    .build_ptr_to_int(alloca, self.i64_type, "ptr_to_int")
                    .unwrap();
                ptr_as_int.into()
            }
            MirExpr::FieldAccess { base, field } => {
                // Handle field access for structs
                let base_val = self.gen_expr(&exprs[base], exprs, None);

                // Convert base value to pointer if it's an integer (pointer stored as i64)
                let ptr = if let BasicValueEnum::IntValue(int_val) = base_val {
                    // Convert i64 to pointer
                    self.builder
                        .build_int_to_ptr(
                            int_val,
                            self.context.ptr_type(AddressSpace::default()),
                            "int_to_ptr",
                        )
                        .unwrap()
                } else if let BasicValueEnum::PointerValue(ptr_val) = base_val {
                    ptr_val
                } else {
                    // Not a pointer or integer, return 0
                    return self.i64_type.const_int(0, true).into();
                };

                // Get the base expression to determine struct type
                let base_expr = &exprs[base];
                let field_count = if let MirExpr::Struct { variant: _, fields } = base_expr {
                    fields.len()
                } else {
                    // Default to 2 fields for Pair<A, B>
                    2
                };

                // Create type key for lookup
                let type_key = format!("struct_fields_{}", field_count);

                // Get cached struct type or create it
                let struct_type = if let Some(ty) = self.specialized_types.get(&type_key) {
                    *ty
                } else {
                    // Create struct type with field_count i64 fields
                    let field_types: Vec<_> =
                        (0..field_count).map(|_| self.i64_type.into()).collect();

                    let ty = self.context.struct_type(&field_types, false);
                    self.specialized_types.insert(type_key.clone(), ty);
                    ty
                };

                let loaded_struct = self
                    .builder
                    .build_load(struct_type, ptr, "load_struct")
                    .unwrap();

                // Extract field based on name
                // For Pair<A, B> struct, field "first" is index 0, "second" is index 1
                let field_index = if field == "first" {
                    0
                } else if field == "second" {
                    1
                } else {
                    // Try to parse as numeric index
                    field.parse::<u32>().unwrap_or(0)
                };

                // Extract value from struct
                self.builder
                    .build_extract_value(
                        loaded_struct.into_struct_value(),
                        field_index,
                        "",
                    )
                    .unwrap()
            }
            MirExpr::As { expr, target_type } => {
                // Generate the expression value
                let expr_val = self.gen_expr(&exprs[expr], exprs, None);
                
                // For now, handle basic numeric conversions
                // TODO: Implement proper type conversion logic
                match target_type {
                    Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::Usize => {
                        // For integer types, just truncate or extend as needed
                        // For now, just return the value as-is (i64)
                        if let BasicValueEnum::IntValue(int_val) = expr_val {
                            // Convert to target integer type
                            match target_type {
                                Type::I8 => self.builder.build_int_truncate(int_val, self.context.i8_type(), "trunc_i8").unwrap().into(),
                                Type::I16 => self.builder.build_int_truncate(int_val, self.context.i16_type(), "trunc_i16").unwrap().into(),
                                Type::I32 => self.builder.build_int_truncate(int_val, self.context.i32_type(), "trunc_i32").unwrap().into(),
                                Type::I64 => int_val.into(), // Already i64
                                Type::U8 => self.builder.build_int_truncate(int_val, self.context.i8_type(), "trunc_u8").unwrap().into(),
                                Type::U16 => self.builder.build_int_truncate(int_val, self.context.i16_type(), "trunc_u16").unwrap().into(),
                                Type::U32 => self.builder.build_int_truncate(int_val, self.context.i32_type(), "trunc_u32").unwrap().into(),
                                Type::U64 => self.builder.build_int_cast(int_val, self.context.i64_type(), "cast_u64").unwrap().into(),
                                Type::Usize => {
                                    // usize is platform-dependent, use i64 for now
                                    self.builder.build_int_cast(int_val, self.context.i64_type(), "cast_usize").unwrap().into()
                                }
                                _ => int_val.into(),
                            }
                        } else {
                            // Not an integer, return as-is
                            expr_val
                        }
                    }
                    Type::F32 | Type::F64 => {
                        // Float conversions
                        // For now, just return 0
                        self.f64_type.const_float(0.0).into()
                    }
                    _ => {
                        // Other types - return expression as-is
                        expr_val
                    }
                }
            }
            MirExpr::StackArray { elements, size } => {
                // Allocate stack array and initialize with elements
                
                // Always use i64 for array elements to match runtime expectations
                // This wastes memory for bool arrays but ensures compatibility
                let elem_type = self.i64_type;
                
                // Create array type
                let array_type = elem_type.array_type(*size as u32);
                
                // Allocate on stack
                let alloca = self.builder.build_alloca(array_type, "stack_array").unwrap();
                
                // Initialize each element
                for (i, element_id) in elements.iter().enumerate() {
                    let element_val = self.gen_expr(&exprs[element_id], exprs, None).into_int_value();
                    let element_ptr = unsafe {
                        self.builder.build_gep(
                            array_type,
                            alloca,
                            &[
                                self.i64_type.const_int(0, false),
                                self.i64_type.const_int(i as u64, false)
                            ],
                            ""
                        ).unwrap()
                    };
                    self.builder.build_store(element_ptr, element_val).unwrap();
                }
                
                // Return pointer to array (as i64)
                let ptr_as_int = self.builder.build_ptr_to_int(alloca, self.i64_type, "array_ptr_to_int").unwrap();
                ptr_as_int.into()
            }
            MirExpr::Range { start, end } => {
                // For now, just return the start value
                // TODO: Implement proper range type
                self.gen_expr(&exprs[start], exprs, None)
            }
            MirExpr::Deref { addr_id, pointee_width } => {
                // *ptr — load through pointer with given element width
                let ptr_as_i64 = self.gen_expr_safe(addr_id, exprs).into_int_value();
                let pointee_llvm_type = self.context.custom_width_int_type((*pointee_width as u32) * 8);
                let pointed_ptr_type = pointee_llvm_type.ptr_type(inkwell::AddressSpace::default());
                let ptr = self.builder.build_int_to_ptr(
                    ptr_as_i64, pointed_ptr_type, "deref_ptr",
                ).unwrap();
                let val = self.builder.build_load(pointee_llvm_type, ptr, "deref_val").unwrap();
                // Zero-extend back to i64
                self.builder.build_int_z_extend(
                    val.into_int_value(),
                    self.i64_type,
                    "deref_ext",
                ).unwrap().into()
            }
        }
    }


    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = *self.locals.get(&id).unwrap();
        self.builder
            .build_load(self.i64_type, ptr, "")
            .unwrap()
    }

    fn call_site_to_basic_value(call: CallSiteValue<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match call.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(basic) => Some(basic),
            _ => None,
        }
    }

    /// Convert a Zeta type to an LLVM type
    pub fn type_to_llvm_type(&self, ty: &Type) -> inkwell::types::BasicTypeEnum<'ctx> {
        match ty {
            Type::I8 => self.context.i8_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I64 => self.context.i64_type().into(),
            Type::U8 => self.context.i8_type().into(),
            Type::U16 => self.context.i16_type().into(),
            Type::U32 => self.context.i32_type().into(),
            Type::U64 => self.context.i64_type().into(),
            Type::Usize => self.context.i64_type().into(), // Platform-dependent, assume 64-bit
            Type::F32 => self.context.f32_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Char => self.context.i32_type().into(), // Unicode scalar value
            Type::Str => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Range => self.context.struct_type(&[self.context.i64_type().into(), self.context.i64_type().into()], false).into(),
            Type::I32x4 => self.context.i32_type().array_type(4).into(),
            Type::I64x2 => self.context.i64_type().array_type(2).into(),
            Type::F32x4 => self.context.f32_type().array_type(4).into(),
            Type::Array(element_type, size) => {
                let element_llvm_type = self.type_to_llvm_type(element_type);
                let array_size = match size {
                    crate::middle::types::ArraySize::Literal(n) => *n as u32,
                    _ => 0, // Default for non-literal sizes
                };
                match element_llvm_type {
                    inkwell::types::BasicTypeEnum::IntType(int_type) => {
                        int_type.array_type(array_size).into()
                    }
                    inkwell::types::BasicTypeEnum::FloatType(float_type) => {
                        float_type.array_type(array_size).into()
                    }
                    inkwell::types::BasicTypeEnum::StructType(struct_type) => {
                        struct_type.array_type(array_size).into()
                    }
                    inkwell::types::BasicTypeEnum::PointerType(ptr_type) => {
                        ptr_type.array_type(array_size).into()
                    }
                    inkwell::types::BasicTypeEnum::VectorType(vec_type) => {
                        vec_type.array_type(array_size).into()
                    }
                    inkwell::types::BasicTypeEnum::ArrayType(array_type) => {
                        array_type.array_type(array_size).into()
                    }
                    inkwell::types::BasicTypeEnum::ScalableVectorType(scalable_vec_type) => {
                        // For now, treat scalable vectors as regular vectors
                        // This might need to be adjusted based on actual usage
                        scalable_vec_type.array_type(array_size).into()
                    }
                }
            }
            Type::Slice(element_type) => {
                let element_llvm_type = self.type_to_llvm_type(element_type);
                // Slice is (pointer, length)
                self.context.struct_type(&[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.i64_type().into()
                ], false).into()
            }
            Type::DynamicArray(element_type) => {
                let element_llvm_type = self.type_to_llvm_type(element_type);
                // Dynamic array is similar to slice
                self.context.struct_type(&[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.i64_type().into(),
                    self.context.i64_type().into() // capacity
                ], false).into()
            }
            Type::Tuple(element_types) => {
                let llvm_element_types: Vec<inkwell::types::BasicTypeEnum> = 
                    element_types.iter().map(|t| self.type_to_llvm_type(t)).collect();
                self.context.struct_type(&llvm_element_types, false).into()
            }
            Type::Ptr(element_type, _) => {
                let _element_llvm_type = self.type_to_llvm_type(element_type);
                self.context.ptr_type(AddressSpace::default()).into()
            }
            Type::Ref(element_type, _, _) => {
                let _element_llvm_type = self.type_to_llvm_type(element_type);
                self.context.ptr_type(AddressSpace::default()).into()
            }
            Type::Vector(element_type, size) => {
                let element_llvm_type = self.type_to_llvm_type(element_type);
                // Create SIMD vector type
                let vector_size = match size {
                    crate::middle::types::ArraySize::Literal(n) => *n as u32,
                    _ => 0, // Default for non-literal sizes
                };
                match element_llvm_type {
                    inkwell::types::BasicTypeEnum::IntType(int_type) => {
                        int_type.vec_type(vector_size).into()
                    }
                    inkwell::types::BasicTypeEnum::FloatType(float_type) => {
                        float_type.vec_type(vector_size).into()
                    }
                    inkwell::types::BasicTypeEnum::ScalableVectorType(_) => {
                        // For scalable vectors, use regular vector for now
                        self.context.i64_type().vec_type(vector_size).into()
                    }
                    _ => {
                        // Fallback: treat as array for unsupported element types
                        // Use i64 array as fallback
                        self.context.i64_type().array_type(vector_size).into()
                    }
                }
            }
            Type::Named(name, type_args) => {
                // For named types, check if we have a cached specialized type
                let type_key = self.mangle_type_name(name, type_args);
                if let Some(&cached_type) = self.specialized_types.get(&type_key) {
                    return cached_type.into();
                }
                
                // Default to i64 for unknown named types
                self.context.i64_type().into()
            }
            Type::TraitObject(_) => {
                // Trait object is (data pointer, vtable pointer)
                self.context.struct_type(&[
                    self.context.ptr_type(AddressSpace::default()).into(),
                    self.context.ptr_type(AddressSpace::default()).into()
                ], false).into()
            }
            Type::Function(param_types, return_type) => {
                // Function types are represented as function pointers
                self.context.ptr_type(AddressSpace::default()).into()
            }
            Type::AsyncFunction(param_types, return_type) => {
                // Async function returns a future, represented as function pointer
                self.context.ptr_type(AddressSpace::default()).into()
            }
            Type::Variable(_) => {
                // Type variable - use i64 as placeholder
                self.context.i64_type().into()
            }
            Type::Constructor(_, _, _) => {
                // Type constructor - use i64 as placeholder
                self.context.i64_type().into()
            }
            Type::PartialApplication(_, _) => {
                // Partial application - use i64 as placeholder
                self.context.i64_type().into()
            }
            Type::Error => {
                // Error type - use i64 as placeholder
                self.context.i64_type().into()
            }
            Type::Identity(_) => {
                // Identity type - treat as string for now
                self.context.ptr_type(AddressSpace::default()).into()
            }
        }
    }

    /// Mangle a type name with type arguments
    fn mangle_type_name(&self, base_name: &str, type_args: &[Type]) -> String {
        if type_args.is_empty() {
            return base_name.to_string();
        }

        let mut mangled = base_name.to_string();
        mangled.push_str("_inst");

        for ty in type_args {
            mangled.push('_');
            mangled.push_str(&ty.mangled_name());
        }

        mangled
    }
}
