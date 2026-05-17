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
    pub loop_stack: Vec<(
        inkwell::basic_block::BasicBlock<'ctx>,
        inkwell::basic_block::BasicBlock<'ctx>,
    )>,

    // Current type map for the function being compiled
    pub current_type_map: Option<std::collections::HashMap<u32, crate::middle::types::Type>>,
    /// Struct type definitions: maps type name to list of field names
    /// Populated from all MIRs during gen_mirs preprocessing.
    pub struct_defs: std::collections::HashMap<String, Vec<String>>,
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
        // SIMD vector operations are handled inline in codegen via LLVM vector IR.
        // No extern declarations needed — vectors are LLVM native types, not heap-allocated.
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
            "time_is_up",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "print_result",
            void_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        // LLVM ctpop intrinsic (POPCNT instruction) — declared with intrinsic name
        // so LLVM recognizes and lowers it to the hardware popcnt instruction
        module.add_function(
            "llvm.ctpop.i64",
            i64_type.fn_type(&[i64_type.into()], false),
            None,
        );
        // Zeta name that maps to llvm.ctpop.i64 in the call handler
        module.add_function(
            "__builtin_ctpop",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        // Declare llvm.memset.i64 intrinsic: void(ptr, i8, i64, i1)
        module.add_function(
            "llvm.memset.i64",
            void_type.fn_type(
                &[
                    context.ptr_type(AddressSpace::default()).into(),
                    context.i8_type().into(),
                    i64_type.into(),
                    context.bool_type().into(),
                ],
                false,
            ),
            None,
        );
        
        // ── Reactor runtime functions (from zeta_src/runtime/reactor.z) ──
        module.add_function(
            "reactor_create",
            i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "reactor_add",
            i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "reactor_remove",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "reactor_poll",
            i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "reactor_event_fd",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "reactor_event_flags",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "reactor_destroy",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "waker_create",
            i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "waker_wake",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "waker_consume",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "waker_destroy",
            void_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "set_nonblocking",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "monotonic_ns",
            i64_type.fn_type(&[], false),
            Some(Linkage::External),
        );
        module.add_function(
            "scheduler_register_waker",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "scheduler_run_reactor",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
// V4I64 vector intrinsics (AVX2) — handled inline in codegen
        module.add_function(
            "__builtin_v4i64_store",
            void_type.fn_type(
                &[
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );
        module.add_function(
            "__builtin_v4i64_andnot",
            void_type.fn_type(
                &[
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
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
            void_type.fn_type(
                &[
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                    i64_type.into(),
                ],
                false,
            ),
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
            "run_sieve_timed",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "parallel_sieve",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "parallel_sieve_timed",
            i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false),
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
        module.add_function(
            "host_str_split",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_join",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_find",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_count",
            i64_type.fn_type(&[i64_type.into(), i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_strip",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_lstrip",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_rstrip",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_isalpha",
            i64_type.fn_type(&[i64_type.into()], false),
            Some(Linkage::External),
        );
        module.add_function(
            "host_str_isnumeric",
            i64_type.fn_type(&[i64_type.into()], false),
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

        // SIMD intrinsics handled inline in codegen via LLVM vector IR.
        // No extern declarations needed.

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
            loop_stack: Vec::new(),

            current_type_map: None,
            struct_defs: std::collections::HashMap::new(),
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
        // Preprocess: collect struct field definitions from all MIR expressions.
        // Store field name -> index mappings for later use in FieldAccess resolution.
        self.struct_defs.clear();
        for mir in mirs {
            for (_, expr) in mir.exprs.iter() {
                if let MirExpr::Struct {
                    variant: v,
                    fields: fds,
                } = expr
                {
                    let type_key = format!("struct_{}_{}", v, fds.len());
                    let field_names: Vec<String> = fds.iter().map(|(n, _)| n.clone()).collect();
                    self.struct_defs
                        .entry(type_key)
                        .or_insert_with(|| field_names);
                }
            }
        }

        // First pass: collect all functions
        for mir in mirs {
            let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());

            // Check if this is a generic function
            let is_generic = self.is_generic_function(mir);

            if is_generic {
                // Store generic definition for later instantiation
                self.generic_defs.insert(fn_name.clone(), mir.clone());
            } else {
                // Non-generic function: declare as before.
                // If function with this name already exists in the module with
                // a DIFFERENT param count, use a mangled name: name_N where N
                // is the param count. This avoids LLVM name collisions for
                // overloaded functions (e.g., fn new() vs fn new(input)).
                let is_overloaded = self.module.get_function(&fn_name)
                    .map(|f| f.count_params() != mir.param_indices.len() as u32)
                    .unwrap_or(false);
                let actual_name = if is_overloaded {
                    format!("{}_{}", fn_name, mir.param_indices.len())
                } else {
                    fn_name.clone()
                };
                if !self.fns.contains_key(&actual_name) && self.module.get_function(&actual_name).is_none() {
                    let param_types: Vec<_> = (0..mir.param_indices.len())
                        .map(|_| self.i64_type.into())
                        .collect();
                    let fn_type = self.i64_type.fn_type(&param_types, false);
                    let fn_val = self.module.add_function(&actual_name, fn_type, None);
                    self.fns.insert(actual_name.clone(), fn_val);
                }
            }
        }

        // Second pass: generate non-generic function bodies
        for mir in mirs {
            if !self.is_generic_function(mir) {
                let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
                let param_count = mir.param_indices.len() as u32;

                // Determine actual function name, matching first-pass logic.
                let actual_name = self.module.get_function(&fn_name)
                    .map(|f| f.count_params() != param_count)
                    .unwrap_or(false)
                    .then(|| format!("{}_{}", fn_name, param_count))
                    .unwrap_or_else(|| fn_name.clone());

                // Skip if this function was pre-declared with a non-i64 return type.
                let should_skip = self.module.get_function(&actual_name)
                    .and_then(|f| match f.get_type().get_return_type() {
                        Some(rt) if rt == self.i64_type.into() => None,
                        _ => Some(()),
                    })
                    .is_some();

                if !should_skip {
                    self.gen_fn(mir);
                }
            }
            // Generic functions are generated on-demand via monomorphization
        }
    }

    fn gen_fn(&mut self, mir: &Mir) {
        // Use param-count-suffixed name if this function is overloaded
        let fn_name = mir.name.as_ref().cloned().unwrap_or("anon".to_string());
        let param_count = mir.param_indices.len() as u32;
        let actual_name = self.module.get_function(&fn_name)
            .map(|f| f.count_params() != param_count)
            .unwrap_or(false)
            .then(|| format!("{}_{}", fn_name, param_count))
            .unwrap_or_else(|| fn_name.clone());
        // First try the actual (potentially mangled) name, then fall back to the original
        let fn_val = self.get_function(&actual_name);

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
            let alloca = self.builder.build_alloca(self.i64_type, "").unwrap();
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
            MirStmt::Store {
                addr_id, val_id, ..
            } => {
                ids.insert(*addr_id);
                if let Some(e) = exprs.get(val_id) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirStmt::Consume { id } => {
                ids.insert(*id);
            }
            MirStmt::Break | MirStmt::Continue => {
                // No IDs to collect
            }
            MirStmt::StructNew {
                variant: _,
                fields,
                dest,
            } => {
                ids.insert(*dest);
                for (_, field_id) in fields {
                    if let Some(e) = exprs.get(field_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirStmt::Swap { a_ptr, b_ptr, size } => {
                ids.insert(*a_ptr);
                ids.insert(*b_ptr);
                ids.insert(*size);
            }
            MirStmt::Pre { cond, .. }
            | MirStmt::Post { cond, .. }
            | MirStmt::Invariant { cond, .. } => {
                if let Some(e) = exprs.get(cond) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
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
            MirExpr::Struct { variant: _, fields } => {
                for (_, field_id) in fields {
                    if let Some(e) = exprs.get(field_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirExpr::FieldAccess { base, field: _ } => {
                if let Some(e) = exprs.get(base) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirExpr::As {
                expr,
                target_type: _,
            } => {
                if let Some(e) = exprs.get(expr) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirExpr::StackArray { elements, size: _ } => {
                for elem_id in elements {
                    if let Some(e) = exprs.get(elem_id) {
                        self.collect_ids_from_expr_safe(e, ids, exprs);
                    }
                }
            }
            MirExpr::Range { start, end } => {
                if let Some(e) = exprs.get(start) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
                if let Some(e) = exprs.get(end) {
                    self.collect_ids_from_expr_safe(e, ids, exprs);
                }
            }
            MirExpr::ConstEval(_) | MirExpr::StringLit(_) | MirExpr::Lit(_) | MirExpr::Syscall(_, _) => {
                // No IDs to collect
            }
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
        name.starts_with("simd_")
            || name.starts_with("vector_")
            || name.starts_with("simd::")
            || name.starts_with("Vector::")
            || name.starts_with("Vector__")
    }

    /// Parse type info from a SIMD operation name.
    fn parse_simd_type_info(name: &str) -> Option<(u32, u32, bool)> {
        if name.contains("i32x4") {
            Some((32, 4, false))
        } else if name.contains("i64x2") {
            Some((64, 2, false))
        } else if name.contains("f32x4") {
            Some((32, 4, true))
        } else if name.contains("u64x8") {
            Some((64, 8, false))
        } else if name.contains("i64x4") {
            Some((64, 4, false))
        } else if name.contains("i32x8") {
            Some((32, 8, false))
        } else {
            None
        }
    }

    fn simd_vector_type(
        &self,
        bit_width: u32,
        lanes: u32,
        is_float: bool,
    ) -> inkwell::types::VectorType<'ctx> {
        if is_float && bit_width == 32 && lanes == 4 {
            self.context.f32_type().vec_type(lanes)
        } else if bit_width == 64 && lanes == 4 {
            self.vec4_i64_type
        } else if bit_width == 64 {
            self.context.i64_type().vec_type(lanes)
        } else if bit_width == 32 {
            self.context.i32_type().vec_type(lanes)
        } else {
            self.vec4_i64_type
        }
    }

    fn simd_alloca_vec(
        &mut self,
        vt: inkwell::types::VectorType<'ctx>,
    ) -> (PointerValue<'ctx>, inkwell::values::IntValue<'ctx>) {
        let alloca = self.builder.build_alloca(vt, "simd_tmp").unwrap();
        let ptr = self
            .builder
            .build_ptr_to_int(alloca, self.i64_type, "simd_ptr")
            .unwrap();
        (alloca, ptr)
    }

    fn simd_load_vec(
        &mut self,
        ptr: inkwell::values::IntValue<'ctx>,
        vt: inkwell::types::VectorType<'ctx>,
    ) -> inkwell::values::VectorValue<'ctx> {
        let cast = self
            .builder
            .build_int_to_ptr(ptr, self.ptr_type, "vec_p")
            .unwrap();
        self.builder
            .build_load(vt, cast, "vec")
            .unwrap()
            .into_vector_value()
    }

    fn simd_store_vec(
        &mut self,
        ptr: inkwell::values::IntValue<'ctx>,
        val: inkwell::values::VectorValue<'ctx>,
    ) {
        let cast = self
            .builder
            .build_int_to_ptr(ptr, self.ptr_type, "vec_p")
            .unwrap();
        self.builder.build_store(cast, val).unwrap();
    }

    fn simd_trunc_val(
        &mut self,
        val: inkwell::values::BasicValueEnum<'ctx>,
        bit_width: u32,
        is_float: bool,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        if bit_width == 32 && !is_float {
            self.builder
                .build_int_truncate(val.into_int_value(), self.context.i32_type(), "trunc")
                .unwrap()
                .into()
        } else if is_float {
            val.into_float_value().into()
        } else {
            val.into_int_value().into()
        }
    }

    fn simd_zext_i64(
        &mut self,
        val: inkwell::values::BasicValueEnum<'ctx>,
    ) -> inkwell::values::BasicValueEnum<'ctx> {
        self.builder
            .build_int_z_extend(val.into_int_value(), self.i64_type, "zext")
            .unwrap()
            .into()
    }

    fn get_function(&self, name: &str) -> FunctionValue<'ctx> {
        if let Some(&f) = self.fns.get(name) {
            return f;
        }

        // Handle Type::method names (static methods)
        if name.contains("::") {
            let mangled = name.replace("::", "__");
            // Try the mangled name first (consistent with gen_mirs and resolver mangling)
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
        // Try param-count-suffixed names: "new" → "new_0", "new_1", etc.
        // (handles overloaded functions disambiguated by gen_mirs)
        for pc in 0..=10u32 {
            let suffixed = format!("{}_{}", name, pc);
            if let Some(f) = self.module.get_function(&suffixed) {
                return f;
            }
            if let Some(&f) = self.fns.get(&suffixed) {
                return f;
            }
        }

        // Strip trailing _N suffix with param count validation.
        // This reverses the MIR gen's name_N disambiguation.
        // e.g., "host_result_is_ok_1" → "host_result_is_ok" only if count matches.
        if let Some(pos) = name.rfind('_') {
            let suffix = &name[pos + 1..];
            let digits: String = suffix.chars().filter(|c| c.is_ascii_digit()).collect();
            if !digits.is_empty() && suffix.len() == digits.len() {
                let expected = digits.parse::<u32>().unwrap_or(0);
                let base = &name[..pos];
                if let Some(&f) = self.fns.get(base) {
                    if f.count_params() == expected { return f; }
                }
                if let Some(f) = self.module.get_function(base) {
                    if f.count_params() == expected { return f; }
                }
            }
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
        // println (no-args or with args) maps to println_i64 in the module.
        // The println! macro and user-declared extern fn println(msg: i64)
        // both resolve through this path.
        if name == "println"
            && let Some(f) = self.module.get_function("println_i64")
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
        if (name == "Result::Ok" || name == "Ok")
            && let Some(f) = self.module.get_function("host_result_make_ok")
        {
            return f;
        }
        if (name == "Result::Err" || name == "Err")
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

        // Handle generic unresolved functions — declare as external rather than panicking
        // This allows path-qualified calls (Resolver::new, HashMap::new, etc.) and
        // other user-defined functions to be resolved by the linker at AOT time, or
        // to be provided via the JIT's global mapping table.
        // If we're still in the module definition phase, emit a declaration.
        if name.contains("::") || self.module.get_function(name).is_none() {
            // Create a declaration for the function (will be resolved at link time)
            let void_type = self.context.void_type();
            let fn_type = void_type.fn_type(&[self.i64_type.into()], false);
            let f = self
                .module
                .add_function(name, fn_type, Some(Linkage::External));
            return f;
        }
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
            // Check if the function already exists (both raw and mangled)
            if let Some(f) = self.module.get_function(name) {
                return f;
            }
            if name.contains("::") {
                let mangled = name.replace("::", "__");
                if let Some(f) = self.module.get_function(&mangled) {
                    return f;
                }
                // Also check fns cache
                if let Some(&f) = self.fns.get(name) {
                    return f;
                }
                if let Some(&f) = self.fns.get(&mangled) {
                    return f;
                }
            }
            // Self-hosting bootstrap: path-qualified calls (e.g., Resolver::new)
            // may not have LLVM declarations yet — create extern stubs.
            // Use mangled name (:: → __) to match gen_mirs declarations.
            if name.contains("::") {
                let mangled = name.replace("::", "__");
                let fn_type = self.i64_type.fn_type(&[self.i64_type.into()], true);
                let f = self
                    .module
                    .add_function(&mangled, fn_type, Some(Linkage::External));
                self.fns.insert(mangled, f);
                return f;
            }
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
        // Use mangled name (includes type args) for extern declaration
        if let Some(f) = self.module.get_function(&mangled_name) {
            return f;
        }
        // Create extern declaration with mangled name — resolution happens at link time
        // Use 0-param variadic so it accepts any call site's argument count.
        let fn_type = self.i64_type.fn_type(&[], false);
        let f = self
            .module
            .add_function(&mangled_name, fn_type, Some(Linkage::External));
        self.specialized_fns.insert(mangled_name.clone(), f);
        f
    }

    /// Map method call mangled names (e.g. `len_str`) back to runtime function names (`str_len`).
    /// Method calls on strings produce "{method}_{type_suffix}" via MonoKey::mangle.
    fn resolve_string_method(name: &str) -> Option<String> {
        if !name.ends_with("_str") && !name.ends_with("_String") && !name.starts_with("str_") {
            return None;
        }
        Some(if name.starts_with("str_") {
            // Direct str_* call: str_len → host_str_len
            name.to_string()
        } else {
            // Method call: len_str → strip _str → str_len
            let method = name.trim_end_matches("_str").trim_end_matches("_String");
            match method {
                "len" | "length" => "str_len".to_string(),
                "concat" => "str_concat".to_string(),
                "lowercase" | "to_lowercase" => "str_to_lowercase".to_string(),
                "uppercase" | "to_uppercase" => "str_to_uppercase".to_string(),
                "trim" => "str_trim".to_string(),
                "strip" => "str_trim".to_string(),
                "starts_with" | "startswith" => "str_starts_with".to_string(),
                "ends_with" | "endswith" => "str_ends_with".to_string(),
                "contains" | "includes" => "str_contains".to_string(),
                "replace" => "str_replace".to_string(),
                "split" => "str_split".to_string(),
                "join" => "str_join".to_string(),
                "find" | "index" => "str_find".to_string(),
                "count" => "str_count".to_string(),
                "isalpha" | "is_alpha" => "str_isalpha".to_string(),
                "isdigit" | "is_digit" | "isnumeric" | "is_numeric" => "str_isnumeric".to_string(),
                _ => return None,
            }
        })
    }

    /// Get or create a function for a call site, ensuring the declaration matches
    /// the actual number of arguments. Creates extern declarations for functions
    /// that don't exist yet (self-hosting bootstrap path).
    fn get_or_declare_function(
        &mut self,
        name: &str,
        type_args: &[crate::middle::types::Type],
        args_count: usize,
    ) -> FunctionValue<'ctx> {
        // Try existing lookup first — check mangled name (:: → __) BEFORE raw name.
        // gen_mirs stores functions with __, call sites reference with ::.
        // Also handle str_* → host_str_* mapping (string runtime functions).
        let host_mapped_name = if name.starts_with("str_") {
            format!("host_{}", name)
        } else if let Some(resolved) = Self::resolve_string_method(name) {
            format!("host_{}", resolved)
        } else {
            String::new()
        };

        if type_args.is_empty() {
            // Check mangled name first (AstNode::Lit → AstNode__Lit)
            if name.contains("::") {
                let mangled = name.replace("::", "__");
                if let Some(f) = self.module.get_function(&mangled) {
                    return f;
                }
                if let Some(&f) = self.fns.get(&mangled) {
                    return f;
                }
                // Also try the method name directly (Self::new → new)
                // with param count validation and suffix search.
                if let Some(method) = name.split("::").last() {
                    // Try bare method name with param count
                    if let Some(f) = self.module.get_function(method) {
                        if f.count_params() == args_count as u32 {
                            return f;
                        }
                    }
                    // Try param-suffixed: new_0, new_1, etc.
                    let suffixed = format!("{}_{}", method, args_count);
                    if let Some(f) = self.module.get_function(&suffixed) {
                        return f;
                    }
                }
            }
            // Check host-mapped name for str_* functions
            if !host_mapped_name.is_empty() {
                if let Some(f) = self.module.get_function(&host_mapped_name) {
                    return f;
                }
                if let Some(&f) = self.fns.get(&host_mapped_name) {
                    return f;
                }
            }
            // Fall through to raw name check.
            // Only return the function if the param count matches.
            // If param count differs, try param-suffixed name below.
            if let Some(f) = self.module.get_function(name) {
                if f.count_params() == args_count as u32 {
                    return f;
                }
            }
            if let Some(&f) = self.fns.get(name) {
                if f.count_params() == args_count as u32 {
                    return f;
                }
            }
            // Try param-count-suffixed name before extern declaration.
            // Handles overloaded functions declared as name_N in gen_mirs.
            let param_suffixed = format!("{}_{}", name, args_count);
            if let Some(f) = self.module.get_function(&param_suffixed) { return f; }
            if let Some(&f) = self.fns.get(&param_suffixed) { return f; }
        } else {
            let mangled = self.mangle_function_name(name, type_args);
            if let Some(f) = self.module.get_function(&mangled) {
                return f;
            }
            eprintln!("CODEGEN_CALL: name={} mangled={} type_args={:?}", name, mangled, type_args);
            // Also try stripping trailing _N from the full name before mangling.
            // MIR gen appends _0 for overload disambiguation (oneshot::channel_0),
            // but the monomorphized function is stored as oneshot::channel_inst_i64.
            if let Some(pos) = name.rfind('_') {
                let suffix = &name[pos + 1..];
                if suffix.chars().all(|c| c.is_ascii_digit()) {
                    let base = &name[..pos];
                    let base_mangled = self.mangle_function_name(base, type_args);
                    if let Some(f) = self.module.get_function(&base_mangled) {
                        return f;
                    }
                }
            }
            // For path-qualified names (e.g., "oneshot::channel::channel_0"), also try mangling
            // just the base method name — monomorphized functions use fully-qualified keys.
            if name.contains("::") {
                if let Some(method) = name.split("::").last() {
                    let method_mangled = self.mangle_function_name(method, type_args);
                    if let Some(f) = self.module.get_function(&method_mangled) {
                        return f;
                    }
                    // Strip trailing _N from method name
                    if let Some(pos) = method.rfind('_') {
                        let suffix = &method[pos + 1..];
                        if suffix.chars().all(|c| c.is_ascii_digit()) {
                            let base_method = &method[..pos];
                            let base_mangled = self.mangle_function_name(base_method, type_args);
                            if let Some(f) = self.module.get_function(&base_mangled) {
                                return f;
                            }
                        }
                    }
                }
                // Also try the fully-qualified module path as the function name.
                // e.g., "oneshot::channel::channel_0" → qualified = "oneshot::channel"
                // Then mangle: "oneshot::channel_inst_i64" and look up with __.
                if let Some(last_colon) = name.rfind("::") {
                    let qualified = &name[..last_colon]; // strip "::channel_0"
                    let qual_mangled = self.mangle_function_name(qualified, type_args);
                    // gen_mirs stores functions with :: replaced by __
                    let qual_stored = qual_mangled.replace("::", "__");
                    if let Some(f) = self.module.get_function(&qual_stored) {
                        return f;
                    }
                    // Also strip _N from the last component of the qualified name
                    if let Some(pos) = qualified.rfind('_') {
                        let suffix = &qualified[pos + 1..];
                        if suffix.chars().all(|c| c.is_ascii_digit()) {
                            let base_qual = &qualified[..pos];
                            let base_qual_mangled = self.mangle_function_name(base_qual, type_args);
                            let base_qual_stored = base_qual_mangled.replace("::", "__");
                            if let Some(f) = self.module.get_function(&base_qual_stored) {
                                return f;
                            }
                        }
                    }
                }
            }
            // Check host-mapped name
            if !host_mapped_name.is_empty()
                && let Some(f) = self.module.get_function(&host_mapped_name)
            {
                return f;
            }
            // Check bare name in case it's a non-generic runtime function
            if let Some(f) = self.module.get_function(name) {
                return f;
            }
        }
        // Try param-count-suffixed name before creating extern.
        let param_suffixed = format!("{}_{}", name, args_count);
        if let Some(f) = self.module.get_function(&param_suffixed) { return f; }
        if let Some(&f) = self.fns.get(&param_suffixed) { return f; }

        // Strip trailing _N suffix and try base name with param count validation.
        // (Reverses the MIR gen's name_N disambiguation for runtime/pre-declared fns.)
        let mut stripped_name = String::new();
        if let Some(pos) = name.rfind('_') {
            let suffix = &name[pos + 1..];
            let digits: String = suffix.chars().filter(|c| c.is_ascii_digit()).collect();
            if !digits.is_empty() {
                let base = &name[..pos];
                if let Some(f) = self.module.get_function(base) {
                    let expected = digits.parse::<u32>().unwrap_or(0);
                    if f.count_params() == expected {
                        return f;
                    }
                }
                if let Some(&f) = self.fns.get(base) {
                    let expected = digits.parse::<u32>().unwrap_or(0);
                    if f.count_params() == expected {
                        return f;
                    }
                }
                // Base not declared yet — use the stripped name so extern declaration
                // (created below) matches runtime symbols (e.g., tcp_bind vs tcp_bind_2).
                stripped_name = base.to_string();
            }
        }

        // Not found — create extern declaration matching the call site's arg count
        let base_name = if stripped_name.is_empty() { name } else { &stripped_name };
        
        // For path-qualified names (e.g., "runtime::reactor_create"), ensure the
        // declaration uses the correct name. Try qualified form first (with __),
        // then bare method name, to avoid colliding names across modules.
        if base_name.contains("::") {
            // First try the qualified form: oneshot::channel → oneshot__channel
            let qualified_actual = if type_args.is_empty() {
                base_name.replace("::", "__")
            } else {
                self.mangle_function_name(base_name, type_args).replace("::", "__")
            };
            if let Some(f) = self.module.get_function(&qualified_actual) {
                return f;
            }
            // Check bare method name (for runtime symbols like reactor_create)
            let bare_method = base_name.split("::").last().unwrap_or(base_name);
            let bare_actual = if type_args.is_empty() {
                bare_method.to_string()
            } else {
                self.mangle_function_name(bare_method, type_args)
            };
            if let Some(f) = self.module.get_function(&bare_actual) {
                // Only return the bare function if its param count matches the call site.
                // Otherwise qualify the name to avoid collisions between different
                // types' methods with the same name (e.g., String::new vs LLVMCodegen::new).
                if f.count_params() == args_count as u32 {
                    return f;
                }
            }
            // Try qualified name before declaring new extern.
            if let Some(f) = self.module.get_function(&qualified_actual) {
                if f.count_params() == args_count as u32 {
                    return f;
                }
            }
            // Use qualified name for extern declaration to avoid collisions between
            // path-qualified names with the same bare method name but different param counts
            // (e.g., String::new() vs LLVMCodegen::new("bench")).
            let param_types: Vec<_> = (0..args_count).map(|_| self.i64_type.into()).collect();
            let fn_type = self.i64_type.fn_type(&param_types, false);
            return self.module.add_function(&qualified_actual, fn_type, Some(Linkage::External));
        }
        
        // Non-qualified name: use as-is
        let actual_name = if type_args.is_empty() {
            base_name.to_string()
        } else {
            self.mangle_function_name(base_name, type_args)
        };
        let param_types: Vec<_> = (0..args_count).map(|_| self.i64_type.into()).collect();
        let fn_type = self.i64_type.fn_type(&param_types, false);
        self.module
            .add_function(&actual_name, fn_type, Some(Linkage::External))
    }

    /// Return the byte size of a type from its monomorphized name suffix.
    fn type_size_from_name(&self, name: &str) -> usize {
        match name {
            "i8" | "u8" => 1,
            "i16" | "u16" => 2,
            "i32" | "u32" | "f32" | "char" => 4,
            "i64" | "u64" | "f64" | "bool" | "usize" | "isize" | "str" | "Range" => 8,
            "V4I64" | "I32x4" => 32,
            "I64x2" => 16,
            "F32x4" => 16,
            s if s.starts_with("Array_")
                || s.starts_with("Slice_")
                || s.starts_with("DynamicArray_")
                || s.starts_with("Vector_") =>
            {
                8
            }
            s if s.starts_with("Ptr_") || s.starts_with("Ref_") => 8,
            s if s.starts_with("Tuple_") || s.starts_with("Named_") => 8,
            // Default: 8 bytes for named structs/unions
            _ => 8,
        }
    }

    /// Return the alignment of a type from its monomorphized name suffix.
    fn type_align_from_name(&self, name: &str) -> usize {
        match name {
            "i8" | "u8" => 1,
            "i16" | "u16" => 2,
            "i32" | "u32" | "f32" | "char" => 4,
            "i64" | "u64" | "f64" | "bool" | "usize" | "isize" | "str" | "Range" => 8,
            "V4I64" => 32,
            "I32x4" | "I64x2" | "F32x4" => 16,
            s if s.starts_with("Array_")
                || s.starts_with("Slice_")
                || s.starts_with("DynamicArray_")
                || s.starts_with("Vector_") =>
            {
                8
            }
            s if s.starts_with("Ptr_") || s.starts_with("Ref_") => 8,
            _ => 8,
        }
    }

    /// Check if a function is generic
    fn is_generic_function(&self, mir: &crate::middle::mir::mir::Mir) -> bool {
        // If the name contains _inst_, it's already monomorphized — skip generic check.
        if let Some(ref name) = mir.name {
            if name.contains("_inst_") {
                return false;
            }
        }
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
            MirStmt::Swap { a_ptr, b_ptr, size } => MirStmt::Swap {
                a_ptr: *a_ptr,
                b_ptr: *b_ptr,
                size: *size,
            },
            MirStmt::Pre { cond, message } => MirStmt::Pre {
                cond: *cond,
                message: message.clone(),
            },
            MirStmt::Post { cond, message } => MirStmt::Post {
                cond: *cond,
                message: message.clone(),
            },
            MirStmt::Invariant { cond, message } => MirStmt::Invariant {
                cond: *cond,
                message: message.clone(),
            },
            MirStmt::MapNew { dest } => MirStmt::MapNew { dest: *dest },
            MirStmt::For {
                iterator,
                pattern,
                var_id,
                body,
            } => MirStmt::For {
                iterator: *iterator,
                pattern: pattern.clone(),
                var_id: *var_id,
                body: body
                    .iter()
                    .map(|s| self.substitute_stmt(s, substitution))
                    .collect(),
            },
            MirStmt::StructNew {
                variant,
                fields,
                dest,
            } => MirStmt::StructNew {
                variant: variant.clone(),
                fields: fields.clone(),
                dest: *dest,
            },
            MirStmt::While { cond, body } => MirStmt::While {
                cond: *cond,
                body: body
                    .iter()
                    .map(|s| self.substitute_stmt(s, substitution))
                    .collect(),
            },
            MirStmt::Break => MirStmt::Break,
            MirStmt::Continue => MirStmt::Continue,
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
                    let result = self
                        .builder
                        .build_int_sub(zero, operand.into_int_value(), "neg")
                        .unwrap();
                    let alloca = *self.locals.get(dest).unwrap();
                    self.builder.build_store(alloca, result).unwrap();
                    return;
                }

                // Handle array_get and stack_array_get specially for inline memory access
                // (avoids function call overhead — 10x speedup for pure Zeta array operations)
                if (func == "array_get" || func == "stack_array_get") && args.len() == 2 {
                    let array_ptr_val = self.gen_expr_safe(&args[0], exprs).into_int_value();
                    let index_val = self.gen_expr_safe(&args[1], exprs).into_int_value();

                    let array_ptr = self
                        .builder
                        .build_int_to_ptr(
                            array_ptr_val,
                            self.context.ptr_type(AddressSpace::default()),
                            "array_ptr",
                        )
                        .unwrap();

                    let elem_ptr = unsafe {
                        self.builder
                            .build_gep(self.i64_type, array_ptr, &[index_val], "elem_ptr")
                            .unwrap()
                    };

                    let value = self
                        .builder
                        .build_load(self.i64_type, elem_ptr, "array_elem")
                        .unwrap();

                    let dest_alloca = *self.locals.get(dest).unwrap();
                    self.builder.build_store(dest_alloca, value).unwrap();
                    return;
                }
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
                                .build_left_shift(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    "shl",
                                )
                                .unwrap(),
                            ">>" | "shr" | "shr_i64" => self
                                .builder
                                .build_right_shift(
                                    left.into_int_value(),
                                    right.into_int_value(),
                                    false,
                                    "shr",
                                )
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
                                let callee =
                                    self.get_or_declare_function(func, type_args, args.len());
                                let arg_vals: Vec<BasicMetadataValueEnum> = args
                                    .iter()
                                    .map(|&id| self.gen_expr_safe(&id, exprs).into())
                                    .collect();
                                let call = self.builder.build_call(callee, &arg_vals, "").unwrap();
                                // Convert BasicValueEnum to IntValue
                                let basic_val = Self::call_site_to_basic_value(call).unwrap();
                                basic_val.into_int_value()
                            }
                        };

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                    } else {
                        // Operator with wrong number of arguments, fall through to regular function call
                        let callee = self.get_or_declare_function(func, type_args, args.len());
                        let arg_vals: Vec<BasicMetadataValueEnum> = args
                            .iter()
                            .map(|&id| self.gen_expr_safe(&id, exprs).into())
                            .collect();
                        let call = self.builder.build_call(callee, &arg_vals, "").unwrap();
                        if let Some(val) = Self::call_site_to_basic_value(call) {
                            let alloca = *self.locals.get(dest).unwrap();
                            self.builder.build_store(alloca, val).unwrap();
                        }
                    }
                } else if self.is_simd_operation(func) {
                    // Handle SIMD operations — generate inline LLVM vector IR
                    self.handle_simd_operation(func, type_args, args, exprs, dest);
                } else {
                    // Regular function call
                    if args.len() == 1 && (func == "-" || func == "unary_minus") {
                        let operand = self.gen_expr_safe(&args[0], exprs);
                        let zero = self.i64_type.const_zero();
                        let result = self
                            .builder
                            .build_int_sub(zero, operand.into_int_value(), "neg")
                            .unwrap();
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                        return;
                    }
                    // Intercept std::mem intrinsics: size_of<T>, align_of<T>, swap, replace, null
                    if (func == "size_of" || func == "align_of") && args.is_empty() {
                        if let Some(ty) = type_args.first() {
                            match ty {
                                crate::middle::types::Type::I8 | crate::middle::types::Type::U8 => {
                                    let val = if func == "size_of" { 1u64 } else { 1u64 };
                                    let r: inkwell::values::IntValue =
                                        self.i64_type.const_int(val, false);
                                    let a = *self.locals.get(dest).unwrap();
                                    self.builder.build_store(a, r).unwrap();
                                    return;
                                }
                                crate::middle::types::Type::I16
                                | crate::middle::types::Type::U16 => {
                                    let val = if func == "size_of" { 2u64 } else { 2u64 };
                                    let r: inkwell::values::IntValue =
                                        self.i64_type.const_int(val, false);
                                    let a = *self.locals.get(dest).unwrap();
                                    self.builder.build_store(a, r).unwrap();
                                    return;
                                }
                                crate::middle::types::Type::I32
                                | crate::middle::types::Type::U32
                                | crate::middle::types::Type::F32
                                | crate::middle::types::Type::Char => {
                                    let val = if func == "size_of" { 4u64 } else { 4u64 };
                                    let r: inkwell::values::IntValue =
                                        self.i64_type.const_int(val, false);
                                    let a = *self.locals.get(dest).unwrap();
                                    self.builder.build_store(a, r).unwrap();
                                    return;
                                }
                                _ => {
                                    let r: inkwell::values::IntValue =
                                        self.i64_type.const_int(8u64, false);
                                    let a = *self.locals.get(dest).unwrap();
                                    self.builder.build_store(a, r).unwrap();
                                    return;
                                }
                            }
                        } else {
                            let r: inkwell::values::IntValue = self.i64_type.const_int(8u64, false);
                            let a = *self.locals.get(dest).unwrap();
                            self.builder.build_store(a, r).unwrap();
                            return;
                        }
                    }
                    // Intercept std::mem::swap<T>(a: &mut T, b: &mut T)
                    if func == "swap" && args.len() == 2 {
                        let ptr_a = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let ptr_b = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let pt = self.context.ptr_type(inkwell::AddressSpace::default());
                        let a_ptr = self.builder.build_int_to_ptr(ptr_a, pt, "a_ptr").unwrap();
                        let b_ptr = self.builder.build_int_to_ptr(ptr_b, pt, "b_ptr").unwrap();
                        let a_i64 = self.builder.build_pointer_cast(a_ptr, pt, "a_i64").unwrap();
                        let b_i64 = self.builder.build_pointer_cast(b_ptr, pt, "b_i64").unwrap();
                        let temp = self
                            .builder
                            .build_load(self.i64_type, a_i64, "temp")
                            .unwrap();
                        let b_val = self
                            .builder
                            .build_load(self.i64_type, b_i64, "b_val")
                            .unwrap();
                        self.builder.build_store(a_i64, b_val).unwrap();
                        self.builder.build_store(b_i64, temp).unwrap();
                        if let Some(&alloca) = self.locals.get(dest) {
                            self.builder
                                .build_store(alloca, self.i64_type.const_int(0, false))
                                .unwrap();
                        }
                        return;
                    }
                    // Intercept ptr::read<T>(ptr: *const T) -> T
                    if func == "read" && args.len() == 1 {
                        let ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let pt = self.context.ptr_type(inkwell::AddressSpace::default());
                        let elem_ptr = self.builder.build_int_to_ptr(ptr, pt, "rd_ptr").unwrap();
                        let elem_i64 = self
                            .builder
                            .build_pointer_cast(elem_ptr, pt, "rd_i64")
                            .unwrap();
                        let val = self
                            .builder
                            .build_load(self.i64_type, elem_i64, "rd_val")
                            .unwrap();
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, val).unwrap();
                        return;
                    }
                    // Intercept ptr::write<T>(ptr: *mut T, val: T)
                    if func == "write" && args.len() == 2 {
                        let ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let val = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let pt = self.context.ptr_type(inkwell::AddressSpace::default());
                        let elem_ptr = self.builder.build_int_to_ptr(ptr, pt, "wr_ptr").unwrap();
                        let elem_i64 = self
                            .builder
                            .build_pointer_cast(elem_ptr, pt, "wr_i64")
                            .unwrap();
                        self.builder.build_store(elem_i64, val).unwrap();
                        if let Some(&alloca) = self.locals.get(dest) {
                            self.builder
                                .build_store(alloca, self.i64_type.const_int(0, false))
                                .unwrap();
                        }
                        return;
                    }
                    // Intercept ptr::null<T>() and ptr::null_mut<T>()
                    if (func == "null" || func == "null_mut") && args.is_empty() {
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder
                            .build_store(alloca, self.i64_type.const_int(0, false))
                            .unwrap();
                        return;
                    }
                    // Intercept ptr::is_null<T>(ptr: *const T)
                    if func == "is_null" && args.len() == 1 {
                        let ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let is_null = self
                            .builder
                            .build_int_compare(
                                inkwell::IntPredicate::EQ,
                                ptr,
                                self.i64_type.const_int(0, false),
                                "is_null",
                            )
                            .unwrap();
                        let result = self
                            .builder
                            .build_int_z_extend(is_null, self.i64_type, "is_null_ext")
                            .unwrap();
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                        return;
                    }
                    // Intercept ptr::copy<T>(dst, src, count) — memcpy
                    if (func == "copy" || func == "copy_nonoverlapping") && args.len() == 3 {
                        let dst = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let src = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let count = self.gen_expr_safe(&args[2], exprs).into_int_value();
                        let dst_ptr = self
                            .builder
                            .build_int_to_ptr(dst, self.ptr_type, "cp_dst")
                            .unwrap();
                        let src_ptr = self
                            .builder
                            .build_int_to_ptr(src, self.ptr_type, "cp_src")
                            .unwrap();
                        // Get element size from type_args
                        let elem_size: u64 = if let Some(ty) = type_args.first() {
                            match ty {
                                crate::middle::types::Type::I8 | crate::middle::types::Type::U8 => {
                                    1
                                }
                                crate::middle::types::Type::I16
                                | crate::middle::types::Type::U16 => 2,
                                crate::middle::types::Type::I32
                                | crate::middle::types::Type::U32
                                | crate::middle::types::Type::F32 => 4,
                                _ => 8,
                            }
                        } else {
                            8
                        };
                        let total_bytes = self
                            .builder
                            .build_int_mul(
                                count,
                                self.i64_type.const_int(elem_size, false),
                                "copysz",
                            )
                            .unwrap();
                        unsafe {
                            self.builder
                                .build_memcpy(dst_ptr, 8, src_ptr, 8, total_bytes)
                                .unwrap();
                        }
                        if let Some(&alloca) = self.locals.get(dest) {
                            self.builder
                                .build_store(alloca, self.i64_type.const_int(0, false))
                                .unwrap();
                        }
                        return;
                    }
                    // Intercept ptr::offset<T>(ptr, count) — pointer arithmetic
                    if func == "offset" && args.len() == 2 {
                        let ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let count = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let elem_size: u64 = if let Some(ty) = type_args.first() {
                            match ty {
                                crate::middle::types::Type::I8 | crate::middle::types::Type::U8 => {
                                    1
                                }
                                crate::middle::types::Type::I16
                                | crate::middle::types::Type::U16 => 2,
                                crate::middle::types::Type::I32
                                | crate::middle::types::Type::U32
                                | crate::middle::types::Type::F32 => 4,
                                _ => 8,
                            }
                        } else {
                            8
                        };
                        let byte_offset = self
                            .builder
                            .build_int_mul(
                                count,
                                self.i64_type.const_int(elem_size, false),
                                "byte_off",
                            )
                            .unwrap();
                        let ptr = self
                            .builder
                            .build_int_add(ptr, byte_offset, "off_ptr")
                            .unwrap();
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, ptr).unwrap();
                        return;
                    }
                    // Intercept std::mem::replace<T>(dest: &mut T, src: T) -> T
                    if func == "replace" && args.len() == 2 {
                        let ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let new_val = self.gen_expr_safe(&args[1], exprs);
                        let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                        let elem_ptr = self
                            .builder
                            .build_int_to_ptr(ptr, ptr_type, "rpl_ptr")
                            .unwrap();
                        let elem_i64 = self
                            .builder
                            .build_pointer_cast(elem_ptr, ptr_type, "rpl_i64")
                            .unwrap();
                        let old_val = self
                            .builder
                            .build_load(self.i64_type, elem_i64, "old")
                            .unwrap();
                        self.builder
                            .build_store(elem_i64, new_val.into_int_value())
                            .unwrap();
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, old_val).unwrap();
                        return;
                    }
                    // Intercept V4I64 vector intrinsics → inline LLVM vector IR
                    // These are void operations but MIR gen generates Call (with dest) for all externs.
                    // V4I64 intrinsics — evaluate all args first to avoid borrow conflicts
                    if func == "__builtin_v4i64_andnot" && args.len() == 6 {
                        let ptr_i64 = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let word_idx = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let m0 = self.gen_expr_safe(&args[2], exprs).into_int_value();
                        let m1 = self.gen_expr_safe(&args[3], exprs).into_int_value();
                        let m2 = self.gen_expr_safe(&args[4], exprs).into_int_value();
                        let m3 = self.gen_expr_safe(&args[5], exprs).into_int_value();

                        let thirty_two = self.i64_type.const_int(32, false);
                        let byte_offset = self
                            .builder
                            .build_int_mul(word_idx, thirty_two, "off")
                            .unwrap();
                        let base_ptr = self
                            .builder
                            .build_int_to_ptr(ptr_i64, self.ptr_type, "base")
                            .unwrap();
                        let vec_ptr = unsafe {
                            self.builder
                                .build_gep(self.context.i8_type(), base_ptr, &[byte_offset], "vptr")
                                .unwrap()
                        };
                        let loaded = self
                            .builder
                            .build_load(self.vec4_i64_type, vec_ptr, "load")
                            .unwrap()
                            .into_vector_value();

                        let poison = self.vec4_i64_type.get_undef();
                        let z = self.i64_type.const_int(0, false);
                        let o = self.i64_type.const_int(1, false);
                        let t = self.i64_type.const_int(2, false);
                        let h = self.i64_type.const_int(3, false);
                        let mut mask = self
                            .builder
                            .build_insert_element(poison, m0, z, "m0")
                            .unwrap();
                        mask = self
                            .builder
                            .build_insert_element(mask, m1, o, "m1")
                            .unwrap();
                        mask = self
                            .builder
                            .build_insert_element(mask, m2, t, "m2")
                            .unwrap();
                        mask = self
                            .builder
                            .build_insert_element(mask, m3, h, "m3")
                            .unwrap();

                        let one_val = self.i64_type.const_int(u64::MAX, false);
                        let mut all_ones = self
                            .builder
                            .build_insert_element(poison, one_val, z, "o0")
                            .unwrap();
                        all_ones = self
                            .builder
                            .build_insert_element(all_ones, one_val, o, "o1")
                            .unwrap();
                        all_ones = self
                            .builder
                            .build_insert_element(all_ones, one_val, t, "o2")
                            .unwrap();
                        all_ones = self
                            .builder
                            .build_insert_element(all_ones, one_val, h, "o3")
                            .unwrap();

                        let not_mask = self.builder.build_xor(mask, all_ones, "not").unwrap();
                        let result = self.builder.build_and(loaded, not_mask, "res").unwrap();
                        self.builder.build_store(vec_ptr, result).unwrap();

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder
                            .build_store(alloca, self.i64_type.const_zero())
                            .unwrap();
                        return;
                    }

                    if func == "__builtin_v4i64_store" && args.len() == 6 {
                        let ptr_i64 = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let word_idx = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let v0 = self.gen_expr_safe(&args[2], exprs).into_int_value();
                        let v1 = self.gen_expr_safe(&args[3], exprs).into_int_value();
                        let v2 = self.gen_expr_safe(&args[4], exprs).into_int_value();
                        let v3 = self.gen_expr_safe(&args[5], exprs).into_int_value();

                        let thirty_two = self.i64_type.const_int(32, false);
                        let byte_offset = self
                            .builder
                            .build_int_mul(word_idx, thirty_two, "off")
                            .unwrap();
                        let base_ptr = self
                            .builder
                            .build_int_to_ptr(ptr_i64, self.ptr_type, "base")
                            .unwrap();
                        let vec_ptr = unsafe {
                            self.builder
                                .build_gep(self.context.i8_type(), base_ptr, &[byte_offset], "vptr")
                                .unwrap()
                        };

                        let poison = self.vec4_i64_type.get_undef();
                        let z = self.i64_type.const_int(0, false);
                        let o = self.i64_type.const_int(1, false);
                        let t = self.i64_type.const_int(2, false);
                        let h = self.i64_type.const_int(3, false);
                        let mut vec = self
                            .builder
                            .build_insert_element(poison, v0, z, "v0")
                            .unwrap();
                        vec = self.builder.build_insert_element(vec, v1, o, "v1").unwrap();
                        vec = self.builder.build_insert_element(vec, v2, t, "v2").unwrap();
                        vec = self.builder.build_insert_element(vec, v3, h, "v3").unwrap();

                        self.builder.build_store(vec_ptr, vec).unwrap();

                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder
                            .build_store(alloca, self.i64_type.const_zero())
                            .unwrap();
                        return;
                    }

                    // Intercept __builtin_ctpop → redirect to llvm.ctpop.i64 (POPCNT instruction)
                    let actual_func = if func == "__builtin_ctpop" {
                        "llvm.ctpop.i64"
                    } else {
                        func
                    };
                    // The MIR gen already appends _N (arg count) to function names.
                    // Use the name as-is; the codegen's get_function has trailing _N
                    // stripping to find the base function declaration.
                    let callee = self.get_or_declare_function(actual_func, type_args, args.len());

                    // Strip trailing _N suffix from function name for special-case checks
                    let base_func = if let Some(pos) = func.rfind('_') {
                        let suffix = &func[pos + 1..];
                        if suffix.chars().all(|c| c.is_ascii_digit()) {
                            &func[..pos]
                        } else { func }
                    } else { func };

                    // Check if this is a runtime function that takes pointer arguments
                    let needs_ptr_arg = base_func == "option_is_some"
                        || base_func == "option_get_data"
                        || base_func == "option_free"
                        || base_func == "host_result_is_ok"
                        || base_func == "host_result_get_data"
                        || base_func == "host_result_free";

                    // Check if this is a runtime function that returns a pointer
                    let returns_ptr = base_func == "option_make_some"
                        || base_func == "option_make_none"
                        || base_func == "host_result_make_ok"
                        || base_func == "host_result_make_err";

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
                    let call = self.builder.build_call(callee, &arg_vals, "").unwrap();
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
                // Handle __builtin_memset → LLVM llvm.memset.i64 intrinsic
                if func == "__builtin_memset" && args.len() >= 3 {
                    let ptr_val = self.gen_expr_safe(&args[0], exprs).into_int_value();
                    let val_val = self.gen_expr_safe(&args[1], exprs).into_int_value();
                    let len_val = self.gen_expr_safe(&args[2], exprs).into_int_value();

                    // Convert i64 ptr to LLVM pointer
                    let dest_ptr = self
                        .builder
                        .build_int_to_ptr(ptr_val, self.ptr_type, "memset_dest")
                        .unwrap();
                    // Truncate i64 fill value to i8
                    let fill_byte = self
                        .builder
                        .build_int_truncate(val_val, self.context.i8_type(), "memset_fill")
                        .unwrap();
                    // Create volatile flag (false)
                    let is_volatile = self.context.bool_type().const_zero();

                    if let Some(memset_fn) = self.module.get_function("llvm.memset.i64") {
                        let _ = self
                            .builder
                            .build_call(
                                memset_fn,
                                &[
                                    dest_ptr.into(),
                                    fill_byte.into(),
                                    len_val.into(),
                                    is_volatile.into(),
                                ],
                                "memset",
                            )
                            .unwrap();
                    }
                    return;
                }

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

                // Handle array_set and stack_array_set specially for inline memory access
                if (func == "array_set" || func == "stack_array_set") && args.len() == 3 {
                    let array_ptr_val = self.gen_expr_safe(&args[0], exprs).into_int_value();
                    let index_val = self.gen_expr_safe(&args[1], exprs).into_int_value();
                    let value_val = self.gen_expr_safe(&args[2], exprs).into_int_value();

                    let array_ptr = self
                        .builder
                        .build_int_to_ptr(
                            array_ptr_val,
                            self.context.ptr_type(AddressSpace::default()),
                            "array_ptr",
                        )
                        .unwrap();

                    let elem_ptr = unsafe {
                        self.builder
                            .build_gep(self.i64_type, array_ptr, &[index_val], "elem_ptr")
                            .unwrap()
                    };

                    self.builder.build_store(elem_ptr, value_val).unwrap();
                    return;
                }

                // Fallback: call runtime function for other void calls
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
                for (i, s) in then.iter().enumerate() {}
                for (i, s) in else_.iter().enumerate() {}
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
                let then_ends_with_break = then.last().is_some_and(|s| matches!(s, MirStmt::Break));
                let then_ends_with_continue =
                    then.last().is_some_and(|s| matches!(s, MirStmt::Continue));
                if then_ends_with_break {
                    for s in &then[..then.len() - 1] {
                        self.gen_stmt(s, exprs);
                    }
                    if let Some((_, exit_bb)) = self.loop_stack.last() {
                        self.builder.build_unconditional_branch(*exit_bb).unwrap();
                    }
                } else if then_ends_with_continue {
                    for s in &then[..then.len() - 1] {
                        self.gen_stmt(s, exprs);
                    }
                    if let Some((cond_bb, _)) = self.loop_stack.last() {
                        self.builder.build_unconditional_branch(*cond_bb).unwrap();
                    }
                } else {
                    for s in then {
                        self.gen_stmt(s, exprs);
                    }
                }
                let then_has_terminal = then_ends_with_break
                    || then_ends_with_continue
                    || then.iter().any(|s| matches!(s, MirStmt::Return { .. }));
                if !then_has_terminal {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }

                // Generate else block
                self.builder.position_at_end(else_bb);
                let else_ends_with_break =
                    else_.last().is_some_and(|s| matches!(s, MirStmt::Break));
                let else_ends_with_continue =
                    else_.last().is_some_and(|s| matches!(s, MirStmt::Continue));
                if else_ends_with_break {
                    for s in &else_[..else_.len() - 1] {
                        self.gen_stmt(s, exprs);
                    }
                    if let Some((_, exit_bb)) = self.loop_stack.last() {
                        self.builder.build_unconditional_branch(*exit_bb).unwrap();
                    }
                } else if else_ends_with_continue {
                    for s in &else_[..else_.len() - 1] {
                        self.gen_stmt(s, exprs);
                    }
                    if let Some((cond_bb, _)) = self.loop_stack.last() {
                        self.builder.build_unconditional_branch(*cond_bb).unwrap();
                    }
                } else {
                    for s in else_ {
                        self.gen_stmt(s, exprs);
                    }
                }
                let else_has_terminal = else_ends_with_break
                    || else_ends_with_continue
                    || else_.iter().any(|s| matches!(s, MirStmt::Return { .. }));
                if !else_has_terminal {
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
                self.builder
                    .build_unconditional_branch(loop_cond_bb)
                    .unwrap();

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
                self.loop_stack.push((loop_cond_bb, loop_exit_bb));
                self.builder.position_at_end(loop_body_bb);
                for s in body {
                    self.gen_stmt(s, exprs);
                }
                self.loop_stack.pop();
                // Branch back to condition (unless body ends with return)
                if !body.iter().any(|s| matches!(s, MirStmt::Return { .. })) {
                    self.builder
                        .build_unconditional_branch(loop_cond_bb)
                        .unwrap();
                }

                // Continue at exit block
                self.builder.position_at_end(loop_exit_bb);
            }

            MirStmt::Break | MirStmt::Continue => {
                if let Some((cond_bb, exit_bb)) = self.loop_stack.last() {
                    if matches!(stmt, MirStmt::Break) {
                        self.builder.build_unconditional_branch(*exit_bb).unwrap();
                    } else {
                        // Continue: branch back to the loop condition block
                        self.builder.build_unconditional_branch(*cond_bb).unwrap();
                    }
                }
            }

            // Swap: exchange two memory regions via memcpy
            MirStmt::Swap { a_ptr, b_ptr, size } => {
                let a_val = self.gen_expr_safe(a_ptr, exprs).into_int_value();
                let b_val = self.gen_expr_safe(b_ptr, exprs).into_int_value();
                let size_val = self.gen_expr_safe(size, exprs).into_int_value();

                let a_ptr = self
                    .builder
                    .build_int_to_ptr(a_val, self.ptr_type, "swap_a")
                    .unwrap();
                let b_ptr = self
                    .builder
                    .build_int_to_ptr(b_val, self.ptr_type, "swap_b")
                    .unwrap();

                // Allocate a temporary buffer on the stack
                let temp = self
                    .builder
                    .build_alloca(self.context.i8_type(), "swap_tmp")
                    .unwrap();
                // We need an array of i8 of size bytes for the temp buffer
                // Use alloca with i8 array type for variable size
                let temp_array = unsafe {
                    self.builder
                        .build_alloca(self.context.i8_type().array_type(256), "swap_tmp_buf")
                        .unwrap()
                };
                let temp_ptr = self
                    .builder
                    .build_pointer_cast(temp_array, self.ptr_type, "tmp_ptr")
                    .unwrap();

                // Get llvm.memcpy intrinsic
                let memcpy_fn = self
                    .module
                    .get_function("llvm.memcpy.p0i8.p0i8.i64")
                    .or_else(|| {
                        // Declare memcpy if not present
                        let void_type = self.context.void_type();
                        Some(self.module.add_function(
                            "llvm.memcpy.p0i8.p0i8.i64",
                            void_type.fn_type(
                                &[
                                    self.ptr_type.into(),
                                    self.ptr_type.into(),
                                    self.i64_type.into(),
                                    self.context.bool_type().into(),
                                ],
                                false,
                            ),
                            None,
                        ))
                    })
                    .expect("memcpy intrinsic");

                let is_volatile = self.context.bool_type().const_zero();

                // 1. temp = *a
                let _ = self
                    .builder
                    .build_call(
                        memcpy_fn,
                        &[
                            temp_ptr.into(),
                            a_ptr.into(),
                            size_val.into(),
                            is_volatile.into(),
                        ],
                        "memcpy_tmp",
                    )
                    .unwrap();
                // 2. *a = *b
                let _ = self
                    .builder
                    .build_call(
                        memcpy_fn,
                        &[
                            a_ptr.into(),
                            b_ptr.into(),
                            size_val.into(),
                            is_volatile.into(),
                        ],
                        "memcpy_a",
                    )
                    .unwrap();
                // 3. *b = temp
                let _ = self
                    .builder
                    .build_call(
                        memcpy_fn,
                        &[
                            b_ptr.into(),
                            temp_ptr.into(),
                            size_val.into(),
                            is_volatile.into(),
                        ],
                        "memcpy_b",
                    )
                    .unwrap();
            }

            // Pre-condition assertion: assert(condition, "message")
            MirStmt::Pre { cond, message } => {
                let cond_val = self.gen_expr_safe(cond, exprs).into_int_value();
                let is_true = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond_val,
                        self.i64_type.const_zero(),
                        "pre_ok",
                    )
                    .unwrap();

                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let pass_bb = self.context.append_basic_block(parent_fn, "pre.pass");
                let fail_bb = self.context.append_basic_block(parent_fn, "pre.fail");

                self.builder
                    .build_conditional_branch(is_true, pass_bb, fail_bb)
                    .unwrap();

                // Fail block: call assertion failed handler
                self.builder.position_at_end(fail_bb);
                let msg_ptr = self
                    .builder
                    .build_global_string_ptr(message, "pre_msg")
                    .unwrap();
                let void_type = self.context.void_type();
                let assert_fn = if let Some(f) = self.module.get_function("__assert_fail") {
                    f
                } else {
                    self.module.add_function(
                        "__assert_fail",
                        void_type.fn_type(&[self.ptr_type.into()], false),
                        Some(Linkage::External),
                    )
                };
                let _ = self
                    .builder
                    .build_call(
                        assert_fn,
                        &[msg_ptr.as_pointer_value().into()],
                        "assert_fail",
                    )
                    .unwrap();
                self.builder.build_unreachable().unwrap();
                self.builder.position_at_end(pass_bb);
            }

            // Post-condition assertion
            MirStmt::Post { cond, message } => {
                // Same implementation as pre for now
                let cond_val = self.gen_expr_safe(cond, exprs).into_int_value();
                let is_true = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond_val,
                        self.i64_type.const_zero(),
                        "post_ok",
                    )
                    .unwrap();

                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let pass_bb = self.context.append_basic_block(parent_fn, "post.pass");
                let fail_bb = self.context.append_basic_block(parent_fn, "post.fail");

                self.builder
                    .build_conditional_branch(is_true, pass_bb, fail_bb)
                    .unwrap();

                self.builder.position_at_end(fail_bb);
                let msg_ptr = self
                    .builder
                    .build_global_string_ptr(message, "post_msg")
                    .unwrap();
                let void_type = self.context.void_type();
                let assert_fn = if let Some(f) = self.module.get_function("__assert_fail") {
                    f
                } else {
                    self.module.add_function(
                        "__assert_fail",
                        void_type.fn_type(&[self.ptr_type.into()], false),
                        Some(Linkage::External),
                    )
                };
                let _ = self
                    .builder
                    .build_call(
                        assert_fn,
                        &[msg_ptr.as_pointer_value().into()],
                        "assert_fail",
                    )
                    .unwrap();
                self.builder.build_unreachable().unwrap();
                self.builder.position_at_end(pass_bb);
            }

            // Loop invariant assertion
            MirStmt::Invariant { cond, message } => {
                // Same as pre-condition, generated at loop start
                let cond_val = self.gen_expr_safe(cond, exprs).into_int_value();
                let is_true = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        cond_val,
                        self.i64_type.const_zero(),
                        "inv_ok",
                    )
                    .unwrap();

                let parent_fn = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let pass_bb = self.context.append_basic_block(parent_fn, "inv.pass");
                let fail_bb = self.context.append_basic_block(parent_fn, "inv.fail");

                self.builder
                    .build_conditional_branch(is_true, pass_bb, fail_bb)
                    .unwrap();

                self.builder.position_at_end(fail_bb);
                let msg_ptr = self
                    .builder
                    .build_global_string_ptr(message, "inv_msg")
                    .unwrap();
                let void_type = self.context.void_type();
                let assert_fn = if let Some(f) = self.module.get_function("__assert_fail") {
                    f
                } else {
                    self.module.add_function(
                        "__assert_fail",
                        void_type.fn_type(&[self.ptr_type.into()], false),
                        Some(Linkage::External),
                    )
                };
                let _ = self
                    .builder
                    .build_call(
                        assert_fn,
                        &[msg_ptr.as_pointer_value().into()],
                        "assert_fail",
                    )
                    .unwrap();
                self.builder.build_unreachable().unwrap();
                self.builder.position_at_end(pass_bb);
            }

            MirStmt::For {
                iterator,
                pattern,
                var_id,
                body,
            } => {
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
                    self.builder
                        .build_unconditional_branch(loop_cond_bb)
                        .unwrap();

                    // Generate condition block
                    self.builder.position_at_end(loop_cond_bb);

                    // Load current loop variable value
                    let current_val = self
                        .builder
                        .build_load(self.i64_type, loop_var_ptr, "")
                        .unwrap()
                        .into_int_value();

                    // Check if current_val < end_val
                    let cond = self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLT, // Signed less than
                            current_val,
                            end_val,
                            "for.cond",
                        )
                        .unwrap();

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
                    let current_val_after = self
                        .builder
                        .build_load(self.i64_type, loop_var_ptr, "")
                        .unwrap()
                        .into_int_value();

                    let next_val = self
                        .builder
                        .build_int_add(current_val_after, self.i64_type.const_int(1, false), "")
                        .unwrap();

                    self.builder.build_store(loop_var_ptr, next_val).unwrap();

                    // Branch back to condition
                    self.builder
                        .build_unconditional_branch(loop_cond_bb)
                        .unwrap();

                    // Continue at exit block
                    self.builder.position_at_end(loop_exit_bb);
                } else {
                    // Not a range iterator - for now, just skip
                }
            }
            MirStmt::Store {
                addr_id,
                val_id,
                pointee_width,
            } => {
                // *addr = val — store val through the pointer
                let addr_i64 = self.gen_expr_safe(addr_id, exprs).into_int_value();
                let val = self.gen_expr_safe(val_id, exprs);
                let pointee_llvm_type = self
                    .context
                    .custom_width_int_type((*pointee_width as u32) * 8);
                let pointed_ptr_type: inkwell::types::PointerType =
                    self.context.ptr_type(inkwell::AddressSpace::default());
                let ptr = self
                    .builder
                    .build_int_to_ptr(addr_i64, pointed_ptr_type, "store_ptr")
                    .unwrap();
                // Truncate the i64 value to the pointee width before storing
                let narrowed = self
                    .builder
                    .build_int_truncate(val.into_int_value(), pointee_llvm_type, "store_trunc")
                    .unwrap();
                self.builder.build_store(ptr, narrowed).unwrap();
            }
            MirStmt::ParamInit { .. } => {} // handled at entry
            MirStmt::Consume { id } => {
                // Ownership consume — no runtime effect for now
                // Simply load and discard the value
                let _ = self.load_local(*id);
            }
            MirStmt::StructNew {
                variant,
                fields,
                dest,
            } => {
                // Allocate struct on stack and store field values
                let type_key = format!("{}_fields_{}", variant, fields.len());
                let struct_type = if let Some(ty) = self.specialized_types.get(&type_key) {
                    *ty
                } else {
                    let field_types: Vec<_> =
                        (0..fields.len()).map(|_| self.i64_type.into()).collect();
                    let ty = self.context.struct_type(&field_types, false);
                    self.specialized_types.insert(type_key.clone(), ty);
                    ty
                };
                let alloca = self
                    .builder
                    .build_alloca(struct_type, "struct_alloca")
                    .unwrap();
                for (i, (field_name, field_id)) in fields.iter().enumerate() {
                    let field_ptr = self
                        .builder
                        .build_struct_gep(struct_type, alloca, i as u32, "")
                        .unwrap();
                    let field_val = self.gen_expr_safe(field_id, exprs).into_int_value();
                    self.builder.build_store(field_ptr, field_val).unwrap();
                }
                let ptr_as_int = self
                    .builder
                    .build_ptr_to_int(alloca, self.i64_type, "struct_ptr")
                    .unwrap();
                let dest_alloca = *self.locals.get(dest).unwrap();
                self.builder.build_store(dest_alloca, ptr_as_int).unwrap();
            }
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

    /// Resolve the field index for a FieldAccess expression.
    /// Looks up the struct definition to find the field index by name.
    /// Falls back to parsing the field name as numeric index.
    /// Resolve a field name to its index in the struct.
    /// If `variant_hint` is provided, only search that specific struct definition.
    fn resolve_struct_field_index(
        &self,
        _base_id: &u32,
        field_name: &str,
        _exprs: &HashMap<u32, MirExpr>,
    ) -> u32 {
        // Search all known struct definitions for the field name
        for (_, fields) in self.struct_defs.iter() {
            for (i, name) in fields.iter().enumerate() {
                if name == field_name {
                    return i as u32;
                }
            }
        }

        // Fallback: try parsing the field name as a numeric index
        field_name.parse::<u32>().unwrap_or(0)
    }

    /// Resolve a field name to its index within a specific struct variant.
    /// Searches only the struct definition matching the given variant and field count.
    fn resolve_struct_field_index_for_variant(
        &self,
        variant: &str,
        field_count: usize,
        field_name: &str,
    ) -> u32 {
        let type_key = format!("struct_{}_{}", variant, field_count);
        if let Some(fields) = self.struct_defs.get(&type_key) {
            for (i, name) in fields.iter().enumerate() {
                if name == field_name {
                    return i as u32;
                }
            }
        }
        0
    }

    fn gen_expr(
        &mut self,
        expr: &MirExpr,
        exprs: &HashMap<u32, MirExpr>,
        expr_id: Option<u32>,
    ) -> BasicValueEnum<'ctx> {
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
                let left = self
                    .gen_expr(&exprs[&values[0]], exprs, None)
                    .into_int_value();
                if values.len() == 1 {
                    // Unary (e.g. unary minus)
                    match op {
                        crate::middle::mir::mir::SemiringOp::Mul
                        | crate::middle::mir::mir::SemiringOp::Add => {
                            // Identity: return value unchanged
                            left.into()
                        }
                    }
                } else {
                    let right = self
                        .gen_expr(&exprs[&values[1]], exprs, None)
                        .into_int_value();
                    let result = match op {
                        crate::middle::mir::mir::SemiringOp::Add => {
                            self.builder.build_int_add(left, right, "sef_add")
                        }
                        crate::middle::mir::mir::SemiringOp::Mul => {
                            self.builder.build_int_mul(left, right, "sef_mul")
                        }
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
            MirExpr::Syscall(num_id, arg_ids) => {
                // Build args: number + up to 6 syscall args
                let num_val = self.gen_expr(&exprs[num_id], exprs, None).into_int_value();
                let mut all_args: Vec<BasicMetadataValueEnum> = vec![num_val.into()];
                for arg_id in arg_ids {
                    let val = self.gen_expr(exprs.get(arg_id).unwrap(), exprs, None).into_int_value();
                    all_args.push(val.into());
                    if all_args.len() >= 7 { break; } // 7 = number + 6 args
                }
                // Pad to 7 args (number + 6 zero args for unused)
                while all_args.len() < 7 {
                    all_args.push(self.i64_type.const_zero().into());
                }
                
                // Declare and call the C syscall wrapper
                let fn_type = self.i64_type.fn_type(
                    &[self.i64_type.into(); 7],
                    false,
                );
                let callee = self.module.add_function("zenith_syscall", fn_type, None);
                let call = self.builder.build_call(callee, &all_args, "syscall").unwrap();
                // CallSiteValue -> BasicValueEnum via try_as_basic_value()
                match call.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(val) => val,
                    _ => self.i64_type.const_zero().into(),
                }
            }
            MirExpr::BinaryOp { op, left, right } => {
                let left_val = self.gen_expr(&exprs[left], exprs, None).into_int_value();
                let right_val = self.gen_expr(&exprs[right], exprs, None).into_int_value();

                match op.as_str() {
                    "<" => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, left_val, right_val, "cmp_lt")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "cmp_ext")
                            .unwrap()
                            .into()
                    }
                    ">" => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, left_val, right_val, "cmp_gt")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "cmp_ext")
                            .unwrap()
                            .into()
                    }
                    "<=" => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, left_val, right_val, "cmp_le")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "cmp_ext")
                            .unwrap()
                            .into()
                    }
                    ">=" => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, left_val, right_val, "cmp_ge")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "cmp_ext")
                            .unwrap()
                            .into()
                    }
                    "==" => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, left_val, right_val, "cmp_eq")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "cmp_ext")
                            .unwrap()
                            .into()
                    }
                    "!=" => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::NE, left_val, right_val, "cmp_ne")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, self.i64_type, "cmp_ext")
                            .unwrap()
                            .into()
                    }
                    "+" => self
                        .builder
                        .build_int_add(left_val, right_val, "add")
                        .unwrap()
                        .into(),
                    "-" => self
                        .builder
                        .build_int_sub(left_val, right_val, "sub")
                        .unwrap()
                        .into(),
                    "*" => self
                        .builder
                        .build_int_mul(left_val, right_val, "mul")
                        .unwrap()
                        .into(),
                    "/" => self
                        .builder
                        .build_int_signed_div(left_val, right_val, "div")
                        .unwrap()
                        .into(),
                    "%" => self
                        .builder
                        .build_int_signed_rem(left_val, right_val, "mod")
                        .unwrap()
                        .into(),
                    "&" => self
                        .builder
                        .build_and(left_val, right_val, "bitand")
                        .unwrap()
                        .into(),
                    "|" => self
                        .builder
                        .build_or(left_val, right_val, "bitor")
                        .unwrap()
                        .into(),
                    "^" => self
                        .builder
                        .build_xor(left_val, right_val, "bitxor")
                        .unwrap()
                        .into(),
                    "<<" => self
                        .builder
                        .build_left_shift(left_val, right_val, "shl")
                        .unwrap()
                        .into(),
                    ">>" => self
                        .builder
                        .build_right_shift(left_val, right_val, true, "shr")
                        .unwrap()
                        .into(),
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
                        .build_struct_gep(struct_type, alloca, i as u32, "")
                        .unwrap();

                    let field_val = self
                        .gen_expr(&exprs[field_id], exprs, None)
                        .into_int_value();
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

                // Trace through Var chain to find the original Struct expression
                let mut base_expr = &exprs[base];
                for _ in 0..10 {
                    match base_expr {
                        MirExpr::Var(v) => { base_expr = match exprs.get(v) { Some(e) => e, None => break, }; }
                        MirExpr::FieldAccess { base: inner, .. } => { base_expr = match exprs.get(inner) { Some(e) => e, None => break, }; }
                        _ => break,
                    }
                }

                // Determine struct type from the Struct expression
                let (variant, field_count) = if let MirExpr::Struct { variant, fields } = base_expr {
                    (variant.clone(), fields.len())
                } else {
                    (String::new(), 2)
                };

                // Use same type key format as the Struct handler: "{variant}_fields_{count}"
                let type_key = if variant.is_empty() {
                    format!("struct_fields_{}", field_count)
                } else {
                    format!("{}_fields_{}", variant, field_count)
                };

                let struct_type = if let Some(ty) = self.specialized_types.get(&type_key) {
                    *ty
                } else {
                    let field_types: Vec<_> = (0..field_count).map(|_| self.i64_type.into()).collect();
                    let ty = self.context.struct_type(&field_types, false);
                    self.specialized_types.insert(type_key.clone(), ty);
                    ty
                };

                let loaded_struct = self
                    .builder
                    .build_load(struct_type, ptr, "load_struct")
                    .unwrap();

                // Extract field based on name
                // Walk the expression chain to find the Struct definition
                // and look up the field index by name
                // Try variant-specific lookup first, then fall back to global search
                let field_index = if !variant.is_empty() {
                    self.resolve_struct_field_index_for_variant(&variant, field_count, field)
                } else {
                    self.resolve_struct_field_index(base, field, exprs)
                };
                // If still out of range, fall back to numeric parse
                let field_index = if field_index >= field_count as u32 {
                    field.parse::<u32>().unwrap_or(0)
                } else {
                    field_index
 };

                // Extract value from struct
                self.builder
                    .build_extract_value(loaded_struct.into_struct_value(), field_index, "")
                    .unwrap()
            }
            MirExpr::As { expr, target_type } => {
                // Generate the expression value
                let expr_val = self.gen_expr(&exprs[expr], exprs, None);

                // For now, handle basic numeric conversions
                // TODO: Implement proper type conversion logic
                match target_type {
                    Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::Usize => {
                        // For integer types, just truncate or extend as needed
                        // For now, just return the value as-is (i64)
                        if let BasicValueEnum::IntValue(int_val) = expr_val {
                            // Convert to target integer type
                            match target_type {
                                Type::I8 => self
                                    .builder
                                    .build_int_truncate(int_val, self.context.i8_type(), "trunc_i8")
                                    .unwrap()
                                    .into(),
                                Type::I16 => self
                                    .builder
                                    .build_int_truncate(
                                        int_val,
                                        self.context.i16_type(),
                                        "trunc_i16",
                                    )
                                    .unwrap()
                                    .into(),
                                Type::I32 => self
                                    .builder
                                    .build_int_truncate(
                                        int_val,
                                        self.context.i32_type(),
                                        "trunc_i32",
                                    )
                                    .unwrap()
                                    .into(),
                                Type::I64 => int_val.into(), // Already i64
                                Type::U8 => self
                                    .builder
                                    .build_int_truncate(int_val, self.context.i8_type(), "trunc_u8")
                                    .unwrap()
                                    .into(),
                                Type::U16 => self
                                    .builder
                                    .build_int_truncate(
                                        int_val,
                                        self.context.i16_type(),
                                        "trunc_u16",
                                    )
                                    .unwrap()
                                    .into(),
                                Type::U32 => self
                                    .builder
                                    .build_int_truncate(
                                        int_val,
                                        self.context.i32_type(),
                                        "trunc_u32",
                                    )
                                    .unwrap()
                                    .into(),
                                Type::U64 => self
                                    .builder
                                    .build_int_cast(int_val, self.context.i64_type(), "cast_u64")
                                    .unwrap()
                                    .into(),
                                Type::Usize => {
                                    // usize is platform-dependent, use i64 for now
                                    self.builder
                                        .build_int_cast(
                                            int_val,
                                            self.context.i64_type(),
                                            "cast_usize",
                                        )
                                        .unwrap()
                                        .into()
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

                // Allocate on HEAP (was stack) — tuples are returned across function
                // boundaries and stack allocation causes use-after-free.
                let total_bytes = self.i64_type.const_int(*size as u64 * 8, false);
                let malloc_fn = self.module.get_function("runtime_malloc").unwrap_or_else(|| {
                    let i64_type = self.context.i64_type();
                    let fn_type = i64_type.fn_type(&[i64_type.into()], false);
                    self.module.add_function("runtime_malloc", fn_type, None)
                });
                let call = self.builder.build_call(malloc_fn, &[total_bytes.into()], "heap_array").unwrap();
                let heap_ptr = Self::call_site_to_basic_value(call)
                    .unwrap()
                    .into_int_value();

                // Initialize each element
                for (i, element_id) in elements.iter().enumerate() {
                    let element_val = self
                        .gen_expr(&exprs[element_id], exprs, None)
                        .into_int_value();
                    let ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                    let heap_ptr_val = self
                        .builder
                        .build_int_to_ptr(heap_ptr, ptr_type, "heap_p")
                        .unwrap();
                    let element_ptr = unsafe {
                        self.builder
                            .build_gep(
                                array_type,
                                heap_ptr_val,
                                &[
                                    self.i64_type.const_int(0, false),
                                    self.i64_type.const_int(i as u64, false),
                                ],
                                "",
                            )
                            .unwrap()
                    };
                    self.builder.build_store(element_ptr, element_val).unwrap();
                }

                // Return heap pointer (as i64)
                heap_ptr.into()
            }
            MirExpr::Range { start, end } => {
                // For now, just return the start value
                // TODO: Implement proper range type
                self.gen_expr(&exprs[start], exprs, None)
            }
            MirExpr::Deref {
                addr_id,
                pointee_width,
            } => {
                // *ptr — load through pointer with given element width
                let ptr_as_i64 = self.gen_expr_safe(addr_id, exprs).into_int_value();
                let pointee_llvm_type = self
                    .context
                    .custom_width_int_type((*pointee_width as u32) * 8);
                let pointed_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                let ptr = self
                    .builder
                    .build_int_to_ptr(ptr_as_i64, pointed_ptr_type, "deref_ptr")
                    .unwrap();
                let val = self
                    .builder
                    .build_load(pointee_llvm_type, ptr, "deref_val")
                    .unwrap();
                // Zero-extend back to i64
                self.builder
                    .build_int_z_extend(val.into_int_value(), self.i64_type, "deref_ext")
                    .unwrap()
                    .into()
            }
        }
    }

    fn load_local(&self, id: u32) -> BasicValueEnum<'ctx> {
        let ptr = *self.locals.get(&id).unwrap();
        self.builder.build_load(self.i64_type, ptr, "").unwrap()
    }

    fn call_site_to_basic_value(call: CallSiteValue<'ctx>) -> Option<BasicValueEnum<'ctx>> {
        match call.try_as_basic_value() {
            inkwell::values::ValueKind::Basic(basic) => Some(basic),
            _ => None,
        }
    }

    /// Convert a Zeta type to an LLVM type
    /// Handle a SIMD operation by name — generate inline LLVM vector IR.
    fn handle_simd_operation(
        &mut self,
        func: &str,
        type_args: &[crate::middle::types::Type],
        args: &[u32],
        exprs: &HashMap<u32, MirExpr>,
        dest: &u32,
    ) {
        // Handle simd::method names inline — resolve and dispatch directly
        if let Some(method) = func.strip_prefix("simd::") {
            let elem_type = if !type_args.is_empty() {
                match &type_args[0] {
                    Type::I32 => "i32",
                    Type::I64 | Type::U64 => "i64",
                    Type::U32 => "u32",
                    Type::F32 => "f32",
                    Type::F64 => "f64",
                    _ => "i32",
                }
            } else {
                "i32"
            };
            let lanes_str = if type_args.len() >= 2 {
                match &type_args[1] {
                    Type::Named(n, _) => n.clone(),
                    _ => "4".to_string(),
                }
            } else {
                "4".to_string()
            };
            let lanes: u32 = lanes_str.parse().unwrap_or(4);
            let is_float = elem_type == "f32" || elem_type == "f64";
            let bit_width: u32 = if is_float {
                32
            } else if elem_type == "i64" || elem_type == "u64" {
                64
            } else {
                32
            };

            let vt = self.simd_vector_type(bit_width, lanes, is_float);
            let (vec_alloca, _ptr) = self.simd_alloca_vec(vt);

            match method {
                "splat" => {
                    let val = self.gen_expr_safe(&args[0], exprs);
                    let scalar = self.simd_trunc_val(val, bit_width, is_float);
                    let poison = vt.get_undef();
                    let mut result = self
                        .builder
                        .build_insert_element(
                            poison,
                            scalar,
                            self.context.i32_type().const_zero(),
                            "sp0",
                        )
                        .unwrap();
                    for i in 1..lanes {
                        let idx = self.context.i32_type().const_int(i as u64, false);
                        let elem = self.simd_trunc_val(val, bit_width, is_float);
                        result = self
                            .builder
                            .build_insert_element(result, elem, idx, &format!("sp{}", i))
                            .unwrap();
                    }
                    self.builder.build_store(vec_alloca, result).unwrap();
                }
                "add" | "sub" | "mul" => {
                    if args.len() >= 2 {
                        let a_ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let b_ptr = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let vec_a = self.simd_load_vec(a_ptr, vt);
                        let vec_b = self.simd_load_vec(b_ptr, vt);
                        let result = match method {
                            "add" if is_float => {
                                self.builder.build_float_add(vec_a, vec_b, "vadd").unwrap()
                            }
                            "add" => self.builder.build_int_add(vec_a, vec_b, "vadd").unwrap(),
                            "sub" if is_float => {
                                self.builder.build_float_sub(vec_a, vec_b, "vsub").unwrap()
                            }
                            "sub" => self.builder.build_int_sub(vec_a, vec_b, "vsub").unwrap(),
                            "mul" if is_float => {
                                self.builder.build_float_mul(vec_a, vec_b, "vmul").unwrap()
                            }
                            "mul" => self.builder.build_int_mul(vec_a, vec_b, "vmul").unwrap(),
                            _ => vec_a,
                        };
                        self.builder.build_store(vec_alloca, result).unwrap();
                    }
                }
                "extract" | "get" => {
                    if args.len() >= 2 {
                        let vec_ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
                        let idx = self.gen_expr_safe(&args[1], exprs).into_int_value();
                        let loaded = self.simd_load_vec(vec_ptr, vt);
                        let extracted = self
                            .builder
                            .build_extract_element(loaded, idx, "extract")
                            .unwrap();
                        let result = if bit_width == 32 && !is_float {
                            self.simd_zext_i64(extracted)
                        } else {
                            extracted
                        };
                        let alloca = *self.locals.get(dest).unwrap();
                        self.builder.build_store(alloca, result).unwrap();
                        return;
                    }
                }
                "free" | "drop" => {
                    let alloca = *self.locals.get(dest).unwrap();
                    self.builder
                        .build_store(alloca, self.i64_type.const_zero())
                        .unwrap();
                    return;
                }
                "is_supported" | "optimal_size" => {
                    return self.handle_simd_const(method, dest);
                }
                _ => {
                    let alloca = *self.locals.get(dest).unwrap();
                    self.builder
                        .build_store(alloca, self.i64_type.const_zero())
                        .unwrap();
                    return;
                }
            }

            // Store the pointer to the stack-allocated vector as i64 result
            let ptr_as_i64 = self
                .builder
                .build_ptr_to_int(vec_alloca, self.i64_type, "simd_res")
                .unwrap();
            let alloca = *self.locals.get(dest).unwrap();
            self.builder.build_store(alloca, ptr_as_i64).unwrap();
            return;
        }

        // Handle Vector::method / Vector__method names
        let (base_func, simd_prefix) =
            if func.starts_with("Vector::") || func.starts_with("Vector__") {
                // Extract method name: Vector::splat → splat, Vector__splat → splat
                let method = if func.starts_with("Vector::") {
                    &func[8..]
                } else {
                    &func[8..]
                };
                // Default to i64x4 when no explicit type info (the resolver registers Vector::splat with u64x8)
                match method {
                    "splat" => ("vector_splat_i64x4", "vector_splat"),
                    "new" | "make" => ("vector_make_i64x4", "vector_make"),
                    "add" => ("vector_add_i64x4", "vector_add"),
                    "sub" | "subtract" => ("vector_sub_i64x4", "vector_sub"),
                    "mul" | "multiply" => ("vector_mul_i64x4", "vector_mul"),
                    "get" | "extract" => ("vector_get_i64x4", "vector_get"),
                    "set" | "insert" => ("vector_set_i64x4", "vector_set"),
                    "free" | "drop" => ("vector_free_i64x4", "vector_free"),
                    _ => ("", ""),
                }
            } else {
                (func, func)
            };

        // Use the vector_* name for type info parsing
        let name_for_type = if !base_func.is_empty() && base_func != func {
            base_func
        } else {
            func
        };

        let Some((bit_width, lanes, is_float)) = Self::parse_simd_type_info(name_for_type) else {
            let alloca = *self.locals.get(dest).unwrap();
            self.builder
                .build_store(alloca, self.i64_type.const_zero())
                .unwrap();
            return;
        };
        let vt = self.simd_vector_type(bit_width, lanes, is_float);
        let (vec_alloca, _ptr) = self.simd_alloca_vec(vt);

        // For Vector::* calls, use simd_prefix (e.g. "simd_splat_i64x4") for dispatch
        let dispatch_name = if !simd_prefix.is_empty() {
            simd_prefix
        } else {
            func
        };

        if dispatch_name.starts_with("simd_splat") || dispatch_name.starts_with("vector_splat") {
            let val = self.gen_expr_safe(&args[0], exprs);
            let scalar = self.simd_trunc_val(val, bit_width, is_float);
            let poison = vt.get_undef();
            let mut result = self
                .builder
                .build_insert_element(poison, scalar, self.context.i32_type().const_zero(), "sp0")
                .unwrap();
            for i in 1..lanes {
                let idx = self.context.i32_type().const_int(i as u64, false);
                let elem = self.simd_trunc_val(val, bit_width, is_float);
                result = self
                    .builder
                    .build_insert_element(result, elem, idx, &format!("sp{}", i))
                    .unwrap();
            }
            self.builder.build_store(vec_alloca, result).unwrap();
        } else if dispatch_name.starts_with("simd_add") || dispatch_name.starts_with("vector_add") {
            self.simd_binop(vt, args, exprs, vec_alloca, "add", is_float);
        } else if dispatch_name.starts_with("simd_sub") || dispatch_name.starts_with("vector_sub") {
            self.simd_binop(vt, args, exprs, vec_alloca, "sub", is_float);
        } else if dispatch_name.starts_with("simd_mul") || dispatch_name.starts_with("vector_mul") {
            self.simd_binop(vt, args, exprs, vec_alloca, "mul", is_float);
        } else if dispatch_name.starts_with("simd_extract")
            || dispatch_name.starts_with("vector_get")
        {
            let vec_ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
            let idx = self.gen_expr_safe(&args[1], exprs).into_int_value();
            let loaded = self.simd_load_vec(vec_ptr, vt);
            let extracted = self
                .builder
                .build_extract_element(loaded, idx, "extract")
                .unwrap();
            let result = if bit_width == 32 && !is_float {
                self.simd_zext_i64(extracted)
            } else {
                extracted
            };
            let alloca = *self.locals.get(dest).unwrap();
            self.builder.build_store(alloca, result).unwrap();
            return;
        } else if dispatch_name.starts_with("simd_insert")
            || dispatch_name.starts_with("vector_set")
        {
            let vec_ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
            let val = self.gen_expr_safe(&args[1], exprs);
            let idx = self.gen_expr_safe(&args[2], exprs).into_int_value();
            let loaded = self.simd_load_vec(vec_ptr, vt);
            let scalar = self.simd_trunc_val(val, bit_width, is_float);
            let result = self
                .builder
                .build_insert_element(loaded, scalar, idx, "ins")
                .unwrap();
            self.simd_store_vec(vec_ptr, result);
            let alloca = *self.locals.get(dest).unwrap();
            self.builder.build_store(alloca, vec_ptr).unwrap();
            return;
        } else if dispatch_name.starts_with("simd_load") {
            let ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
            let loaded = self.simd_load_vec(ptr, vt);
            self.builder.build_store(vec_alloca, loaded).unwrap();
        } else if dispatch_name.starts_with("simd_store") {
            let dst = self.gen_expr_safe(&args[0], exprs).into_int_value();
            let src = self.gen_expr_safe(&args[1], exprs).into_int_value();
            let loaded = self.simd_load_vec(src, vt);
            self.simd_store_vec(dst, loaded);
            let alloca = *self.locals.get(dest).unwrap();
            self.builder
                .build_store(alloca, self.i64_type.const_zero())
                .unwrap();
            return;
        } else if dispatch_name.starts_with("simd_free") || dispatch_name.starts_with("vector_free")
        {
            let alloca = *self.locals.get(dest).unwrap();
            self.builder
                .build_store(alloca, self.i64_type.const_zero())
                .unwrap();
            return;
        } else if dispatch_name.starts_with("vector_make") {
            let poison = vt.get_undef();
            let mut result = poison;
            let n = args.len().min(lanes as usize);
            for i in 0..n {
                let val = self.gen_expr_safe(&args[i], exprs);
                let idx = self.context.i32_type().const_int(i as u64, false);
                let scalar = self.simd_trunc_val(val, bit_width, is_float);
                result = self
                    .builder
                    .build_insert_element(result, scalar, idx, &format!("mk{}", i))
                    .unwrap();
            }
            self.builder.build_store(vec_alloca, result).unwrap();
        } else {
            let alloca = *self.locals.get(dest).unwrap();
            self.builder
                .build_store(alloca, self.i64_type.const_zero())
                .unwrap();
            return;
        }

        let ptr_as_i64 = self
            .builder
            .build_ptr_to_int(vec_alloca, self.i64_type, "simd_res")
            .unwrap();
        let alloca = *self.locals.get(dest).unwrap();
        self.builder.build_store(alloca, ptr_as_i64).unwrap();
    }

    /// Handle binary SIMD operation (add/sub/mul).
    fn simd_binop(
        &mut self,
        vt: inkwell::types::VectorType<'ctx>,
        args: &[u32],
        exprs: &HashMap<u32, MirExpr>,
        result_alloca: PointerValue<'ctx>,
        op: &str,
        is_float: bool,
    ) {
        if args.len() < 2 {
            return;
        }
        let a_ptr = self.gen_expr_safe(&args[0], exprs).into_int_value();
        let b_ptr = self.gen_expr_safe(&args[1], exprs).into_int_value();
        let vec_a = self.simd_load_vec(a_ptr, vt);
        let vec_b = self.simd_load_vec(b_ptr, vt);
        let result = match op {
            "add" if is_float => self.builder.build_float_add(vec_a, vec_b, "vadd").unwrap(),
            "add" => self.builder.build_int_add(vec_a, vec_b, "vadd").unwrap(),
            "sub" if is_float => self.builder.build_float_sub(vec_a, vec_b, "vsub").unwrap(),
            "sub" => self.builder.build_int_sub(vec_a, vec_b, "vsub").unwrap(),
            "mul" if is_float => self.builder.build_float_mul(vec_a, vec_b, "vmul").unwrap(),
            "mul" => self.builder.build_int_mul(vec_a, vec_b, "vmul").unwrap(),
            _ => vec_a,
        };
        self.builder.build_store(result_alloca, result).unwrap();
    }

    /// Handle trivial SIMD constants (is_supported, optimal_size).
    fn handle_simd_const(&mut self, method: &str, dest: &u32) {
        let alloca = *self.locals.get(dest).unwrap();
        let val = if method == "is_supported" { 1 } else { 4 };
        self.builder
            .build_store(alloca, self.i64_type.const_int(val, false))
            .unwrap();
    }

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
            Type::Range => self
                .context
                .struct_type(
                    &[
                        self.context.i64_type().into(),
                        self.context.i64_type().into(),
                    ],
                    false,
                )
                .into(),
            Type::V4I64 => self.vec4_i64_type.into(),
            Type::I32x4 => self.context.i32_type().vec_type(4).into(),
            Type::I64x2 => self.context.i64_type().vec_type(2).into(),
            Type::F32x4 => self.context.f32_type().vec_type(4).into(),
            Type::Vector(element_type, size) => {
                let element_llvm = self.type_to_llvm_type(element_type);
                let lane_count = match size {
                    crate::middle::types::ArraySize::Literal(n) => *n as u32,
                    _ => 4, // default
                };
                match element_llvm {
                    inkwell::types::BasicTypeEnum::IntType(int_type) => {
                        int_type.vec_type(lane_count).into()
                    }
                    inkwell::types::BasicTypeEnum::FloatType(float_type) => {
                        float_type.vec_type(lane_count).into()
                    }
                    _ => self.context.i64_type().vec_type(lane_count).into(),
                }
            }
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
                self.context
                    .struct_type(
                        &[
                            self.context.ptr_type(AddressSpace::default()).into(),
                            self.context.i64_type().into(),
                        ],
                        false,
                    )
                    .into()
            }
            Type::DynamicArray(element_type) => {
                let element_llvm_type = self.type_to_llvm_type(element_type);
                // Dynamic array is similar to slice
                self.context
                    .struct_type(
                        &[
                            self.context.ptr_type(AddressSpace::default()).into(),
                            self.context.i64_type().into(),
                            self.context.i64_type().into(), // capacity
                        ],
                        false,
                    )
                    .into()
            }
            Type::Tuple(element_types) => {
                let llvm_element_types: Vec<inkwell::types::BasicTypeEnum> = element_types
                    .iter()
                    .map(|t| self.type_to_llvm_type(t))
                    .collect();
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
                self.context
                    .struct_type(
                        &[
                            self.context.ptr_type(AddressSpace::default()).into(),
                            self.context.ptr_type(AddressSpace::default()).into(),
                        ],
                        false,
                    )
                    .into()
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
            // Stepanov concept types — represented as i64 at runtime
            Type::Regular
            | Type::TotallyOrdered
            | Type::Semigroup
            | Type::Monoid
            | Type::Group
            | Type::Ring => self.context.i64_type().into(),
            Type::TraitResult(_) => self.context.i64_type().into(),
            // Iterator category tags — represented as i64 at runtime
            Type::InputIterator
            | Type::ForwardIterator
            | Type::BidirectionalIterator
            | Type::RandomAccessIterator => self.context.i64_type().into(),
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
