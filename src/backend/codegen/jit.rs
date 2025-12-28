// src/backend/codegen/jit.rs
use super::codegen::LLVMCodegen;
use crate::runtime::actor::channel::{host_channel_recv, host_channel_send};
use crate::runtime::actor::scheduler::host_spawn;
use crate::runtime::host::{
    host_datetime_now, host_free, host_http_get, host_tls_handshake,
};
use crate::runtime::xai::XAIClient;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::passes::PassManager;
use inkwell::targets::{InitializationConfig, Target, TargetMachine};
use inkwell::OptimizationLevel;
use serde_json::Value;

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn finalize_and_jit(
        &mut self,
    ) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e| e.to_string())?;

        // Initialize native target for proper vectorization
        Target::initialize_native(&InitializationConfig::default())?;
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple)?;
        let target_machine = target.create_target_machine(
            &target_triple,
            &TargetMachine::get_host_cpu_name().to_string(),
            &TargetMachine::get_host_cpu_features().to_string(),
            OptimizationLevel::Aggressive,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        ).ok_or("Failed to create target machine")?;

        self.module.set_triple(&target_triple);
        self.module.set_data_layout(&target_machine.get_target_data().get_data_layout());

        // Accurate MIR statistics for AI prompt
        let mut stmt_count = 0;
        let mut local_count = 0;
        let mut simd_eligible = 0;
        for func in self.module.get_functions() {
            for bb in func.get_basic_blocks() {
                stmt_count += bb.get_instructions().count() as u32;
                for inst in bb.get_instructions() {
                    if matches!(
                        inst.get_opcode(),
                        inkwell::values::InstructionOpcode::Add
                            | inkwell::values::InstructionOpcode::Mul
                    ) {
                        simd_eligible += 1;
                    }
                }
            }
            local_count += self.locals.len();
        }

        let mir_stats = format!(
            "Functions: {}, Instructions: {}, Locals: {}, SIMD-eligible ops: {}",
            self.module.get_functions().count(),
            stmt_count,
            local_count,
            simd_eligible
        );

        // Query xAI for recommended passes
        let client = XAIClient::new().map_err(|e| format!("XAI init error: {}", e))?;
        let rec = client.mlgo_optimize(&mir_stats)?;
        let json: Value = serde_json::from_str(&rec).unwrap_or(Value::Null);
        let passes = json["passes"].as_array().cloned().unwrap_or_default();

        // Function-level pass manager
        let fpm = PassManager::create(());

        for pass in passes {
            if let Some(name) = pass.as_str() {
                match name {
                    "instcombine" => fpm.add_instruction_combining_pass(),
                    "reassociate" => fpm.add_reassociate_pass(),
                    "gvn" => fpm.add_gvn_pass(),
                    "cfg-simplification" => fpm.add_cfg_simplification_pass(),
                    "slp-vectorize" => fpm.add_slp_vectorize_pass(),
                    "vectorize" => {
                        fpm.add_loop_vectorize_pass();
                        fpm.add_slp_vectorize_pass();
                    }
                    "loop-unroll" => fpm.add_loop_unroll_pass(),
                    "licm" => fpm.add_licm_pass(),
                    _ => {}
                }
                eprintln!("Running MLGO-recommended pass: {}", name);
            }
        }

        fpm.initialize();
        for func in self.module.get_functions() {
            fpm.run_on(&func);
        }

        // Extra module-level vectorization
        let mpm = PassManager::create(&self.module);
        mpm.add_slp_vectorize_pass();
        mpm.add_loop_vectorize_pass();
        mpm.run_on(&self.module);

        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;

        // Map host intrinsics
        ee.add_global_mapping(&self.module.get_function("datetime_now").unwrap(), host_datetime_now as usize);
        ee.add_global_mapping(&self.module.get_function("free").unwrap(), host_free as usize);
        ee.add_global_mapping(&self.module.get_function("channel_send").unwrap(), host_channel_send as usize);
        ee.add_global_mapping(&self.module.get_function("channel_recv").unwrap(), host_channel_recv as usize);
        ee.add_global_mapping(&self.module.get_function("spawn").unwrap(), host_spawn as usize);
        ee.add_global_mapping(&self.module.get_function("http_get").unwrap(), host_http_get as usize);
        ee.add_global_mapping(&self.module.get_function("tls_handshake").unwrap(), host_tls_handshake as usize);

        Ok(ee)
    }
}
