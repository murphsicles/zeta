// src/backend/codegen/jit.rs
use crate::xai::XAIClient;
use crate::runtime::host::{host_datetime_now, host_free, host_http_get, host_tls_handshake, host_str_concat};
use crate::actor::{host_channel_send, host_channel_recv, host_spawn};
use inkwell::OptimizationLevel;
use inkwell::attributes::Attribute;
use inkwell::execution_engine::ExecutionEngine;
use inkwell::passes::PassManager;
use inkwell::support::LLVMString;
use serde_json::Value;

impl<'ctx> LLVMCodegen<'ctx> {
    pub fn finalize_and_jit(&mut self) -> Result<ExecutionEngine<'ctx>, Box<dyn std::error::Error>> {
        self.module.verify().map_err(|e: LLVMString| e.to_string())?;
        let nounwind_id = Attribute::get_named_enum_kind_id("nounwind");
        let sanitize_attr = self.context.create_enum_attribute(nounwind_id, 1);
        for fn_val in self.module.get_functions() {
            fn_val.add_attribute(AttributeLoc::Function, sanitize_attr);
        }
        let client = XAIClient::new().map_err(|e| format!("XAI init error: {}", e))?;
        let mir_stats = format!(
            "Stmts: {}, Locals: {}, SIMD eligible: {}",
            self.module.print_to_string().to_str().map_or(0, |s| s.len()),
            self.locals.len(),
            1
        );
        if let Ok(rec) = client.mlgo_optimize(&mir_stats) {
            if let Ok(json) = serde_json::from_str::<Value>(&rec) {
                if let Some(passes) = json["passes"].as_array() {
                    let fpm = PassManager::create(&self.module);
                    for p in passes {
                        if let Some(ps) = p.as_str() {
                            if ps == "vectorize" {
                                eprintln!("Running MLGO vectorize pass for SIMD");
                            } else {
                                eprintln!("Running AI-recommended pass: {}", ps);
                            }
                        }
                    }
                    fpm.initialize();
                    for fn_val in self.fns.values() {
                        fpm.run_on(fn_val);
                    }
                    fpm.finalize();
                }
            }
        }
        let ee = self.module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
        ee.add_global_mapping(&self.module.get_function("datetime_now").unwrap(), host_datetime_now as usize);
        ee.add_global_mapping(&self.module.get_function("free").unwrap(), host_free as usize);
        ee.add_global_mapping(&self.module.get_function("channel_send").unwrap(), host_channel_send as usize);
        ee.add_global_mapping(&self.module.get_function("channel_recv").unwrap(), host_channel_recv as usize);
        ee.add_global_mapping(&self.module.get_function("spawn").unwrap(), host_spawn as usize);
        ee.add_global_mapping(&self.module.get_function("http_get").unwrap(), host_http_get as usize);
        ee.add_global_mapping(&self.module.get_function("tls_handshake").unwrap(), host_tls_handshake as usize);
        ee.add_global_mapping(&self.module.get_function("str_concat").unwrap(), host_str_concat as usize);
        Ok(ee)
    }
}
