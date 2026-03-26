// codegen_integration.rs - Integration between existing codegen and advanced LLVM features
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\codegen_integration.rs
// Purpose: Bridge between Zeta's current codegen and new advanced features

use inkwell::context::Context;
use crate::llvm_advanced::{AdvancedCodeGen, OptimizationLevel};
use crate::llvm_extensions_simple::SimpleCodeGen;

/// Integrated code generator that combines simple and advanced features
pub struct IntegratedCodeGen<'ctx> {
    pub simple: SimpleCodeGen<'ctx>,
    pub advanced: AdvancedCodeGen<'ctx>,
    pub context: &'ctx Context,
}

impl<'ctx> IntegratedCodeGen<'ctx> {
    /// Create a new integrated code generator
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let simple = SimpleCodeGen::new(context, module_name);
        let advanced = AdvancedCodeGen::new(context, module_name);
        
        Self {
            simple,
            advanced,
            context,
        }
    }
    
    /// Create a function using simple codegen
    pub fn create_simple_function(&mut self) -> Result<(), String> {
        self.simple.create_add_function()
    }
    
    /// Create an optimized function using advanced codegen
    pub fn create_optimized_function(&mut self) -> Result<(), String> {
        self.advanced.set_optimization_level(OptimizationLevel::Aggressive);
        self.advanced.create_optimized_factorial()
    }
    
    /// Apply optimizations to both generators
    pub fn apply_all_optimizations(&self) -> Result<(), String> {
        println!("Applying optimizations to integrated codegen...");
        
        // Apply simple optimizations (if any)
        println!("Simple codegen optimizations: Basic verification");
        self.simple.verify()?;
        
        // Apply advanced optimizations
        println!("Advanced codegen optimizations:");
        self.advanced.apply_optimizations()?;
        
        Ok(())
    }
    
    /// Generate output files from both generators
    pub fn generate_outputs(&self, base_name: &str) -> Result<(), String> {
        let simple_ir = format!("{}_simple.ll", base_name);
        let advanced_ir = format!("{}_advanced.ll", base_name);
        let object_file = format!("{}.o", base_name);
        let wasm_file = format!("{}.wasm", base_name);
        let assembly_file = format!("{}.s", base_name);
        
        println!("Generating integrated outputs...");
        
        // Generate simple IR
        self.simple.write_to_file(&simple_ir)?;
        println!("  Simple IR: {}", simple_ir);
        
        // Generate advanced IR
        self.advanced.write_optimized_ir(&advanced_ir)?;
        println!("  Advanced IR: {}", advanced_ir);
        
        // Generate object file
        self.advanced.generate_object_file(&object_file)?;
        println!("  Object file: {}", object_file);
        
        // Generate WebAssembly
        self.advanced.generate_wasm(&wasm_file)?;
        println!("  WebAssembly: {}", wasm_file);
        
        // Generate assembly
        self.advanced.generate_assembly(&assembly_file)?;
        println!("  Assembly: {}", assembly_file);
        
        Ok(())
    }
    
    /// Verify both generators
    pub fn verify_all(&self) -> Result<(), String> {
        println!("Verifying integrated codegen...");
        
        println!("Simple codegen verification:");
        self.simple.verify()?;
        
        println!("Advanced codegen verification:");
        self.advanced.verify_with_diagnostics()?;
        
        println!("✅ All verifications passed");
        Ok(())
    }
    
    /// Print comprehensive report
    pub fn print_integration_report(&self) {
        println!("Integrated Code Generation Report");
        println!("=================================");
        println!();
        
        println!("Simple CodeGen Features:");
        println!("  - Basic function creation");
        println!("  - Module verification");
        println!("  - IR file output");
        println!();
        
        println!("Advanced CodeGen Features:");
        println!("  - Multiple optimization levels");
        println!("  - Debug information support");
        println!("  - Vectorized operations");
        println!("  - Cross-platform compilation");
        println!("  - WebAssembly generation");
        println!("  - Profile-guided optimization");
        println!("  - Link-time optimization");
        println!();
        
        self.advanced.print_optimization_report();
    }
    
    /// Create a benchmark suite for performance comparison
    pub fn create_benchmark_suite(&mut self) -> Result<(), String> {
        println!("Creating benchmark suite...");
        
        // Create simple version
        self.create_simple_function()?;
        println!("  Created simple function for baseline");
        
        // Create optimized version
        self.create_optimized_function()?;
        println!("  Created optimized function for comparison");
        
        // Create vectorized version
        self.advanced.create_vectorized_add()?;
        println!("  Created vectorized function for SIMD comparison");
        
        println!("✅ Benchmark suite created");
        println!("   Compare performance between:");
        println!("   1. Simple implementation (baseline)");
        println!("   2. Optimized implementation");
        println!("   3. Vectorized implementation (SIMD)");
        
        Ok(())
    }
}

/// Configuration for integrated code generation
pub struct IntegrationConfig {
    pub enable_optimizations: bool,
    pub enable_debug_info: bool,
    pub enable_cross_platform: bool,
    pub enable_benchmarks: bool,
}

impl IntegrationConfig {
    pub fn new() -> Self {
        Self {
            enable_optimizations: true,
            enable_debug_info: false,
            enable_cross_platform: true,
            enable_benchmarks: true,
        }
    }
    
    pub fn development() -> Self {
        Self {
            enable_optimizations: false,
            enable_debug_info: true,
            enable_cross_platform: false,
            enable_benchmarks: false,
        }
    }
    
    pub fn production() -> Self {
        Self {
            enable_optimizations: true,
            enable_debug_info: false,
            enable_cross_platform: true,
            enable_benchmarks: false,
        }
    }
    
    pub fn benchmark() -> Self {
        Self {
            enable_optimizations: true,
            enable_debug_info: false,
            enable_cross_platform: false,
            enable_benchmarks: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_integrated_codegen_creation() {
        let context = Context::create();
        let codegen = IntegratedCodeGen::new(&context, "test_integrated");
        
        codegen.print_integration_report();
        assert!(codegen.verify_all().is_ok());
    }
    
    #[test]
    fn test_simple_function_creation() {
        let context = Context::create();
        let mut codegen = IntegratedCodeGen::new(&context, "test_simple_func");
        
        assert!(codegen.create_simple_function().is_ok());
        assert!(codegen.verify_all().is_ok());
    }
    
    #[test]
    fn test_optimized_function_creation() {
        let context = Context::create();
        let mut codegen = IntegratedCodeGen::new(&context, "test_opt_func");
        
        assert!(codegen.create_optimized_function().is_ok());
        assert!(codegen.apply_all_optimizations().is_ok());
        assert!(codegen.verify_all().is_ok());
    }
    
    #[test]
    fn test_benchmark_suite_creation() {
        let context = Context::create();
        let mut codegen = IntegratedCodeGen::new(&context, "test_benchmarks");
        
        assert!(codegen.create_benchmark_suite().is_ok());
        assert!(codegen.verify_all().is_ok());
    }
    
    #[test]
    fn test_configurations() {
        let dev_config = IntegrationConfig::development();
        assert!(!dev_config.enable_optimizations);
        assert!(dev_config.enable_debug_info);
        
        let prod_config = IntegrationConfig::production();
        assert!(prod_config.enable_optimizations);
        assert!(!prod_config.enable_debug_info);
        
        let bench_config = IntegrationConfig::benchmark();
        assert!(bench_config.enable_optimizations);
        assert!(bench_config.enable_benchmarks);
    }
    
    #[test]
    fn test_output_generation() {
        let context = Context::create();
        let mut codegen = IntegratedCodeGen::new(&context, "test_outputs");
        
        // Create some functions first
        assert!(codegen.create_simple_function().is_ok());
        
        // Test output generation (files won't actually be created in test)
        let result = codegen.generate_outputs("test_output");
        assert!(result.is_ok());
    }
}
