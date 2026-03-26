// llvm_extensions_simple.rs - Simple LLVM extensions for Zeta v0.3.5
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\llvm_extensions_simple.rs
// Purpose: Basic LLVM extension functionality that compiles

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::OptimizationLevel;
use std::path::Path;

/// Simple extended LLVM code generator
pub struct SimpleCodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: inkwell::builder::Builder<'ctx>,
}

impl<'ctx> SimpleCodeGen<'ctx> {
    /// Create a new simple code generator
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        
        Self {
            context,
            module,
            builder,
        }
    }
    
    /// Create a simple add function for testing
    pub fn create_add_function(&mut self) -> Result<(), String> {
        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);
        let function = self.module.add_function("add", fn_type, None);
        
        // Create basic block
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);
        
        // Build addition
        let a = function.get_nth_param(0).expect("operation failed").into_int_value();
        let b = function.get_nth_param(1).expect("operation failed").into_int_value();
        let sum = self.builder.build_int_add(a, b, "sum");
        
        // Return the sum
        self.builder.build_return(Some(&sum));
        
        Ok(())
    }
    
    /// Verify module
    pub fn verify(&self) -> Result<(), String> {
        match self.module.verify() {
            Ok(_) => {
                println!("Module verification passed");
                Ok(())
            }
            Err(e) => Err(format!("Module verification failed: {:?}", e)),
        }
    }
    
    /// Print module IR
    pub fn print_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }
    
    /// Write module to file
    pub fn write_to_file(&self, path: &str) -> Result<(), String> {
        match self.module.print_to_file(Path::new(path)) {
            Ok(_) => {
                println!("Module written to: {}", path);
                Ok(())
            }
            Err(e) => Err(format!("Failed to write module: {:?}", e)),
        }
    }
}

/// Platform information
pub struct PlatformInfo {
    pub target_triple: String,
}

impl PlatformInfo {
    /// Get current platform info
    pub fn current() -> Self {
        // Get target triple from LLVM
        let triple = inkwell::targets::TargetMachine::get_default_triple();
        
        Self {
            target_triple: triple.to_str().unwrap_or("unknown-unknown-unknown").to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_simple_codegen_creation() {
        let context = Context::create();
        let codegen = SimpleCodeGen::new(&context, "test_module");
        
        assert!(!codegen.print_ir().is_empty());
    }
    
    #[test]
    fn test_create_add_function() {
        let context = Context::create();
        let mut codegen = SimpleCodeGen::new(&context, "test_add");
        
        assert!(codegen.create_add_function().is_ok());
        assert!(codegen.verify().is_ok());
        
        let ir = codegen.print_ir();
        assert!(ir.contains("define i32 @add"));
    }
    
    #[test]
    fn test_platform_info() {
        let info = PlatformInfo::current();
        assert!(!info.target_triple.is_empty());
    }
    
    #[test]
    fn test_write_to_file() {
        let context = Context::create();
        let mut codegen = SimpleCodeGen::new(&context, "test_write");
        
        assert!(codegen.create_add_function().is_ok());
        
        // Write to temporary file
        let temp_path = "test_output.ll";
        let result = codegen.write_to_file(temp_path);
        
        // Clean up
        let _ = std::fs::remove_file(temp_path);
        
        assert!(result.is_ok());
    }
}
