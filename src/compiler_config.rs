//! Compiler configuration and flags
//!
//! This module handles compiler configuration, command-line flags,
//! and settings for diagnostics, optimizations, and tooling.

use std::collections::HashMap;
use std::path::PathBuf;

use crate::diagnostics::{WarningLevel, DiagnosticReporter};
use crate::middle::optimization::OptLevel;

/// Compiler configuration
#[derive(Debug, Clone)]
pub struct CompilerConfig {
    /// Input file path
    pub input_file: Option<PathBuf>,
    
    /// Output file path
    pub output_file: Option<PathBuf>,
    
    /// Optimization level
    pub opt_level: OptLevel,
    
    /// Emit LLVM IR instead of binary
    pub emit_llvm: bool,
    
    /// Emit assembly instead of binary
    pub emit_asm: bool,
    
    /// Target triple (e.g., "x86_64-unknown-linux-gnu")
    pub target_triple: Option<String>,
    
    /// Warning level configuration
    pub warning_level: WarningLevel,
    
    /// Specific warning configurations
    pub warning_config: HashMap<String, WarningLevel>,
    
    /// Enable debug information
    pub debug_info: bool,
    
    /// Enable incremental compilation
    pub incremental: bool,
    
    /// Enable color in diagnostics
    pub color_diagnostics: bool,
    
    /// Enable verbose output
    pub verbose: bool,
    
    /// Additional include paths
    pub include_paths: Vec<PathBuf>,
    
    /// Define macros
    pub defines: HashMap<String, Option<String>>,
    
    /// Enable sanitizers
    pub sanitizers: Vec<Sanitizer>,
    
    /// Enable link-time optimization
    pub lto: bool,
    
    /// Enable profile-guided optimization
    pub pgo: bool,
}

/// Sanitizer types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sanitizer {
    Address,
    Memory,
    Thread,
    UndefinedBehavior,
}

impl Default for CompilerConfig {
    fn default() -> Self {
        Self {
            input_file: None,
            output_file: None,
            opt_level: OptLevel::O0,
            emit_llvm: false,
            emit_asm: false,
            target_triple: None,
            warning_level: WarningLevel::Warn,
            warning_config: HashMap::new(),
            debug_info: false,
            incremental: false,
            color_diagnostics: true,
            verbose: false,
            include_paths: Vec::new(),
            defines: HashMap::new(),
            sanitizers: Vec::new(),
            lto: false,
            pgo: false,
        }
    }
}

impl CompilerConfig {
    /// Create a new compiler configuration
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Parse command-line arguments
    pub fn from_args(args: &[String]) -> Result<Self, String> {
        let mut config = Self::new();
        let mut i = 0;
        
        while i < args.len() {
            match args[i].as_str() {
                "-o" => {
                    i += 1;
                    if i < args.len() {
                        config.output_file = Some(PathBuf::from(&args[i]));
                    } else {
                        return Err("Missing argument for -o".to_string());
                    }
                }
                "-O0" => config.opt_level = OptLevel::O0,
                "-O1" => config.opt_level = OptLevel::O1,
                "-O2" => config.opt_level = OptLevel::O2,
                "-O3" => config.opt_level = OptLevel::O3,
                "--emit-llvm" => config.emit_llvm = true,
                "--emit-asm" => config.emit_asm = true,
                "--target" => {
                    i += 1;
                    if i < args.len() {
                        config.target_triple = Some(args[i].clone());
                    } else {
                        return Err("Missing argument for --target".to_string());
                    }
                }
                "-W" => {
                    i += 1;
                    if i < args.len() {
                        match args[i].as_str() {
                            "all" => config.warning_level = WarningLevel::Warn,
                            "error" => config.warning_level = WarningLevel::Deny,
                            "allow" => config.warning_level = WarningLevel::Allow,
                            "forbid" => config.warning_level = WarningLevel::Forbid,
                            _ => return Err(format!("Unknown warning level: {}", args[i])),
                        }
                    } else {
                        return Err("Missing argument for -W".to_string());
                    }
                }
                "-Wno-" => {
                    i += 1;
                    if i < args.len() {
                        config.warning_config.insert(args[i].clone(), WarningLevel::Allow);
                    } else {
                        return Err("Missing argument for -Wno-".to_string());
                    }
                }
                "-Werror-" => {
                    i += 1;
                    if i < args.len() {
                        config.warning_config.insert(args[i].clone(), WarningLevel::Deny);
                    } else {
                        return Err("Missing argument for -Werror-".to_string());
                    }
                }
                "-g" => config.debug_info = true,
                "--incremental" => config.incremental = true,
                "--no-color" => config.color_diagnostics = false,
                "-v" | "--verbose" => config.verbose = true,
                "-I" => {
                    i += 1;
                    if i < args.len() {
                        config.include_paths.push(PathBuf::from(&args[i]));
                    } else {
                        return Err("Missing argument for -I".to_string());
                    }
                }
                "-D" => {
                    i += 1;
                    if i < args.len() {
                        let def = args[i].clone();
                        if let Some((name, value)) = def.split_once('=') {
                            config.defines.insert(name.to_string(), Some(value.to_string()));
                        } else {
                            config.defines.insert(def, None);
                        }
                    } else {
                        return Err("Missing argument for -D".to_string());
                    }
                }
                "--sanitizer" => {
                    i += 1;
                    if i < args.len() {
                        match args[i].as_str() {
                            "address" => config.sanitizers.push(Sanitizer::Address),
                            "memory" => config.sanitizers.push(Sanitizer::Memory),
                            "thread" => config.sanitizers.push(Sanitizer::Thread),
                            "undefined" => config.sanitizers.push(Sanitizer::UndefinedBehavior),
                            _ => return Err(format!("Unknown sanitizer: {}", args[i])),
                        }
                    } else {
                        return Err("Missing argument for --sanitizer".to_string());
                    }
                }
                "--lto" => config.lto = true,
                "--pgo" => config.pgo = true,
                "--help" | "-h" => {
                    return Err(Self::help_text());
                }
                arg if arg.starts_with('-') => {
                    return Err(format!("Unknown option: {}", arg));
                }
                _ => {
                    // Assume it's an input file
                    if config.input_file.is_none() {
                        config.input_file = Some(PathBuf::from(&args[i]));
                    } else {
                        return Err("Multiple input files not supported".to_string());
                    }
                }
            }
            i += 1;
        }
        
        Ok(config)
    }
    
    /// Get help text
    pub fn help_text() -> String {
        r#"Zeta Compiler v0.3.38 - Compiler Improvements Release

Usage: zeta [OPTIONS] <input-file>

Options:
  -o <file>                Write output to <file>
  -O0, -O1, -O2, -O3       Set optimization level (default: -O0)
  --emit-llvm              Emit LLVM IR instead of binary
  --emit-asm               Emit assembly instead of binary
  --target <triple>        Target architecture (e.g., x86_64-unknown-linux-gnu)
  
  Warnings and Diagnostics:
  -W <level>               Set warning level (all, error, allow, forbid)
  -Wno-<warning>           Disable specific warning
  -Werror-<warning>        Treat specific warning as error
  -g                       Generate debug information
  --no-color               Disable colored diagnostics
  -v, --verbose            Verbose output
  
  Compilation Features:
  --incremental            Enable incremental compilation
  -I <path>                Add include path
  -D <name>[=<value>]      Define macro
  --sanitizer <type>       Enable sanitizer (address, memory, thread, undefined)
  --lto                    Enable link-time optimization
  --pgo                    Enable profile-guided optimization
  
  Miscellaneous:
  -h, --help               Display this help message
  
Examples:
  zeta -O2 -o program program.z
  zeta --emit-llvm --target wasm32-unknown-unknown program.z
  zeta -W error -g program.z
"#.to_string()
    }
    
    /// Create a diagnostic reporter configured with warning settings
    pub fn create_diagnostic_reporter(&self) -> DiagnosticReporter {
        let mut reporter = DiagnosticReporter::new()
            .with_warning_level(self.warning_level);
        
        // Apply specific warning configurations
        for (code, level) in &self.warning_config {
            // Convert WarningLevel to the diagnostic WarningLevel
            let diag_level = match level {
                WarningLevel::Allow => crate::diagnostics::WarningLevel::Allow,
                WarningLevel::Warn => crate::diagnostics::WarningLevel::Warn,
                WarningLevel::Deny => crate::diagnostics::WarningLevel::Deny,
                WarningLevel::Forbid => crate::diagnostics::WarningLevel::Forbid,
            };
            
            // Get description from error codes if available
            let description = crate::error_codes::ERROR_CODES
                .get(code)
                .map(|ec| ec.description.clone())
                .unwrap_or_else(|| format!("Warning {}", code));
            
            reporter.configure_warning(code, diag_level, &description);
        }
        
        reporter
    }
    
    /// Check if configuration is valid
    pub fn validate(&self) -> Result<(), String> {
        if self.input_file.is_none() {
            return Err("No input file specified".to_string());
        }
        
        // Check for conflicting options
        if self.emit_llvm && self.emit_asm {
            return Err("Cannot emit both LLVM IR and assembly".to_string());
        }
        
        // Check target triple format if specified
        if let Some(triple) = &self.target_triple {
            if triple.split('-').count() < 3 {
                return Err(format!("Invalid target triple: {}", triple));
            }
        }
        
        Ok(())
    }
}

/// Build configuration for incremental compilation
#[derive(Debug, Clone)]
pub struct BuildConfig {
    /// Build directory for incremental compilation artifacts
    pub build_dir: PathBuf,
    
    /// Cache directory for compiled modules
    pub cache_dir: PathBuf,
    
    /// Whether to force rebuild
    pub force_rebuild: bool,
    
    /// Maximum cache size in bytes
    pub max_cache_size: u64,
}

impl Default for BuildConfig {
    fn default() -> Self {
        Self {
            build_dir: PathBuf::from("target"),
            cache_dir: PathBuf::from("target/.zeta_cache"),
            force_rebuild: false,
            max_cache_size: 100 * 1024 * 1024, // 100 MB
        }
    }
}

/// Profile-guided optimization configuration
#[derive(Debug, Clone)]
pub struct PGOConfig {
    /// Directory for PGO profiles
    pub profile_dir: PathBuf,
    
    /// Generate instrumentation for profiling
    pub gen_profile: bool,
    
    /// Use existing profile data
    pub use_profile: bool,
    
    /// Profile data files
    pub profile_files: Vec<PathBuf>,
}

impl Default for PGOConfig {
    fn default() -> Self {
        Self {
            profile_dir: PathBuf::from("target/pgo"),
            gen_profile: false,
            use_profile: false,
            profile_files: Vec::new(),
        }
    }
}