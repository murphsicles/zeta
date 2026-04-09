//! Debug information generation and management

use std::collections::HashMap;
use serde::{Deserialize, Serialize};

/// Source location
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SourceLocation {
    /// File path
    pub file: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
}

/// Variable information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableInfo {
    /// Variable name
    pub name: String,
    /// Variable type
    pub type_name: String,
    /// Memory address (if available)
    pub address: Option<u64>,
    /// Source location where variable is declared
    pub declaration: SourceLocation,
    /// Scope information
    pub scope: VariableScope,
}

/// Variable scope
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VariableScope {
    /// Global variable
    Global,
    /// Function parameter
    Parameter(usize), // parameter index
    /// Local variable
    Local {
        /// Function name
        function: String,
        /// Stack frame offset
        frame_offset: i64,
    },
}

/// Function debug information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionInfo {
    /// Function name
    pub name: String,
    /// Return type
    pub return_type: String,
    /// Parameters
    pub parameters: Vec<VariableInfo>,
    /// Local variables
    pub locals: Vec<VariableInfo>,
    /// Source location of function definition
    pub definition: SourceLocation,
    /// Source locations for each instruction
    pub instruction_map: HashMap<u64, SourceLocation>,
}

/// Debug information for a compilation unit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DebugInfo {
    /// Compilation unit name (usually file name)
    pub unit_name: String,
    /// Source files
    pub source_files: Vec<String>,
    /// Functions
    pub functions: HashMap<String, FunctionInfo>,
    /// Global variables
    pub globals: HashMap<String, VariableInfo>,
    /// Type information
    pub types: HashMap<String, TypeInfo>,
}

/// Type information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeInfo {
    /// Type name
    pub name: String,
    /// Size in bytes
    pub size: usize,
    /// Alignment in bytes
    pub alignment: usize,
    /// Fields (for structs/enums)
    pub fields: Vec<FieldInfo>,
    /// Methods (for types with implementations)
    pub methods: Vec<MethodInfo>,
}

/// Field information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldInfo {
    /// Field name
    pub name: String,
    /// Field type
    pub type_name: String,
    /// Offset in bytes
    pub offset: usize,
}

/// Method information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MethodInfo {
    /// Method name
    pub name: String,
    /// Function information
    pub function: FunctionInfo,
}

impl DebugInfo {
    /// Create new debug information
    pub fn new(unit_name: &str) -> Self {
        Self {
            unit_name: unit_name.to_string(),
            source_files: Vec::new(),
            functions: HashMap::new(),
            globals: HashMap::new(),
            types: HashMap::new(),
        }
    }
    
    /// Add a source file
    pub fn add_source_file(&mut self, file_path: &str) {
        self.source_files.push(file_path.to_string());
    }
    
    /// Add a function
    pub fn add_function(&mut self, function: FunctionInfo) {
        self.functions.insert(function.name.clone(), function);
    }
    
    /// Add a global variable
    pub fn add_global(&mut self, variable: VariableInfo) {
        self.globals.insert(variable.name.clone(), variable);
    }
    
    /// Add a type
    pub fn add_type(&mut self, type_info: TypeInfo) {
        self.types.insert(type_info.name.clone(), type_info);
    }
    
    /// Find function by address
    pub fn find_function_by_address(&self, address: u64) -> Option<&FunctionInfo> {
        self.functions.values().find(|func| {
            func.instruction_map.keys().any(|&addr| addr == address)
        })
    }
    
    /// Find source location for address
    pub fn find_source_location(&self, address: u64) -> Option<SourceLocation> {
        for function in self.functions.values() {
            if let Some(location) = function.instruction_map.get(&address) {
                return Some(location.clone());
            }
        }
        None
    }
    
    /// Get variable information
    pub fn get_variable_info(&self, name: &str, scope: &str) -> Option<&VariableInfo> {
        // First check globals
        if let Some(var) = self.globals.get(name) {
            return Some(var);
        }
        
        // Then check function locals
        for function in self.functions.values() {
            if function.name == scope {
                if let Some(var) = function.locals.iter().find(|v| v.name == name) {
                    return Some(var);
                }
                if let Some(var) = function.parameters.iter().find(|v| v.name == name) {
                    return Some(var);
                }
            }
        }
        
        None
    }
    
    /// Serialize to JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
    
    /// Deserialize from JSON
    pub fn from_json(json: &str) -> Result<Self, serde_json::Error> {
        serde_json::from_str(json)
    }
}