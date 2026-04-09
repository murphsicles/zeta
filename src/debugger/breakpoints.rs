//! Breakpoint management

use std::collections::HashMap;
use super::DebuggerError;

/// Breakpoint type
#[derive(Debug, Clone)]
pub enum BreakpointType {
    /// Break at source location
    Source {
        /// File path
        file: String,
        /// Line number
        line: u32,
    },
    /// Break at function entry
    Function {
        /// Function name
        name: String,
    },
    /// Break at address
    Address {
        /// Memory address
        address: u64,
    },
    /// Conditional breakpoint
    Conditional {
        /// Base breakpoint
        base: Box<BreakpointType>,
        /// Condition expression
        condition: String,
    },
}

/// Breakpoint
#[derive(Debug, Clone)]
pub struct Breakpoint {
    /// Breakpoint ID
    pub id: usize,
    /// Breakpoint type
    pub breakpoint_type: BreakpointType,
    /// Whether breakpoint is enabled
    pub enabled: bool,
    /// Hit count
    pub hit_count: u64,
}

impl Breakpoint {
    /// Create a new breakpoint
    pub fn new(id: usize, breakpoint_type: BreakpointType) -> Self {
        Self {
            id,
            breakpoint_type,
            enabled: true,
            hit_count: 0,
        }
    }
    
    /// Check if breakpoint matches a source location
    pub fn matches_source(&self, file: &str, line: u32) -> bool {
        if !self.enabled {
            return false;
        }
        
        match &self.breakpoint_type {
            BreakpointType::Source { file: bp_file, line: bp_line } => {
                bp_file == file && *bp_line == line
            }
            _ => false,
        }
    }
    
    /// Check if breakpoint matches a function
    pub fn matches_function(&self, name: &str) -> bool {
        if !self.enabled {
            return false;
        }
        
        match &self.breakpoint_type {
            BreakpointType::Function { name: bp_name } => bp_name == name,
            _ => false,
        }
    }
    
    /// Check if breakpoint matches an address
    pub fn matches_address(&self, address: u64) -> bool {
        if !self.enabled {
            return false;
        }
        
        match &self.breakpoint_type {
            BreakpointType::Address { address: bp_address } => *bp_address == address,
            _ => false,
        }
    }
    
    /// Increment hit count
    pub fn increment_hit(&mut self) {
        self.hit_count += 1;
    }
    
    /// Enable breakpoint
    pub fn enable(&mut self) {
        self.enabled = true;
    }
    
    /// Disable breakpoint
    pub fn disable(&mut self) {
        self.enabled = false;
    }
    
    /// Toggle breakpoint
    pub fn toggle(&mut self) {
        self.enabled = !self.enabled;
    }
}

/// Breakpoint manager
pub struct BreakpointManager {
    /// Next breakpoint ID
    next_id: usize,
    /// Breakpoints by ID
    breakpoints: HashMap<usize, Breakpoint>,
}

impl BreakpointManager {
    /// Create a new breakpoint manager
    pub fn new() -> Self {
        Self {
            next_id: 1,
            breakpoints: HashMap::new(),
        }
    }
    
    /// Add a breakpoint
    pub fn add_breakpoint(&mut self, location: &str) -> Result<usize, DebuggerError> {
        let breakpoint_type = self.parse_location(location)?;
        let id = self.next_id;
        self.next_id += 1;
        
        let breakpoint = Breakpoint::new(id, breakpoint_type);
        self.breakpoints.insert(id, breakpoint);
        
        Ok(id)
    }
    
    /// Remove a breakpoint
    pub fn remove_breakpoint(&mut self, id: usize) -> Result<(), DebuggerError> {
        if self.breakpoints.remove(&id).is_none() {
            return Err(DebuggerError::InvalidBreakpoint(
                format!("Breakpoint {} not found", id)
            ));
        }
        Ok(())
    }
    
    /// Enable a breakpoint
    pub fn enable_breakpoint(&mut self, id: usize) -> Result<(), DebuggerError> {
        if let Some(breakpoint) = self.breakpoints.get_mut(&id) {
            breakpoint.enable();
            Ok(())
        } else {
            Err(DebuggerError::InvalidBreakpoint(
                format!("Breakpoint {} not found", id)
            ))
        }
    }
    
    /// Disable a breakpoint
    pub fn disable_breakpoint(&mut self, id: usize) -> Result<(), DebuggerError> {
        if let Some(breakpoint) = self.breakpoints.get_mut(&id) {
            breakpoint.disable();
            Ok(())
        } else {
            Err(DebuggerError::InvalidBreakpoint(
                format!("Breakpoint {} not found", id)
            ))
        }
    }
    
    /// Toggle a breakpoint
    pub fn toggle_breakpoint(&mut self, id: usize) -> Result<(), DebuggerError> {
        if let Some(breakpoint) = self.breakpoints.get_mut(&id) {
            breakpoint.toggle();
            Ok(())
        } else {
            Err(DebuggerError::InvalidBreakpoint(
                format!("Breakpoint {} not found", id)
            ))
        }
    }
    
    /// List all breakpoints
    pub fn list_breakpoints(&self) -> Vec<&Breakpoint> {
        self.breakpoints.values().collect()
    }
    
    /// Check if any breakpoint matches source location
    pub fn check_source_breakpoint(&mut self, file: &str, line: u32) -> Option<usize> {
        for breakpoint in self.breakpoints.values_mut() {
            if breakpoint.matches_source(file, line) {
                breakpoint.increment_hit();
                return Some(breakpoint.id);
            }
        }
        None
    }
    
    /// Check if any breakpoint matches function
    pub fn check_function_breakpoint(&mut self, name: &str) -> Option<usize> {
        for breakpoint in self.breakpoints.values_mut() {
            if breakpoint.matches_function(name) {
                breakpoint.increment_hit();
                return Some(breakpoint.id);
            }
        }
        None
    }
    
    /// Check if any breakpoint matches address
    pub fn check_address_breakpoint(&mut self, address: u64) -> Option<usize> {
        for breakpoint in self.breakpoints.values_mut() {
            if breakpoint.matches_address(address) {
                breakpoint.increment_hit();
                return Some(breakpoint.id);
            }
        }
        None
    }
    
    /// Parse location string into breakpoint type
    fn parse_location(&self, location: &str) -> Result<BreakpointType, DebuggerError> {
        // Try to parse as file:line
        if let Some((file, line_str)) = location.split_once(':') {
            if let Ok(line) = line_str.parse::<u32>() {
                return Ok(BreakpointType::Source {
                    file: file.to_string(),
                    line,
                });
            }
        }
        
        // Try to parse as hex address
        if location.starts_with("0x") {
            if let Ok(address) = u64::from_str_radix(&location[2..], 16) {
                return Ok(BreakpointType::Address { address });
            }
        }
        
        // Try to parse as decimal address
        if let Ok(address) = location.parse::<u64>() {
            return Ok(BreakpointType::Address { address });
        }
        
        // Assume it's a function name
        Ok(BreakpointType::Function {
            name: location.to_string(),
        })
    }
}