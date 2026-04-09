//! Debugger support for Zeta
//!
//! This module provides debug information generation, breakpoint support,
//! step debugging, and variable inspection capabilities.

mod debug_info;
mod breakpoints;
mod inspector;

pub use debug_info::DebugInfo;
pub use breakpoints::{Breakpoint, BreakpointManager};
pub use inspector::{VariableInspector, VariableValue};

/// Debugger error types
#[derive(Debug)]
pub enum DebuggerError {
    /// Invalid breakpoint location
    InvalidBreakpoint(String),
    /// Debug information not available
    NoDebugInfo(String),
    /// Execution control error
    ExecutionControl(String),
    /// Variable inspection error
    InspectionError(String),
}

impl std::fmt::Display for DebuggerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DebuggerError::InvalidBreakpoint(msg) => write!(f, "Invalid breakpoint: {}", msg),
            DebuggerError::NoDebugInfo(msg) => write!(f, "No debug information: {}", msg),
            DebuggerError::ExecutionControl(msg) => write!(f, "Execution control error: {}", msg),
            DebuggerError::InspectionError(msg) => write!(f, "Inspection error: {}", msg),
        }
    }
}

impl std::error::Error for DebuggerError {}

/// Debugger result type
pub type DebuggerResult<T> = Result<T, DebuggerError>;

/// Debugger session state
#[derive(Debug, Clone)]
pub enum DebuggerState {
    /// Debugger is running
    Running,
    /// Debugger is paused at a breakpoint
    Paused {
        /// Breakpoint that caused the pause
        breakpoint_id: usize,
        /// Program counter location
        pc: u64,
    },
    /// Debugger is stepping
    Stepping {
        /// Step type (into, over, out)
        step_type: StepType,
    },
    /// Debugger is stopped
    Stopped,
}

/// Step type for debugger
#[derive(Debug, Clone, Copy)]
pub enum StepType {
    /// Step into functions
    Into,
    /// Step over functions
    Over,
    /// Step out of current function
    Out,
}

/// Debugger session
pub struct DebuggerSession {
    /// Current debugger state
    state: DebuggerState,
    /// Breakpoint manager
    breakpoints: BreakpointManager,
    /// Debug information
    debug_info: Option<DebugInfo>,
    /// Variable inspector
    inspector: VariableInspector,
}

impl DebuggerSession {
    /// Create a new debugger session
    pub fn new() -> Self {
        Self {
            state: DebuggerState::Stopped,
            breakpoints: BreakpointManager::new(),
            debug_info: None,
            inspector: VariableInspector::new(),
        }
    }
    
    /// Load debug information
    pub fn load_debug_info(&mut self, debug_info: DebugInfo) {
        self.debug_info = Some(debug_info);
    }
    
    /// Set a breakpoint
    pub fn set_breakpoint(&mut self, location: &str) -> DebuggerResult<usize> {
        self.breakpoints.add_breakpoint(location)
    }
    
    /// Remove a breakpoint
    pub fn remove_breakpoint(&mut self, breakpoint_id: usize) -> DebuggerResult<()> {
        self.breakpoints.remove_breakpoint(breakpoint_id)
    }
    
    /// List all breakpoints
    pub fn list_breakpoints(&self) -> Vec<&Breakpoint> {
        self.breakpoints.list_breakpoints()
    }
    
    /// Start debugging
    pub fn start(&mut self) -> DebuggerResult<()> {
        self.state = DebuggerState::Running;
        Ok(())
    }
    
    /// Stop debugging
    pub fn stop(&mut self) -> DebuggerResult<()> {
        self.state = DebuggerState::Stopped;
        Ok(())
    }
    
    /// Continue execution
    pub fn continue_execution(&mut self) -> DebuggerResult<()> {
        match self.state {
            DebuggerState::Paused { .. } | DebuggerState::Stepping { .. } => {
                self.state = DebuggerState::Running;
                Ok(())
            }
            _ => Err(DebuggerError::ExecutionControl(
                "Cannot continue from current state".to_string(),
            )),
        }
    }
    
    /// Step into
    pub fn step_into(&mut self) -> DebuggerResult<()> {
        self.state = DebuggerState::Stepping {
            step_type: StepType::Into,
        };
        Ok(())
    }
    
    /// Step over
    pub fn step_over(&mut self) -> DebuggerResult<()> {
        self.state = DebuggerState::Stepping {
            step_type: StepType::Over,
        };
        Ok(())
    }
    
    /// Step out
    pub fn step_out(&mut self) -> DebuggerResult<()> {
        self.state = DebuggerState::Stepping {
            step_type: StepType::Out,
        };
        Ok(())
    }
    
    /// Get current state
    pub fn state(&self) -> &DebuggerState {
        &self.state
    }
    
    /// Inspect variable
    pub fn inspect_variable(&self, name: &str) -> DebuggerResult<String> {
        self.inspector.inspect(name)
    }
    
    /// Get call stack
    pub fn call_stack(&self) -> DebuggerResult<Vec<String>> {
        // This would be implemented with actual stack trace
        Ok(vec![])
    }
}