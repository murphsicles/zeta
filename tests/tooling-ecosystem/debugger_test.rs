//! Debugger support tests

#[cfg(test)]
mod tests {
    use zetac::debugger::*;
    use zetac::debugger::debug_info::*;

    /// Test debugger session creation
    #[test]
    fn test_debugger_session() {
        let session = DebuggerSession::new();
        
        match session.state() {
            DebuggerState::Stopped => assert!(true, "Debugger starts in stopped state"),
            _ => panic!("Debugger should start in stopped state"),
        }
    }

    /// Test breakpoint management
    #[test]
    fn test_breakpoints() {
        let mut session = DebuggerSession::new();
        
        // Test setting breakpoints
        let bp1 = session.set_breakpoint("main.rs:10").unwrap();
        let bp2 = session.set_breakpoint("0x1234").unwrap();
        let bp3 = session.set_breakpoint("some_function").unwrap();
        
        assert_eq!(bp1, 1);
        assert_eq!(bp2, 2);
        assert_eq!(bp3, 3);
        
        // Test listing breakpoints
        let breakpoints = session.list_breakpoints();
        assert_eq!(breakpoints.len(), 3);
        
        // Test removing breakpoint
        session.remove_breakpoint(2).unwrap();
        let breakpoints = session.list_breakpoints();
        assert_eq!(breakpoints.len(), 2);
    }

    /// Test debug information
    #[test]
    fn test_debug_info() {
        let mut debug_info = DebugInfo::new("test_unit");
        
        // Add source file
        debug_info.add_source_file("src/main.z");
        
        // Create function info
        let func_info = FunctionInfo {
            name: "main".to_string(),
            return_type: "i32".to_string(),
            parameters: vec![],
            locals: vec![],
            definition: SourceLocation {
                file: "src/main.z".to_string(),
                line: 1,
                column: 1,
            },
            instruction_map: std::collections::HashMap::new(),
        };
        
        debug_info.add_function(func_info);
        
        // Test serialization
        let json = debug_info.to_json().unwrap();
        let parsed = DebugInfo::from_json(&json).unwrap();
        
        assert_eq!(parsed.unit_name, "test_unit");
        assert_eq!(parsed.source_files.len(), 1);
        assert!(parsed.functions.contains_key("main"));
    }

    /// Test variable inspector
    #[test]
    fn test_variable_inspector() {
        let inspector = VariableInspector::new();
        
        // Test evaluation of literals
        let int_val = inspector.evaluate("42").unwrap();
        let float_val = inspector.evaluate("3.14").unwrap();
        let bool_val = inspector.evaluate("true").unwrap();
        let string_val = inspector.evaluate("\"hello\"").unwrap();
        
        match int_val {
            VariableValue::Integer(42) => assert!(true),
            _ => panic!("Expected Integer(42)"),
        }
        
        match float_val {
            VariableValue::Float(f) => assert!((f - 3.14).abs() < 0.001),
            _ => panic!("Expected Float(3.14)"),
        }
        
        match bool_val {
            VariableValue::Boolean(true) => assert!(true),
            _ => panic!("Expected Boolean(true)"),
        }
        
        match string_val {
            VariableValue::String(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected String(\"hello\")"),
        }
    }

    /// Test debugger state transitions
    #[test]
    fn test_debugger_state_transitions() {
        let mut session = DebuggerSession::new();
        
        // Start debugging
        session.start().unwrap();
        match session.state() {
            DebuggerState::Running => assert!(true),
            _ => panic!("Should be in Running state"),
        }
        
        // Step into
        session.step_into().unwrap();
        match session.state() {
            DebuggerState::Stepping { step_type: StepType::Into } => assert!(true),
            _ => panic!("Should be in Stepping state"),
        }
        
        // Continue
        session.continue_execution().unwrap();
        match session.state() {
            DebuggerState::Running => assert!(true),
            _ => panic!("Should be in Running state"),
        }
        
        // Stop
        session.stop().unwrap();
        match session.state() {
            DebuggerState::Stopped => assert!(true),
            _ => panic!("Should be in Stopped state"),
        }
    }
}