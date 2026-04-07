//! Coordination Protocols Between Components
//!
//! Defines the communication protocols and interfaces between
//! parser (LEX), type checker (SEM), codegen (GEN), and integration (SYN).

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use super::generic_integration::{
    GenericFunction, GenericIntegration, GenericParam, IntegrationError,
};
use crate::frontend::ast::AstNode;
use crate::middle::types::Type;

/// Message types for inter-component communication
#[derive(Debug, Clone)]
pub enum CoordinationMessage {
    /// Parser -> Type Checker: AST with enhanced generic information
    ParsedAst {
        ast: Arc<AstNode>,
        generic_params: Vec<GenericParam>,
        type_context: HashMap<String, Type>,
    },

    /// Type Checker -> Integration: Type checking results
    TypeCheckResult {
        ast_id: String,
        inferred_types: HashMap<String, Type>,
        constraints: Vec<String>,
        errors: Vec<String>,
    },

    /// Integration -> Codegen: Request for monomorphization
    MonomorphizationRequest {
        generic_fn: Arc<GenericFunction>,
        type_args: Vec<Type>,
        location: String,
    },

    /// Codegen -> Integration: Monomorphization result
    MonomorphizationResult {
        request_id: String,
        concrete_fn: String, // Identifier for concrete function
        success: bool,
        errors: Vec<String>,
    },

    /// Error reporting
    ErrorReport {
        component: String, // "parser", "typechecker", "codegen"
        error: IntegrationError,
        location: String,
    },

    /// Status update
    StatusUpdate {
        component: String,
        status: ComponentStatus,
        message: String,
    },
}

/// Component status
#[derive(Debug, Clone, PartialEq)]
pub enum ComponentStatus {
    Ready,
    Processing,
    Waiting,
    Error,
    Complete,
}

/// Coordination manager for component communication
pub struct CoordinationManager {
    /// Message queue
    messages: Arc<Mutex<Vec<CoordinationMessage>>>,

    /// Component status tracking
    component_status: Arc<Mutex<HashMap<String, ComponentStatus>>>,

    /// Integration bridge
    integration: Arc<Mutex<GenericIntegration>>,

    /// Callback registry (reserved for future use)
    #[allow(dead_code)]
    callbacks: HashMap<String, Box<dyn Fn(CoordinationMessage) + Send + Sync>>,
}

impl Default for CoordinationManager {
    fn default() -> Self {
        Self::new()
    }
}

impl CoordinationManager {
    /// Create a new coordination manager
    pub fn new() -> Self {
        Self {
            messages: Arc::new(Mutex::new(Vec::new())),
            component_status: Arc::new(Mutex::new(HashMap::new())),
            integration: Arc::new(Mutex::new(GenericIntegration::new())),
            callbacks: HashMap::new(),
        }
    }

    /// Register a component
    pub fn register_component(&mut self, name: &str) {
        let mut status = self.component_status.lock().unwrap();
        status.insert(name.to_string(), ComponentStatus::Ready);
    }

    /// Update component status
    pub fn update_status(&mut self, component: &str, status: ComponentStatus, message: &str) {
        let mut status_map = self.component_status.lock().unwrap();
        status_map.insert(component.to_string(), status.clone());

        // Send status update message
        self.send_message(CoordinationMessage::StatusUpdate {
            component: component.to_string(),
            status,
            message: message.to_string(),
        });
    }

    /// Send a message to the coordination system
    pub fn send_message(&self, message: CoordinationMessage) {
        let mut messages = self.messages.lock().unwrap();
        messages.push(message);

        // Process messages (in a real system, this would be async)
        self.process_messages();
    }

    /// Process pending messages
    fn process_messages(&self) {
        let mut messages = self.messages.lock().unwrap();
        let messages_to_process = messages.drain(..).collect::<Vec<_>>();

        for message in messages_to_process {
            self.handle_message(message);
        }
    }

    /// Handle a coordination message
    fn handle_message(&self, message: CoordinationMessage) {
        match message {
            CoordinationMessage::ParsedAst {
                ast,
                generic_params,
                type_context,
            } => {
                self.handle_parsed_ast(ast, generic_params, type_context);
            }
            CoordinationMessage::TypeCheckResult {
                ast_id,
                inferred_types,
                constraints,
                errors,
            } => {
                self.handle_type_check_result(ast_id, inferred_types, constraints, errors);
            }
            CoordinationMessage::MonomorphizationRequest {
                generic_fn,
                type_args,
                location,
            } => {
                self.handle_monomorphization_request(generic_fn, type_args, location);
            }
            CoordinationMessage::MonomorphizationResult {
                request_id,
                concrete_fn,
                success,
                errors,
            } => {
                self.handle_monomorphization_result(request_id, concrete_fn, success, errors);
            }
            CoordinationMessage::ErrorReport {
                component,
                error,
                location,
            } => {
                self.handle_error_report(component, error, location);
            }
            CoordinationMessage::StatusUpdate {
                component,
                status,
                message,
            } => {
                self.handle_status_update(component, status, message);
            }
        }
    }

    /// Handle parsed AST from parser
    fn handle_parsed_ast(
        &self,
        ast: Arc<AstNode>,
        generic_params: Vec<GenericParam>,
        type_context: HashMap<String, Type>,
    ) {
        // Update integration with parsed information
        let mut integration = self.integration.lock().unwrap();

        // Process based on AST type
        match &*ast {
            AstNode::FuncDef { name, .. } => {
                // Register generic function
                // This is simplified - real implementation would extract more info
                let generic_fn = GenericFunction {
                    name: name.clone(),
                    generic_params,
                    param_types: Vec::new(), // Would extract from AST
                    return_type: Type::Named("()".to_string(), Vec::new()), // Placeholder
                    body: ast.clone(),
                    instantiations: HashMap::new(),
                };

                integration.register_generic_function(generic_fn);

                // Send to type checker
                self.send_message(CoordinationMessage::StatusUpdate {
                    component: "integration".to_string(),
                    status: ComponentStatus::Processing,
                    message: format!("Registered generic function: {}", name),
                });
            }
            _ => {
                // Handle other AST node types
            }
        }
    }

    /// Handle type check results
    fn handle_type_check_result(
        &self,
        ast_id: String,
        inferred_types: HashMap<String, Type>,
        constraints: Vec<String>,
        errors: Vec<String>,
    ) {
        // Process type checking results
        let mut integration = self.integration.lock().unwrap();

        if !errors.is_empty() {
            for error in errors {
                integration.add_error(IntegrationError::TypeError(error));
            }
        }

        // Update integration with inferred types
        // (Implementation would update type context with inferred types)

        self.send_message(CoordinationMessage::StatusUpdate {
            component: "integration".to_string(),
            status: ComponentStatus::Processing,
            message: format!("Processed type check results for {}", ast_id),
        });
    }

    /// Handle monomorphization request
    fn handle_monomorphization_request(
        &self,
        generic_fn: Arc<GenericFunction>,
        type_args: Vec<Type>,
        location: String,
    ) {
        let mut integration = self.integration.lock().unwrap();

        match integration.get_concrete_function(&generic_fn.name, &type_args) {
            Ok(concrete_fn) => {
                // Send to codegen for actual code generation
                self.send_message(CoordinationMessage::StatusUpdate {
                    component: "integration".to_string(),
                    status: ComponentStatus::Processing,
                    message: format!("Monomorphized {} with {:?}", generic_fn.name, type_args),
                });

                // In real implementation, would send MonomorphizationResult back
            }
            Err(e) => {
                integration.add_error(e);

                self.send_message(CoordinationMessage::ErrorReport {
                    component: "integration".to_string(),
                    error: IntegrationError::MonomorphizationError(format!(
                        "Failed to monomorphize {}: {:?}",
                        generic_fn.name, type_args
                    )),
                    location,
                });
            }
        }
    }

    /// Handle monomorphization result
    fn handle_monomorphization_result(
        &self,
        request_id: String,
        concrete_fn: String,
        success: bool,
        errors: Vec<String>,
    ) {
        // Process codegen results
        if !success {
            let mut integration = self.integration.lock().unwrap();
            for error in errors {
                integration.add_error(IntegrationError::MonomorphizationError(error));
            }
        }

        self.send_message(CoordinationMessage::StatusUpdate {
            component: "integration".to_string(),
            status: if success {
                ComponentStatus::Complete
            } else {
                ComponentStatus::Error
            },
            message: format!("Monomorphization {}: {}", request_id, concrete_fn),
        });
    }

    /// Handle error report
    fn handle_error_report(&self, component: String, error: IntegrationError, location: String) {
        let mut integration = self.integration.lock().unwrap();
        integration.add_error(error);

        // Update component status
        let mut status_map = self.component_status.lock().unwrap();
        status_map.insert(component.clone(), ComponentStatus::Error);

        self.send_message(CoordinationMessage::StatusUpdate {
            component: "integration".to_string(),
            status: ComponentStatus::Error,
            message: format!(
                "Error from {} at {}: {:?}",
                component,
                location,
                integration.get_errors().last()
            ),
        });
    }

    /// Handle status update
    fn handle_status_update(&self, component: String, status: ComponentStatus, message: String) {
        // Log status update
        println!("[COORDINATION] {}: {:?} - {}", component, status, message);

        // Update component status
        let mut status_map = self.component_status.lock().unwrap();
        status_map.insert(component, status);
    }

    /// Get integration errors
    pub fn get_errors(&self) -> Vec<IntegrationError> {
        let integration = self.integration.lock().unwrap();
        integration.get_errors()
    }

    /// Check if all components are ready
    pub fn all_components_ready(&self) -> bool {
        let status_map = self.component_status.lock().unwrap();
        status_map.values().all(|s| *s == ComponentStatus::Ready)
    }

    /// Check if any component has errors
    pub fn has_errors(&self) -> bool {
        let status_map = self.component_status.lock().unwrap();
        status_map.values().any(|s| *s == ComponentStatus::Error)
    }
}

/// Protocol definitions for each component
pub mod protocols {
    use super::*;

    /// Parser (LEX) protocol
    pub mod parser {
        use super::*;

        /// Send parsed AST to coordination system
        pub fn send_parsed_ast(
            manager: &CoordinationManager,
            ast: Arc<AstNode>,
            generic_params: Vec<GenericParam>,
            type_context: HashMap<String, Type>,
        ) {
            manager.send_message(CoordinationMessage::ParsedAst {
                ast,
                generic_params,
                type_context,
            });
        }

        /// Report parser error
        pub fn report_error(manager: &CoordinationManager, error: String, location: String) {
            manager.send_message(CoordinationMessage::ErrorReport {
                component: "parser".to_string(),
                error: IntegrationError::ParseError(error),
                location,
            });
        }
    }

    /// Type Checker (SEM) protocol
    pub mod type_checker {
        use super::*;

        /// Send type checking results
        pub fn send_type_check_result(
            manager: &CoordinationManager,
            ast_id: String,
            inferred_types: HashMap<String, Type>,
            constraints: Vec<String>,
            errors: Vec<String>,
        ) {
            manager.send_message(CoordinationMessage::TypeCheckResult {
                ast_id,
                inferred_types,
                constraints,
                errors,
            });
        }

        /// Report type checking error
        pub fn report_error(manager: &CoordinationManager, error: String, location: String) {
            manager.send_message(CoordinationMessage::ErrorReport {
                component: "type_checker".to_string(),
                error: IntegrationError::TypeError(error),
                location,
            });
        }
    }

    /// Codegen (GEN) protocol
    pub mod codegen {
        use super::*;

        /// Request monomorphization
        pub fn request_monomorphization(
            manager: &CoordinationManager,
            generic_fn: Arc<GenericFunction>,
            type_args: Vec<Type>,
            location: String,
        ) {
            manager.send_message(CoordinationMessage::MonomorphizationRequest {
                generic_fn,
                type_args,
                location,
            });
        }

        /// Send monomorphization results
        pub fn send_monomorphization_result(
            manager: &CoordinationManager,
            request_id: String,
            concrete_fn: String,
            success: bool,
            errors: Vec<String>,
        ) {
            manager.send_message(CoordinationMessage::MonomorphizationResult {
                request_id,
                concrete_fn,
                success,
                errors,
            });
        }

        /// Report codegen error
        pub fn report_error(manager: &CoordinationManager, error: String, location: String) {
            manager.send_message(CoordinationMessage::ErrorReport {
                component: "codegen".to_string(),
                error: IntegrationError::MonomorphizationError(error),
                location,
            });
        }
    }

    /// Integration (SYN) protocol
    pub mod integration {
        use super::*;

        /// Get integration bridge
        pub fn get_integration(manager: &CoordinationManager) -> Arc<Mutex<GenericIntegration>> {
            manager.integration.clone()
        }

        /// Send status update
        pub fn send_status(manager: &CoordinationManager, status: ComponentStatus, message: &str) {
            manager.send_message(CoordinationMessage::StatusUpdate {
                component: "integration".to_string(),
                status,
                message: message.to_string(),
            });
        }

        /// Check for errors
        pub fn check_errors(manager: &CoordinationManager) -> Vec<IntegrationError> {
            manager.get_errors()
        }
    }
}

/// Test utilities for coordination
#[cfg(test)]
pub mod test_utils {
    use super::*;

    /// Create a test coordination manager with all components registered
    pub fn create_test_manager() -> CoordinationManager {
        let mut manager = CoordinationManager::new();

        manager.register_component("parser");
        manager.register_component("type_checker");
        manager.register_component("codegen");
        manager.register_component("integration");

        manager
    }

    /// Simulate a full compilation pipeline
    pub fn simulate_compilation_pipeline(manager: &CoordinationManager) {
        // Simulate parser sending AST
        let ast = Arc::new(AstNode::Program(vec![]));
        let generic_params = vec![GenericParam::Type {
            name: "T".to_string(),
            bounds: vec![],
        }];

        protocols::parser::send_parsed_ast(manager, ast, generic_params, HashMap::new());

        // Simulate type checker results
        let mut inferred_types = HashMap::new();
        inferred_types.insert("x".to_string(), Type::I32);

        protocols::type_checker::send_type_check_result(
            manager,
            "test_ast".to_string(),
            inferred_types,
            vec!["T = i32".to_string()],
            vec![],
        );

        // Simulate integration status
        protocols::integration::send_status(
            manager,
            ComponentStatus::Complete,
            "Test compilation complete",
        );
    }
}
