//! Integration between frontend macro expander and middle macro system

use crate::frontend::macro_expand::{MacroExpander as FrontendExpander, DeclarativeMacro};
use crate::frontend::ast::AstNode;
use super::expander::MacroExpander as MiddleExpander;
use super::registry::MacroRegistry;
use super::hygiene::HygieneContext;

/// Bridge between frontend and middle macro systems
pub struct MacroSystemBridge {
    /// Frontend expander (for built-in macros)
    frontend: FrontendExpander,
    /// Middle expander (for user-defined macros)
    middle: MiddleExpander,
    /// Whether to use hygiene
    use_hygiene: bool,
}

impl MacroSystemBridge {
    /// Create a new bridge
    pub fn new() -> Self {
        Self {
            frontend: FrontendExpander::new(),
            middle: MiddleExpander::new(),
            use_hygiene: true,
        }
    }
    
    /// Create a bridge with custom settings
    pub fn with_hygiene(use_hygiene: bool) -> Self {
        Self {
            frontend: FrontendExpander::new(),
            middle: MacroExpander::new(),
            use_hygiene,
        }
    }
    
    /// Register a declarative macro from frontend format
    pub fn register_declarative_macro(&mut self, macro_def: DeclarativeMacro) {
        // Convert frontend macro to middle format
        // For now, register a simple placeholder
        // TODO: Convert pattern/expansion tokens to AST
        
        println!("[MACRO BRIDGE] Registered declarative macro: {}", macro_def.name);
    }
    
    /// Expand macros in AST
    pub fn expand(&mut self, ast: &AstNode) -> AstNode {
        // First try frontend expander (for built-in macros)
        // Then use middle expander (for user-defined macros)
        
        // For now, use middle expander
        self.middle.expand(ast)
    }
    
    /// Expand a list of AST nodes
    pub fn expand_all(&mut self, asts: &[AstNode]) -> Vec<AstNode> {
        asts.iter()
            .map(|ast| self.expand(ast))
            .collect()
    }
    
    /// Get reference to middle expander
    pub fn middle_expander(&self) -> &MiddleExpander {
        &self.middle
    }
    
    /// Get mutable reference to middle expander
    pub fn middle_expander_mut(&mut self) -> &mut MiddleExpander {
        &mut self.middle
    }
    
    /// Get reference to frontend expander
    pub fn frontend_expander(&self) -> &FrontendExpander {
        &self.frontend
    }
    
    /// Enable or disable hygiene
    pub fn set_hygiene(&mut self, enabled: bool) {
        self.use_hygiene = enabled;
    }
    
    /// Check if hygiene is enabled
    pub fn hygiene_enabled(&self) -> bool {
        self.use_hygiene
    }
}

/// Macro expansion pass for the compiler pipeline
pub struct MacroExpansionPass {
    bridge: MacroSystemBridge,
}

impl MacroExpansionPass {
    /// Create a new macro expansion pass
    pub fn new() -> Self {
        Self {
            bridge: MacroSystemBridge::new(),
        }
    }
    
    /// Run macro expansion on AST
    pub fn run(&mut self, asts: &[AstNode]) -> Vec<AstNode> {
        println!("[MACRO PASS] Running macro expansion on {} AST nodes", asts.len());
        
        let expanded = self.bridge.expand_all(asts);
        
        println!("[MACRO PASS] Expansion complete");
        expanded
    }
    
    /// Get the bridge for configuration
    pub fn bridge(&self) -> &MacroSystemBridge {
        &self.bridge
    }
    
    /// Get mutable bridge for configuration
    pub fn bridge_mut(&mut self) -> &mut MacroSystemBridge {
        &mut self.bridge
    }
}

impl Default for MacroSystemBridge {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for MacroExpansionPass {
    fn default() -> Self {
        Self::new()
    }
}