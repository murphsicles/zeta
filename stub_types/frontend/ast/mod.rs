/// Stub AstNode type for v0.5.0 compilation
#[derive(Debug, Clone)]
pub enum AstNode {
    /// Placeholder variant
    Placeholder,
    /// Unit variant
    Unit,
}

impl AstNode {
    /// Create a new placeholder AstNode
    pub fn new() -> Self {
        AstNode::Placeholder
    }
}

impl Default for AstNode {
    fn default() -> Self {
        Self::new()
    }
}