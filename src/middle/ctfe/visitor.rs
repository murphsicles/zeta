//! AST Visitor pattern for CTFE transformations

use super::error::CtfeResult;
use crate::frontend::ast::AstNode;

/// Visitor trait for AST traversal
pub trait AstVisitor {
    /// Visit a node (can be overridden for custom traversal)
    fn visit(&mut self, node: &AstNode) -> CtfeResult<()> {
        self.visit_children(node)
    }

    /// Visit all children of a node
    fn visit_children(&mut self, node: &AstNode) -> CtfeResult<()> {
        match node {
            // Program nodes
            AstNode::Program(nodes) => {
                for child in nodes {
                    self.visit(child)?;
                }
            }
            // Function definition
            AstNode::FuncDef {
                params,
                body,
                ret_expr,
                ..
            } => {
                // Visit parameter types
                for (_, param_type) in params {
                    self.visit_str(param_type)?;
                }
                // Visit body statements
                for stmt in body {
                    self.visit(stmt)?;
                }
                // Visit return expression if present
                if let Some(expr) = ret_expr {
                    self.visit(expr)?;
                }
            }
            // Constant definition
            AstNode::ConstDef { value, .. } => {
                self.visit(value)?;
            }
            // Let binding
            AstNode::Let { pattern, expr, .. } => {
                self.visit(pattern)?;
                self.visit(expr)?;
            }
            // Assignment
            AstNode::Assign(left, right) => {
                self.visit(left)?;
                self.visit(right)?;
            }
            // Compound assignment
            AstNode::AssignOp { target, value, .. } => {
                self.visit(target)?;
                self.visit(value)?;
            }
            // If statement
            AstNode::If { cond, then, else_ } => {
                self.visit(cond)?;
                for stmt in then {
                    self.visit(stmt)?;
                }
                for stmt in else_ {
                    self.visit(stmt)?;
                }
            }
            // While loop
            AstNode::While { cond, body } => {
                self.visit(cond)?;
                for stmt in body {
                    self.visit(stmt)?;
                }
            }
            // For loop
            AstNode::For { pattern, expr, body } => {
                self.visit(pattern)?;
                self.visit(expr)?;
                for stmt in body {
                    self.visit(stmt)?;
                }
            }
            // Loop
            AstNode::Loop { body } => {
                for stmt in body {
                    self.visit(stmt)?;
                }
            }
            // Block
            AstNode::Block { body } => {
                for stmt in body {
                    self.visit(stmt)?;
                }
            }
            // Function/method call
            AstNode::Call {
                receiver,
                args,
                ..
            } => {
                if let Some(receiver) = receiver {
                    self.visit(receiver)?;
                }
                for arg in args {
                    self.visit(arg)?;
                }
            }
            // Binary operation
            AstNode::BinaryOp { left, right, .. } => {
                self.visit(left)?;
                self.visit(right)?;
            }
            // Unary operation
            AstNode::UnaryOp { expr, .. } => {
                self.visit(expr)?;
            }
            // Array literal
            AstNode::ArrayLit(elements) => {
                for elem in elements {
                    self.visit(elem)?;
                }
            }
            // Return statement
            AstNode::Return(expr) => {
                self.visit(expr)?;
            }
            // Break statement
            AstNode::Break(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.visit(expr)?;
                }
            }
            // Continue statement
            AstNode::Continue(expr_opt) => {
                if let Some(expr) = expr_opt {
                    self.visit(expr)?;
                }
            }
            // Expression statement
            AstNode::ExprStmt { expr } => {
                self.visit(expr)?;
            }
            // Subscript access (array indexing)
            AstNode::Subscript { base, index } => {
                self.visit(base)?;
                self.visit(index)?;
            }
            // Array repeat literal: [value; size]
            AstNode::ArrayRepeat { value, size } => {
                self.visit(value)?;
                self.visit(size)?;
            }
            // Leaf nodes with no children
            AstNode::Lit(_)
            | AstNode::Bool(_)
            | AstNode::Var(_)
            | AstNode::FloatLit(_)
            | AstNode::StringLit(_)
            | AstNode::Ignore => {}
            // Other nodes (not yet implemented for CTFE)
            _ => {}
        }
        Ok(())
    }

    /// Visit a string (for type names)
    fn visit_str(&mut self, _s: &str) -> CtfeResult<()> {
        Ok(())
    }
}

/// Transformer trait for AST transformation
pub trait AstTransformer {
    /// Transform a node, returning a new node
    fn transform(&mut self, node: AstNode) -> CtfeResult<AstNode>;
}