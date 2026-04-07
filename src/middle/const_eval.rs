// src/middle/const_eval.rs
//! Compile-Time Function Evaluation (CTFE) for Zeta v0.5.0
//!
//! Evaluates const functions at compile time.

use crate::frontend::ast::AstNode;
use std::collections::HashMap;
use std::fmt;

/// Value that can be computed at compile time
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConstValue {
    /// Integer literal
    Int(i64),
    /// Array of compile-time values
    Array(Vec<ConstValue>),
    /// Boolean value
    Bool(bool),
}

impl ConstValue {
    /// Convert to i64 if this is an integer
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            ConstValue::Int(n) => Some(*n),
            _ => None,
        }
    }
    
    /// Convert to bool if this is a boolean
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            ConstValue::Bool(b) => Some(*b),
            _ => None,
        }
    }
    
    /// Get array length if this is an array
    pub fn array_len(&self) -> Option<usize> {
        match self {
            ConstValue::Array(arr) => Some(arr.len()),
            _ => None,
        }
    }
}

impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstValue::Int(n) => write!(f, "{}", n),
            ConstValue::Bool(b) => write!(f, "{}", b),
            ConstValue::Array(arr) => {
                write!(f, "[")?;
                for (i, elem) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
        }
    }
}

/// Constant evaluator for compile-time evaluation
pub struct ConstEvaluator {
    /// Symbol table for variables in current scope
    symbols: HashMap<String, ConstValue>,
    /// Stack of symbol tables for nested scopes
    symbol_stack: Vec<HashMap<String, ConstValue>>,
    /// Function definitions for const/comptime functions
    functions: HashMap<String, AstNode>,
}

impl Default for ConstEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstEvaluator {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            symbol_stack: Vec::new(),
            functions: HashMap::new(),
        }
    }
    
    /// Register a function definition
    pub fn register_function(&mut self, name: String, func: AstNode) {
        self.functions.insert(name, func);
    }

    /// Enter a new scope
    fn enter_scope(&mut self) {
        let current_symbols = std::mem::take(&mut self.symbols);
        self.symbol_stack.push(current_symbols);
        self.symbols = HashMap::new();
    }

    /// Exit current scope
    fn exit_scope(&mut self) {
        if let Some(prev_symbols) = self.symbol_stack.pop() {
            self.symbols = prev_symbols;
        }
    }

    /// Look up a variable in current or parent scopes
    fn lookup_variable(&self, name: &str) -> Option<ConstValue> {
        // Check current scope first
        if let Some(value) = self.symbols.get(name) {
            return Some(value.clone());
        }
        
        // Check parent scopes
        for scope in self.symbol_stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        
        None
    }

    /// Define a variable in current scope
    fn define_variable(&mut self, name: String, value: ConstValue) {
        self.symbols.insert(name, value);
    }

    /// Set a variable in current or parent scope
    fn set_variable(&mut self, name: String, value: ConstValue) -> bool {
        // Check current scope first
        if self.symbols.contains_key(&name) {
            self.symbols.insert(name, value);
            return true;
        }
        
        // Check parent scopes
        for scope in self.symbol_stack.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name, value);
                return true;
            }
        }
        
        false
    }

    /// Evaluate a block of statements
    fn eval_block(&mut self, statements: &[AstNode]) -> Result<ConstValue, String> {
        let mut last_value = ConstValue::Int(0);
        
        for stmt in statements {
            last_value = self.eval_const_expr(stmt)?;
        }
        
        Ok(last_value)
    }

    /// Evaluate a const expression to a ConstValue
    pub fn eval_const_expr(&mut self, expr: &AstNode) -> Result<ConstValue, String> {
        // Check cache first (but not for Var expressions since they can change)
        // Actually, disable caching for now because it causes issues with loops
        // if !matches!(expr, AstNode::Var(_)) {
        //     if let Some(value) = self.cache.get(expr) {
        //         return Ok(value.clone());
        //     }
        // }

        let value = match expr {
            AstNode::Lit(n) => Ok(ConstValue::Int(*n)),
            AstNode::BinaryOp { op, left, right } => {
                let left_val = self.eval_const_expr(left)?;
                let right_val = self.eval_const_expr(right)?;

                // For now, only support integer operations
                let left_int = left_val.as_i64().ok_or_else(|| {
                    format!("Left operand must be integer for binary operator {}, got {:?}", op, left_val)
                })?;
                let right_int = right_val.as_i64().ok_or_else(|| {
                    format!("Right operand must be integer for binary operator {}, got {:?}", op, right_val)
                })?;

                match op.as_str() {
                    "+" => Ok(ConstValue::Int(left_int + right_int)),
                    "-" => Ok(ConstValue::Int(left_int - right_int)),
                    "*" => Ok(ConstValue::Int(left_int * right_int)),
                    "/" => {
                        if right_int == 0 {
                            return Err("Division by zero in const expression".to_string());
                        }
                        Ok(ConstValue::Int(left_int / right_int))
                    }
                    "%" => {
                        if right_int == 0 {
                            return Err("Modulo by zero in const expression".to_string());
                        }
                        Ok(ConstValue::Int(left_int % right_int))
                    }
                    "<<" => Ok(ConstValue::Int(left_int << right_int)),
                    ">>" => Ok(ConstValue::Int(left_int >> right_int)),
                    "&" => Ok(ConstValue::Int(left_int & right_int)),
                    "|" => Ok(ConstValue::Int(left_int | right_int)),
                    "^" => Ok(ConstValue::Int(left_int ^ right_int)),
                    "<" => Ok(ConstValue::Bool(left_int < right_int)),
                    "<=" => Ok(ConstValue::Bool(left_int <= right_int)),
                    ">" => Ok(ConstValue::Bool(left_int > right_int)),
                    ">=" => Ok(ConstValue::Bool(left_int >= right_int)),
                    "==" => Ok(ConstValue::Bool(left_int == right_int)),
                    "!=" => Ok(ConstValue::Bool(left_int != right_int)),
                    _ => {
                        return Err(format!(
                            "Unsupported binary operator in const expression: {}",
                            op
                        ));
                    }
                }
            }
            AstNode::UnaryOp { op, expr } => {
                let val = self.eval_const_expr(expr)?;
                let int_val = val.as_i64().ok_or_else(|| {
                    format!("Operand must be integer for unary operator {}, got {:?}", op, val)
                })?;
                
                Ok(match op.as_str() {
                    "-" => ConstValue::Int(-int_val),
                    "!" => ConstValue::Int(!int_val),
                    "~" => ConstValue::Int(!int_val),
                    _ => {
                        return Err(format!(
                            "Unsupported unary operator in const expression: {}",
                            op
                        ));
                    }
                })
            }
            AstNode::Call {
                receiver,
                method,
                args,
                ..
            } => {
                // Check if it's a const function call
                if receiver.is_some() {
                    return Err("Method calls not supported in const expressions".to_string());
                }

                // For now, only support simple built-in const functions
                match method.as_str() {
                    "min" => {
                        if args.len() != 2 {
                            return Err("min() requires 2 arguments".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?.as_i64()
                            .ok_or_else(|| "min() requires integer arguments".to_string())?;
                        let b = self.eval_const_expr(&args[1])?.as_i64()
                            .ok_or_else(|| "min() requires integer arguments".to_string())?;
                        Ok(ConstValue::Int(a.min(b)))
                    }
                    "max" => {
                        if args.len() != 2 {
                            return Err("max() requires 2 arguments".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?.as_i64()
                            .ok_or_else(|| "max() requires integer arguments".to_string())?;
                        let b = self.eval_const_expr(&args[1])?.as_i64()
                            .ok_or_else(|| "max() requires integer arguments".to_string())?;
                        Ok(ConstValue::Int(a.max(b)))
                    }
                    "abs" => {
                        if args.len() != 1 {
                            return Err("abs() requires 1 argument".to_string());
                        }
                        let a = self.eval_const_expr(&args[0])?.as_i64()
                            .ok_or_else(|| "abs() requires integer argument".to_string())?;
                        Ok(ConstValue::Int(a.abs()))
                    }
                    _ => {
                        // Try to find and call a const function
                        if let Some(func_node) = self.functions.get(method) {
                            // Clone the function node so we can release the borrow
                            let func_node_clone = func_node.clone();
                            
                            // Evaluate all arguments
                            let mut arg_values = Vec::new();
                            for arg in args {
                                arg_values.push(self.eval_const_expr(arg)?);
                            }
                            
                            // Convert arg values to AST nodes (simplified - just literals for now)
                            let mut arg_ast_nodes = Vec::new();
                            for arg_val in &arg_values {
                                match arg_val {
                                    ConstValue::Int(n) => arg_ast_nodes.push(AstNode::Lit(*n)),
                                    ConstValue::Bool(b) => arg_ast_nodes.push(AstNode::Bool(*b)),
                                    ConstValue::Array(arr) => {
                                        // Convert array elements to AST nodes
                                        let mut elements = Vec::new();
                                        for elem in arr {
                                            match elem {
                                                ConstValue::Int(n) => elements.push(AstNode::Lit(*n)),
                                                ConstValue::Bool(b) => elements.push(AstNode::Bool(*b)),
                                                ConstValue::Array(_) => {
                                                    // Nested arrays not supported yet
                                                    return Err("Nested arrays in function arguments not supported".to_string());
                                                }
                                            }
                                        }
                                        arg_ast_nodes.push(AstNode::ArrayLit(elements));
                                    }
                                }
                            }
                            
                            // Call the function
                            match self.try_eval_const_call(&func_node_clone, &arg_ast_nodes) {
                                Ok(Some(value)) => Ok(value),
                                Ok(None) => Err(format!("Function {} cannot be evaluated at compile time", method)),
                                Err(e) => Err(e),
                            }
                        } else {
                            Err(format!("Function not found or not const/comptime: {}", method))
                        }
                    }
                }
            }
            AstNode::ConstDef { value, comptime_, .. } => {
                // Evaluate the constant's value
                Ok(self.eval_const_expr(value)?)
            }
            AstNode::ArrayLit(elements) => {
                // Evaluate all array elements
                let mut const_elements = Vec::new();
                for elem in elements {
                    const_elements.push(self.eval_const_expr(elem)?);
                }
                Ok(ConstValue::Array(const_elements))
            }
            AstNode::Bool(b) => Ok(ConstValue::Bool(*b)),
            AstNode::Var(name) => {
                // Look up variable in symbol table
                Ok(self.lookup_variable(name)
                    .ok_or_else(|| format!("Undefined variable in const context: {}", name))?)
            }
            AstNode::Let { mut_, pattern, ty: _, expr } => {
                // Evaluate the expression
                let value = self.eval_const_expr(expr)?;
                
                // For now, only support simple variable patterns
                match &**pattern {
                    AstNode::Var(var_name) => {
                        // Define the variable in current scope
                        self.define_variable(var_name.clone(), value.clone());
                        Ok(value)
                    }
                    _ => return Err("Complex patterns not supported in const let bindings".to_string()),
                }
            }
            AstNode::Assign(target, value_expr) => {
                // Evaluate the value
                let value = self.eval_const_expr(value_expr)?;
                
                // For now, only support simple variable assignment
                match &**target {
                    AstNode::Var(var_name) => {
                        // Update variable in symbol table
                        if !self.set_variable(var_name.clone(), value.clone()) {
                            // Variable doesn't exist, create it in current scope
                            self.define_variable(var_name.clone(), value.clone());
                        }
                        Ok(value)
                    }
                    AstNode::Subscript { base, index } => {
                        // Array element assignment
                        let array_val = self.eval_const_expr(base)?;
                        let index_val = self.eval_const_expr(index)?;
                        
                        match (array_val, index_val) {
                            (ConstValue::Array(mut arr), ConstValue::Int(idx)) => {
                                let idx = idx as usize;
                                if idx >= arr.len() {
                                    return Err(format!("Array index out of bounds: {} >= {}", idx, arr.len()));
                                }
                                arr[idx] = value.clone();
                                Ok(ConstValue::Array(arr))
                            }
                            _ => return Err("Array subscript assignment requires array and integer index".to_string()),
                        }
                    }
                    _ => return Err("Complex assignment targets not supported in const context".to_string()),
                }
            }
            AstNode::Subscript { base, index } => {
                // Array indexing
                let array_val = self.eval_const_expr(base)?;
                let index_val = self.eval_const_expr(index)?;
                
                match (array_val, index_val) {
                    (ConstValue::Array(arr), ConstValue::Int(idx)) => {
                        let idx = idx as usize;
                        if idx >= arr.len() {
                            return Err(format!("Array index out of bounds: {} >= {}", idx, arr.len()));
                        }
                        Ok(arr[idx].clone())
                    }
                    _ => return Err("Array subscript requires array and integer index".to_string()),
                }
            }
            AstNode::If { cond, then, else_ } => {
                // Evaluate condition
                let cond_val = self.eval_const_expr(cond)?;
                let cond_bool = cond_val.as_bool()
                    .ok_or_else(|| "If condition must evaluate to boolean".to_string())?;
                
                Ok(if cond_bool {
                    // Evaluate then branch
                    self.enter_scope();
                    let result = self.eval_block(then)?;
                    self.exit_scope();
                    result
                } else {
                    // Evaluate else branch
                    self.enter_scope();
                    let result = self.eval_block(else_)?;
                    self.exit_scope();
                    result
                })
            }
            AstNode::Block { body } => {
                self.enter_scope();
                let result = self.eval_block(body)?;
                self.exit_scope();
                Ok(result)
            }
            AstNode::ExprStmt { expr } => {
                // Evaluate the expression
                // Special case: if it's just "var", ignore it (parser artifact)
                if let AstNode::Var(name) = &**expr {
                    if name == "var" {
                        return Ok(ConstValue::Int(0));
                    }
                }
                Ok(self.eval_const_expr(expr)?)
            }
            AstNode::Return(expr) => {
                // Evaluate the return expression
                Ok(self.eval_const_expr(expr)?)
            }
            AstNode::For { pattern, expr, body } => {
                // Simple for loop implementation for range expressions
                // For now, only support: for i in start..end
                
                // Try to get range bounds
                // This is a simplification - real implementation would parse range syntax
                match &**expr {
                    AstNode::BinaryOp { op, left, right } if op == ".." => {
                        let start_val = self.eval_const_expr(left)?.as_i64()
                            .ok_or_else(|| "Range start must be integer".to_string())?;
                        let end_val = self.eval_const_expr(right)?.as_i64()
                            .ok_or_else(|| "Range end must be integer".to_string())?;
                        
                        // Enter loop scope
                        self.enter_scope();
                        
                        let mut last_value = ConstValue::Int(0);
                        
                        // Simple pattern: for i in start..end
                        match &**pattern {
                            AstNode::Var(var_name) => {
                                for i in start_val..end_val {
                                    // Set loop variable
                                    self.define_variable(var_name.clone(), ConstValue::Int(i));
                                    
                                    // Evaluate loop body
                                    for stmt in body {
                                        last_value = self.eval_const_expr(stmt)?;
                                        
                                        // Check for break/continue
                                        // For now, just ignore
                                    }
                                }
                                
                                // Exit loop scope
                                self.exit_scope();
                                
                                Ok(last_value)
                            }
                            _ => Err("Complex for loop patterns not supported".to_string()),
                        }
                    }
                    _ => Err("Only simple range for loops supported in const context".to_string()),
                }
            }
            _ => {
                return Err(format!(
                    "Unsupported expression in const context: {:?}",
                    expr
                ));
            }
        };

        // Cache the result (but not for Var expressions since they can change)
        // Actually, disable caching for now because it causes issues with loops
        // if !matches!(expr, AstNode::Var(_)) {
        //     self.cache.insert(expr.clone(), value.clone());
        // }
        value
    }

    /// Check if a function can be evaluated at compile time
    pub fn is_const_function(&self, func: &AstNode) -> bool {
        match func {
            AstNode::FuncDef { const_, comptime_, .. } => *const_ || *comptime_,
            _ => false,
        }
    }

    /// Try to evaluate a const function call at compile time
    pub fn try_eval_const_call(
        &mut self,
        func: &AstNode,
        args: &[AstNode],
    ) -> Result<Option<ConstValue>, String> {
        match func {
            AstNode::FuncDef {
                const_,
                comptime_,
                params,
                body,
                ret_expr,
                ..
            } => {
                if !*const_ && !*comptime_ {
                    return Ok(None);
                }

                // Enter function scope
                self.enter_scope();
                
                // Handle function parameters
                if params.len() != args.len() {
                    return Err(format!(
                        "Function expects {} arguments, but {} were provided",
                        params.len(),
                        args.len()
                    ));
                }
                
                // Bind arguments to parameters
                for (i, (param_name, _param_type)) in params.iter().enumerate() {
                    let arg_value = self.eval_const_expr(&args[i])?;
                    self.define_variable(param_name.clone(), arg_value);
                }
                
                let result = if let Some(expr) = ret_expr {
                    // Function with explicit return expression - still need to evaluate body for side effects
                    if !body.is_empty() {
                        for stmt in body {
                            self.eval_const_expr(stmt)?;
                        }
                    }
                    self.eval_const_expr(expr).map(Some)
                } else if !body.is_empty() {
                    // Evaluate the function body
                    let mut last_value = ConstValue::Int(0);
                    for stmt in body {
                        last_value = self.eval_const_expr(stmt)?;
                        
                        // Check for return statement
                        if let AstNode::Return(expr) = stmt {
                            last_value = self.eval_const_expr(expr)?;
                            break;
                        }
                    }
                    Ok(Some(last_value))
                } else {
                    Ok(Some(ConstValue::Int(0)))
                };
                
                // Exit function scope
                self.exit_scope();
                
                result
            }
            _ => Ok(None),
        }
    }
}

/// Evaluate constant definitions at compile time
pub fn evaluate_constants(asts: &[AstNode]) -> Result<Vec<AstNode>, String> {
    let mut evaluator = ConstEvaluator::new();
    let mut result = Vec::new();

    // First, register all const/comptime functions
    for ast in asts {
        if let AstNode::FuncDef {
            name,
            const_,
            comptime_,
            ..
        } = ast {
            if *const_ || *comptime_ {
                evaluator.register_function(name.clone(), ast.clone());
            }
        }
    }

    for ast in asts {
        match ast {
            AstNode::ConstDef {
                name,
                ty,
                value,
                attrs,
                pub_,
                comptime_,
            } => {
                // Try to evaluate the constant
                match evaluator.eval_const_expr(value) {
                    Ok(ConstValue::Int(val)) => {
                        // Replace with a literal if evaluation succeeds
                        result.push(AstNode::ConstDef {
                            name: name.clone(),
                            ty: ty.clone(),
                            value: Box::new(AstNode::Lit(val)),
                            attrs: attrs.clone(),
                            pub_: *pub_,
                            comptime_: *comptime_,
                        });
                    }
                    Ok(_) => {
                        // Non-integer constant value - keep as-is for now
                        result.push(ast.clone());
                    }
                    Err(e) => {
                        // Keep as-is if evaluation fails
                        eprintln!("Warning: Could not evaluate constant {}: {}", name, e);
                        result.push(ast.clone());
                    }
                }
            }
            _ => result.push(ast.clone()),
        }
    }

    Ok(result)
}
