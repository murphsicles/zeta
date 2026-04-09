//! Refinement Types for Zeta
//!
//! Syntax: {variable: BaseType | predicate}
//! Example: {n: u64 | n > 0}

use std::fmt;

/// Refinement type
#[derive(Debug, Clone, PartialEq)]
pub struct RefinementType {
    /// Variable name
    pub variable: String,
    /// Base type
    pub base_type: BaseType,
    /// Refinement predicate
    pub predicate: Predicate,
}

/// Base types supported by refinement
#[derive(Debug, Clone, PartialEq)]
pub enum BaseType {
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 16-bit integer
    U16,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
    /// Signed 8-bit integer
    I8,
    /// Signed 16-bit integer
    I16,
    /// Signed 32-bit integer
    I32,
    /// Signed 64-bit integer
    I64,
    /// Boolean
    Bool,
    /// Array of type
    Array(Box<BaseType>),
    /// Tuple of types
    Tuple(Vec<BaseType>),
}

/// Predicate expression
#[derive(Debug, Clone, PartialEq)]
pub enum Predicate {
    /// Comparison: left op right
    Comparison {
        left: Expr,
        op: ComparisonOp,
        right: Expr,
    },
    /// Logical AND
    And(Box<Predicate>, Box<Predicate>),
    /// Logical OR
    Or(Box<Predicate>, Box<Predicate>),
    /// Logical NOT
    Not(Box<Predicate>),
    /// Forall quantifier
    Forall {
        variable: String,
        base_type: BaseType,
        body: Box<Predicate>,
    },
    /// Exists quantifier
    Exists {
        variable: String,
        base_type: BaseType,
        body: Box<Predicate>,
    },
    /// True predicate
    True,
    /// False predicate
    False,
}

/// Comparison operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ComparisonOp {
    /// Equal
    Eq,
    /// Not equal
    Ne,
    /// Less than
    Lt,
    /// Less than or equal
    Le,
    /// Greater than
    Gt,
    /// Greater than or equal
    Ge,
}

/// Expression in predicate
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Variable reference
    Var(String),
    /// Integer literal
    Int(i64),
    /// Boolean literal
    Bool(bool),
    /// Array length
    Len(Box<Expr>),
    /// Array access
    Index(Box<Expr>, Box<Expr>),
    /// Binary operation
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    /// Function application
    Apply(String, Vec<Expr>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    /// Addition
    Add,
    /// Subtraction
    Sub,
    /// Multiplication
    Mul,
    /// Division
    Div,
    /// Modulo
    Mod,
}

impl RefinementType {
    /// Parse a refinement type from string
    pub fn parse(s: &str) -> Result<Self, String> {
        // Simple parser for refinement types
        // Format: {variable: type | predicate}
        let s = s.trim();
        
        if !s.starts_with('{') || !s.ends_with('}') {
            return Err("Refinement type must be enclosed in {}".to_string());
        }
        
        let inner = &s[1..s.len()-1];
        let parts: Vec<&str> = inner.split('|').collect();
        
        if parts.len() != 2 {
            return Err("Refinement type must have format {variable: type | predicate}".to_string());
        }
        
        let type_part = parts[0].trim();
        let predicate_part = parts[1].trim();
        
        // Parse variable and type
        let colon_pos = type_part.find(':').ok_or("Missing ':' in type specification")?;
        let variable = type_part[..colon_pos].trim().to_string();
        let type_str = type_part[colon_pos+1..].trim();
        
        let base_type = BaseType::parse(type_str)?;
        let predicate = Predicate::parse(predicate_part)?;
        
        Ok(Self {
            variable,
            base_type,
            predicate,
        })
    }
    
    /// Convert to SMT-LIB2 representation
    pub fn to_smt(&self) -> String {
        format!(
            "(declare-const {} {})\n(assert {})",
            self.variable,
            self.base_type.to_smt(),
            self.predicate.to_smt()
        )
    }
}

impl BaseType {
    /// Parse base type from string
    pub fn parse(s: &str) -> Result<Self, String> {
        match s {
            "u8" => Ok(BaseType::U8),
            "u16" => Ok(BaseType::U16),
            "u32" => Ok(BaseType::U32),
            "u64" => Ok(BaseType::U64),
            "i8" => Ok(BaseType::I8),
            "i16" => Ok(BaseType::I16),
            "i32" => Ok(BaseType::I32),
            "i64" => Ok(BaseType::I64),
            "bool" => Ok(BaseType::Bool),
            _ if s.starts_with('[') && s.ends_with(']') => {
                let inner = &s[1..s.len()-1];
                let inner_type = BaseType::parse(inner)?;
                Ok(BaseType::Array(Box::new(inner_type)))
            }
            _ if s.starts_with('(') && s.ends_with(')') => {
                let inner = &s[1..s.len()-1];
                let types: Vec<BaseType> = inner
                    .split(',')
                    .map(|t| BaseType::parse(t.trim()))
                    .collect::<Result<_, _>>()?;
                Ok(BaseType::Tuple(types))
            }
            _ => Err(format!("Unknown type: {}", s)),
        }
    }
    
    /// Convert to SMT-LIB2 type
    pub fn to_smt(&self) -> String {
        match self {
            BaseType::U8 => "(_ BitVec 8)".to_string(),
            BaseType::U16 => "(_ BitVec 16)".to_string(),
            BaseType::U32 => "(_ BitVec 32)".to_string(),
            BaseType::U64 => "(_ BitVec 64)".to_string(),
            BaseType::I8 => "(_ BitVec 8)".to_string(),
            BaseType::I16 => "(_ BitVec 16)".to_string(),
            BaseType::I32 => "(_ BitVec 32)".to_string(),
            BaseType::I64 => "(_ BitVec 64)".to_string(),
            BaseType::Bool => "Bool".to_string(),
            BaseType::Array(inner) => format!("(Array Int {})", inner.to_smt()),
            BaseType::Tuple(types) => {
                let type_strs: Vec<String> = types.iter().map(|t| t.to_smt()).collect();
                format!("(Tuple {})", type_strs.join(" "))
            }
        }
    }
}

impl Predicate {
    /// Parse predicate from string
    pub fn parse(s: &str) -> Result<Self, String> {
        // Simple predicate parser
        // For now, just parse basic comparisons
        if s.contains("&&") {
            let parts: Vec<&str> = s.split("&&").collect();
            if parts.len() == 2 {
                let left = Predicate::parse(parts[0].trim())?;
                let right = Predicate::parse(parts[1].trim())?;
                return Ok(Predicate::And(Box::new(left), Box::new(right)));
            }
        }
        
        if s.contains("||") {
            let parts: Vec<&str> = s.split("||").collect();
            if parts.len() == 2 {
                let left = Predicate::parse(parts[0].trim())?;
                let right = Predicate::parse(parts[1].trim())?;
                return Ok(Predicate::Or(Box::new(left), Box::new(right)));
            }
        }
        
        if s.starts_with('!') {
            let inner = &s[1..].trim();
            let inner_pred = Predicate::parse(inner)?;
            return Ok(Predicate::Not(Box::new(inner_pred)));
        }
        
        // Parse comparison
        let ops = [">=", "<=", ">", "<", "==", "!="];
        for op in ops {
            if s.contains(op) {
                let parts: Vec<&str> = s.split(op).collect();
                if parts.len() == 2 {
                    let left = Expr::parse(parts[0].trim())?;
                    let right = Expr::parse(parts[1].trim())?;
                    let op_enum = match op {
                        "==" => ComparisonOp::Eq,
                        "!=" => ComparisonOp::Ne,
                        "<" => ComparisonOp::Lt,
                        "<=" => ComparisonOp::Le,
                        ">" => ComparisonOp::Gt,
                        ">=" => ComparisonOp::Ge,
                        _ => return Err(format!("Unknown operator: {}", op)),
                    };
                    return Ok(Predicate::Comparison { left, op: op_enum, right });
                }
            }
        }
        
        // Special cases
        if s == "true" {
            return Ok(Predicate::True);
        }
        if s == "false" {
            return Ok(Predicate::False);
        }
        
        Err(format!("Could not parse predicate: {}", s))
    }
    
    /// Convert to SMT-LIB2 expression
    pub fn to_smt(&self) -> String {
        match self {
            Predicate::Comparison { left, op, right } => {
                let op_str = match op {
                    ComparisonOp::Eq => "=",
                    ComparisonOp::Ne => "distinct",
                    ComparisonOp::Lt => "<",
                    ComparisonOp::Le => "<=",
                    ComparisonOp::Gt => ">",
                    ComparisonOp::Ge => ">=",
                };
                format!("({} {} {})", op_str, left.to_smt(), right.to_smt())
            }
            Predicate::And(left, right) => {
                format!("(and {} {})", left.to_smt(), right.to_smt())
            }
            Predicate::Or(left, right) => {
                format!("(or {} {})", left.to_smt(), right.to_smt())
            }
            Predicate::Not(inner) => {
                format!("(not {})", inner.to_smt())
            }
            Predicate::Forall { variable, base_type, body } => {
                format!("(forall (({} {})) {})", variable, base_type.to_smt(), body.to_smt())
            }
            Predicate::Exists { variable, base_type, body } => {
                format!("(exists (({} {})) {})", variable, base_type.to_smt(), body.to_smt())
            }
            Predicate::True => "true".to_string(),
            Predicate::False => "false".to_string(),
        }
    }
}

impl Expr {
    /// Parse expression from string
    pub fn parse(s: &str) -> Result<Self, String> {
        // Simple expression parser
        if let Ok(int_val) = s.parse::<i64>() {
            return Ok(Expr::Int(int_val));
        }
        
        if s == "true" {
            return Ok(Expr::Bool(true));
        }
        if s == "false" {
            return Ok(Expr::Bool(false));
        }
        
        // Check for binary operations
        let ops = ["+", "-", "*", "/", "%"];
        for op in ops {
            if s.contains(op) {
                let parts: Vec<&str> = s.split(op).collect();
                if parts.len() == 2 {
                    let left = Expr::parse(parts[0].trim())?;
                    let right = Expr::parse(parts[1].trim())?;
                    let op_enum = match op {
                        "+" => BinOp::Add,
                        "-" => BinOp::Sub,
                        "*" => BinOp::Mul,
                        "/" => BinOp::Div,
                        "%" => BinOp::Mod,
                        _ => return Err(format!("Unknown operator: {}", op)),
                    };
                    return Ok(Expr::BinOp(Box::new(left), op_enum, Box::new(right)));
                }
            }
        }
        
        // Check for array length
        if s.starts_with("len(") && s.ends_with(')') {
            let inner = &s[4..s.len()-1];
            let inner_expr = Expr::parse(inner)?;
            return Ok(Expr::Len(Box::new(inner_expr)));
        }
        
        // Check for array index
        if s.contains('[') && s.contains(']') {
            let bracket_pos = s.find('[').unwrap();
            let array_expr = Expr::parse(&s[..bracket_pos])?;
            let index_expr = Expr::parse(&s[bracket_pos+1..s.len()-1])?;
            return Ok(Expr::Index(Box::new(array_expr), Box::new(index_expr)));
        }
        
        // Default to variable
        Ok(Expr::Var(s.to_string()))
    }
    
    /// Convert to SMT-LIB2 expression
    pub fn to_smt(&self) -> String {
        match self {
            Expr::Var(name) => name.clone(),
            Expr::Int(value) => value.to_string(),
            Expr::Bool(value) => value.to_string(),
            Expr::Len(expr) => format!("(select {} {})", expr.to_smt(), "length"),
            Expr::Index(array, index) => {
                format!("(select {} {})", array.to_smt(), index.to_smt())
            }
            Expr::BinOp(left, op, right) => {
                let op_str = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "div",
                    BinOp::Mod => "mod",
                };
                format!("({} {} {})", op_str, left.to_smt(), right.to_smt())
            }
            Expr::Apply(func, args) => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_smt()).collect();
                format!("({} {})", func, args_str.join(" "))
            }
        }
    }
}

impl fmt::Display for RefinementType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}: {} | {}}}", self.variable, self.base_type, self.predicate)
    }
}

impl fmt::Display for BaseType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BaseType::U8 => write!(f, "u8"),
            BaseType::U16 => write!(f, "u16"),
            BaseType::U32 => write!(f, "u32"),
            BaseType::U64 => write!(f, "u64"),
            BaseType::I8 => write!(f, "i8"),
            BaseType::I16 => write!(f, "i16"),
            BaseType::I32 => write!(f, "i32"),
            BaseType::I64 => write!(f, "i64"),
            BaseType::Bool => write!(f, "bool"),
            BaseType::Array(inner) => write!(f, "[{}]", inner),
            BaseType::Tuple(types) => {
                let type_strs: Vec<String> = types.iter().map(|t| t.to_string()).collect();
                write!(f, "({})", type_strs.join(", "))
            }
        }
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Predicate::Comparison { left, op, right } => {
                let op_str = match op {
                    ComparisonOp::Eq => "==",
                    ComparisonOp::Ne => "!=",
                    ComparisonOp::Lt => "<",
                    ComparisonOp::Le => "<=",
                    ComparisonOp::Gt => ">",
                    ComparisonOp::Ge => ">=",
                };
                write!(f, "{} {} {}", left, op_str, right)
            }
            Predicate::And(left, right) => write!(f, "({} && {})", left, right),
            Predicate::Or(left, right) => write!(f, "({} || {})", left, right),
            Predicate::Not(inner) => write!(f, "!{}", inner),
            Predicate::Forall { variable, base_type, body } => {
                write!(f, "∀{}:{}. {}", variable, base_type, body)
            }
            Predicate::Exists { variable, base_type, body } => {
                write!(f, "∃{}:{}. {}", variable, base_type, body)
            }
            Predicate::True => write!(f, "true"),
            Predicate::False => write!(f, "false"),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Int(value) => write!(f, "{}", value),
            Expr::Bool(value) => write!(f, "{}", value),
            Expr::Len(expr) => write!(f, "len({})", expr),
            Expr::Index(array, index) => write!(f, "{}[{}]", array, index),
            Expr::BinOp(left, op, right) => {
                let op_str = match op {
                    BinOp::Add => "+",
                    BinOp::Sub => "-",
                    BinOp::Mul => "*",
                    BinOp::Div => "/",
                    BinOp::Mod => "%",
                };
                write!(f, "({} {} {})", left, op_str, right)
            }
            Expr::Apply(func, args) => {
                let args_str: Vec<String> = args.iter().map(|a| a.to_string()).collect();
                write!(f, "{}({})", func, args_str.join(", "))
            }
        }
    }
}
