//! Annotation Parser for Zeta Verification
//!
//! This module parses verification annotations from Zeta source code.

use crate::refinement::{RefinementType, Predicate};

/// Verification annotation
#[derive(Debug, Clone)]
pub enum Annotation {
    /// Function contract
    FunctionContract {
        name: String,
        params: Vec<Param>,
        returns: Option<RefinementType>,
    },
    /// Loop invariant
    LoopInvariant {
        predicate: Predicate,
    },
    /// Assertion
    Assertion {
        predicate: Predicate,
    },
    /// Precondition
    Precondition {
        predicate: Predicate,
    },
    /// Postcondition
    Postcondition {
        predicate: Predicate,
    },
}

/// Function parameter with optional refinement
#[derive(Debug, Clone)]
pub struct Param {
    /// Parameter name
    pub name: String,
    /// Parameter type (may be refinement type)
    pub ty: ParamType,
}

/// Parameter type (regular or refined)
#[derive(Debug, Clone)]
pub enum ParamType {
    /// Regular type
    Regular(String),
    /// Refinement type
    Refined(RefinementType),
}

/// Annotated AST node
#[derive(Debug, Clone)]
pub struct AnnotatedAst {
    /// Original source code
    pub source: String,
    /// Extracted annotations
    pub annotations: Vec<Annotation>,
    /// Line numbers for annotations
    pub line_numbers: Vec<usize>,
}

/// Parse annotations from Zeta source code
pub fn parse_annotations(source: &str) -> Result<AnnotatedAst, String> {
    let lines: Vec<&str> = source.lines().collect();
    let mut annotations = Vec::new();
    let mut line_numbers = Vec::new();
    
    for (i, line) in lines.iter().enumerate() {
        let line_num = i + 1;
        let trimmed = line.trim();
        
        // Skip empty lines and comments (unless they contain annotations)
        if trimmed.is_empty() {
            continue;
        }
        
        // Check for function definitions with contracts
        if trimmed.starts_with("fn ") {
            if let Some(annotation) = parse_function_contract(trimmed, line_num)? {
                annotations.push(annotation);
                line_numbers.push(line_num);
            }
        }
        
        // Check for loop invariants
        if trimmed.contains("@invariant") {
            if let Some(annotation) = parse_loop_invariant(trimmed, line_num)? {
                annotations.push(annotation);
                line_numbers.push(line_num);
            }
        }
        
        // Check for assertions
        if trimmed.contains("@assert") {
            if let Some(annotation) = parse_assertion(trimmed, line_num)? {
                annotations.push(annotation);
                line_numbers.push(line_num);
            }
        }
        
        // Check for preconditions
        if trimmed.contains("@pre") {
            if let Some(annotation) = parse_precondition(trimmed, line_num)? {
                annotations.push(annotation);
                line_numbers.push(line_num);
            }
        }
        
        // Check for postconditions
        if trimmed.contains("@post") {
            if let Some(annotation) = parse_postcondition(trimmed, line_num)? {
                annotations.push(annotation);
                line_numbers.push(line_num);
            }
        }
    }
    
    Ok(AnnotatedAst {
        source: source.to_string(),
        annotations,
        line_numbers,
    })
}

/// Parse function contract annotation
fn parse_function_contract(line: &str, line_num: usize) -> Result<Option<Annotation>, String> {
    // Extract function name
    let fn_start = line.find("fn ").unwrap() + 3;
    let fn_rest = &line[fn_start..];
    
    // Find opening parenthesis
    let paren_open = fn_rest.find('(').ok_or("Missing '(' in function definition")?;
    let fn_name = fn_rest[..paren_open].trim();
    
    // Find closing parenthesis
    let paren_close = fn_rest.find(')').ok_or("Missing ')' in function definition")?;
    let params_str = &fn_rest[paren_open + 1..paren_close];
    
    // Parse parameters
    let params = parse_params(params_str)?;
    
    // Check for return type
    let after_paren = &fn_rest[paren_close + 1..];
    let returns = if after_paren.contains("->") {
        let arrow_pos = after_paren.find("->").unwrap();
        let return_str = after_paren[arrow_pos + 2..].trim();
        
        // Check if return type is a refinement type
        if return_str.starts_with('{') && return_str.contains('|') && return_str.ends_with('}') {
            let refinement = RefinementType::parse(return_str)?;
            Some(refinement)
        } else {
            // Regular return type - no annotation needed
            None
        }
    } else {
        None
    };
    
    // Check if any parameters have refinement types
    let has_refined_params = params.iter().any(|p| matches!(p.ty, ParamType::Refined(_)));
    
    if has_refined_params || returns.is_some() {
        Ok(Some(Annotation::FunctionContract {
            name: fn_name.to_string(),
            params,
            returns,
        }))
    } else {
        Ok(None)
    }
}

/// Parse function parameters
fn parse_params(params_str: &str) -> Result<Vec<Param>, String> {
    if params_str.trim().is_empty() {
        return Ok(Vec::new());
    }
    
    let param_strs: Vec<&str> = params_str.split(',').collect();
    let mut params = Vec::new();
    
    for param_str in param_strs {
        let trimmed = param_str.trim();
        if trimmed.is_empty() {
            continue;
        }
        
        // Split by colon
        let parts: Vec<&str> = trimmed.split(':').collect();
        if parts.len() != 2 {
            return Err(format!("Invalid parameter syntax: {}", trimmed));
        }
        
        let name = parts[0].trim().to_string();
        let type_str = parts[1].trim();
        
        // Check if type is refinement type
        let ty = if type_str.starts_with('{') && type_str.contains('|') && type_str.ends_with('}') {
            let refinement = RefinementType::parse(type_str)?;
            ParamType::Refined(refinement)
        } else {
            ParamType::Regular(type_str.to_string())
        };
        
        params.push(Param { name, ty });
    }
    
    Ok(params)
}

/// Parse loop invariant annotation
fn parse_loop_invariant(line: &str, _line_num: usize) -> Result<Option<Annotation>, String> {
    // Extract invariant predicate
    let start = line.find("@invariant").unwrap() + 10;
    let predicate_str = line[start..].trim();
    
    if predicate_str.is_empty() {
        return Ok(None);
    }
    
    let predicate = Predicate::parse(predicate_str)?;
    
    Ok(Some(Annotation::LoopInvariant { predicate }))
}

/// Parse assertion annotation
fn parse_assertion(line: &str, _line_num: usize) -> Result<Option<Annotation>, String> {
    // Extract assertion predicate
    let start = line.find("@assert").unwrap() + 7;
    let predicate_str = line[start..].trim();
    
    if predicate_str.is_empty() {
        return Ok(None);
    }
    
    let predicate = Predicate::parse(predicate_str)?;
    
    Ok(Some(Annotation::Assertion { predicate }))
}

/// Parse precondition annotation
fn parse_precondition(line: &str, _line_num: usize) -> Result<Option<Annotation>, String> {
    // Extract precondition predicate
    let start = line.find("@pre").unwrap() + 4;
    let predicate_str = line[start..].trim();
    
    if predicate_str.is_empty() {
        return Ok(None);
    }
    
    let predicate = Predicate::parse(predicate_str)?;
    
    Ok(Some(Annotation::Precondition { predicate }))
}

/// Parse postcondition annotation
fn parse_postcondition(line: &str, _line_num: usize) -> Result<Option<Annotation>, String> {
    // Extract postcondition predicate
    let start = line.find("@post").unwrap() + 5;
    let predicate_str = line[start..].trim();
    
    if predicate_str.is_empty() {
        return Ok(None);
    }
    
    let predicate = Predicate::parse(predicate_str)?;
    
    Ok(Some(Annotation::Postcondition { predicate }))
}

/// Create annotated Murphy's Sieve example
pub fn create_murphy_sieve_example() -> AnnotatedAst {
    let source = r#"// Verified Murphy's Sieve
fn murphy_sieve(limit: {n: u64 | n >= 2}) -> 
    {count: u64 | count == prime_count(limit)} 
{
    let bits: [dynamic]u8 = [dynamic]u8{};
    
    // Initialize array
    let mut i: u64 = 0;
    while i < limit {
        bits.push(0);
        i += 1;
    }
    
    // Mark 0 and 1 as composite
    bits[0] = 1;
    bits[1] = 1;
    
    // Sieve with loop invariant
    let mut p: u64 = 2;
    while p * p < limit {
        // @invariant ∀k ∈ [2, p). ∀m ∈ [k*k, limit). bits[m] == 1 → ¬is_prime(m)
        if bits[p] == 0 {
            let mut multiple: u64 = p * p;
            while multiple < limit {
                // @invariant ∀m ∈ [p*p, multiple). bits[m] == 1
                bits[multiple] = 1;
                multiple += p;
            }
        }
        p += 1;
    }
    
    // Count primes with invariant
    let mut count: u64 = 0;
    let mut n: u64 = 0;
    while n < limit {
        // @invariant count == |{k ∈ [0, n) | bits[k] == 0}|
        if bits[n] == 0 {
            count += 1;
        }
        n += 1;
    }
    
    return count;
}"#;
    
    parse_annotations(source).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_function_contract() {
        let source = "fn sqrt(x: {n: u64 | n >= 0}) -> {r: u64 | r * r <= x && (r + 1) * (r + 1) > x}";
        
        let ast = parse_annotations(source).unwrap();
        assert_eq!(ast.annotations.len(), 1);
        
        match &ast.annotations[0] {
            Annotation::FunctionContract { name, params, returns } => {
                assert_eq!(name, "sqrt");
                assert_eq!(params.len(), 1);
                assert!(returns.is_some());
            }
            _ => panic!("Expected FunctionContract annotation"),
        }
    }
    
    #[test]
    fn test_parse_loop_invariant() {
        let source = "// @invariant total == sum(arr[0..i])";
        
        let ast = parse_annotations(source).unwrap();
        assert_eq!(ast.annotations.len(), 1);
        
        match &ast.annotations[0] {
            Annotation::LoopInvariant { predicate: _ } => {
                // Success
            }
            _ => panic!("Expected LoopInvariant annotation"),
        }
    }
    
    #[test]
    fn test_parse_assertion() {
        let source = "// @assert x > 0";
        
        let ast = parse_annotations(source).unwrap();
        assert_eq!(ast.annotations.len(), 1);
        
        match &ast.annotations[0] {
            Annotation::Assertion { predicate: _ } => {
                // Success
            }
            _ => panic!("Expected Assertion annotation"),
        }
    }
    
    #[test]
    fn test_murphy_sieve_example() {
        let ast = create_murphy_sieve_example();
        
        // Should have multiple annotations
        assert!(!ast.annotations.is_empty());
        
        // Check for function contract
        let has_function_contract = ast.annotations.iter().any(|a| {
            matches!(a, Annotation::FunctionContract { .. })
        });
        assert!(has_function_contract);
        
        // Check for loop invariants
        let has_loop_invariants = ast.annotations.iter().any(|a| {
            matches!(a, Annotation::LoopInvariant { .. })
        });
        assert!(has_loop_invariants);
    }
}