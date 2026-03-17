// ast_extensions.rs - AST extensions for enhanced trait system
// File: C:\Users\mummy\OneDrive\Documents\DarkFactory\zeta-0.3.4\src\ast_extensions.rs
// Purpose: Extend AST with associated types, default methods, supertraits

use std::collections::HashMap;

/// Extended concept definition with associated types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtendedConceptDef {
    pub name: String,
    pub generics: Vec<String>,
    pub associated_types: Vec<AssociatedTypeDef>,
    pub methods: Vec<ExtendedMethod>,
    pub supertraits: Vec<String>,
    pub doc: String,
}

/// Associated type definition
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AssociatedTypeDef {
    pub name: String,
    pub bounds: Vec<String>,
    pub default: Option<String>,
    pub doc: String,
}

/// Extended method with default implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtendedMethod {
    pub name: String,
    pub params: Vec<(String, String)>,
    pub ret: String,
    pub generics: Vec<String>,
    pub default_body: Option<Vec<crate::frontend::ast::AstNode>>,
    pub doc: String,
}

/// Extended implementation with associated type mappings
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExtendedImplBlock {
    pub concept: String,
    pub generics: Vec<String>,
    pub ty: String,
    pub associated_type_mappings: HashMap<String, String>,
    pub method_implementations: Vec<MethodImpl>,
    pub doc: String,
}

/// Method implementation
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MethodImpl {
    pub name: String,
    pub body: Vec<crate::frontend::ast::AstNode>,
    pub doc: String,
}

/// Convert basic AST concept to extended concept
pub fn extend_concept(ast_concept: &crate::frontend::ast::AstNode) -> Option<ExtendedConceptDef> {
    match ast_concept {
        crate::frontend::ast::AstNode::ConceptDef { name, generics, methods, doc } => {
            let extended_methods = methods.iter()
                .filter_map(|node| {
                    if let crate::frontend::ast::AstNode::Method { name, params, ret, generics, doc } = node {
                        Some(ExtendedMethod {
                            name: name.clone(),
                            params: params.clone(),
                            ret: ret.clone(),
                            generics: generics.clone(),
                            default_body: None,
                            doc: doc.clone(),
                        })
                    } else {
                        None
                    }
                })
                .collect();
            
            Some(ExtendedConceptDef {
                name: name.clone(),
                generics: generics.clone(),
                associated_types: Vec::new(),
                methods: extended_methods,
                supertraits: Vec::new(),
                doc: doc.clone(),
            })
        }
        _ => None,
    }
}

/// Convert basic AST impl block to extended impl block
pub fn extend_impl_block(ast_impl: &crate::frontend::ast::AstNode) -> Option<ExtendedImplBlock> {
    match ast_impl {
        crate::frontend::ast::AstNode::ImplBlock { concept, generics, ty, body, doc } => {
            // Extract method implementations from body
            let method_implementations = body.iter()
                .filter_map(|node| {
                    if let crate::frontend::ast::AstNode::FuncDef { name, body, doc, .. } = node {
                        Some(MethodImpl {
                            name: name.clone(),
                            body: body.clone(),
                            doc: doc.clone(),
                        })
                    } else {
                        None
                    }
                })
                .collect();
            
            Some(ExtendedImplBlock {
                concept: concept.clone(),
                generics: generics.clone(),
                ty: ty.clone(),
                associated_type_mappings: HashMap::new(),
                method_implementations,
                doc: doc.clone(),
            })
        }
        _ => None,
    }
}

/// Parser for extended concept syntax
pub mod parser {
    use nom::IResult;
    
    /// Parse associated type: `type Name: Bound1 + Bound2 = Default;`
    pub fn parse_associated_type(input: &str) -> IResult<&str, AssociatedTypeDef> {
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, multispace0, not_line_ending};
        use nom::sequence::{delimited, tuple};
        
        let (input, _) = tag("type")(input)?;
        let (input, _) = multispace0(input)?;
        let (input, name) = alpha1(input)?;
        
        // TODO: Implement full parsing
        // For now, return simple associated type
        
        Ok((input, AssociatedTypeDef {
            name: name.to_string(),
            bounds: Vec::new(),
            default: None,
            doc: String::new(),
        }))
    }
    
    /// Parse supertraits: `: Trait1 + Trait2`
    pub fn parse_supertraits(input: &str) -> IResult<&str, Vec<String>> {
        use nom::bytes::complete::tag;
        use nom::character::complete::{alpha1, multispace0};
        use nom::multi::separated_list1;
        use nom::sequence::{preceded, tuple};
        
        let (input, _) = tag(":")(input)?;
        let (input, _) = multispace0(input)?;
        
        let (input, traits) = separated_list1(
            tuple((multispace0, tag("+"), multispace0)),
            alpha1
        )(input)?;
        
        Ok((input, traits.iter().map(|s| s.to_string()).collect()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_extend_concept() {
        use crate::frontend::ast::AstNode;
        
        let ast_concept = AstNode::ConceptDef {
            name: "Addable".to_string(),
            generics: vec!["Rhs".to_string()],
            methods: vec![
                AstNode::Method {
                    name: "add".to_string(),
                    params: vec![
                        ("self".to_string(), "Self".to_string()),
                        ("rhs".to_string(), "Rhs".to_string()),
                    ],
                    ret: "Self".to_string(),
                    generics: Vec::new(),
                    doc: "Add two values".to_string(),
                }
            ],
            doc: "Things that can be added".to_string(),
        };
        
        let extended = extend_concept(&ast_concept).unwrap();
        
        assert_eq!(extended.name, "Addable");
        assert_eq!(extended.generics, vec!["Rhs"]);
        assert_eq!(extended.methods.len(), 1);
        assert_eq!(extended.methods[0].name, "add");
    }
    
    #[test]
    fn test_extend_impl_block() {
        use crate::frontend::ast::AstNode;
        
        let ast_impl = AstNode::ImplBlock {
            concept: "Addable".to_string(),
            generics: Vec::new(),
            ty: "i32".to_string(),
            body: vec![
                AstNode::FuncDef {
                    name: "add".to_string(),
                    generics: Vec::new(),
                    params: vec![
                        ("self".to_string(), "i32".to_string()),
                        ("rhs".to_string(), "i32".to_string()),
                    ],
                    ret: "i32".to_string(),
                    body: vec![AstNode::Lit(42)],  // Simple body for test
                    attrs: Vec::new(),
                    ret_expr: None,
                    single_line: false,
                    doc: "Add i32".to_string(),
                }
            ],
            doc: "Addable for i32".to_string(),
        };
        
        let extended = extend_impl_block(&ast_impl).unwrap();
        
        assert_eq!(extended.concept, "Addable");
        assert_eq!(extended.ty, "i32");
        assert_eq!(extended.method_implementations.len(), 1);
        assert_eq!(extended.method_implementations[0].name, "add");
    }
    
    #[test]
    fn test_associated_type_struct() {
        let assoc_type = AssociatedTypeDef {
            name: "Item".to_string(),
            bounds: vec!["Debug".to_string(), "Clone".to_string()],
            default: Some("Self".to_string()),
            doc: "Type of items".to_string(),
        };
        
        assert_eq!(assoc_type.name, "Item");
        assert_eq!(assoc_type.bounds.len(), 2);
        assert_eq!(assoc_type.default, Some("Self".to_string()));
    }
    
    #[test]
    fn test_extended_method() {
        let method = ExtendedMethod {
            name: "next".to_string(),
            params: vec![
                ("self".to_string(), "&mut Self".to_string()),
            ],
            ret: "Option<Self::Item>".to_string(),
            generics: Vec::new(),
            default_body: None,
            doc: "Get next item".to_string(),
        };
        
        assert_eq!(method.name, "next");
        assert_eq!(method.ret, "Option<Self::Item>");
    }
}