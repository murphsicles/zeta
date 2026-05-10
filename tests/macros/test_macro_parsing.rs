// Test macro parsing functionality

use zetac::frontend::macro_expand;

#[test]
fn test_parse_macro_rules() {
    // Test parsing a simple macro
    let input = r#"macro_rules! say_hello {
        () => {
            println!("Hello!")
        };
    }"#;
    
    let result = macro_expand::parse_macro_rules(input);
    assert!(result.is_ok());
    
    let macro_def = result.unwrap();
    assert_eq!(macro_def.name, "say_hello");
    assert_eq!(macro_def.patterns.len(), 1);
}

#[test]
fn test_parse_macro_with_args() {
    // Test parsing a macro with arguments
    let input = r#"macro_rules! add {
        ($a:expr, $b:expr) => {
            $a + $b
        };
    }"#;
    
    let result = macro_expand::parse_macro_rules(input);
    assert!(result.is_ok());
    
    let macro_def = result.unwrap();
    assert_eq!(macro_def.name, "add");
    assert_eq!(macro_def.patterns.len(), 1);
}

#[test]
fn test_macro_expander_creation() {
    // Test that we can create a macro expander
    let expander = macro_expand::MacroExpander::new();
    // Just testing that it compiles and can be created
    assert!(true);
}