use zetac::diagnostics::{SourceLocation, SourceSpan};

fn main() {
    let span = SourceSpan::new(
        SourceLocation::new("test.z", 3, 10, 25),
        SourceLocation::new("test.z", 3, 15, 30),
    );
    
    println!("Formatted span: {}", span.format());
    println!("Expected: test.z:3:10-15");
    
    // Check what the format method actually returns
    let formatted = span.format();
    println!("Contains 'test.z:3:10-15': {}", formatted.contains("test.z:3:10-15"));
    println!("Full string: '{}'", formatted);
}