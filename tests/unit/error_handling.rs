//! Tests for enhanced error handling in Zeta compiler

use zetac::diagnostics::{Diagnostic, DiagnosticReporter, Severity, SourceLocation, SourceSpan};
use zetac::error_codes::diagnostic_from_code;

#[test]
fn test_diagnostic_creation() {
    let span = SourceSpan::new(
        SourceLocation::new("test.z", 1, 5, 4),
        SourceLocation::new("test.z", 1, 10, 9),
    );

    let diag = diagnostic_from_code("E1001", "expected `;`, found `}`".to_string(), Some(span));

    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(diag.code, Some("E1001".to_string()));
    assert!(diag.message.contains("expected"));
}

#[test]
fn test_diagnostic_formatting() {
    let span = SourceSpan::new(
        SourceLocation::new("test.z", 3, 10, 25),
        SourceLocation::new("test.z", 3, 15, 30),
    );

    let diag = Diagnostic::error("E1002", "unterminated string literal".to_string())
        .with_span(span)
        .with_help("Add closing quote `\"`".to_string());

    let source = r#"fn main() {
    let s = "hello world;
}"#;

    let formatted = diag.format(Some(source));

    println!("Formatted output:\n{}", formatted);
    println!("Looking for: test.z:3:10-15");
    println!(
        "Contains error[E1002]: {}",
        formatted.contains("error[E1002]")
    );
    println!(
        "Contains test.z:3:10-15: {}",
        formatted.contains("test.z:3:10-15")
    );
    println!("Contains help:: {}", formatted.contains("help:"));
    println!(
        "Contains unterminated string literal: {}",
        formatted.contains("unterminated string literal")
    );

    assert!(formatted.contains("error[E1002]"));
    assert!(formatted.contains("test.z:3"));
    assert!(formatted.contains("help:"));
    assert!(formatted.contains("unterminated string literal"));
}

#[test]
fn test_diagnostic_reporter() {
    let mut reporter = DiagnosticReporter::new();
    let source = r#"fn main() {
    let x = 42;
    let y = x + z;  // z is undefined
}"#;

    reporter.add_source("test.z", source.to_string());

    let span = SourceSpan::new(
        SourceLocation::new("test.z", 3, 11, 30),
        SourceLocation::new("test.z", 3, 12, 31),
    );

    let diag = Diagnostic::error("E3001", "undefined variable `z`".to_string())
        .with_span(span)
        .with_suggestion("Define `z` before using it".to_string())
        .with_suggestion("Check for typos".to_string());

    reporter.report(diag);

    // Add a warning
    let warning_span = SourceSpan::new(
        SourceLocation::new("test.z", 2, 9, 15),
        SourceLocation::new("test.z", 2, 11, 17),
    );

    let warning = Diagnostic::warning("W1001", "unused variable `x`".to_string())
        .with_span(warning_span)
        .with_help("Consider using `_x` if intentional".to_string());

    reporter.report(warning);

    assert!(reporter.has_errors());
    assert!(reporter.has_warnings());

    let formatted = reporter.format_all();
    assert!(formatted.contains("error[E3001]"));
    assert!(formatted.contains("warning[W1001]"));
    assert!(formatted.contains("1 error, 1 warning"));
}

#[test]
fn test_error_context_extraction() {
    let source = r#"fn main() {
    let x = 42;
    let y = x + z;
    println!("{}", y);
}"#;

    let span = SourceSpan::new(
        SourceLocation::new("test.z", 3, 11, 30),
        SourceLocation::new("test.z", 3, 12, 31),
    );

    let context = Diagnostic::extract_context(source, span);

    assert!(context.contains("let y = x + z;"));
    assert!(context.contains("^")); // Should have caret
}

#[test]
fn test_multiple_errors() {
    let mut reporter = DiagnosticReporter::new();
    let source = r#"fn main() {
    let x = ;
    let y = x + z;
}"#;

    reporter.add_source("test.z", source.to_string());

    // First error: missing expression after =
    let span1 = SourceSpan::new(
        SourceLocation::new("test.z", 2, 11, 15),
        SourceLocation::new("test.z", 2, 11, 15),
    );

    reporter.report(
        Diagnostic::error("E2003", "cannot infer type".to_string())
            .with_span(span1)
            .with_help("Add type annotation or initial value".to_string()),
    );

    // Second error: undefined variable z
    let span2 = SourceSpan::new(
        SourceLocation::new("test.z", 3, 15, 30),
        SourceLocation::new("test.z", 3, 16, 31),
    );

    reporter
        .report(Diagnostic::error("E3001", "undefined variable `z`".to_string()).with_span(span2));

    let formatted = reporter.format_all();

    // Should report both errors
    assert!(formatted.contains("cannot infer type"));
    assert!(formatted.contains("undefined variable"));
    assert!(formatted.contains("2 errors"));
}

#[test]
fn test_error_suggestions() {
    let span = SourceSpan::new(
        SourceLocation::new("test.z", 1, 1, 0),
        SourceLocation::new("test.z", 1, 5, 4),
    );

    let diag = Diagnostic::error("E1003", "missing closing brace".to_string())
        .with_span(span)
        .with_suggestion("Add closing brace `}`".to_string())
        .with_suggestion("Check brace nesting".to_string())
        .with_note("This often happens with nested blocks".to_string());

    let formatted = diag.format(None);

    assert!(formatted.contains("suggestions:"));
    assert!(formatted.contains("1. Add closing brace `}`"));
    assert!(formatted.contains("2. Check brace nesting"));
    assert!(formatted.contains("note:"));
}
