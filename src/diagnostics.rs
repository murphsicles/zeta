//! Enhanced diagnostic reporting for Zeta compiler
//!
//! Provides file:line:column location tracking, context snippets,
//! and multiple error reporting.

use std::collections::HashMap;
use std::fmt;

/// Source location with file, line, and column
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceLocation {
    pub file: &'static str, // For now, static string; could be PathBuf later
    pub line: usize,
    pub column: usize,
    pub offset: usize, // Byte offset in source
}

impl SourceLocation {
    pub fn new(file: &'static str, line: usize, column: usize, offset: usize) -> Self {
        Self {
            file,
            line,
            column,
            offset,
        }
    }

    pub fn format(&self) -> String {
        format!("{}:{}:{}", self.file, self.line, self.column)
    }
}

/// Source span from start to end location
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan {
    pub start: SourceLocation,
    pub end: SourceLocation,
}

impl SourceSpan {
    pub fn new(start: SourceLocation, end: SourceLocation) -> Self {
        Self { start, end }
    }

    pub fn single(loc: SourceLocation) -> Self {
        Self {
            start: loc,
            end: loc,
        }
    }

    pub fn format(&self) -> String {
        if self.start.line == self.end.line && self.start.column == self.end.column {
            self.start.format()
        } else if self.start.line == self.end.line {
            format!(
                "{}:{}-{}",
                self.start.file, self.start.line, self.start.column
            )
        } else {
            format!(
                "{}:{}.{}-{}.{}",
                self.start.file, self.start.line, self.start.column, self.end.line, self.end.column
            )
        }
    }
}

/// Diagnostic severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Note,
    Help,
    Warning,
    Error,
    Fatal,
}

/// Warning level configuration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WarningLevel {
    Allow,  // Don't emit warning
    Warn,   // Emit warning but continue
    Deny,   // Treat warning as error
    Forbid, // Same as deny but cannot be overridden
}

impl WarningLevel {
    pub fn should_emit(&self) -> bool {
        matches!(
            self,
            WarningLevel::Warn | WarningLevel::Deny | WarningLevel::Forbid
        )
    }

    pub fn is_error(&self) -> bool {
        matches!(self, WarningLevel::Deny | WarningLevel::Forbid)
    }
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Note => write!(f, "note"),
            Severity::Help => write!(f, "help"),
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::Fatal => write!(f, "fatal"),
        }
    }
}

/// Enhanced diagnostic with source location and context
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<String>,
    pub message: String,
    pub span: Option<SourceSpan>,
    pub context: Option<String>, // Context snippet showing problematic code
    pub help: Option<String>,
    pub note: Option<String>,
    pub suggestions: Vec<String>, // Suggested fixes
}

impl Diagnostic {
    pub fn error(code: &str, message: String) -> Self {
        Self {
            severity: Severity::Error,
            code: Some(code.to_string()),
            message,
            span: None,
            context: None,
            help: None,
            note: None,
            suggestions: Vec::new(),
        }
    }

    pub fn warning(code: &str, message: String) -> Self {
        Self {
            severity: Severity::Warning,
            code: Some(code.to_string()),
            message,
            span: None,
            context: None,
            help: None,
            note: None,
            suggestions: Vec::new(),
        }
    }

    pub fn with_span(mut self, span: SourceSpan) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }

    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }

    pub fn with_note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }

    pub fn with_suggestion(mut self, suggestion: String) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    /// Extract context snippet from source code (Rust-style with line numbers and underlines)
    pub fn extract_context(source: &str, span: SourceSpan) -> String {
        let lines: Vec<&str> = source.lines().collect();

        if span.start.line == 0 || span.start.line > lines.len() {
            return String::new();
        }

        let line_idx = span.start.line - 1;
        let line = lines[line_idx];
        let line_num = span.start.line;
        const LN_WIDTH: usize = 4;

        let mut result = String::new();

        // Source line with right-aligned line number prefix
        // e.g., "  12 |     let x: int = 42;\n"
        result.push_str(&format!(
            "{:>width$} | {}\n",
            line_num,
            line,
            width = LN_WIDTH
        ));

        // Underline row: spaces + carets pointing at the error span
        let start_col = span.start.column.saturating_sub(1); // 0-based column
        let end_col = std::cmp::min(
            span.end.column.saturating_sub(1),
            line.len().saturating_sub(1),
        );

        let padding = " ".repeat(start_col);
        let count = std::cmp::max(1, end_col.saturating_sub(start_col) + 1);
        let carets = "^".repeat(count);

        result.push_str(&format!(
            "{:>width$} | {}{}\n",
            "",
            padding,
            carets,
            width = LN_WIDTH
        ));

        result
    }

    pub fn format(&self, source: Option<&str>) -> String {
        let no_color = std::env::var("NO_COLOR").is_ok();

        let mut output = String::new();

        // Helper: wrap text in ANSI escape code unless NO_COLOR is set
        let ansi = |code: &str, text: &str| -> String {
            if no_color {
                text.to_string()
            } else {
                format!("\x1b{}{}\x1b[0m", code, text)
            }
        };

        // ── Severity + code ────────────────────────────────────────────
        let (sev_ansi, code_ansi) = match self.severity {
            Severity::Error | Severity::Fatal => ("[1;31m", "[1;31m"), // bold red
            Severity::Warning => ("[1;33m", "[1;33m"),                 // bold yellow
            Severity::Note => ("[34m", "[34m"),                        // blue
            Severity::Help => ("[34m", "[34m"),                        // blue
        };

        let severity_colored = ansi(sev_ansi, &self.severity.to_string());
        if let Some(code) = &self.code {
            let code_colored = ansi(code_ansi, &format!("[{}]", code));
            output.push_str(&format!(
                "{}{}: {}\n",
                severity_colored, code_colored, self.message
            ));
        } else {
            output.push_str(&format!("{}: {}\n", severity_colored, self.message));
        }

        // ── Location (cyan arrow) ──────────────────────────────────────
        if let Some(span) = &self.span {
            let loc = format!(
                "{}:{}:{}",
                span.start.file, span.start.line, span.start.column
            );
            output.push_str(&format!("  {}\n", ansi("[36m", &format!("--> {}", loc))));

            // ── Source context ─────────────────────────────────────────
            if let Some(src) = source {
                let ctx = Self::extract_context(src, *span);
                if !ctx.is_empty() {
                    output.push_str("   |\n");
                    output.push_str(&ctx);
                }
            }
        }

        // ── Note ────────────────────────────────────────────────────────
        if let Some(note) = &self.note {
            output.push_str(&format!("   = {}: {}\n", ansi("[34m", "note"), note));
        }

        // ── Help ────────────────────────────────────────────────────────
        if let Some(help) = &self.help {
            output.push_str(&format!("   = {}: {}\n", ansi("[34m", "help"), help));
        }

        // ── Suggestions ─────────────────────────────────────────────────
        if !self.suggestions.is_empty() {
            for suggestion in &self.suggestions {
                output.push_str(&format!(
                    "     = {}: {}\n",
                    ansi("[34m", "suggestion"),
                    suggestion
                ));
            }
        }

        output
    }
}

/// Warning configuration for specific warning codes
#[derive(Debug, Clone)]
pub struct WarningConfig {
    pub level: WarningLevel,
    pub description: String,
}

/// Collection of diagnostics for multiple error reporting
#[derive(Debug)]
pub struct DiagnosticReporter {
    diagnostics: Vec<Diagnostic>,
    source_map: HashMap<String, String>, // filename -> source code
    warning_config: HashMap<String, WarningConfig>, // warning code -> configuration
    default_warning_level: WarningLevel,
}

impl Default for DiagnosticReporter {
    fn default() -> Self {
        Self {
            diagnostics: Vec::new(),
            source_map: HashMap::new(),
            warning_config: HashMap::new(),
            default_warning_level: WarningLevel::Warn,
        }
    }
}

impl DiagnosticReporter {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_warning_level(mut self, level: WarningLevel) -> Self {
        self.default_warning_level = level;
        self
    }

    pub fn configure_warning(&mut self, code: &str, level: WarningLevel, description: &str) {
        self.warning_config.insert(
            code.to_string(),
            WarningConfig {
                level,
                description: description.to_string(),
            },
        );
    }

    pub fn get_warning_level(&self, code: &str) -> WarningLevel {
        self.warning_config
            .get(code)
            .map(|config| config.level)
            .unwrap_or(self.default_warning_level)
    }

    pub fn add_source(&mut self, filename: &str, source: String) {
        self.source_map.insert(filename.to_string(), source);
    }

    pub fn report(&mut self, diagnostic: Diagnostic) {
        // Check if this is a warning and handle warning levels
        if diagnostic.severity == Severity::Warning {
            if let Some(code) = &diagnostic.code {
                let warning_level = self.get_warning_level(code);

                match warning_level {
                    WarningLevel::Allow => {
                        // Don't add the warning
                    }
                    WarningLevel::Warn => {
                        // Add as warning
                        self.diagnostics.push(diagnostic);
                    }
                    WarningLevel::Deny | WarningLevel::Forbid => {
                        // Upgrade to error
                        let mut error_diagnostic = diagnostic;
                        error_diagnostic.severity = Severity::Error;
                        error_diagnostic.message = format!("[deny] {}", error_diagnostic.message);
                        self.diagnostics.push(error_diagnostic);
                    }
                }
            } else {
                // Warning without code uses default level
                if self.default_warning_level.should_emit() {
                    self.diagnostics.push(diagnostic);
                }
            }
        } else {
            // Not a warning, just add it
            self.diagnostics.push(diagnostic);
        }
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error || d.severity == Severity::Fatal)
    }

    pub fn has_warnings(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| d.severity == Severity::Warning)
    }

    pub fn clear(&mut self) {
        self.diagnostics.clear();
    }

    pub fn format_all(&self) -> String {
        let mut output = String::new();

        for diagnostic in &self.diagnostics {
            let source = diagnostic
                .span
                .as_ref()
                .and_then(|span| self.source_map.get(span.start.file));

            output.push_str(&diagnostic.format(source.map(|s| s.as_str())));
            output.push('\n');
        }

        // Summary
        let error_count = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error || d.severity == Severity::Fatal)
            .count();
        let warning_count = self
            .diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .count();

        if error_count > 0 || warning_count > 0 {
            output.push_str(&format!(
                "\n{} error{}, {} warning{}",
                error_count,
                if error_count == 1 { "" } else { "s" },
                warning_count,
                if warning_count == 1 { "" } else { "s" }
            ));
        }

        output
    }

    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }
}

/// Common error creation helpers
pub mod errors {
    use super::*;

    pub fn parse_error(code: &str, message: &str, span: SourceSpan) -> Diagnostic {
        Diagnostic::error(code, message.to_string())
            .with_span(span)
            .with_help("Check syntax and try again".to_string())
    }

    pub fn type_error(code: &str, message: &str, span: SourceSpan) -> Diagnostic {
        Diagnostic::error(code, message.to_string())
            .with_span(span)
            .with_help("Check type annotations and definitions".to_string())
    }

    pub fn undefined_variable(name: &str, span: SourceSpan) -> Diagnostic {
        Diagnostic::error("E3001", format!("undefined variable `{}`", name))
            .with_span(span)
            .with_suggestion(format!("Define `{}` before using it", name))
            .with_suggestion(format!("Check for typos in `{}`", name))
    }

    pub fn unexpected_token(expected: &str, found: &str, span: SourceSpan) -> Diagnostic {
        Diagnostic::error(
            "E1001",
            format!("expected `{}`, found `{}`", expected, found),
        )
        .with_span(span)
        .with_suggestion(format!("Replace `{}` with `{}`", found, expected))
    }

    pub fn unterminated_string(span: SourceSpan) -> Diagnostic {
        Diagnostic::error("E1002", "unterminated string literal".to_string())
            .with_span(span)
            .with_suggestion("Add closing quote `\"`".to_string())
    }

    pub fn missing_closing_brace(span: SourceSpan) -> Diagnostic {
        Diagnostic::error("E1003", "missing closing brace".to_string())
            .with_span(span)
            .with_suggestion("Add closing brace `}`".to_string())
    }
}

// ---------------------------------------------------------------------------
// Thread-local reporter for ad-hoc diagnostics (no &mut passing required)
// ---------------------------------------------------------------------------

thread_local! {
    static TL_REPORTER: std::cell::RefCell<DiagnosticReporter> =
        std::cell::RefCell::new(DiagnosticReporter::new());
}

/// Emit a diagnostic to the thread-local reporter.
pub fn emit(diagnostic: Diagnostic) {
    TL_REPORTER.with(|r| r.borrow_mut().report(diagnostic));
}
