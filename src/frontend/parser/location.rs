//! Location tracking for parser errors
//!
//! Provides utilities to track line, column, and offset positions
//! during parsing for better error messages.

use nom::IResult;
use std::ops::Range;

/// Tracks position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub offset: usize, // Byte offset from start
    pub line: usize,   // 1-indexed line number
    pub column: usize, // 1-indexed column number
}

impl Position {
    pub fn new() -> Self {
        Self {
            offset: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn advance(&mut self, ch: char) {
        self.offset += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }

    pub fn advance_str(&mut self, s: &str) {
        for ch in s.chars() {
            self.advance(ch);
        }
    }
}

/// Tracks source span with start and end positions
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn single(pos: Position) -> Self {
        Self {
            start: pos,
            end: pos,
        }
    }

    pub fn merge(self, other: Span) -> Self {
        Self {
            start: if self.start.offset < other.start.offset {
                self.start
            } else {
                other.start
            },
            end: if self.end.offset > other.end.offset {
                self.end
            } else {
                other.end
            },
        }
    }

    pub fn to_range(&self) -> Range<usize> {
        self.start.offset..self.end.offset
    }
}

/// Wrapper for parser input that tracks position
#[derive(Debug, Clone)]
pub struct LocatedInput<'a> {
    pub original: &'a str,
    pub remaining: &'a str,
    pub position: Position,
}

impl<'a> LocatedInput<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            original: input,
            remaining: input,
            position: Position::new(),
        }
    }

    pub fn advance(&mut self, consumed: &str) {
        for ch in consumed.chars() {
            self.position.advance(ch);
        }
        self.remaining = &self.remaining[consumed.len()..];
    }

    pub fn slice(&self, range: Range<usize>) -> &'a str {
        &self.original[range]
    }

    pub fn span_from(&self, start_pos: Position) -> Span {
        Span::new(start_pos, self.position)
    }

    pub fn current_span(&self, length: usize) -> Span {
        let mut end_pos = self.position;
        let slice = &self.remaining[..length.min(self.remaining.len())];
        end_pos.advance_str(slice);
        Span::new(self.position, end_pos)
    }
}

/// Parser result with span information
pub type LocatedResult<'a, O> =
    IResult<LocatedInput<'a>, (O, Span), nom::error::Error<LocatedInput<'a>>>;

/// Convert a regular parser to one that tracks location
pub fn with_location<'a, P, O>(
    mut parser: P,
) -> impl FnMut(LocatedInput<'a>) -> LocatedResult<'a, O>
where
    P: FnMut(&'a str) -> IResult<&'a str, O>,
{
    move |mut input: LocatedInput<'a>| {
        let start_pos = input.position;
        match parser(input.remaining) {
            Ok((remaining, output)) => {
                let consumed_len = input.remaining.len() - remaining.len();
                let consumed = &input.remaining[..consumed_len];

                let span = Span::new(start_pos, {
                    let mut end_pos = start_pos;
                    end_pos.advance_str(consumed);
                    end_pos
                });

                input.advance(consumed);
                Ok((input, (output, span)))
            }
            Err(e) => Err(e.map(|err| nom::error::Error {
                input: LocatedInput {
                    original: input.original,
                    remaining: err.input,
                    position: start_pos,
                },
                code: err.code,
            })),
        }
    }
}

/// Helper to create a diagnostic from a parse error
pub fn parse_error_diagnostic(
    code: &str,
    message: &str,
    span: Span,
    filename: &'static str,
) -> crate::diagnostics::Diagnostic {
    use crate::diagnostics::{SourceLocation, SourceSpan as DiagSpan};

    let start_loc = SourceLocation::new(
        filename,
        span.start.line,
        span.start.column,
        span.start.offset,
    );
    let end_loc = SourceLocation::new(filename, span.end.line, span.end.column, span.end.offset);
    let diag_span = DiagSpan::new(start_loc, end_loc);

    crate::diagnostics::Diagnostic::error(code, message.to_string())
        .with_span(diag_span)
        .with_context(format!("at {}", diag_span.format()))
}

/// Calculate line and column from offset
pub fn position_from_offset(source: &str, offset: usize) -> Position {
    let mut pos = Position::new();
    let mut current_offset = 0;

    for ch in source.chars() {
        if current_offset >= offset {
            break;
        }
        pos.advance(ch);
        current_offset += ch.len_utf8();
    }

    pos
}

/// Extract a line from source for error context
pub fn extract_line(source: &str, line_num: usize) -> Option<&str> {
    source.lines().nth(line_num - 1)
}

/// Create a context string showing the error location
pub fn create_context(source: &str, span: Span) -> String {
    if span.start.line == 0 || span.end.line == 0 {
        return String::new();
    }

    let mut context = String::new();

    // Show the line with error
    if let Some(line) = extract_line(source, span.start.line) {
        context.push_str(line);
        context.push('\n');

        // Add caret indicator
        if span.start.column > 0 && span.start.column <= line.len() + 1 {
            let spaces = " ".repeat(span.start.column - 1);
            let carets = "^".repeat(if span.start.line == span.end.line {
                std::cmp::max(1, span.end.column.saturating_sub(span.start.column))
            } else {
                1
            });
            context.push_str(&format!("{}{}", spaces, carets));
        }
    }

    context
}
