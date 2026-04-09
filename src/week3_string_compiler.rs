//! Week 3: String-based Identity Compiler
//! 
//! A simplified compiler that takes Zeta code as a string and returns
//! an identity function. This is the foundation for the Week 3 implementation
//! of v0.3.55.

use std::collections::HashMap;

/// Simple token types for the string-based compiler
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Number(i64),
    StringLiteral(String),
    Keyword(Keyword),
    Operator(Operator),
    Punctuation(Punctuation),
    Whitespace,
    Comment,
    Eof,
}

/// Keywords in Zeta
#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Fn,
    Let,
    Mut,
    Return,
    If,
    Else,
    While,
    For,
    In,
    Match,
    Struct,
    Enum,
    Impl,
    Trait,
    Type,
    Pub,
    Use,
    Mod,
    As,
    Where,
    Const,
    Static,
    Extern,
    Unsafe,
    Async,
    Await,
    Yield,
    Move,
    Ref,
    Box,
    Dyn,
}

/// Operators in Zeta
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,       // +
    Minus,      // -
    Star,       // *
    Slash,      // /
    Percent,    // %
    Caret,      // ^
    Not,        // !
    And,        // &&
    Or,         // ||
    Eq,         // ==
    Ne,         // !=
    Lt,         // <
    Le,         // <=
    Gt,         // >
    Ge,         // >=
    Assign,     // =
    PlusEq,     // +=
    MinusEq,    // -=
    StarEq,     // *=
    SlashEq,    // /=
    PercentEq,  // %=
    CaretEq,    // ^=
    AndEq,      // &=
    OrEq,       // |=
    Shl,        // <<
    Shr,        // >>
    ShlEq,      // <<=
    ShrEq,      // >>=
    Dot,        // .
    Range,      // ..
    RangeEq,    // ..=
    Question,   // ?
    Colon,      // :
    PathSep,    // ::
    Arrow,      // ->
    FatArrow,   // =>
}

/// Punctuation in Zeta
#[derive(Debug, Clone, PartialEq)]
pub enum Punctuation {
    Comma,      // ,
    Semicolon,  // ;
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    LBracket,   // [
    RBracket,   // ]
    At,         // @
    Pound,      // #
    Dollar,     // $
    Underscore, // _
    Backtick,   // `
}

/// Simple lexer for Zeta code
pub struct Lexer {
    input: String,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    /// Create a new lexer from input string
    pub fn new(input: String) -> Self {
        Self {
            input,
            position: 0,
            line: 1,
            column: 1,
        }
    }
    
    /// Get the next character without advancing
    fn peek(&self) -> Option<char> {
        self.input.chars().nth(self.position)
    }
    
    /// Get the next character and advance
    fn next(&mut self) -> Option<char> {
        let ch = self.peek();
        if let Some(ch) = ch {
            self.position += ch.len_utf8();
            if ch == '\n' {
                self.line += 1;
                self.column = 1;
            } else {
                self.column += 1;
            }
        }
        ch
    }
    
    /// Skip whitespace and comments
    fn skip_whitespace_and_comments(&mut self) {
        while let Some(ch) = self.peek() {
            if ch.is_whitespace() {
                self.next();
            } else if ch == '/' {
                let start_pos = self.position;
                self.next();
                if let Some(next_ch) = self.peek() {
                    if next_ch == '/' {
                        // Line comment
                        self.next();
                        while let Some(ch) = self.peek() {
                            if ch == '\n' {
                                break;
                            }
                            self.next();
                        }
                    } else if next_ch == '*' {
                        // Block comment
                        self.next();
                        let mut depth = 1;
                        while depth > 0 {
                            match self.next() {
                                Some('*') => {
                                    if let Some('/') = self.peek() {
                                        self.next();
                                        depth -= 1;
                                    }
                                }
                                Some('/') => {
                                    if let Some('*') = self.peek() {
                                        self.next();
                                        depth += 1;
                                    }
                                }
                                Some(_) => {}
                                None => break, // Unterminated comment
                            }
                        }
                    } else {
                        // Not a comment, backtrack
                        self.position = start_pos;
                        self.column -= 1;
                        break;
                    }
                } else {
                    // End of input after '/'
                    break;
                }
            } else {
                break;
            }
        }
    }
    
    /// Lex an identifier or keyword
    fn lex_ident_or_keyword(&mut self, first_char: char) -> Token {
        let mut ident = String::new();
        ident.push(first_char);
        
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.next();
            } else {
                break;
            }
        }
        
        // Check if it's a keyword
        match ident.as_str() {
            "fn" => Token::Keyword(Keyword::Fn),
            "let" => Token::Keyword(Keyword::Let),
            "mut" => Token::Keyword(Keyword::Mut),
            "return" => Token::Keyword(Keyword::Return),
            "if" => Token::Keyword(Keyword::If),
            "else" => Token::Keyword(Keyword::Else),
            "while" => Token::Keyword(Keyword::While),
            "for" => Token::Keyword(Keyword::For),
            "in" => Token::Keyword(Keyword::In),
            "match" => Token::Keyword(Keyword::Match),
            "struct" => Token::Keyword(Keyword::Struct),
            "enum" => Token::Keyword(Keyword::Enum),
            "impl" => Token::Keyword(Keyword::Impl),
            "trait" => Token::Keyword(Keyword::Trait),
            "type" => Token::Keyword(Keyword::Type),
            "pub" => Token::Keyword(Keyword::Pub),
            "use" => Token::Keyword(Keyword::Use),
            "mod" => Token::Keyword(Keyword::Mod),
            "as" => Token::Keyword(Keyword::As),
            "where" => Token::Keyword(Keyword::Where),
            "const" => Token::Keyword(Keyword::Const),
            "static" => Token::Keyword(Keyword::Static),
            "extern" => Token::Keyword(Keyword::Extern),
            "unsafe" => Token::Keyword(Keyword::Unsafe),
            "async" => Token::Keyword(Keyword::Async),
            "await" => Token::Keyword(Keyword::Await),
            "yield" => Token::Keyword(Keyword::Yield),
            "move" => Token::Keyword(Keyword::Move),
            "ref" => Token::Keyword(Keyword::Ref),
            "box" => Token::Keyword(Keyword::Box),
            "dyn" => Token::Keyword(Keyword::Dyn),
            _ => Token::Ident(ident),
        }
    }
    
    /// Lex a number
    fn lex_number(&mut self, first_char: char) -> Token {
        let mut num_str = String::new();
        num_str.push(first_char);
        
        while let Some(ch) = self.peek() {
            if ch.is_digit(10) || ch == '_' {
                if ch != '_' {
                    num_str.push(ch);
                }
                self.next();
            } else {
                break;
            }
        }
        
        // Parse as i64 (simplified - no floats, hex, octal, binary for now)
        if let Ok(num) = num_str.parse::<i64>() {
            Token::Number(num)
        } else {
            // Should not happen since we only collected digits
            Token::Number(0)
        }
    }
    
    /// Lex a string literal
    fn lex_string_literal(&mut self) -> Result<Token, String> {
        let mut string = String::new();
        self.next(); // Skip opening quote
        
        while let Some(ch) = self.next() {
            match ch {
                '"' => return Ok(Token::StringLiteral(string)),
                '\\' => {
                    if let Some(escaped) = self.next() {
                        match escaped {
                            'n' => string.push('\n'),
                            'r' => string.push('\r'),
                            't' => string.push('\t'),
                            '\\' => string.push('\\'),
                            '"' => string.push('"'),
                            '0' => string.push('\0'),
                            _ => {
                                string.push('\\');
                                string.push(escaped);
                            }
                        }
                    } else {
                        return Err("Unterminated escape sequence".to_string());
                    }
                }
                _ => string.push(ch),
            }
        }
        
        Err("Unterminated string literal".to_string())
    }
    
    /// Lex an operator or punctuation
    fn lex_operator_or_punctuation(&mut self, first_char: char) -> Token {
        match first_char {
            '+' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::PlusEq)
                } else {
                    Token::Operator(Operator::Plus)
                }
            }
            '-' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::MinusEq)
                } else if let Some('>') = self.peek() {
                    self.next();
                    Token::Operator(Operator::Arrow)
                } else {
                    Token::Operator(Operator::Minus)
                }
            }
            '*' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::StarEq)
                } else {
                    Token::Operator(Operator::Star)
                }
            }
            '/' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::SlashEq)
                } else {
                    Token::Operator(Operator::Slash)
                }
            }
            '%' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::PercentEq)
                } else {
                    Token::Operator(Operator::Percent)
                }
            }
            '^' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::CaretEq)
                } else {
                    Token::Operator(Operator::Caret)
                }
            }
            '!' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::Ne)
                } else {
                    Token::Operator(Operator::Not)
                }
            }
            '=' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::Eq)
                } else if let Some('>') = self.peek() {
                    self.next();
                    Token::Operator(Operator::FatArrow)
                } else {
                    Token::Operator(Operator::Assign)
                }
            }
            '<' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::Le)
                } else if let Some('<') = self.peek() {
                    self.next();
                    if let Some('=') = self.peek() {
                        self.next();
                        Token::Operator(Operator::ShlEq)
                    } else {
                        Token::Operator(Operator::Shl)
                    }
                } else {
                    Token::Operator(Operator::Lt)
                }
            }
            '>' => {
                if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::Ge)
                } else if let Some('>') = self.peek() {
                    self.next();
                    if let Some('=') = self.peek() {
                        self.next();
                        Token::Operator(Operator::ShrEq)
                    } else {
                        Token::Operator(Operator::Shr)
                    }
                } else {
                    Token::Operator(Operator::Gt)
                }
            }
            '&' => {
                if let Some('&') = self.peek() {
                    self.next();
                    Token::Operator(Operator::And)
                } else if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::AndEq)
                } else {
                    Token::Punctuation(Punctuation::At) // Actually & but we'll use At for now
                }
            }
            '|' => {
                if let Some('|') = self.peek() {
                    self.next();
                    Token::Operator(Operator::Or)
                } else if let Some('=') = self.peek() {
                    self.next();
                    Token::Operator(Operator::OrEq)
                } else {
                    Token::Punctuation(Punctuation::At) // Actually | but we'll use At for now
                }
            }
            '.' => {
                if let Some('.') = self.peek() {
                    self.next();
                    if let Some('=') = self.peek() {
                        self.next();
                        Token::Operator(Operator::RangeEq)
                    } else {
                        Token::Operator(Operator::Range)
                    }
                } else {
                    Token::Operator(Operator::Dot)
                }
            }
            ':' => {
                if let Some(':') = self.peek() {
                    self.next();
                    Token::Operator(Operator::PathSep)
                } else {
                    Token::Operator(Operator::Colon)
                }
            }
            '?' => Token::Operator(Operator::Question),
            ',' => Token::Punctuation(Punctuation::Comma),
            ';' => Token::Punctuation(Punctuation::Semicolon),
            '(' => Token::Punctuation(Punctuation::LParen),
            ')' => Token::Punctuation(Punctuation::RParen),
            '{' => Token::Punctuation(Punctuation::LBrace),
            '}' => Token::Punctuation(Punctuation::RBrace),
            '[' => Token::Punctuation(Punctuation::LBracket),
            ']' => Token::Punctuation(Punctuation::RBracket),
            '@' => Token::Punctuation(Punctuation::At),
            '#' => Token::Punctuation(Punctuation::Pound),
            '$' => Token::Punctuation(Punctuation::Dollar),
            '_' => Token::Punctuation(Punctuation::Underscore),
            '`' => Token::Punctuation(Punctuation::Backtick),
            _ => Token::Ident(first_char.to_string()), // Fallback
        }
    }
    
    /// Get the next token
    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace_and_comments();
        
        match self.peek() {
            Some(ch) => {
                if ch.is_alphabetic() || ch == '_' {
                    Ok(self.lex_ident_or_keyword(ch))
                } else if ch.is_digit(10) {
                    Ok(self.lex_number(ch))
                } else if ch == '"' {
                    self.lex_string_literal()
                } else {
                    Ok(self.lex_operator_or_punctuation(ch))
                }
            }
            None => Ok(Token::Eof),
        }
    }
    
    /// Lex all tokens from input
    pub fn lex_all(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            match token {
                Token::Eof => break,
                _ => tokens.push(token),
            }
        }
        Ok(tokens)
    }
}

/// Simple parser for Zeta code
pub struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

/// AST node for the simple compiler
#[derive(Debug, Clone)]
pub enum AstNode {
    Ident(String),
    Number(i64),
    StringLiteral(String),
    BinaryOp(Box<AstNode>, String, Box<AstNode>),
    FunctionDef {
        name: String,
        params: Vec<String>,
        body: Box<AstNode>,
    },
    Block(Vec<AstNode>),
    Return(Box<AstNode>),
    Call {
        callee: String,
        args: Vec<AstNode>,
    },
}

impl Parser {
    /// Create a new parser from tokens
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            position: 0,
        }
    }
    
    /// Peek at the current token
    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.position)
    }
    
    /// Consume the current token
    fn consume(&mut self) -> Option<&Token> {
        let token = self.peek();
        if token.is_some() {
            self.position += 1;
        }
        token
    }
    
    /// Expect a specific token
    fn expect(&mut self, expected: &Token) -> Result<(), String> {
        match self.peek() {
            Some(token) if token == expected => {
                self.consume();
                Ok(())
            }
            Some(token) => Err(format!("Expected {:?}, found {:?}", expected, token)),
            None => Err(format!("Expected {:?}, found EOF", expected)),
        }
    }
    
    /// Parse an identifier
    fn parse_ident(&mut self) -> Result<AstNode, String> {
        match self.consume() {
            Some(Token::Ident(name)) => Ok(AstNode::Ident(name.clone())),
            Some(token) => Err(format!("Expected identifier, found {:?}", token)),
            None => Err("Expected identifier, found EOF