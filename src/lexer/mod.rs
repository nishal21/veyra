//! Veyra Lexer - High-Performance Tokenization
//! 
//! Uses the `logos` crate for blazing-fast lexical analysis.
//! Tokenizes Veyra source code into a stream of tokens.

mod tokens;

pub use tokens::{Token, TokenKind};

use crate::error::{VeyraError, Span};
use logos::Logos;

/// High-performance lexer for Veyra source code
pub struct Lexer<'a> {
    source: &'a str,
    lexer: logos::Lexer<'a, TokenKind>,
    line: usize,
    column: usize,
    last_newline_pos: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source code
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            lexer: TokenKind::lexer(source),
            line: 1,
            column: 1,
            last_newline_pos: 0,
        }
    }
    
    /// Tokenize the entire source into a vector of tokens
    pub fn tokenize(&mut self) -> Result<Vec<Token>, VeyraError> {
        let mut tokens = Vec::with_capacity(1024);
        
        while let Some(kind_result) = self.lexer.next() {
            let span = self.lexer.span();
            
            // Update line and column tracking
            self.update_position(span.start);
            
            let token_span = Span::new(
                span.start,
                span.end,
                self.line,
                self.column,
            );
            
            match kind_result {
                Ok(kind) => {
                    let slice = self.lexer.slice();
                    let token = Token::new(kind, slice.to_string(), token_span);
                    tokens.push(token);
                }
                Err(_) => {
                    return Err(VeyraError::LexerError {
                        message: format!("Unexpected character: '{}'", &self.source[span.start..span.end]),
                        span: token_span,
                        source: self.source.to_string(),
                    });
                }
            }
        }
        
        // Add EOF token
        let eof_span = Span::new(self.source.len(), self.source.len(), self.line, self.column);
        tokens.push(Token::new(TokenKind::Eof, String::new(), eof_span));
        
        Ok(tokens)
    }
    
    fn update_position(&mut self, pos: usize) {
        // Count newlines between last position and current
        let slice = &self.source[self.last_newline_pos..pos];
        for (i, c) in slice.char_indices() {
            if c == '\n' {
                self.line += 1;
                self.last_newline_pos = self.last_newline_pos + i + 1;
                self.column = 1;
            }
        }
        self.column = pos - self.last_newline_pos + 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_hello_world() {
        let source = r#"println("Hello, World!");"#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(tokens.len() > 0);
        assert_eq!(tokens[0].kind, TokenKind::Identifier);
    }
    
    #[test]
    fn test_function() {
        let source = r#"
            fn main() {
                let x = 42;
                println(x);
            }
        "#;
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Fn));
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Let));
    }
    
    #[test]
    fn test_operators() {
        let source = "a + b - c * d / e == f != g < h > i <= j >= k && l || m";
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Plus));
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Minus));
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Star));
        assert!(tokens.iter().any(|t| t.kind == TokenKind::Slash));
    }
}
