//! Veyra Error Types
//! 
//! Beautiful, helpful error messages that guide developers to fix issues.

use colored::*;
use std::fmt;

/// Location in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub column: usize,
}

impl Span {
    pub fn new(start: usize, end: usize, line: usize, column: usize) -> Self {
        Self { start, end, line, column }
    }
}

/// Veyra error types
#[derive(Debug)]
pub enum VeyraError {
    /// Lexer error - invalid character or token
    LexerError {
        message: String,
        span: Span,
        source: String,
    },
    
    /// Parser error - syntax error
    ParseError {
        message: String,
        span: Span,
        source: String,
        expected: Option<String>,
    },
    
    /// Type error - type mismatch
    TypeError {
        message: String,
        span: Span,
        source: String,
        expected_type: String,
        found_type: String,
    },
    
    /// Name error - undefined variable or function
    NameError {
        name: String,
        span: Span,
        source: String,
        suggestions: Vec<String>,
    },
    
    /// Runtime error
    RuntimeError {
        message: String,
    },
    
    /// IO error
    IoError(std::io::Error),
}

impl fmt::Display for VeyraError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VeyraError::LexerError { message, span, source } => {
                write_error_header(f, "Syntax Error", message)?;
                write_source_context(f, source, span)?;
                Ok(())
            }
            
            VeyraError::ParseError { message, span, source, expected } => {
                write_error_header(f, "Parse Error", message)?;
                write_source_context(f, source, span)?;
                if let Some(exp) = expected {
                    writeln!(f)?;
                    writeln!(f, "  {} Expected: {}", "help:".cyan().bold(), exp)?;
                }
                Ok(())
            }
            
            VeyraError::TypeError { message, span, source, expected_type, found_type } => {
                write_error_header(f, "Type Error", message)?;
                write_source_context(f, source, span)?;
                writeln!(f)?;
                writeln!(f, "  {} expected `{}`, found `{}`", 
                    "note:".cyan().bold(),
                    expected_type.green(),
                    found_type.red()
                )?;
                Ok(())
            }
            
            VeyraError::NameError { name, span, source, suggestions } => {
                write_error_header(f, "Name Error", &format!("undefined: `{}`", name))?;
                write_source_context(f, source, span)?;
                if !suggestions.is_empty() {
                    writeln!(f)?;
                    writeln!(f, "  {} Did you mean:", "help:".cyan().bold())?;
                    for sug in suggestions.iter().take(3) {
                        writeln!(f, "         - {}", sug.green())?;
                    }
                }
                Ok(())
            }
            
            VeyraError::RuntimeError { message } => {
                write_error_header(f, "Runtime Error", message)?;
                Ok(())
            }
            
            VeyraError::IoError(e) => {
                write_error_header(f, "IO Error", &e.to_string())?;
                Ok(())
            }
        }
    }
}

fn write_error_header(f: &mut fmt::Formatter<'_>, kind: &str, message: &str) -> fmt::Result {
    writeln!(f, "{}{} {}", kind.red().bold(), ":".red().bold(), message.bright_white())
}

fn write_source_context(f: &mut fmt::Formatter<'_>, source: &str, span: &Span) -> fmt::Result {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = span.line.saturating_sub(1);
    
    if line_idx < lines.len() {
        let line = lines[line_idx];
        let line_num = span.line.to_string();
        let padding = " ".repeat(line_num.len());
        
        writeln!(f, "  {} {}:{}:{}", 
            "-->".bright_blue().bold(),
            "source.veyra",
            span.line,
            span.column
        )?;
        writeln!(f, "   {}", "|".bright_blue().bold())?;
        writeln!(f, " {} {} {}", 
            line_num.bright_blue().bold(),
            "|".bright_blue().bold(),
            line
        )?;
        
        // Underline the error
        let underline_start = span.column.saturating_sub(1);
        let underline_len = (span.end - span.start).max(1);
        let underline = format!("{}{}",
            " ".repeat(underline_start),
            "^".repeat(underline_len)
        );
        writeln!(f, "   {} {}", 
            "|".bright_blue().bold(),
            underline.red().bold()
        )?;
    }
    
    Ok(())
}

impl std::error::Error for VeyraError {}

impl From<std::io::Error> for VeyraError {
    fn from(e: std::io::Error) -> Self {
        VeyraError::IoError(e)
    }
}
