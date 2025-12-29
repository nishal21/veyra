//! Veyra Programming Language
//! 
//! The fastest, simplest, and most loved programming language in the world.
//! 
//! # Architecture
//! 
//! ```text
//! Source Code → Lexer → Parser → Type Checker → Code Generator → Native Binary
//! ```

pub mod lexer;
pub mod parser;
pub mod typechecker;
pub mod codegen;
pub mod error;
pub mod stdlib;
pub mod runtime;
pub mod interpreter;

pub use lexer::Lexer;
pub use parser::Parser;
pub use typechecker::TypeChecker;
pub use codegen::CodeGenerator;
pub use interpreter::Interpreter;

use inkwell::context::Context;

/// Veyra version
pub const VERSION: &str = "1.0.0";

/// Compile Veyra source code to an executable
pub fn compile(source: &str, output_path: &str) -> Result<(), error::VeyraError> {
    // Lexical analysis
    let tokens = Lexer::new(source).tokenize()?;
    
    // Parsing
    let ast = Parser::new(tokens).parse()?;
    
    // Type checking
    let typed_ast = TypeChecker::new().check(ast)?;
    
    // Code generation
    let context = Context::create();
    let mut codegen = CodeGenerator::new(&context, "veyra_main");
    codegen.generate(typed_ast, output_path)?;
    
    Ok(())
}

/// Run Veyra source code directly (interpreter mode)
pub fn run(source: &str) -> Result<i32, error::VeyraError> {
    // Lexical analysis
    let tokens = Lexer::new(source).tokenize()?;
    
    // Parsing
    let ast = Parser::new(tokens).parse()?;
    
    // Type checking
    let typed_ast = TypeChecker::new().check(ast)?;
    
    // Interpret
    let mut interpreter = Interpreter::new();
    interpreter.run(typed_ast)?;
    
    Ok(0)
}

