//! Veyra Parser - Syntax Analysis
//! 
//! Transforms tokens into an Abstract Syntax Tree (AST).

mod ast;

pub use ast::*;

use crate::error::{VeyraError, Span};
use crate::lexer::{Token, TokenKind};

/// Parser for Veyra source code
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    source: String,
}

impl Parser {
    /// Create a new parser from tokens
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            source: String::new(),
        }
    }
    
    /// Set the source code for error messages
    pub fn with_source(mut self, source: &str) -> Self {
        self.source = source.to_string();
        self
    }
    
    /// Parse tokens into an AST
    pub fn parse(&mut self) -> Result<Program, VeyraError> {
        let mut functions = Vec::new();
        let mut classes = Vec::new();
        let mut statements = Vec::new();
        let mut imports = Vec::new();
        
        while !self.is_at_end() {
            // Skip newlines
            self.skip_newlines();
            
            if self.is_at_end() {
                break;
            }
            
            match self.peek().kind {
                TokenKind::Fn => {
                    functions.push(self.parse_function()?);
                }
                TokenKind::Class => {
                    classes.push(self.parse_class()?);
                }
                TokenKind::Struct => {
                    classes.push(self.parse_struct()?);
                }
                TokenKind::Import => {
                    imports.push(self.parse_import()?);
                }
                TokenKind::Pub => {
                    self.advance();
                    if self.check(TokenKind::Fn) {
                        let mut func = self.parse_function()?;
                        func.is_public = true;
                        functions.push(func);
                    } else if self.check(TokenKind::Class) {
                        let mut cls = self.parse_class()?;
                        cls.is_public = true;
                        classes.push(cls);
                    }
                }
                _ => {
                    statements.push(self.parse_statement()?);
                }
            }
        }
        
        Ok(Program {
            imports,
            functions,
            classes,
            statements,
        })
    }
    
    // ==================== DECLARATIONS ====================
    
    fn parse_function(&mut self) -> Result<Function, VeyraError> {
        self.expect(TokenKind::Fn)?;
        
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.value.clone();
        
        self.expect(TokenKind::LParen)?;
        let params = self.parse_parameters()?;
        self.expect(TokenKind::RParen)?;
        
        // Optional return type
        let return_type = if self.check(TokenKind::Arrow) {
            self.advance();
            self.parse_type()?
        } else {
            Type::Void
        };
        
        let body = self.parse_block()?;
        
        Ok(Function {
            name,
            params,
            return_type,
            body,
            is_public: false,
            is_async: false,
        })
    }
    
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, VeyraError> {
        let mut params = Vec::new();
        
        if self.check(TokenKind::RParen) {
            return Ok(params);
        }
        
        loop {
            let name_token = self.expect(TokenKind::Identifier)?;
            let name = name_token.value.clone();
            
            // Optional type annotation
            let ty = if self.check(TokenKind::Colon) {
                self.advance();
                self.parse_type()?
            } else {
                Type::Inferred
            };
            
            params.push(Parameter { name, ty });
            
            if !self.check(TokenKind::Comma) {
                break;
            }
            self.advance();
        }
        
        Ok(params)
    }
    
    fn parse_type(&mut self) -> Result<Type, VeyraError> {
        let token = self.advance();
        
        match token.kind {
            TokenKind::TypeInt | TokenKind::TypeI32 => Ok(Type::Int),
            TokenKind::TypeI8 => Ok(Type::I8),
            TokenKind::TypeI16 => Ok(Type::I16),
            TokenKind::TypeI64 => Ok(Type::I64),
            TokenKind::TypeU8 => Ok(Type::U8),
            TokenKind::TypeU16 => Ok(Type::U16),
            TokenKind::TypeU32 => Ok(Type::U32),
            TokenKind::TypeU64 => Ok(Type::U64),
            TokenKind::TypeFloat | TokenKind::TypeF64 => Ok(Type::Float),
            TokenKind::TypeF32 => Ok(Type::F32),
            TokenKind::TypeString => Ok(Type::String),
            TokenKind::TypeBool => Ok(Type::Bool),
            TokenKind::TypeVoid => Ok(Type::Void),
            TokenKind::TypeAny => Ok(Type::Any),
            TokenKind::Identifier => {
                // User-defined type
                Ok(Type::Custom(token.value.clone()))
            }
            TokenKind::LBracket => {
                // Array type: [int]
                let element_type = self.parse_type()?;
                self.expect(TokenKind::RBracket)?;
                Ok(Type::Array(Box::new(element_type)))
            }
            _ => Err(VeyraError::ParseError {
                message: format!("Expected type, found '{}'", token.kind.name()),
                span: token.span,
                source: self.source.clone(),
                expected: Some("type".to_string()),
            }),
        }
    }
    
    fn parse_class(&mut self) -> Result<Class, VeyraError> {
        self.expect(TokenKind::Class)?;
        
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.value.clone();
        
        self.expect(TokenKind::LBrace)?;
        
        let mut fields = Vec::new();
        let mut methods = Vec::new();
        
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            self.skip_newlines();
            
            if self.check(TokenKind::Fn) {
                methods.push(self.parse_function()?);
            } else if self.check(TokenKind::Let) {
                self.advance();
                let field_name = self.expect(TokenKind::Identifier)?.value.clone();
                self.expect(TokenKind::Colon)?;
                let field_type = self.parse_type()?;
                self.expect(TokenKind::Semicolon)?;
                fields.push(Field { name: field_name, ty: field_type });
            }
            
            self.skip_newlines();
        }
        
        self.expect(TokenKind::RBrace)?;
        
        Ok(Class {
            name,
            fields,
            methods,
            is_public: false,
        })
    }
    
    fn parse_struct(&mut self) -> Result<Class, VeyraError> {
        self.expect(TokenKind::Struct)?;
        
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.value.clone();
        
        self.expect(TokenKind::LBrace)?;
        
        let mut fields = Vec::new();
        
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            self.skip_newlines();
            
            if self.check(TokenKind::RBrace) {
                break;
            }
            
            let field_name = self.expect(TokenKind::Identifier)?.value.clone();
            self.expect(TokenKind::Colon)?;
            let field_type = self.parse_type()?;
            
            fields.push(Field { name: field_name, ty: field_type });
            
            if self.check(TokenKind::Comma) {
                self.advance();
            }
            
            self.skip_newlines();
        }
        
        self.expect(TokenKind::RBrace)?;
        
        Ok(Class {
            name,
            fields,
            methods: Vec::new(),
            is_public: false,
        })
    }
    
    fn parse_import(&mut self) -> Result<Import, VeyraError> {
        self.expect(TokenKind::Import)?;
        
        let mut path = Vec::new();
        
        loop {
            let token = self.expect(TokenKind::Identifier)?;
            path.push(token.value.clone());
            
            if self.check(TokenKind::Dot) {
                self.advance();
            } else {
                break;
            }
        }
        
        if self.check(TokenKind::Semicolon) {
            self.advance();
        }
        
        Ok(Import { path })
    }
    
    // ==================== STATEMENTS ====================
    
    fn parse_block(&mut self) -> Result<Block, VeyraError> {
        self.expect(TokenKind::LBrace)?;
        
        let mut statements = Vec::new();
        
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            self.skip_newlines();
            
            if self.check(TokenKind::RBrace) {
                break;
            }
            
            statements.push(self.parse_statement()?);
        }
        
        self.expect(TokenKind::RBrace)?;
        
        Ok(Block { statements })
    }
    
    fn parse_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.skip_newlines();
        
        let stmt = match self.peek().kind {
            TokenKind::Let => self.parse_let_statement()?,
            TokenKind::Const => self.parse_const_statement()?,
            TokenKind::If => self.parse_if_statement()?,
            TokenKind::While => self.parse_while_statement()?,
            TokenKind::For => self.parse_for_statement()?,
            TokenKind::Loop => self.parse_loop_statement()?,
            TokenKind::Return => self.parse_return_statement()?,
            TokenKind::Break => {
                self.advance();
                Stmt::Break
            }
            TokenKind::Continue => {
                self.advance();
                Stmt::Continue
            }
            TokenKind::Try => self.parse_try_statement()?,
            TokenKind::Match => self.parse_match_statement()?,
            _ => {
                let expr = self.parse_expression()?;
                
                // Check for assignment
                if self.check(TokenKind::Assign) {
                    self.advance();
                    let value = self.parse_expression()?;
                    Stmt::Assign { target: expr, value }
                } else if self.check(TokenKind::PlusAssign) {
                    self.advance();
                    let value = self.parse_expression()?;
                    Stmt::CompoundAssign { target: expr, op: BinaryOp::Add, value }
                } else if self.check(TokenKind::MinusAssign) {
                    self.advance();
                    let value = self.parse_expression()?;
                    Stmt::CompoundAssign { target: expr, op: BinaryOp::Sub, value }
                } else if self.check(TokenKind::StarAssign) {
                    self.advance();
                    let value = self.parse_expression()?;
                    Stmt::CompoundAssign { target: expr, op: BinaryOp::Mul, value }
                } else if self.check(TokenKind::SlashAssign) {
                    self.advance();
                    let value = self.parse_expression()?;
                    Stmt::CompoundAssign { target: expr, op: BinaryOp::Div, value }
                } else {
                    Stmt::Expr(expr)
                }
            }
        };
        
        // Optional semicolon
        if self.check(TokenKind::Semicolon) {
            self.advance();
        }
        
        Ok(stmt)
    }
    
    fn parse_let_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::Let)?;
        
        let mutable = if self.check(TokenKind::Mut) {
            self.advance();
            true
        } else {
            false
        };
        
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.value.clone();
        
        // Optional type annotation
        let ty = if self.check(TokenKind::Colon) {
            self.advance();
            self.parse_type()?
        } else {
            Type::Inferred
        };
        
        // Optional initializer
        let value = if self.check(TokenKind::Assign) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        Ok(Stmt::Let { name, ty, value, mutable })
    }
    
    fn parse_const_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::Const)?;
        
        let name_token = self.expect(TokenKind::Identifier)?;
        let name = name_token.value.clone();
        
        // Optional type annotation
        let ty = if self.check(TokenKind::Colon) {
            self.advance();
            self.parse_type()?
        } else {
            Type::Inferred
        };
        
        // Required initializer for const
        self.expect(TokenKind::Assign)?;
        let value = self.parse_expression()?;
        
        Ok(Stmt::Const { name, ty, value })
    }
    
    fn parse_if_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::If)?;
        
        let condition = self.parse_expression()?;
        let then_block = self.parse_block()?;
        
        let else_block = if self.check(TokenKind::Else) {
            self.advance();
            if self.check(TokenKind::If) {
                // else if
                Some(Block {
                    statements: vec![self.parse_if_statement()?],
                })
            } else {
                Some(self.parse_block()?)
            }
        } else {
            None
        };
        
        Ok(Stmt::If { condition, then_block, else_block })
    }
    
    fn parse_while_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::While)?;
        
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        
        Ok(Stmt::While { condition, body })
    }
    
    fn parse_for_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::For)?;
        
        let var_token = self.expect(TokenKind::Identifier)?;
        let var = var_token.value.clone();
        
        self.expect(TokenKind::In)?;
        
        let iterable = self.parse_expression()?;
        let body = self.parse_block()?;
        
        Ok(Stmt::For { var, iterable, body })
    }
    
    fn parse_loop_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::Loop)?;
        let body = self.parse_block()?;
        Ok(Stmt::Loop { body })
    }
    
    fn parse_return_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::Return)?;
        
        let value = if !self.check(TokenKind::Semicolon) && 
                       !self.check(TokenKind::RBrace) &&
                       !self.check(TokenKind::Newline) {
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        Ok(Stmt::Return(value))
    }
    
    fn parse_try_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::Try)?;
        let try_block = self.parse_block()?;
        
        self.expect(TokenKind::Catch)?;
        self.expect(TokenKind::LParen)?;
        let error_var = self.expect(TokenKind::Identifier)?.value.clone();
        self.expect(TokenKind::RParen)?;
        let catch_block = self.parse_block()?;
        
        Ok(Stmt::Try { try_block, error_var, catch_block })
    }
    
    fn parse_match_statement(&mut self) -> Result<Stmt, VeyraError> {
        self.expect(TokenKind::Match)?;
        let value = self.parse_expression()?;
        
        self.expect(TokenKind::LBrace)?;
        
        let mut arms = Vec::new();
        
        while !self.check(TokenKind::RBrace) && !self.is_at_end() {
            self.skip_newlines();
            
            if self.check(TokenKind::RBrace) {
                break;
            }
            
            let pattern = self.parse_expression()?;
            self.expect(TokenKind::FatArrow)?;
            
            let body = if self.check(TokenKind::LBrace) {
                let block = self.parse_block()?;
                Stmt::Block(block)
            } else {
                let expr = self.parse_expression()?;
                Stmt::Expr(expr)
            };
            
            arms.push(MatchArm { pattern, body });
            
            if self.check(TokenKind::Comma) {
                self.advance();
            }
            
            self.skip_newlines();
        }
        
        self.expect(TokenKind::RBrace)?;
        
        Ok(Stmt::Match { value, arms })
    }
    
    // ==================== EXPRESSIONS ====================
    
    fn parse_expression(&mut self) -> Result<Expr, VeyraError> {
        self.parse_or()
    }
    
    fn parse_or(&mut self) -> Result<Expr, VeyraError> {
        let mut left = self.parse_and()?;
        
        while self.check(TokenKind::Or) {
            self.advance();
            let right = self.parse_and()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::Or,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    fn parse_and(&mut self) -> Result<Expr, VeyraError> {
        let mut left = self.parse_equality()?;
        
        while self.check(TokenKind::And) {
            self.advance();
            let right = self.parse_equality()?;
            left = Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::And,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    fn parse_equality(&mut self) -> Result<Expr, VeyraError> {
        let mut left = self.parse_comparison()?;
        
        while self.check(TokenKind::Eq) || self.check(TokenKind::NotEq) {
            let op = if self.check(TokenKind::Eq) {
                BinaryOp::Eq
            } else {
                BinaryOp::NotEq
            };
            self.advance();
            let right = self.parse_comparison()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    fn parse_comparison(&mut self) -> Result<Expr, VeyraError> {
        let mut left = self.parse_range()?;
        
        while self.check(TokenKind::Lt) || self.check(TokenKind::Gt) ||
              self.check(TokenKind::LtEq) || self.check(TokenKind::GtEq) {
            let op = match self.peek().kind {
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::LtEq => BinaryOp::LtEq,
                TokenKind::GtEq => BinaryOp::GtEq,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_range()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    fn parse_range(&mut self) -> Result<Expr, VeyraError> {
        let left = self.parse_term()?;
        
        if self.check(TokenKind::Range) {
            self.advance();
            let right = self.parse_term()?;
            return Ok(Expr::Range {
                start: Box::new(left),
                end: Box::new(right),
            });
        }
        
        Ok(left)
    }
    
    fn parse_term(&mut self) -> Result<Expr, VeyraError> {
        let mut left = self.parse_factor()?;
        
        while self.check(TokenKind::Plus) || self.check(TokenKind::Minus) {
            let op = if self.check(TokenKind::Plus) {
                BinaryOp::Add
            } else {
                BinaryOp::Sub
            };
            self.advance();
            let right = self.parse_factor()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    fn parse_factor(&mut self) -> Result<Expr, VeyraError> {
        let mut left = self.parse_power()?;
        
        while self.check(TokenKind::Star) || self.check(TokenKind::Slash) || 
              self.check(TokenKind::Percent) {
            let op = match self.peek().kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => unreachable!(),
            };
            self.advance();
            let right = self.parse_power()?;
            left = Expr::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        
        Ok(left)
    }
    
    fn parse_power(&mut self) -> Result<Expr, VeyraError> {
        let left = self.parse_unary()?;
        
        if self.check(TokenKind::Power) {
            self.advance();
            let right = self.parse_power()?; // Right associative
            return Ok(Expr::Binary {
                left: Box::new(left),
                op: BinaryOp::Pow,
                right: Box::new(right),
            });
        }
        
        Ok(left)
    }
    
    fn parse_unary(&mut self) -> Result<Expr, VeyraError> {
        if self.check(TokenKind::Not) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                operand: Box::new(operand),
            });
        }
        
        if self.check(TokenKind::Minus) {
            self.advance();
            let operand = self.parse_unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Neg,
                operand: Box::new(operand),
            });
        }
        
        self.parse_call()
    }
    
    fn parse_call(&mut self) -> Result<Expr, VeyraError> {
        let mut expr = self.parse_primary()?;
        
        loop {
            if self.check(TokenKind::LParen) {
                // Function call
                self.advance();
                let args = self.parse_arguments()?;
                self.expect(TokenKind::RParen)?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
            } else if self.check(TokenKind::Dot) {
                // Method call or field access
                self.advance();
                let name_token = self.expect(TokenKind::Identifier)?;
                let name = name_token.value.clone();
                
                if self.check(TokenKind::LParen) {
                    self.advance();
                    let args = self.parse_arguments()?;
                    self.expect(TokenKind::RParen)?;
                    expr = Expr::MethodCall {
                        object: Box::new(expr),
                        method: name,
                        args,
                    };
                } else {
                    expr = Expr::FieldAccess {
                        object: Box::new(expr),
                        field: name,
                    };
                }
            } else if self.check(TokenKind::LBracket) {
                // Index access
                self.advance();
                let index = self.parse_expression()?;
                self.expect(TokenKind::RBracket)?;
                expr = Expr::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else {
                break;
            }
        }
        
        Ok(expr)
    }
    
    fn parse_arguments(&mut self) -> Result<Vec<Expr>, VeyraError> {
        let mut args = Vec::new();
        
        if self.check(TokenKind::RParen) {
            return Ok(args);
        }
        
        loop {
            args.push(self.parse_expression()?);
            
            if !self.check(TokenKind::Comma) {
                break;
            }
            self.advance();
        }
        
        Ok(args)
    }
    
    fn parse_primary(&mut self) -> Result<Expr, VeyraError> {
        let token = self.advance();
        
        match token.kind {
            TokenKind::IntLiteral => {
                let value: i64 = token.value.replace('_', "").parse().unwrap_or(0);
                Ok(Expr::Int(value))
            }
            TokenKind::HexLiteral => {
                let value = i64::from_str_radix(&token.value[2..].replace('_', ""), 16).unwrap_or(0);
                Ok(Expr::Int(value))
            }
            TokenKind::BinaryLiteral => {
                let value = i64::from_str_radix(&token.value[2..].replace('_', ""), 2).unwrap_or(0);
                Ok(Expr::Int(value))
            }
            TokenKind::FloatLiteral => {
                let value: f64 = token.value.replace('_', "").parse().unwrap_or(0.0);
                Ok(Expr::Float(value))
            }
            TokenKind::StringLiteral => {
                // Remove quotes
                let s = &token.value[1..token.value.len()-1];
                Ok(Expr::String(s.to_string()))
            }
            TokenKind::TemplateString => {
                // Remove backticks
                let s = &token.value[1..token.value.len()-1];
                Ok(Expr::String(s.to_string()))
            }
            TokenKind::True => Ok(Expr::Bool(true)),
            TokenKind::False => Ok(Expr::Bool(false)),
            TokenKind::Null => Ok(Expr::Null),
            TokenKind::Identifier => {
                Ok(Expr::Identifier(token.value.clone()))
            }
            TokenKind::LParen => {
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Grouped(Box::new(expr)))
            }
            TokenKind::LBracket => {
                // Array literal
                let mut elements = Vec::new();
                
                if !self.check(TokenKind::RBracket) {
                    loop {
                        elements.push(self.parse_expression()?);
                        
                        if !self.check(TokenKind::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                
                self.expect(TokenKind::RBracket)?;
                Ok(Expr::Array(elements))
            }
            TokenKind::LBrace => {
                // Object literal
                let mut fields = Vec::new();
                
                if !self.check(TokenKind::RBrace) {
                    loop {
                        self.skip_newlines();
                        
                        if self.check(TokenKind::RBrace) {
                            break;
                        }
                        
                        let key = self.expect(TokenKind::Identifier)?.value.clone();
                        self.expect(TokenKind::Colon)?;
                        let value = self.parse_expression()?;
                        fields.push((key, value));
                        
                        if !self.check(TokenKind::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                
                self.expect(TokenKind::RBrace)?;
                Ok(Expr::Object(fields))
            }
            TokenKind::New => {
                let class_name = self.expect(TokenKind::Identifier)?.value.clone();
                self.expect(TokenKind::LParen)?;
                let args = self.parse_arguments()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::New { class: class_name, args })
            }
            TokenKind::Fn => {
                // Lambda/anonymous function
                self.expect(TokenKind::LParen)?;
                let params = self.parse_parameters()?;
                self.expect(TokenKind::RParen)?;
                
                let body = if self.check(TokenKind::FatArrow) {
                    self.advance();
                    let expr = self.parse_expression()?;
                    Block {
                        statements: vec![Stmt::Return(Some(expr))],
                    }
                } else {
                    self.parse_block()?
                };
                
                Ok(Expr::Lambda { params, body })
            }
            _ => Err(VeyraError::ParseError {
                message: format!("Unexpected token: '{}'", token.kind.name()),
                span: token.span,
                source: self.source.clone(),
                expected: Some("expression".to_string()),
            }),
        }
    }
    
    // ==================== HELPERS ====================
    
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    
    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.tokens[self.current - 1].clone()
    }
    
    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().kind == kind
    }
    
    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() || self.peek().kind == TokenKind::Eof
    }
    
    fn expect(&mut self, kind: TokenKind) -> Result<Token, VeyraError> {
        if self.check(kind) {
            Ok(self.advance())
        } else {
            let token = self.peek();
            Err(VeyraError::ParseError {
                message: format!("Expected '{}', found '{}'", kind.name(), token.kind.name()),
                span: token.span,
                source: self.source.clone(),
                expected: Some(kind.name().to_string()),
            })
        }
    }
    
    fn skip_newlines(&mut self) {
        while self.check(TokenKind::Newline) {
            self.advance();
        }
    }
}
