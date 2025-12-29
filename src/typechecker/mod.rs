//! Veyra Type Checker - Semantic Analysis
//! 
//! Validates types and resolves type inference.

mod types;

pub use types::*;

use crate::error::{VeyraError, Span};
use crate::parser::{Program, Function, Class, Stmt, Expr, Type, BinaryOp, UnaryOp, Block};
use rustc_hash::FxHashMap;

/// Type environment for a scope
#[derive(Debug, Clone)]
pub struct TypeEnv {
    /// Variables in scope: name -> (type, mutable)
    variables: FxHashMap<String, (Type, bool)>,
    /// Functions in scope: name -> (param_types, return_type)
    functions: FxHashMap<String, (Vec<Type>, Type)>,
    /// Classes in scope: name -> fields
    classes: FxHashMap<String, Vec<(String, Type)>>,
    /// Parent scope
    parent: Option<Box<TypeEnv>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            variables: FxHashMap::default(),
            functions: FxHashMap::default(),
            classes: FxHashMap::default(),
            parent: None,
        }
    }
    
    pub fn with_parent(parent: TypeEnv) -> Self {
        Self {
            variables: FxHashMap::default(),
            functions: FxHashMap::default(),
            classes: FxHashMap::default(),
            parent: Some(Box::new(parent)),
        }
    }
    
    pub fn define_var(&mut self, name: String, ty: Type, mutable: bool) {
        self.variables.insert(name, (ty, mutable));
    }
    
    pub fn define_fn(&mut self, name: String, params: Vec<Type>, return_type: Type) {
        self.functions.insert(name, (params, return_type));
    }
    
    pub fn define_class(&mut self, name: String, fields: Vec<(String, Type)>) {
        self.classes.insert(name, fields);
    }
    
    pub fn get_var(&self, name: &str) -> Option<&(Type, bool)> {
        self.variables.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.get_var(name))
        })
    }
    
    pub fn get_fn(&self, name: &str) -> Option<&(Vec<Type>, Type)> {
        self.functions.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.get_fn(name))
        })
    }
    
    pub fn get_class(&self, name: &str) -> Option<&Vec<(String, Type)>> {
        self.classes.get(name).or_else(|| {
            self.parent.as_ref().and_then(|p| p.get_class(name))
        })
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// Type checker for Veyra programs
pub struct TypeChecker {
    env: TypeEnv,
    current_return_type: Option<Type>,
    source: String,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut env = TypeEnv::new();
        
        // Register built-in functions
        env.define_fn("println".to_string(), vec![Type::Any], Type::Void);
        env.define_fn("print".to_string(), vec![Type::Any], Type::Void);
        env.define_fn("len".to_string(), vec![Type::Any], Type::Int);
        env.define_fn("type_of".to_string(), vec![Type::Any], Type::String);
        env.define_fn("to_string".to_string(), vec![Type::Any], Type::String);
        env.define_fn("to_int".to_string(), vec![Type::Any], Type::Int);
        env.define_fn("to_float".to_string(), vec![Type::Any], Type::Float);
        env.define_fn("input".to_string(), vec![], Type::String);
        env.define_fn("read_file".to_string(), vec![Type::String], Type::String);
        env.define_fn("write_file".to_string(), vec![Type::String, Type::String], Type::Void);
        env.define_fn("http_get".to_string(), vec![Type::String], Type::String);
        env.define_fn("http_post".to_string(), vec![Type::String, Type::String], Type::String);
        env.define_fn("json_parse".to_string(), vec![Type::String], Type::Any);
        env.define_fn("json_stringify".to_string(), vec![Type::Any], Type::String);
        env.define_fn("rand".to_string(), vec![], Type::Float);
        env.define_fn("rand_int".to_string(), vec![Type::Int, Type::Int], Type::Int);
        env.define_fn("sqrt".to_string(), vec![Type::Float], Type::Float);
        env.define_fn("pow".to_string(), vec![Type::Float, Type::Float], Type::Float);
        env.define_fn("sin".to_string(), vec![Type::Float], Type::Float);
        env.define_fn("cos".to_string(), vec![Type::Float], Type::Float);
        env.define_fn("abs".to_string(), vec![Type::Float], Type::Float);
        env.define_fn("floor".to_string(), vec![Type::Float], Type::Int);
        env.define_fn("ceil".to_string(), vec![Type::Float], Type::Int);
        env.define_fn("round".to_string(), vec![Type::Float], Type::Int);
        env.define_fn("min".to_string(), vec![Type::Any, Type::Any], Type::Any);
        env.define_fn("max".to_string(), vec![Type::Any, Type::Any], Type::Any);
        env.define_fn("sleep".to_string(), vec![Type::Int], Type::Void);
        env.define_fn("now".to_string(), vec![], Type::Int);
        env.define_fn("exit".to_string(), vec![Type::Int], Type::Void);
        env.define_fn("assert".to_string(), vec![Type::Bool], Type::Void);
        env.define_fn("panic".to_string(), vec![Type::String], Type::Void);
        
        // Web/server functions
        env.define_fn("web_serve".to_string(), vec![Type::Any, Type::Int], Type::Void);
        env.define_fn("create_page".to_string(), vec![Type::String, Type::String, Type::String], Type::String);
        env.define_fn("html_element".to_string(), vec![Type::String, Type::String, Type::Any], Type::String);
        env.define_fn("route".to_string(), vec![Type::String, Type::Any], Type::Void);
        env.define_fn("css_style".to_string(), vec![Type::String, Type::String], Type::String);
        
        // Array functions
        env.define_fn("push".to_string(), vec![Type::Any, Type::Any], Type::Void);
        env.define_fn("pop".to_string(), vec![Type::Any], Type::Any);
        env.define_fn("shift".to_string(), vec![Type::Any], Type::Any);
        env.define_fn("slice".to_string(), vec![Type::Any, Type::Int, Type::Int], Type::Any);
        env.define_fn("join".to_string(), vec![Type::Any, Type::String], Type::String);
        env.define_fn("split".to_string(), vec![Type::String, Type::String], Type::Array(Box::new(Type::String)));
        env.define_fn("map".to_string(), vec![Type::Any, Type::Any], Type::Any);
        env.define_fn("filter".to_string(), vec![Type::Any, Type::Any], Type::Any);
        env.define_fn("reduce".to_string(), vec![Type::Any, Type::Any, Type::Any], Type::Any);
        
        // Concurrency
        env.define_fn("channel".to_string(), vec![], Type::Any);
        env.define_fn("send".to_string(), vec![Type::Any, Type::Any], Type::Void);
        env.define_fn("receive".to_string(), vec![Type::Any], Type::Any);
        
        // Matrix/AI
        env.define_fn("matrix_multiply".to_string(), vec![Type::Any, Type::Any], Type::Any);
        env.define_fn("relu".to_string(), vec![Type::Float], Type::Float);
        
        // String operations
        env.define_fn("replace".to_string(), vec![Type::String, Type::String, Type::String], Type::String);
        env.define_fn("trim".to_string(), vec![Type::String], Type::String);
        env.define_fn("upper".to_string(), vec![Type::String], Type::String);
        env.define_fn("lower".to_string(), vec![Type::String], Type::String);
        env.define_fn("contains".to_string(), vec![Type::String, Type::String], Type::Bool);
        env.define_fn("starts_with".to_string(), vec![Type::String, Type::String], Type::Bool);
        env.define_fn("ends_with".to_string(), vec![Type::String, Type::String], Type::Bool);
        
        Self {
            env,
            current_return_type: None,
            source: String::new(),
        }
    }
    
    pub fn with_source(mut self, source: &str) -> Self {
        self.source = source.to_string();
        self
    }
    
    /// Type check a program
    pub fn check(&mut self, program: Program) -> Result<TypedProgram, VeyraError> {
        // First pass: register all function signatures
        for func in &program.functions {
            let param_types: Vec<Type> = func.params.iter().map(|p| p.ty.clone()).collect();
            self.env.define_fn(func.name.clone(), param_types, func.return_type.clone());
        }
        
        // Second pass: register all classes
        for class in &program.classes {
            let fields: Vec<(String, Type)> = class.fields.iter()
                .map(|f| (f.name.clone(), f.ty.clone()))
                .collect();
            self.env.define_class(class.name.clone(), fields);
        }
        
        // Third pass: type check function bodies
        let mut typed_functions = Vec::new();
        for func in program.functions {
            typed_functions.push(self.check_function(func)?);
        }
        
        // Type check classes
        let mut typed_classes = Vec::new();
        for class in program.classes {
            typed_classes.push(self.check_class(class)?);
        }
        
        // Type check top-level statements
        let mut typed_statements = Vec::new();
        for stmt in program.statements {
            typed_statements.push(self.check_statement(stmt)?);
        }
        
        Ok(TypedProgram {
            imports: program.imports,
            functions: typed_functions,
            classes: typed_classes,
            statements: typed_statements,
        })
    }
    
    fn check_function(&mut self, func: Function) -> Result<TypedFunction, VeyraError> {
        // Create new scope - clone first to avoid borrow issue
        let parent_env = self.env.clone();
        let old_env = std::mem::replace(&mut self.env, TypeEnv::with_parent(parent_env));
        
        // Add parameters to scope
        for param in &func.params {
            self.env.define_var(param.name.clone(), param.ty.clone(), false);
        }
        
        // Set expected return type
        self.current_return_type = Some(func.return_type.clone());
        
        // Check body
        let typed_body = self.check_block(func.body)?;
        
        // Restore scope
        self.env = old_env;
        self.current_return_type = None;
        
        Ok(TypedFunction {
            name: func.name,
            params: func.params,
            return_type: func.return_type,
            body: typed_body,
            is_public: func.is_public,
            is_async: func.is_async,
        })
    }
    
    fn check_class(&mut self, class: Class) -> Result<TypedClass, VeyraError> {
        let mut typed_methods = Vec::new();
        
        for method in class.methods {
            typed_methods.push(self.check_function(method)?);
        }
        
        Ok(TypedClass {
            name: class.name,
            fields: class.fields,
            methods: typed_methods,
            is_public: class.is_public,
        })
    }
    
    fn check_block(&mut self, block: Block) -> Result<TypedBlock, VeyraError> {
        let mut typed_stmts = Vec::new();
        
        for stmt in block.statements {
            typed_stmts.push(self.check_statement(stmt)?);
        }
        
        Ok(TypedBlock { statements: typed_stmts })
    }
    
    fn check_statement(&mut self, stmt: Stmt) -> Result<TypedStmt, VeyraError> {
        match stmt {
            Stmt::Let { name, ty, value, mutable } => {
                let (typed_value, inferred_type) = if let Some(val) = value {
                    let typed = self.check_expression(val)?;
                    let val_type = typed.ty.clone();
                    (Some(typed), val_type)
                } else {
                    (None, Type::Void)
                };
                
                let final_type = if ty == Type::Inferred {
                    inferred_type
                } else {
                    ty
                };
                
                self.env.define_var(name.clone(), final_type.clone(), mutable);
                
                Ok(TypedStmt::Let {
                    name,
                    ty: final_type,
                    value: typed_value,
                    mutable,
                })
            }
            
            Stmt::Const { name, ty, value } => {
                let typed_value = self.check_expression(value)?;
                let val_type = typed_value.ty.clone();
                
                let final_type = if ty == Type::Inferred {
                    val_type
                } else {
                    ty
                };
                
                self.env.define_var(name.clone(), final_type.clone(), false);
                
                Ok(TypedStmt::Const {
                    name,
                    ty: final_type,
                    value: typed_value,
                })
            }
            
            Stmt::Assign { target, value } => {
                let typed_target = self.check_expression(target)?;
                let typed_value = self.check_expression(value)?;
                
                Ok(TypedStmt::Assign {
                    target: typed_target,
                    value: typed_value,
                })
            }
            
            Stmt::CompoundAssign { target, op, value } => {
                let typed_target = self.check_expression(target)?;
                let typed_value = self.check_expression(value)?;
                
                Ok(TypedStmt::CompoundAssign {
                    target: typed_target,
                    op,
                    value: typed_value,
                })
            }
            
            Stmt::If { condition, then_block, else_block } => {
                let typed_cond = self.check_expression(condition)?;
                let typed_then = self.check_block(then_block)?;
                let typed_else = if let Some(eb) = else_block {
                    Some(self.check_block(eb)?)
                } else {
                    None
                };
                
                Ok(TypedStmt::If {
                    condition: typed_cond,
                    then_block: typed_then,
                    else_block: typed_else,
                })
            }
            
            Stmt::While { condition, body } => {
                let typed_cond = self.check_expression(condition)?;
                let typed_body = self.check_block(body)?;
                
                Ok(TypedStmt::While {
                    condition: typed_cond,
                    body: typed_body,
                })
            }
            
            Stmt::For { var, iterable, body } => {
                let typed_iter = self.check_expression(iterable)?;
                
                // Add loop variable to scope
                let parent_env = self.env.clone();
                let old_env = std::mem::replace(&mut self.env, TypeEnv::with_parent(parent_env));
                
                // Infer element type from iterable
                let elem_type = match &typed_iter.ty {
                    Type::Array(inner) => *inner.clone(),
                    _ => Type::Any,
                };
                
                self.env.define_var(var.clone(), elem_type, false);
                let typed_body = self.check_block(body)?;
                
                self.env = old_env;
                
                Ok(TypedStmt::For {
                    var,
                    iterable: typed_iter,
                    body: typed_body,
                })
            }
            
            Stmt::Loop { body } => {
                let typed_body = self.check_block(body)?;
                Ok(TypedStmt::Loop { body: typed_body })
            }
            
            Stmt::Break => Ok(TypedStmt::Break),
            Stmt::Continue => Ok(TypedStmt::Continue),
            
            Stmt::Return(value) => {
                let typed_value = if let Some(val) = value {
                    Some(self.check_expression(val)?)
                } else {
                    None
                };
                
                Ok(TypedStmt::Return(typed_value))
            }
            
            Stmt::Try { try_block, error_var, catch_block } => {
                let typed_try = self.check_block(try_block)?;
                
                let parent_env = self.env.clone();
                let old_env = std::mem::replace(&mut self.env, TypeEnv::with_parent(parent_env));
                self.env.define_var(error_var.clone(), Type::String, false);
                let typed_catch = self.check_block(catch_block)?;
                self.env = old_env;
                
                Ok(TypedStmt::Try {
                    try_block: typed_try,
                    error_var,
                    catch_block: typed_catch,
                })
            }
            
            Stmt::Match { value, arms } => {
                let typed_value = self.check_expression(value)?;
                let mut typed_arms = Vec::new();
                
                for arm in arms {
                    let typed_pattern = self.check_expression(arm.pattern)?;
                    let typed_body = self.check_statement(arm.body)?;
                    typed_arms.push(TypedMatchArm {
                        pattern: typed_pattern,
                        body: typed_body,
                    });
                }
                
                Ok(TypedStmt::Match {
                    value: typed_value,
                    arms: typed_arms,
                })
            }
            
            Stmt::Block(block) => {
                let typed = self.check_block(block)?;
                Ok(TypedStmt::Block(typed))
            }
            
            Stmt::Expr(expr) => {
                let typed = self.check_expression(expr)?;
                Ok(TypedStmt::Expr(typed))
            }
        }
    }
    
    fn check_expression(&mut self, expr: Expr) -> Result<TypedExpr, VeyraError> {
        match expr {
            Expr::Int(v) => Ok(TypedExpr::new(TypedExprKind::Int(v), Type::Int)),
            Expr::Float(v) => Ok(TypedExpr::new(TypedExprKind::Float(v), Type::Float)),
            Expr::String(v) => Ok(TypedExpr::new(TypedExprKind::String(v), Type::String)),
            Expr::Bool(v) => Ok(TypedExpr::new(TypedExprKind::Bool(v), Type::Bool)),
            Expr::Null => Ok(TypedExpr::new(TypedExprKind::Null, Type::Null)),
            
            Expr::Identifier(name) => {
                let ty = if let Some((t, _)) = self.env.get_var(&name) {
                    t.clone()
                } else {
                    Type::Any // Unknown variable, assume any
                };
                Ok(TypedExpr::new(TypedExprKind::Identifier(name), ty))
            }
            
            Expr::Binary { left, op, right } => {
                let typed_left = self.check_expression(*left)?;
                let typed_right = self.check_expression(*right)?;
                
                let result_type = self.binary_result_type(&typed_left.ty, &op, &typed_right.ty);
                
                Ok(TypedExpr::new(
                    TypedExprKind::Binary {
                        left: Box::new(typed_left),
                        op,
                        right: Box::new(typed_right),
                    },
                    result_type,
                ))
            }
            
            Expr::Unary { op, operand } => {
                let typed_operand = self.check_expression(*operand)?;
                let result_type = match op {
                    UnaryOp::Not => Type::Bool,
                    UnaryOp::Neg => typed_operand.ty.clone(),
                    UnaryOp::BitNot => Type::Int,
                };
                
                Ok(TypedExpr::new(
                    TypedExprKind::Unary {
                        op,
                        operand: Box::new(typed_operand),
                    },
                    result_type,
                ))
            }
            
            Expr::Call { callee, args } => {
                let mut typed_args = Vec::new();
                for arg in args {
                    typed_args.push(self.check_expression(arg)?);
                }
                
                let (typed_callee, return_type) = match *callee {
                    Expr::Identifier(name) => {
                        let ret_type = if let Some((_, ret)) = self.env.get_fn(&name) {
                            ret.clone()
                        } else {
                            Type::Any
                        };
                        (TypedExpr::new(TypedExprKind::Identifier(name), Type::Any), ret_type)
                    }
                    _ => {
                        let typed = self.check_expression(*callee)?;
                        (typed, Type::Any)
                    }
                };
                
                Ok(TypedExpr::new(
                    TypedExprKind::Call {
                        callee: Box::new(typed_callee),
                        args: typed_args,
                    },
                    return_type,
                ))
            }
            
            Expr::MethodCall { object, method, args } => {
                let typed_obj = self.check_expression(*object)?;
                let mut typed_args = Vec::new();
                for arg in args {
                    typed_args.push(self.check_expression(arg)?);
                }
                
                Ok(TypedExpr::new(
                    TypedExprKind::MethodCall {
                        object: Box::new(typed_obj),
                        method,
                        args: typed_args,
                    },
                    Type::Any,
                ))
            }
            
            Expr::FieldAccess { object, field } => {
                let typed_obj = self.check_expression(*object)?;
                
                Ok(TypedExpr::new(
                    TypedExprKind::FieldAccess {
                        object: Box::new(typed_obj),
                        field,
                    },
                    Type::Any,
                ))
            }
            
            Expr::Index { object, index } => {
                let typed_obj = self.check_expression(*object)?;
                let typed_idx = self.check_expression(*index)?;
                
                let elem_type = match &typed_obj.ty {
                    Type::Array(inner) => *inner.clone(),
                    Type::String => Type::String,
                    _ => Type::Any,
                };
                
                Ok(TypedExpr::new(
                    TypedExprKind::Index {
                        object: Box::new(typed_obj),
                        index: Box::new(typed_idx),
                    },
                    elem_type,
                ))
            }
            
            Expr::Array(elements) => {
                let mut typed_elements = Vec::new();
                let mut elem_type = Type::Any;
                
                for (i, elem) in elements.into_iter().enumerate() {
                    let typed = self.check_expression(elem)?;
                    if i == 0 {
                        elem_type = typed.ty.clone();
                    }
                    typed_elements.push(typed);
                }
                
                Ok(TypedExpr::new(
                    TypedExprKind::Array(typed_elements),
                    Type::Array(Box::new(elem_type)),
                ))
            }
            
            Expr::Object(fields) => {
                let mut typed_fields = Vec::new();
                for (key, value) in fields {
                    let typed_value = self.check_expression(value)?;
                    typed_fields.push((key, typed_value));
                }
                
                Ok(TypedExpr::new(
                    TypedExprKind::Object(typed_fields),
                    Type::Any,
                ))
            }
            
            Expr::Range { start, end } => {
                let typed_start = self.check_expression(*start)?;
                let typed_end = self.check_expression(*end)?;
                
                Ok(TypedExpr::new(
                    TypedExprKind::Range {
                        start: Box::new(typed_start),
                        end: Box::new(typed_end),
                    },
                    Type::Array(Box::new(Type::Int)),
                ))
            }
            
            Expr::New { class, args } => {
                let mut typed_args = Vec::new();
                for arg in args {
                    typed_args.push(self.check_expression(arg)?);
                }
                
                Ok(TypedExpr::new(
                    TypedExprKind::New {
                        class: class.clone(),
                        args: typed_args,
                    },
                    Type::Custom(class),
                ))
            }
            
            Expr::Lambda { params, body } => {
                let parent_env = self.env.clone();
                let old_env = std::mem::replace(&mut self.env, TypeEnv::with_parent(parent_env));
                
                for param in &params {
                    self.env.define_var(param.name.clone(), param.ty.clone(), false);
                }
                
                let typed_body = self.check_block(body)?;
                
                self.env = old_env;
                
                let param_types: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();
                
                Ok(TypedExpr::new(
                    TypedExprKind::Lambda {
                        params,
                        body: typed_body,
                    },
                    Type::Function(param_types, Box::new(Type::Any)),
                ))
            }
            
            Expr::Grouped(inner) => {
                let typed = self.check_expression(*inner)?;
                let ty = typed.ty.clone();
                Ok(TypedExpr::new(TypedExprKind::Grouped(Box::new(typed)), ty))
            }
            
            Expr::Ternary { condition, then_expr, else_expr } => {
                let typed_cond = self.check_expression(*condition)?;
                let typed_then = self.check_expression(*then_expr)?;
                let typed_else = self.check_expression(*else_expr)?;
                
                let result_type = typed_then.ty.clone();
                
                Ok(TypedExpr::new(
                    TypedExprKind::Ternary {
                        condition: Box::new(typed_cond),
                        then_expr: Box::new(typed_then),
                        else_expr: Box::new(typed_else),
                    },
                    result_type,
                ))
            }
            
            Expr::Await(inner) => {
                let typed = self.check_expression(*inner)?;
                let ty = typed.ty.clone();
                Ok(TypedExpr::new(TypedExprKind::Await(Box::new(typed)), ty))
            }
        }
    }
    
    fn binary_result_type(&self, left: &Type, op: &BinaryOp, right: &Type) -> Type {
        match op {
            // Comparison operators always return bool
            BinaryOp::Eq | BinaryOp::NotEq | BinaryOp::Lt | BinaryOp::Gt |
            BinaryOp::LtEq | BinaryOp::GtEq => Type::Bool,
            
            // Logical operators return bool
            BinaryOp::And | BinaryOp::Or => Type::Bool,
            
            // Arithmetic on numbers
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div |
            BinaryOp::Mod | BinaryOp::Pow => {
                if left.is_float() || right.is_float() {
                    Type::Float
                } else if left == &Type::String || right == &Type::String {
                    Type::String // String concatenation
                } else {
                    Type::Int
                }
            }
            
            // Bitwise operations return int
            BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor |
            BinaryOp::Shl | BinaryOp::Shr => Type::Int,
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
