//! Veyra Interpreter
//! 
//! Tree-walking interpreter with full stdlib support.

use crate::runtime::Value;
use crate::typechecker::{TypedProgram, TypedFunction, TypedBlock, TypedStmt, TypedExpr, TypedExprKind};
use crate::parser::{BinaryOp, UnaryOp};
use crate::error::VeyraError;
use std::collections::HashMap;

/// Interpreter for Veyra programs
pub struct Interpreter {
    globals: HashMap<String, Value>,
    functions: HashMap<String, TypedFunction>,
    scopes: Vec<HashMap<String, Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            functions: HashMap::new(),
            scopes: vec![HashMap::new()],
        }
    }
    
    /// Run a typed program
    pub fn run(&mut self, program: TypedProgram) -> Result<Value, VeyraError> {
        // Register all functions
        for func in program.functions {
            self.functions.insert(func.name.clone(), func);
        }
        
        // Execute top-level statements
        for stmt in &program.statements {
            self.exec_stmt(stmt)?;
        }
        
        // Call main if exists
        if self.functions.contains_key("main") {
            return self.call_function("main", vec![]);
        }
        
        Ok(Value::Null)
    }
    
    fn exec_stmt(&mut self, stmt: &TypedStmt) -> Result<Option<Value>, VeyraError> {
        match stmt {
            TypedStmt::Let { name, value, .. } => {
                let val = if let Some(v) = value {
                    self.eval_expr(v)?
                } else {
                    Value::Null
                };
                self.set_var(name.clone(), val);
                Ok(None)
            }
            
            TypedStmt::Const { name, value, .. } => {
                let val = self.eval_expr(value)?;
                self.set_var(name.clone(), val);
                Ok(None)
            }
            
            TypedStmt::Assign { target, value } => {
                let val = self.eval_expr(value)?;
                if let TypedExprKind::Identifier(name) = &target.kind {
                    self.set_var(name.clone(), val);
                }
                Ok(None)
            }
            
            TypedStmt::CompoundAssign { target, op, value } => {
                if let TypedExprKind::Identifier(name) = &target.kind {
                    let current = self.get_var(name).unwrap_or(Value::Int(0));
                    let right = self.eval_expr(value)?;
                    let result = self.eval_binary_op(&current, *op, &right);
                    self.set_var(name.clone(), result);
                }
                Ok(None)
            }
            
            TypedStmt::If { condition, then_block, else_block } => {
                let cond = self.eval_expr(condition)?;
                if cond.is_truthy() {
                    self.exec_block(then_block)?;
                } else if let Some(eb) = else_block {
                    self.exec_block(eb)?;
                }
                Ok(None)
            }
            
            TypedStmt::While { condition, body } => {
                while self.eval_expr(condition)?.is_truthy() {
                    if let Some(v) = self.exec_block(body)? {
                        return Ok(Some(v));
                    }
                }
                Ok(None)
            }
            
            TypedStmt::For { var, iterable, body } => {
                let iter_val = self.eval_expr(iterable)?;
                
                // Handle range
                if let TypedExprKind::Range { start, end } = &iterable.kind {
                    let start = self.eval_expr(start)?.to_int();
                    let end = self.eval_expr(end)?.to_int();
                    
                    for i in start..end {
                        self.push_scope();
                        self.set_var(var.clone(), Value::Int(i));
                        if let Some(v) = self.exec_block(body)? {
                            self.pop_scope();
                            return Ok(Some(v));
                        }
                        self.pop_scope();
                    }
                } else if let Value::Array(arr) = iter_val {
                    for item in arr {
                        self.push_scope();
                        self.set_var(var.clone(), item);
                        if let Some(v) = self.exec_block(body)? {
                            self.pop_scope();
                            return Ok(Some(v));
                        }
                        self.pop_scope();
                    }
                }
                
                Ok(None)
            }
            
            TypedStmt::Loop { body } => {
                loop {
                    if let Some(v) = self.exec_block(body)? {
                        return Ok(Some(v));
                    }
                }
            }
            
            TypedStmt::Break => Ok(None), // TODO: proper break handling
            TypedStmt::Continue => Ok(None), // TODO: proper continue handling
            
            TypedStmt::Return(value) => {
                let val = if let Some(v) = value {
                    self.eval_expr(v)?
                } else {
                    Value::Null
                };
                Ok(Some(val))
            }
            
            TypedStmt::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(None)
            }
            
            TypedStmt::Block(block) => {
                self.push_scope();
                let result = self.exec_block(block)?;
                self.pop_scope();
                Ok(result)
            }
            
            _ => Ok(None),
        }
    }
    
    fn exec_block(&mut self, block: &TypedBlock) -> Result<Option<Value>, VeyraError> {
        for stmt in &block.statements {
            if let Some(v) = self.exec_stmt(stmt)? {
                return Ok(Some(v));
            }
        }
        Ok(None)
    }
    
    fn eval_expr(&mut self, expr: &TypedExpr) -> Result<Value, VeyraError> {
        match &expr.kind {
            TypedExprKind::Int(v) => Ok(Value::Int(*v)),
            TypedExprKind::Float(v) => Ok(Value::Float(*v)),
            TypedExprKind::String(s) => Ok(Value::String(s.clone())),
            TypedExprKind::Bool(b) => Ok(Value::Bool(*b)),
            TypedExprKind::Null => Ok(Value::Null),
            
            TypedExprKind::Identifier(name) => {
                Ok(self.get_var(name).unwrap_or(Value::Null))
            }
            
            TypedExprKind::Binary { left, op, right } => {
                let l = self.eval_expr(left)?;
                let r = self.eval_expr(right)?;
                Ok(self.eval_binary_op(&l, *op, &r))
            }
            
            TypedExprKind::Unary { op, operand } => {
                let val = self.eval_expr(operand)?;
                Ok(self.eval_unary_op(*op, &val))
            }
            
            TypedExprKind::Call { callee, args } => {
                let mut arg_vals = Vec::new();
                for arg in args {
                    arg_vals.push(self.eval_expr(arg)?);
                }
                
                if let TypedExprKind::Identifier(name) = &callee.kind {
                    self.call_function(name, arg_vals)
                } else {
                    Ok(Value::Null)
                }
            }
            
            TypedExprKind::Array(elements) => {
                let mut vals = Vec::new();
                for elem in elements {
                    vals.push(self.eval_expr(elem)?);
                }
                Ok(Value::Array(vals))
            }
            
            TypedExprKind::Object(fields) => {
                let mut obj = HashMap::new();
                for (key, value) in fields {
                    obj.insert(key.clone(), self.eval_expr(value)?);
                }
                Ok(Value::Object(obj))
            }
            
            TypedExprKind::Index { object, index } => {
                let obj = self.eval_expr(object)?;
                let idx = self.eval_expr(index)?;
                
                match (obj, idx) {
                    (Value::Array(arr), Value::Int(i)) => {
                        Ok(arr.get(i as usize).cloned().unwrap_or(Value::Null))
                    }
                    (Value::Object(map), Value::String(key)) => {
                        Ok(map.get(&key).cloned().unwrap_or(Value::Null))
                    }
                    (Value::String(s), Value::Int(i)) => {
                        Ok(Value::String(
                            s.chars().nth(i as usize).map(|c| c.to_string()).unwrap_or_default()
                        ))
                    }
                    _ => Ok(Value::Null),
                }
            }
            
            TypedExprKind::FieldAccess { object, field } => {
                let obj = self.eval_expr(object)?;
                if let Value::Object(map) = obj {
                    Ok(map.get(field).cloned().unwrap_or(Value::Null))
                } else {
                    Ok(Value::Null)
                }
            }
            
            TypedExprKind::Range { start, end } => {
                let s = self.eval_expr(start)?.to_int();
                let e = self.eval_expr(end)?.to_int();
                let arr: Vec<Value> = (s..e).map(Value::Int).collect();
                Ok(Value::Array(arr))
            }
            
            TypedExprKind::Grouped(inner) => self.eval_expr(inner),
            
            TypedExprKind::Ternary { condition, then_expr, else_expr } => {
                let cond = self.eval_expr(condition)?;
                if cond.is_truthy() {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
            }
            
            _ => Ok(Value::Null),
        }
    }
    
    fn eval_binary_op(&self, left: &Value, op: BinaryOp, right: &Value) -> Value {
        match op {
            BinaryOp::Add => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 + b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a + *b as f64),
                (Value::String(a), Value::String(b)) => Value::String(format!("{}{}", a, b)),
                (Value::String(a), b) => Value::String(format!("{}{}", a, b.to_string())),
                (a, Value::String(b)) => Value::String(format!("{}{}", a.to_string(), b)),
                _ => Value::Null,
            },
            BinaryOp::Sub => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a - b),
                (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 - b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a - *b as f64),
                _ => Value::Null,
            },
            BinaryOp::Mul => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a * b),
                (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 * b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a * *b as f64),
                _ => Value::Null,
            },
            BinaryOp::Div => match (left, right) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Value::Int(a / b),
                (Value::Float(a), Value::Float(b)) => Value::Float(a / b),
                (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 / b),
                (Value::Float(a), Value::Int(b)) => Value::Float(a / *b as f64),
                _ => Value::Null,
            },
            BinaryOp::Mod => match (left, right) {
                (Value::Int(a), Value::Int(b)) if *b != 0 => Value::Int(a % b),
                _ => Value::Null,
            },
            BinaryOp::Pow => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int((*a as f64).powf(*b as f64) as i64),
                (Value::Float(a), Value::Float(b)) => Value::Float(a.powf(*b)),
                (Value::Int(a), Value::Float(b)) => Value::Float((*a as f64).powf(*b)),
                (Value::Float(a), Value::Int(b)) => Value::Float(a.powf(*b as f64)),
                _ => Value::Null,
            },
            BinaryOp::Eq => Value::Bool(left == right),
            BinaryOp::NotEq => Value::Bool(left != right),
            BinaryOp::Lt => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a < b),
                (Value::Float(a), Value::Float(b)) => Value::Bool(a < b),
                (Value::Int(a), Value::Float(b)) => Value::Bool((*a as f64) < *b),
                (Value::Float(a), Value::Int(b)) => Value::Bool(*a < *b as f64),
                _ => Value::Bool(false),
            },
            BinaryOp::Gt => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a > b),
                (Value::Float(a), Value::Float(b)) => Value::Bool(a > b),
                (Value::Int(a), Value::Float(b)) => Value::Bool((*a as f64) > *b),
                (Value::Float(a), Value::Int(b)) => Value::Bool(*a > *b as f64),
                _ => Value::Bool(false),
            },
            BinaryOp::LtEq => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a <= b),
                (Value::Float(a), Value::Float(b)) => Value::Bool(a <= b),
                _ => Value::Bool(false),
            },
            BinaryOp::GtEq => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Bool(a >= b),
                (Value::Float(a), Value::Float(b)) => Value::Bool(a >= b),
                _ => Value::Bool(false),
            },
            BinaryOp::And => Value::Bool(left.is_truthy() && right.is_truthy()),
            BinaryOp::Or => Value::Bool(left.is_truthy() || right.is_truthy()),
            BinaryOp::BitAnd => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a & b),
                _ => Value::Null,
            },
            BinaryOp::BitOr => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a | b),
                _ => Value::Null,
            },
            BinaryOp::BitXor => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a ^ b),
                _ => Value::Null,
            },
            BinaryOp::Shl => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a << b),
                _ => Value::Null,
            },
            BinaryOp::Shr => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Value::Int(a >> b),
                _ => Value::Null,
            },
        }
    }
    
    fn eval_unary_op(&self, op: UnaryOp, operand: &Value) -> Value {
        match op {
            UnaryOp::Neg => match operand {
                Value::Int(n) => Value::Int(-n),
                Value::Float(f) => Value::Float(-f),
                _ => Value::Null,
            },
            UnaryOp::Not => Value::Bool(!operand.is_truthy()),
            UnaryOp::BitNot => match operand {
                Value::Int(n) => Value::Int(!n),
                _ => Value::Null,
            },
        }
    }
    
    fn call_function(&mut self, name: &str, args: Vec<Value>) -> Result<Value, VeyraError> {
        // Built-in functions
        match name {
            "println" => {
                for arg in &args {
                    println!("{}", arg.to_string());
                }
                return Ok(Value::Null);
            }
            "print" => {
                for arg in &args {
                    print!("{}", arg.to_string());
                }
                return Ok(Value::Null);
            }
            "len" => {
                if let Some(arg) = args.first() {
                    return Ok(match arg {
                        Value::String(s) => Value::Int(s.len() as i64),
                        Value::Array(arr) => Value::Int(arr.len() as i64),
                        Value::Object(obj) => Value::Int(obj.len() as i64),
                        _ => Value::Int(0),
                    });
                }
                return Ok(Value::Int(0));
            }
            "type_of" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::String(arg.type_name().to_string()));
                }
                return Ok(Value::String("null".to_string()));
            }
            "to_string" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::String(arg.to_string()));
                }
                return Ok(Value::String(String::new()));
            }
            "to_int" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Int(arg.to_int()));
                }
                return Ok(Value::Int(0));
            }
            "to_float" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Float(arg.to_float()));
                }
                return Ok(Value::Float(0.0));
            }
            "sqrt" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Float(arg.to_float().sqrt()));
                }
                return Ok(Value::Float(0.0));
            }
            "pow" => {
                if args.len() >= 2 {
                    let base = args[0].to_float();
                    let exp = args[1].to_float();
                    return Ok(Value::Float(base.powf(exp)));
                }
                return Ok(Value::Float(0.0));
            }
            "sin" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Float(arg.to_float().sin()));
                }
                return Ok(Value::Float(0.0));
            }
            "cos" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Float(arg.to_float().cos()));
                }
                return Ok(Value::Float(0.0));
            }
            "abs" => {
                if let Some(arg) = args.first() {
                    return Ok(match arg {
                        Value::Int(n) => Value::Int(n.abs()),
                        Value::Float(f) => Value::Float(f.abs()),
                        _ => Value::Null,
                    });
                }
                return Ok(Value::Null);
            }
            "floor" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Int(arg.to_float().floor() as i64));
                }
                return Ok(Value::Int(0));
            }
            "ceil" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Int(arg.to_float().ceil() as i64));
                }
                return Ok(Value::Int(0));
            }
            "round" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Int(arg.to_float().round() as i64));
                }
                return Ok(Value::Int(0));
            }
            "rand" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
                let random = ((seed % 10000) as f64) / 10000.0;
                return Ok(Value::Float(random));
            }
            "rand_int" => {
                if args.len() >= 2 {
                    use std::time::{SystemTime, UNIX_EPOCH};
                    let min = args[0].to_int();
                    let max = args[1].to_int();
                    let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
                    let range = (max - min) as u128;
                    if range > 0 {
                        let random = min + ((seed % range) as i64);
                        return Ok(Value::Int(random));
                    }
                }
                return Ok(Value::Int(0));
            }
            "now" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_millis() as i64;
                return Ok(Value::Int(now));
            }
            "sleep" => {
                if let Some(arg) = args.first() {
                    let ms = arg.to_int() as u64;
                    std::thread::sleep(std::time::Duration::from_millis(ms));
                }
                return Ok(Value::Null);
            }
            "read_file" => {
                if let Some(Value::String(path)) = args.first() {
                    match std::fs::read_to_string(path) {
                        Ok(content) => return Ok(Value::String(content)),
                        Err(_) => return Ok(Value::Null),
                    }
                }
                return Ok(Value::Null);
            }
            "write_file" => {
                if args.len() >= 2 {
                    if let (Value::String(path), Value::String(content)) = (&args[0], &args[1]) {
                        let _ = std::fs::write(path, content);
                    }
                }
                return Ok(Value::Null);
            }
            "push" => {
                // Array push - returns new array
                if args.len() >= 2 {
                    if let Value::Array(mut arr) = args[0].clone() {
                        arr.push(args[1].clone());
                        return Ok(Value::Array(arr));
                    }
                }
                return Ok(Value::Null);
            }
            "pop" => {
                if let Some(Value::Array(mut arr)) = args.first().cloned() {
                    return Ok(arr.pop().unwrap_or(Value::Null));
                }
                return Ok(Value::Null);
            }
            "join" => {
                if args.len() >= 2 {
                    if let (Value::Array(arr), Value::String(sep)) = (&args[0], &args[1]) {
                        let items: Vec<String> = arr.iter().map(|v| v.to_string()).collect();
                        return Ok(Value::String(items.join(sep)));
                    }
                }
                return Ok(Value::String(String::new()));
            }
            "split" => {
                if args.len() >= 2 {
                    if let (Value::String(s), Value::String(sep)) = (&args[0], &args[1]) {
                        let parts: Vec<Value> = s.split(sep).map(|p| Value::String(p.to_string())).collect();
                        return Ok(Value::Array(parts));
                    }
                }
                return Ok(Value::Array(vec![]));
            }
            "trim" => {
                if let Some(Value::String(s)) = args.first() {
                    return Ok(Value::String(s.trim().to_string()));
                }
                return Ok(Value::String(String::new()));
            }
            "upper" => {
                if let Some(Value::String(s)) = args.first() {
                    return Ok(Value::String(s.to_uppercase()));
                }
                return Ok(Value::String(String::new()));
            }
            "lower" => {
                if let Some(Value::String(s)) = args.first() {
                    return Ok(Value::String(s.to_lowercase()));
                }
                return Ok(Value::String(String::new()));
            }
            "replace" => {
                if args.len() >= 3 {
                    if let (Value::String(s), Value::String(from), Value::String(to)) = (&args[0], &args[1], &args[2]) {
                        return Ok(Value::String(s.replace(from, to)));
                    }
                }
                return Ok(Value::String(String::new()));
            }
            "contains" => {
                if args.len() >= 2 {
                    if let (Value::String(s), Value::String(sub)) = (&args[0], &args[1]) {
                        return Ok(Value::Bool(s.contains(sub)));
                    }
                }
                return Ok(Value::Bool(false));
            }
            "starts_with" => {
                if args.len() >= 2 {
                    if let (Value::String(s), Value::String(prefix)) = (&args[0], &args[1]) {
                        return Ok(Value::Bool(s.starts_with(prefix)));
                    }
                }
                return Ok(Value::Bool(false));
            }
            "ends_with" => {
                if args.len() >= 2 {
                    if let (Value::String(s), Value::String(suffix)) = (&args[0], &args[1]) {
                        return Ok(Value::Bool(s.ends_with(suffix)));
                    }
                }
                return Ok(Value::Bool(false));
            }
            "exit" => {
                if let Some(arg) = args.first() {
                    std::process::exit(arg.to_int() as i32);
                }
                std::process::exit(0);
            }
            "assert" => {
                if let Some(arg) = args.first() {
                    if !arg.is_truthy() {
                        return Err(VeyraError::RuntimeError {
                            message: "Assertion failed".to_string(),
                        });
                    }
                }
                return Ok(Value::Null);
            }
            "panic" => {
                let msg = args.first().map(|v| v.to_string()).unwrap_or("panic".to_string());
                return Err(VeyraError::RuntimeError { message: msg });
            }
            
            // === JSON Functions ===
            "json_parse" => {
                if let Some(Value::String(s)) = args.first() {
                    return Ok(parse_json(s));
                }
                return Ok(Value::Null);
            }
            "json_stringify" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::String(stringify_json(arg)));
                }
                return Ok(Value::String("null".to_string()));
            }
            
            // === HTML/Web Functions ===
            "html" => {
                if let Some(Value::String(content)) = args.first() {
                    let html = format!(r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Veyra App</title>
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }}
    </style>
</head>
<body>
{}
</body>
</html>"#, content);
                    return Ok(Value::String(html));
                }
                return Ok(Value::String(String::new()));
            }
            "html_element" => {
                if args.len() >= 2 {
                    let tag = args[0].to_string();
                    let content = args[1].to_string();
                    let attrs = if args.len() >= 3 {
                        args[2].to_string()
                    } else {
                        String::new()
                    };
                    return Ok(Value::String(format!("<{} {}>{}</{}>", tag, attrs, content, tag)));
                }
                return Ok(Value::String(String::new()));
            }
            "div" => {
                let content = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<div>{}</div>", content)));
            }
            "span" => {
                let content = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<span>{}</span>", content)));
            }
            "h1" => {
                let content = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<h1>{}</h1>", content)));
            }
            "h2" => {
                let content = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<h2>{}</h2>", content)));
            }
            "p" => {
                let content = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<p>{}</p>", content)));
            }
            "button" => {
                let content = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<button>{}</button>", content)));
            }
            "input" => {
                let placeholder = args.first().map(|v| v.to_string()).unwrap_or_default();
                return Ok(Value::String(format!("<input placeholder=\"{}\" />", placeholder)));
            }
            "link" => {
                if args.len() >= 2 {
                    let text = args[0].to_string();
                    let href = args[1].to_string();
                    return Ok(Value::String(format!("<a href=\"{}\">{}</a>", href, text)));
                }
                return Ok(Value::String(String::new()));
            }
            "img" => {
                if let Some(Value::String(src)) = args.first() {
                    let alt = args.get(1).map(|v| v.to_string()).unwrap_or_default();
                    return Ok(Value::String(format!("<img src=\"{}\" alt=\"{}\" />", src, alt)));
                }
                return Ok(Value::String(String::new()));
            }
            "style" => {
                if let Some(Value::String(css)) = args.first() {
                    return Ok(Value::String(format!("<style>{}</style>", css)));
                }
                return Ok(Value::String(String::new()));
            }
            "script" => {
                if let Some(Value::String(js)) = args.first() {
                    return Ok(Value::String(format!("<script>{}</script>", js)));
                }
                return Ok(Value::String(String::new()));
            }
            
            // === Web Server Functions ===
            "serve_html" => {
                if args.len() >= 2 {
                    let html = args[0].to_string();
                    let port = args[1].to_int() as u16;
                    println!("🌐 Starting Veyra web server on http://localhost:{}", port);
                    
                    // Simple HTTP server
                    use std::net::TcpListener;
                    use std::io::{Read, Write};
                    
                    let listener = TcpListener::bind(format!("127.0.0.1:{}", port));
                    if let Ok(listener) = listener {
                        println!("✅ Server running! Press Ctrl+C to stop.");
                        for stream in listener.incoming() {
                            if let Ok(mut stream) = stream {
                                let mut buffer = [0; 1024];
                                let _ = stream.read(&mut buffer);
                                
                                let response = format!(
                                    "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nContent-Length: {}\r\n\r\n{}",
                                    html.len(),
                                    html
                                );
                                let _ = stream.write_all(response.as_bytes());
                            }
                        }
                    } else {
                        println!("❌ Failed to start server on port {}", port);
                    }
                }
                return Ok(Value::Null);
            }
            
            // === Environment & System ===
            "env" => {
                if let Some(Value::String(key)) = args.first() {
                    return Ok(std::env::var(key)
                        .map(Value::String)
                        .unwrap_or(Value::Null));
                }
                return Ok(Value::Null);
            }
            "set_env" => {
                if args.len() >= 2 {
                    if let (Value::String(key), Value::String(val)) = (&args[0], &args[1]) {
                        std::env::set_var(key, val);
                    }
                }
                return Ok(Value::Null);
            }
            "cwd" => {
                return Ok(std::env::current_dir()
                    .map(|p| Value::String(p.to_string_lossy().to_string()))
                    .unwrap_or(Value::Null));
            }
            "exec" => {
                if let Some(Value::String(cmd)) = args.first() {
                    #[cfg(target_os = "windows")]
                    let output = std::process::Command::new("cmd")
                        .args(["/C", cmd])
                        .output();
                    
                    #[cfg(not(target_os = "windows"))]
                    let output = std::process::Command::new("sh")
                        .args(["-c", cmd])
                        .output();
                    
                    if let Ok(out) = output {
                        return Ok(Value::String(String::from_utf8_lossy(&out.stdout).to_string()));
                    }
                }
                return Ok(Value::Null);
            }
            "args" => {
                let args: Vec<Value> = std::env::args()
                    .skip(1)
                    .map(Value::String)
                    .collect();
                return Ok(Value::Array(args));
            }
            
            // === Matrix/AI Functions ===
            "matrix" => {
                // Create a matrix from nested arrays
                return Ok(args.first().cloned().unwrap_or(Value::Null));
            }
            "matrix_add" => {
                if args.len() >= 2 {
                    if let (Value::Array(a), Value::Array(b)) = (&args[0], &args[1]) {
                        let result: Vec<Value> = a.iter().zip(b.iter())
                            .map(|(x, y)| Value::Float(x.to_float() + y.to_float()))
                            .collect();
                        return Ok(Value::Array(result));
                    }
                }
                return Ok(Value::Null);
            }
            "matrix_mul" => {
                if args.len() >= 2 {
                    if let (Value::Array(a), Value::Array(b)) = (&args[0], &args[1]) {
                        let result: Vec<Value> = a.iter().zip(b.iter())
                            .map(|(x, y)| Value::Float(x.to_float() * y.to_float()))
                            .collect();
                        return Ok(Value::Array(result));
                    }
                }
                return Ok(Value::Null);
            }
            "dot" => {
                // Dot product
                if args.len() >= 2 {
                    if let (Value::Array(a), Value::Array(b)) = (&args[0], &args[1]) {
                        let sum: f64 = a.iter().zip(b.iter())
                            .map(|(x, y)| x.to_float() * y.to_float())
                            .sum();
                        return Ok(Value::Float(sum));
                    }
                }
                return Ok(Value::Float(0.0));
            }
            "relu" => {
                if let Some(arg) = args.first() {
                    let x = arg.to_float();
                    return Ok(Value::Float(if x > 0.0 { x } else { 0.0 }));
                }
                return Ok(Value::Float(0.0));
            }
            "sigmoid" => {
                if let Some(arg) = args.first() {
                    let x = arg.to_float();
                    return Ok(Value::Float(1.0 / (1.0 + (-x).exp())));
                }
                return Ok(Value::Float(0.5));
            }
            "tanh" => {
                if let Some(arg) = args.first() {
                    return Ok(Value::Float(arg.to_float().tanh()));
                }
                return Ok(Value::Float(0.0));
            }
            "softmax" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let max = arr.iter().map(|v| v.to_float()).fold(f64::NEG_INFINITY, f64::max);
                    let exps: Vec<f64> = arr.iter().map(|v| (v.to_float() - max).exp()).collect();
                    let sum: f64 = exps.iter().sum();
                    let result: Vec<Value> = exps.iter().map(|e| Value::Float(e / sum)).collect();
                    return Ok(Value::Array(result));
                }
                return Ok(Value::Null);
            }
            "sum" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let total: f64 = arr.iter().map(|v| v.to_float()).sum();
                    return Ok(Value::Float(total));
                }
                return Ok(Value::Float(0.0));
            }
            "mean" => {
                if let Some(Value::Array(arr)) = args.first() {
                    if !arr.is_empty() {
                        let total: f64 = arr.iter().map(|v| v.to_float()).sum();
                        return Ok(Value::Float(total / arr.len() as f64));
                    }
                }
                return Ok(Value::Float(0.0));
            }
            "min" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let min = arr.iter().map(|v| v.to_float()).fold(f64::INFINITY, f64::min);
                    return Ok(Value::Float(min));
                } else if args.len() >= 2 {
                    let a = args[0].to_float();
                    let b = args[1].to_float();
                    return Ok(Value::Float(a.min(b)));
                }
                return Ok(Value::Float(0.0));
            }
            "max" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let max = arr.iter().map(|v| v.to_float()).fold(f64::NEG_INFINITY, f64::max);
                    return Ok(Value::Float(max));
                } else if args.len() >= 2 {
                    let a = args[0].to_float();
                    let b = args[1].to_float();
                    return Ok(Value::Float(a.max(b)));
                }
                return Ok(Value::Float(0.0));
            }
            
            // === Range/Map/Filter ===
            "range" => {
                if args.len() >= 2 {
                    let start = args[0].to_int();
                    let end = args[1].to_int();
                    let arr: Vec<Value> = (start..end).map(Value::Int).collect();
                    return Ok(Value::Array(arr));
                }
                return Ok(Value::Array(vec![]));
            }
            "reverse" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let mut reversed = arr.clone();
                    reversed.reverse();
                    return Ok(Value::Array(reversed));
                } else if let Some(Value::String(s)) = args.first() {
                    return Ok(Value::String(s.chars().rev().collect()));
                }
                return Ok(Value::Null);
            }
            "sort" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let mut sorted = arr.clone();
                    sorted.sort_by(|a, b| a.to_float().partial_cmp(&b.to_float()).unwrap_or(std::cmp::Ordering::Equal));
                    return Ok(Value::Array(sorted));
                }
                return Ok(Value::Null);
            }
            "unique" => {
                if let Some(Value::Array(arr)) = args.first() {
                    let mut seen = std::collections::HashSet::new();
                    let unique: Vec<Value> = arr.iter()
                        .filter(|v| seen.insert(v.to_string()))
                        .cloned()
                        .collect();
                    return Ok(Value::Array(unique));
                }
                return Ok(Value::Null);
            }
            
            // === Date/Time Functions ===
            "date" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let secs = now.as_secs();
                // Simple date calculation
                let days = secs / 86400;
                let years = 1970 + (days / 365);
                let remaining_days = days % 365;
                let months = remaining_days / 30 + 1;
                let day = remaining_days % 30 + 1;
                return Ok(Value::String(format!("{:04}-{:02}-{:02}", years, months, day)));
            }
            "time" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
                let secs = now.as_secs();
                let hours = (secs % 86400) / 3600;
                let mins = (secs % 3600) / 60;
                let s = secs % 60;
                return Ok(Value::String(format!("{:02}:{:02}:{:02}", hours, mins, s)));
            }
            "timestamp" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs() as i64;
                return Ok(Value::Int(now));
            }
            "format_date" => {
                if let Some(Value::Int(ts)) = args.first() {
                    let secs = *ts as u64;
                    let days = secs / 86400;
                    let years = 1970 + (days / 365);
                    let remaining_days = days % 365;
                    let months = remaining_days / 30 + 1;
                    let day = remaining_days % 30 + 1;
                    return Ok(Value::String(format!("{:04}-{:02}-{:02}", years, months, day)));
                }
                return Ok(Value::String(String::new()));
            }
            
            // === Crypto/Hashing Functions ===
            "hash" | "hash_str" => {
                if let Some(Value::String(s)) = args.first() {
                    // Simple hash (djb2)
                    let mut hash: u64 = 5381;
                    for c in s.bytes() {
                        hash = hash.wrapping_mul(33).wrapping_add(c as u64);
                    }
                    return Ok(Value::String(format!("{:016x}", hash)));
                }
                return Ok(Value::String(String::new()));
            }
            "md5" => {
                if let Some(Value::String(s)) = args.first() {
                    // Simple MD5-like hash (not cryptographically secure)
                    let mut hash: u128 = 0;
                    for (i, c) in s.bytes().enumerate() {
                        hash = hash.wrapping_add((c as u128).wrapping_mul((i as u128 + 1).wrapping_mul(31)));
                    }
                    return Ok(Value::String(format!("{:032x}", hash)));
                }
                return Ok(Value::String(String::new()));
            }
            "uuid" => {
                use std::time::{SystemTime, UNIX_EPOCH};
                let now = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
                let uuid = format!("{:08x}-{:04x}-{:04x}-{:04x}-{:012x}",
                    (now >> 96) as u32,
                    (now >> 80) as u16,
                    ((now >> 64) as u16 & 0x0fff) | 0x4000,
                    ((now >> 48) as u16 & 0x3fff) | 0x8000,
                    now as u64 & 0xffffffffffff
                );
                return Ok(Value::String(uuid));
            }
            "random_bytes" => {
                let len = args.first().map(|v| v.to_int()).unwrap_or(16) as usize;
                use std::time::{SystemTime, UNIX_EPOCH};
                let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
                let bytes: Vec<Value> = (0..len).map(|i| {
                    let b = ((seed.wrapping_mul(31).wrapping_add(i as u128)) % 256) as i64;
                    Value::Int(b)
                }).collect();
                return Ok(Value::Array(bytes));
            }
            
            // === Base64 Encoding ===
            "base64_encode" => {
                if let Some(Value::String(s)) = args.first() {
                    const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
                    let bytes = s.as_bytes();
                    let mut result = String::new();
                    for chunk in bytes.chunks(3) {
                        let mut buf = [0u8; 3];
                        buf[..chunk.len()].copy_from_slice(chunk);
                        let n = ((buf[0] as u32) << 16) | ((buf[1] as u32) << 8) | (buf[2] as u32);
                        result.push(ALPHABET[((n >> 18) & 63) as usize] as char);
                        result.push(ALPHABET[((n >> 12) & 63) as usize] as char);
                        if chunk.len() > 1 {
                            result.push(ALPHABET[((n >> 6) & 63) as usize] as char);
                        } else {
                            result.push('=');
                        }
                        if chunk.len() > 2 {
                            result.push(ALPHABET[(n & 63) as usize] as char);
                        } else {
                            result.push('=');
                        }
                    }
                    return Ok(Value::String(result));
                }
                return Ok(Value::String(String::new()));
            }
            "base64_decode" => {
                if let Some(Value::String(s)) = args.first() {
                    const DECODE: [i8; 128] = [
                        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
                        -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
                        52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
                        -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
                        15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
                        -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
                        41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
                    ];
                    let mut bytes = Vec::new();
                    let chars: Vec<char> = s.chars().filter(|c| *c != '=').collect();
                    for chunk in chars.chunks(4) {
                        if chunk.len() >= 2 {
                            let a = DECODE.get(chunk[0] as usize).copied().unwrap_or(-1);
                            let b = DECODE.get(chunk[1] as usize).copied().unwrap_or(-1);
                            let c = chunk.get(2).map(|&ch| DECODE.get(ch as usize).copied().unwrap_or(-1)).unwrap_or(0);
                            let d = chunk.get(3).map(|&ch| DECODE.get(ch as usize).copied().unwrap_or(-1)).unwrap_or(0);
                            if a >= 0 && b >= 0 {
                                bytes.push(((a << 2) | (b >> 4)) as u8);
                                if c >= 0 { bytes.push((((b & 15) << 4) | (c >> 2)) as u8); }
                                if d >= 0 { bytes.push((((c & 3) << 6) | d) as u8); }
                            }
                        }
                    }
                    return Ok(Value::String(String::from_utf8_lossy(&bytes).to_string()));
                }
                return Ok(Value::String(String::new()));
            }
            
            // === URL Encoding ===
            "url_encode" => {
                if let Some(Value::String(s)) = args.first() {
                    let mut result = String::new();
                    for c in s.chars() {
                        if c.is_ascii_alphanumeric() || c == '-' || c == '_' || c == '.' || c == '~' {
                            result.push(c);
                        } else {
                            for b in c.to_string().as_bytes() {
                                result.push_str(&format!("%{:02X}", b));
                            }
                        }
                    }
                    return Ok(Value::String(result));
                }
                return Ok(Value::String(String::new()));
            }
            "url_decode" => {
                if let Some(Value::String(s)) = args.first() {
                    let mut result = String::new();
                    let mut chars = s.chars().peekable();
                    while let Some(c) = chars.next() {
                        if c == '%' {
                            let hex: String = chars.by_ref().take(2).collect();
                            if let Ok(byte) = u8::from_str_radix(&hex, 16) {
                                result.push(byte as char);
                            }
                        } else if c == '+' {
                            result.push(' ');
                        } else {
                            result.push(c);
                        }
                    }
                    return Ok(Value::String(result));
                }
                return Ok(Value::String(String::new()));
            }
            
            // === File System Functions ===
            "file_exists" => {
                if let Some(Value::String(path)) = args.first() {
                    return Ok(Value::Bool(std::path::Path::new(path).exists()));
                }
                return Ok(Value::Bool(false));
            }
            "is_file" => {
                if let Some(Value::String(path)) = args.first() {
                    return Ok(Value::Bool(std::path::Path::new(path).is_file()));
                }
                return Ok(Value::Bool(false));
            }
            "is_dir" => {
                if let Some(Value::String(path)) = args.first() {
                    return Ok(Value::Bool(std::path::Path::new(path).is_dir()));
                }
                return Ok(Value::Bool(false));
            }
            "list_dir" => {
                if let Some(Value::String(path)) = args.first() {
                    if let Ok(entries) = std::fs::read_dir(path) {
                        let files: Vec<Value> = entries
                            .filter_map(|e| e.ok())
                            .map(|e| Value::String(e.file_name().to_string_lossy().to_string()))
                            .collect();
                        return Ok(Value::Array(files));
                    }
                }
                return Ok(Value::Array(vec![]));
            }
            "mkdir" => {
                if let Some(Value::String(path)) = args.first() {
                    let _ = std::fs::create_dir_all(path);
                }
                return Ok(Value::Null);
            }
            "rmdir" => {
                if let Some(Value::String(path)) = args.first() {
                    let _ = std::fs::remove_dir_all(path);
                }
                return Ok(Value::Null);
            }
            "delete_file" => {
                if let Some(Value::String(path)) = args.first() {
                    let _ = std::fs::remove_file(path);
                }
                return Ok(Value::Null);
            }
            "copy_file" => {
                if args.len() >= 2 {
                    if let (Value::String(src), Value::String(dst)) = (&args[0], &args[1]) {
                        let _ = std::fs::copy(src, dst);
                    }
                }
                return Ok(Value::Null);
            }
            "move_file" | "rename" => {
                if args.len() >= 2 {
                    if let (Value::String(src), Value::String(dst)) = (&args[0], &args[1]) {
                        let _ = std::fs::rename(src, dst);
                    }
                }
                return Ok(Value::Null);
            }
            "file_size" => {
                if let Some(Value::String(path)) = args.first() {
                    if let Ok(meta) = std::fs::metadata(path) {
                        return Ok(Value::Int(meta.len() as i64));
                    }
                }
                return Ok(Value::Int(0));
            }
            "read_bytes" => {
                if let Some(Value::String(path)) = args.first() {
                    if let Ok(bytes) = std::fs::read(path) {
                        let arr: Vec<Value> = bytes.iter().map(|&b| Value::Int(b as i64)).collect();
                        return Ok(Value::Array(arr));
                    }
                }
                return Ok(Value::Array(vec![]));
            }
            "write_bytes" => {
                if args.len() >= 2 {
                    if let (Value::String(path), Value::Array(arr)) = (&args[0], &args[1]) {
                        let bytes: Vec<u8> = arr.iter().map(|v| v.to_int() as u8).collect();
                        let _ = std::fs::write(path, bytes);
                    }
                }
                return Ok(Value::Null);
            }
            "append_file" => {
                if args.len() >= 2 {
                    if let (Value::String(path), Value::String(content)) = (&args[0], &args[1]) {
                        use std::io::Write;
                        if let Ok(mut file) = std::fs::OpenOptions::new().append(true).create(true).open(path) {
                            let _ = file.write_all(content.as_bytes());
                        }
                    }
                }
                return Ok(Value::Null);
            }
            
            // === String Formatting ===
            "format" => {
                if let Some(Value::String(template)) = args.first() {
                    let mut result = template.clone();
                    for (i, arg) in args.iter().skip(1).enumerate() {
                        result = result.replace(&format!("{{{}}}", i), &arg.to_string());
                    }
                    return Ok(Value::String(result));
                }
                return Ok(Value::String(String::new()));
            }
            "pad_left" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let len = args[1].to_int() as usize;
                    let pad = args.get(2).map(|v| v.to_string()).unwrap_or(" ".to_string());
                    if s.len() < len {
                        let padding = pad.repeat((len - s.len()) / pad.len() + 1);
                        return Ok(Value::String(format!("{}{}", &padding[..len-s.len()], s)));
                    }
                    return Ok(Value::String(s));
                }
                return Ok(Value::String(String::new()));
            }
            "pad_right" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let len = args[1].to_int() as usize;
                    let pad = args.get(2).map(|v| v.to_string()).unwrap_or(" ".to_string());
                    if s.len() < len {
                        let padding = pad.repeat((len - s.len()) / pad.len() + 1);
                        return Ok(Value::String(format!("{}{}", s, &padding[..len-s.len()])));
                    }
                    return Ok(Value::String(s));
                }
                return Ok(Value::String(String::new()));
            }
            "repeat" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let n = args[1].to_int() as usize;
                    return Ok(Value::String(s.repeat(n)));
                }
                return Ok(Value::String(String::new()));
            }
            "char_at" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let i = args[1].to_int() as usize;
                    if let Some(c) = s.chars().nth(i) {
                        return Ok(Value::String(c.to_string()));
                    }
                }
                return Ok(Value::String(String::new()));
            }
            "char_code" => {
                if let Some(Value::String(s)) = args.first() {
                    if let Some(c) = s.chars().next() {
                        return Ok(Value::Int(c as i64));
                    }
                }
                return Ok(Value::Int(0));
            }
            "from_char_code" => {
                if let Some(arg) = args.first() {
                    let code = arg.to_int() as u32;
                    if let Some(c) = char::from_u32(code) {
                        return Ok(Value::String(c.to_string()));
                    }
                }
                return Ok(Value::String(String::new()));
            }
            "substring" | "substr" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let start = args[1].to_int() as usize;
                    let len = args.get(2).map(|v| v.to_int() as usize).unwrap_or(s.len() - start);
                    let result: String = s.chars().skip(start).take(len).collect();
                    return Ok(Value::String(result));
                }
                return Ok(Value::String(String::new()));
            }
            "index_of" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let sub = args[1].to_string();
                    return Ok(Value::Int(s.find(&sub).map(|i| i as i64).unwrap_or(-1)));
                }
                return Ok(Value::Int(-1));
            }
            "last_index_of" => {
                if args.len() >= 2 {
                    let s = args[0].to_string();
                    let sub = args[1].to_string();
                    return Ok(Value::Int(s.rfind(&sub).map(|i| i as i64).unwrap_or(-1)));
                }
                return Ok(Value::Int(-1));
            }
            
            // === Export Functions ===
            "export_json" => {
                if args.len() >= 2 {
                    let data = stringify_json(&args[0]);
                    if let Value::String(path) = &args[1] {
                        let _ = std::fs::write(path, data);
                    }
                }
                return Ok(Value::Null);
            }
            "export_csv" => {
                if args.len() >= 2 {
                    if let (Value::Array(rows), Value::String(path)) = (&args[0], &args[1]) {
                        let mut csv = String::new();
                        for row in rows {
                            if let Value::Array(cols) = row {
                                let line: Vec<String> = cols.iter().map(|v| v.to_string()).collect();
                                csv.push_str(&line.join(","));
                                csv.push('\n');
                            }
                        }
                        let _ = std::fs::write(path, csv);
                    }
                }
                return Ok(Value::Null);
            }
            "export_html" => {
                if args.len() >= 2 {
                    let content = args[0].to_string();
                    if let Value::String(path) = &args[1] {
                        let html = format!(r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Veyra Export</title>
</head>
<body>
{}
</body>
</html>"#, content);
                        let _ = std::fs::write(path, html);
                    }
                }
                return Ok(Value::Null);
            }
            
            // === Concurrency ===
            "spawn" => {
                // Note: True concurrency would require async runtime
                // This executes synchronously for now
                if let Some(Value::String(cmd)) = args.first() {
                    #[cfg(target_os = "windows")]
                    let _ = std::process::Command::new("cmd").args(["/C", cmd]).spawn();
                    #[cfg(not(target_os = "windows"))]
                    let _ = std::process::Command::new("sh").args(["-c", cmd]).spawn();
                }
                return Ok(Value::Null);
            }
            
            // === Network/HTTP ===
            "http_get" | "fetch" => {
                if let Some(Value::String(url)) = args.first() {
                    // Simple HTTP GET via TCP (no TLS support)
                    if url.starts_with("http://") {
                        let url_part = &url[7..];
                        if let Some(slash_pos) = url_part.find('/') {
                            let host = &url_part[..slash_pos];
                            let path = &url_part[slash_pos..];
                            
                            use std::net::TcpStream;
                            use std::io::{Read, Write};
                            
                            if let Ok(mut stream) = TcpStream::connect(format!("{}:80", host)) {
                                let request = format!(
                                    "GET {} HTTP/1.1\r\nHost: {}\r\nConnection: close\r\n\r\n",
                                    path, host
                                );
                                let _ = stream.write_all(request.as_bytes());
                                let mut response = String::new();
                                let _ = stream.read_to_string(&mut response);
                                
                                // Extract body
                                if let Some(body_start) = response.find("\r\n\r\n") {
                                    return Ok(Value::String(response[body_start+4..].to_string()));
                                }
                                return Ok(Value::String(response));
                            }
                        }
                    }
                }
                return Ok(Value::Null);
            }
            
            // === Type Checking ===
            "is_null" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::Null)).unwrap_or(true)));
            }
            "is_int" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::Int(_))).unwrap_or(false)));
            }
            "is_float" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::Float(_))).unwrap_or(false)));
            }
            "is_string" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::String(_))).unwrap_or(false)));
            }
            "is_array" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::Array(_))).unwrap_or(false)));
            }
            "is_object" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::Object(_))).unwrap_or(false)));
            }
            "is_bool" => {
                return Ok(Value::Bool(args.first().map(|v| matches!(v, Value::Bool(_))).unwrap_or(false)));
            }
            
            // === Object Operations ===
            "keys" => {
                if let Some(Value::Object(obj)) = args.first() {
                    let keys: Vec<Value> = obj.keys().map(|k| Value::String(k.clone())).collect();
                    return Ok(Value::Array(keys));
                }
                return Ok(Value::Array(vec![]));
            }
            "values" => {
                if let Some(Value::Object(obj)) = args.first() {
                    let vals: Vec<Value> = obj.values().cloned().collect();
                    return Ok(Value::Array(vals));
                }
                return Ok(Value::Array(vec![]));
            }
            "has_key" => {
                if args.len() >= 2 {
                    if let (Value::Object(obj), Value::String(key)) = (&args[0], &args[1]) {
                        return Ok(Value::Bool(obj.contains_key(key)));
                    }
                }
                return Ok(Value::Bool(false));
            }
            "get" => {
                if args.len() >= 2 {
                    match &args[0] {
                        Value::Object(obj) => {
                            let key = args[1].to_string();
                            return Ok(obj.get(&key).cloned().unwrap_or(
                                args.get(2).cloned().unwrap_or(Value::Null)
                            ));
                        }
                        Value::Array(arr) => {
                            let idx = args[1].to_int() as usize;
                            return Ok(arr.get(idx).cloned().unwrap_or(
                                args.get(2).cloned().unwrap_or(Value::Null)
                            ));
                        }
                        _ => {}
                    }
                }
                return Ok(Value::Null);
            }
            "set" => {
                if args.len() >= 3 {
                    match args[0].clone() {
                        Value::Object(mut obj) => {
                            let key = args[1].to_string();
                            obj.insert(key, args[2].clone());
                            return Ok(Value::Object(obj));
                        }
                        Value::Array(mut arr) => {
                            let idx = args[1].to_int() as usize;
                            if idx < arr.len() {
                                arr[idx] = args[2].clone();
                            }
                            return Ok(Value::Array(arr));
                        }
                        _ => {}
                    }
                }
                return Ok(Value::Null);
            }
            "merge" => {
                if args.len() >= 2 {
                    if let (Value::Object(mut a), Value::Object(b)) = (args[0].clone(), args[1].clone()) {
                        for (k, v) in b {
                            a.insert(k, v);
                        }
                        return Ok(Value::Object(a));
                    }
                    if let (Value::Array(mut a), Value::Array(b)) = (args[0].clone(), args[1].clone()) {
                        a.extend(b);
                        return Ok(Value::Array(a));
                    }
                }
                return Ok(Value::Null);
            }
            "clone" | "copy" => {
                return Ok(args.first().cloned().unwrap_or(Value::Null));
            }
            
            // === Print Formatting ===
            "printf" => {
                if let Some(Value::String(fmt)) = args.first() {
                    let mut result = fmt.clone();
                    for arg in args.iter().skip(1) {
                        if let Some(pos) = result.find("{}") {
                            result.replace_range(pos..pos+2, &arg.to_string());
                        }
                    }
                    print!("{}", result);
                }
                return Ok(Value::Null);
            }
            "sprintf" => {
                if let Some(Value::String(fmt)) = args.first() {
                    let mut result = fmt.clone();
                    for arg in args.iter().skip(1) {
                        if let Some(pos) = result.find("{}") {
                            result.replace_range(pos..pos+2, &arg.to_string());
                        }
                    }
                    return Ok(Value::String(result));
                }
                return Ok(Value::String(String::new()));
            }
            
            _ => {}
        }
        
        // User-defined functions
        if let Some(func) = self.functions.get(name).cloned() {
            self.push_scope();
            
            // Bind parameters
            for (i, param) in func.params.iter().enumerate() {
                let val = args.get(i).cloned().unwrap_or(Value::Null);
                self.set_var(param.name.clone(), val);
            }
            
            // Execute body
            let result = self.exec_block(&func.body)?;
            
            self.pop_scope();
            
            return Ok(result.unwrap_or(Value::Null));
        }
        
        Ok(Value::Null)
    }
    
    fn get_var(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        self.globals.get(name).cloned()
    }
    
    fn set_var(&mut self, name: String, value: Value) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, value);
        }
    }
    
    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    
    fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

// JSON helper functions
fn parse_json(s: &str) -> Value {
    let s = s.trim();
    
    if s == "null" {
        return Value::Null;
    }
    if s == "true" {
        return Value::Bool(true);
    }
    if s == "false" {
        return Value::Bool(false);
    }
    
    // Number
    if s.starts_with('-') || s.chars().next().map_or(false, |c| c.is_ascii_digit()) {
        if s.contains('.') {
            if let Ok(f) = s.parse::<f64>() {
                return Value::Float(f);
            }
        } else if let Ok(n) = s.parse::<i64>() {
            return Value::Int(n);
        }
    }
    
    // String
    if s.starts_with('"') && s.ends_with('"') {
        return Value::String(s[1..s.len()-1].to_string());
    }
    
    // Array
    if s.starts_with('[') && s.ends_with(']') {
        let inner = s[1..s.len()-1].trim();
        if inner.is_empty() {
            return Value::Array(vec![]);
        }
        // Simple parsing (doesn't handle nested)
        let items: Vec<Value> = inner.split(',')
            .map(|item| parse_json(item.trim()))
            .collect();
        return Value::Array(items);
    }
    
    // Object
    if s.starts_with('{') && s.ends_with('}') {
        let inner = s[1..s.len()-1].trim();
        if inner.is_empty() {
            return Value::Object(HashMap::new());
        }
        let mut obj = HashMap::new();
        for pair in inner.split(',') {
            if let Some(colon_pos) = pair.find(':') {
                let key = pair[..colon_pos].trim().trim_matches('"');
                let value = parse_json(pair[colon_pos+1..].trim());
                obj.insert(key.to_string(), value);
            }
        }
        return Value::Object(obj);
    }
    
    Value::String(s.to_string())
}

fn stringify_json(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(n) => n.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
        Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(stringify_json).collect();
            format!("[{}]", items.join(","))
        }
        Value::Object(obj) => {
            let items: Vec<String> = obj.iter()
                .map(|(k, v)| format!("\"{}\":{}", k, stringify_json(v)))
                .collect();
            format!("{{{}}}", items.join(","))
        }
        Value::Function(_) => "null".to_string(),
        Value::NativePtr(_) => "null".to_string(),
    }
}

