//! Veyra Runtime Library
//! 
//! Runtime support for advanced language features including
//! dynamic typing, reflection, interop with other languages.

use std::any::Any;
use std::collections::HashMap;
use std::sync::Arc;

/// Dynamic value that can hold any Veyra type
#[derive(Clone)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
    Function(Arc<dyn Fn(Vec<Value>) -> Value + Send + Sync>),
    NativePtr(*mut std::ffi::c_void),
}

impl Value {
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
    
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::Object(o) => !o.is_empty(),
            Value::Function(_) => true,
            Value::NativePtr(p) => !p.is_null(),
        }
    }
    
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Null => "null",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Object(_) => "object",
            Value::Function(_) => "function",
            Value::NativePtr(_) => "native_ptr",
        }
    }
    
    pub fn to_int(&self) -> i64 {
        match self {
            Value::Int(n) => *n,
            Value::Float(f) => *f as i64,
            Value::Bool(b) => if *b { 1 } else { 0 },
            Value::String(s) => s.parse().unwrap_or(0),
            _ => 0,
        }
    }
    
    pub fn to_float(&self) -> f64 {
        match self {
            Value::Float(f) => *f,
            Value::Int(n) => *n as f64,
            Value::Bool(b) => if *b { 1.0 } else { 0.0 },
            Value::String(s) => s.parse().unwrap_or(0.0),
            _ => 0.0,
        }
    }
    
    pub fn to_string(&self) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => s.clone(),
            Value::Array(a) => {
                let items: Vec<String> = a.iter().map(|v| v.to_string()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Object(o) => {
                let items: Vec<String> = o.iter()
                    .map(|(k, v)| format!("{}: {}", k, v.to_string()))
                    .collect();
                format!("{{{}}}", items.join(", "))
            }
            Value::Function(_) => "<function>".to_string(),
            Value::NativePtr(p) => format!("<native_ptr:{:p}>", p),
        }
    }
    
    pub fn to_bool(&self) -> bool {
        self.is_truthy()
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) => {
                (*a as f64 - b).abs() < f64::EPSILON
            }
            _ => false,
        }
    }
}

/// Runtime environment for dynamic execution
pub struct Runtime {
    globals: HashMap<String, Value>,
    call_stack: Vec<HashMap<String, Value>>,
}

impl Runtime {
    pub fn new() -> Self {
        let mut globals = HashMap::new();
        
        // Register built-in constants
        globals.insert("PI".to_string(), Value::Float(std::f64::consts::PI));
        globals.insert("E".to_string(), Value::Float(std::f64::consts::E));
        globals.insert("INFINITY".to_string(), Value::Float(f64::INFINITY));
        globals.insert("NAN".to_string(), Value::Float(f64::NAN));
        
        Self {
            globals,
            call_stack: vec![HashMap::new()],
        }
    }
    
    pub fn get(&self, name: &str) -> Option<&Value> {
        // Search local scopes first (innermost to outermost)
        for scope in self.call_stack.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v);
            }
        }
        // Then globals
        self.globals.get(name)
    }
    
    pub fn set(&mut self, name: String, value: Value) {
        if let Some(scope) = self.call_stack.last_mut() {
            scope.insert(name, value);
        }
    }
    
    pub fn set_global(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }
    
    pub fn push_scope(&mut self) {
        self.call_stack.push(HashMap::new());
    }
    
    pub fn pop_scope(&mut self) {
        if self.call_stack.len() > 1 {
            self.call_stack.pop();
        }
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

/// Foreign Function Interface for calling native code
pub mod ffi {
    use super::*;
    use std::ffi::{CStr, CString};
    
    /// Load a shared library
    #[cfg(not(target_family = "wasm"))]
    pub fn load_library(path: &str) -> Result<*mut std::ffi::c_void, String> {
        #[cfg(target_os = "windows")]
        {
            use std::os::windows::ffi::OsStrExt;
            use std::ffi::OsStr;
            
            let wide: Vec<u16> = OsStr::new(path)
                .encode_wide()
                .chain(Some(0))
                .collect();
            
            let handle = unsafe {
                // Windows LoadLibraryW
                extern "system" {
                    fn LoadLibraryW(lpLibFileName: *const u16) -> *mut std::ffi::c_void;
                }
                LoadLibraryW(wide.as_ptr())
            };
            
            if handle.is_null() {
                Err(format!("Failed to load library: {}", path))
            } else {
                Ok(handle)
            }
        }
        
        #[cfg(target_family = "unix")]
        {
            let c_path = CString::new(path).map_err(|e| e.to_string())?;
            let handle = unsafe {
                extern "C" {
                    fn dlopen(filename: *const i8, flags: i32) -> *mut std::ffi::c_void;
                }
                dlopen(c_path.as_ptr(), 1) // RTLD_LAZY
            };
            
            if handle.is_null() {
                Err(format!("Failed to load library: {}", path))
            } else {
                Ok(handle)
            }
        }
    }
    
    #[cfg(target_family = "wasm")]
    pub fn load_library(_path: &str) -> Result<*mut std::ffi::c_void, String> {
        Err("FFI not supported in WebAssembly".to_string())
    }
    
    /// Convert Veyra value to C compatible format
    pub fn to_c_value(value: &Value) -> Result<*mut std::ffi::c_void, String> {
        match value {
            Value::Int(n) => Ok(*n as *mut std::ffi::c_void),
            Value::Float(f) => {
                let bits = f.to_bits();
                Ok(bits as *mut std::ffi::c_void)
            }
            Value::String(s) => {
                let c_str = CString::new(s.as_str()).map_err(|e| e.to_string())?;
                Ok(c_str.into_raw() as *mut std::ffi::c_void)
            }
            Value::NativePtr(p) => Ok(*p),
            _ => Err(format!("Cannot convert {} to C value", value.type_name())),
        }
    }
}

/// Async runtime support
pub mod async_runtime {
    use super::*;
    use std::future::Future;
    use std::pin::Pin;
    use std::task::{Context, Poll, Waker};
    use std::sync::{Mutex, Arc};
    use std::collections::VecDeque;
    
    /// Simple task queue for async execution
    pub struct TaskQueue {
        tasks: Mutex<VecDeque<Pin<Box<dyn Future<Output = Value> + Send>>>>,
    }
    
    impl TaskQueue {
        pub fn new() -> Self {
            Self {
                tasks: Mutex::new(VecDeque::new()),
            }
        }
        
        pub fn spawn<F>(&self, future: F) 
        where
            F: Future<Output = Value> + Send + 'static,
        {
            let mut tasks = self.tasks.lock().unwrap();
            tasks.push_back(Box::pin(future));
        }
    }
    
    impl Default for TaskQueue {
        fn default() -> Self {
            Self::new()
        }
    }
}

/// Pattern matching utilities
pub mod patterns {
    use super::*;
    
    /// Check if value matches a pattern
    pub fn matches(value: &Value, pattern: &Value) -> bool {
        match (value, pattern) {
            // Wildcard matches everything
            (_, Value::String(s)) if s == "_" => true,
            
            // Exact match
            (a, b) if a == b => true,
            
            // Array pattern matching
            (Value::Array(arr), Value::Array(pat)) => {
                if arr.len() != pat.len() {
                    return false;
                }
                arr.iter().zip(pat.iter()).all(|(v, p)| matches(v, p))
            }
            
            // Object pattern matching (pattern fields must exist in value)
            (Value::Object(obj), Value::Object(pat)) => {
                pat.iter().all(|(k, pv)| {
                    obj.get(k).map_or(false, |v| matches(v, pv))
                })
            }
            
            // Type pattern matching
            (v, Value::String(s)) if s.starts_with("type:") => {
                let type_name = &s[5..];
                v.type_name() == type_name
            }
            
            _ => false,
        }
    }
    
    /// Extract bindings from pattern match
    pub fn extract_bindings(value: &Value, pattern: &Value) -> HashMap<String, Value> {
        let mut bindings = HashMap::new();
        extract_bindings_recursive(value, pattern, &mut bindings);
        bindings
    }
    
    fn extract_bindings_recursive(
        value: &Value,
        pattern: &Value,
        bindings: &mut HashMap<String, Value>,
    ) {
        match pattern {
            // Named binding: starts with $
            Value::String(s) if s.starts_with('$') => {
                let name = s[1..].to_string();
                bindings.insert(name, value.clone());
            }
            
            // Array destructuring
            (Value::Array(parr)) => {
                if let Value::Array(arr) = value {
                    for (v, p) in arr.iter().zip(parr.iter()) {
                        extract_bindings_recursive(v, p, bindings);
                    }
                }
            }
            
            // Object destructuring
            (Value::Object(pobj)) => {
                if let Value::Object(obj) = value {
                    for (k, pv) in pobj {
                        if let Some(v) = obj.get(k) {
                            extract_bindings_recursive(v, pv, bindings);
                        }
                    }
                }
            }
            
            _ => {}
        }
    }
}

/// Generators and iterators
pub mod generators {
    use super::*;
    
    /// Generator state
    pub struct Generator {
        state: i64,
        next_fn: Box<dyn Fn(i64) -> Option<(Value, i64)>>,
    }
    
    impl Generator {
        pub fn new<F>(next_fn: F) -> Self
        where
            F: Fn(i64) -> Option<(Value, i64)> + 'static,
        {
            Self {
                state: 0,
                next_fn: Box::new(next_fn),
            }
        }
        
        pub fn next(&mut self) -> Option<Value> {
            let result = (self.next_fn)(self.state)?;
            self.state = result.1;
            Some(result.0)
        }
    }
    
    impl Iterator for Generator {
        type Item = Value;
        
        fn next(&mut self) -> Option<Self::Item> {
            Generator::next(self)
        }
    }
    
    /// Range generator
    pub fn range(start: i64, end: i64) -> Generator {
        Generator::new(move |state| {
            let current = start + state;
            if current < end {
                Some((Value::Int(current), state + 1))
            } else {
                None
            }
        })
    }
    
    /// Infinite sequence generator
    pub fn infinite(start: i64) -> Generator {
        Generator::new(move |state| {
            Some((Value::Int(start + state), state + 1))
        })
    }
}

/// Serialization and deserialization
pub mod serde {
    use super::*;
    
    /// Serialize value to JSON string
    pub fn to_json(value: &Value) -> String {
        match value {
            Value::Null => "null".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Int(n) => n.to_string(),
            Value::Float(f) => {
                if f.is_nan() {
                    "null".to_string()
                } else if f.is_infinite() {
                    if *f > 0.0 { "1e999".to_string() } else { "-1e999".to_string() }
                } else {
                    f.to_string()
                }
            }
            Value::String(s) => format!("\"{}\"", escape_json(s)),
            Value::Array(arr) => {
                let items: Vec<String> = arr.iter().map(|v| to_json(v)).collect();
                format!("[{}]", items.join(","))
            }
            Value::Object(obj) => {
                let items: Vec<String> = obj.iter()
                    .map(|(k, v)| format!("\"{}\":{}", escape_json(k), to_json(v)))
                    .collect();
                format!("{{{}}}", items.join(","))
            }
            Value::Function(_) => "null".to_string(),
            Value::NativePtr(_) => "null".to_string(),
        }
    }
    
    fn escape_json(s: &str) -> String {
        let mut result = String::with_capacity(s.len());
        for c in s.chars() {
            match c {
                '"' => result.push_str("\\\""),
                '\\' => result.push_str("\\\\"),
                '\n' => result.push_str("\\n"),
                '\r' => result.push_str("\\r"),
                '\t' => result.push_str("\\t"),
                c if c.is_control() => {
                    result.push_str(&format!("\\u{:04x}", c as u32));
                }
                c => result.push(c),
            }
        }
        result
    }
    
    /// Parse JSON string to value
    pub fn from_json(json: &str) -> Result<Value, String> {
        let json = json.trim();
        
        if json == "null" {
            return Ok(Value::Null);
        }
        
        if json == "true" {
            return Ok(Value::Bool(true));
        }
        
        if json == "false" {
            return Ok(Value::Bool(false));
        }
        
        // Number
        if json.starts_with('-') || json.chars().next().map_or(false, |c| c.is_ascii_digit()) {
            if json.contains('.') || json.contains('e') || json.contains('E') {
                return json.parse::<f64>()
                    .map(Value::Float)
                    .map_err(|e| e.to_string());
            } else {
                return json.parse::<i64>()
                    .map(Value::Int)
                    .map_err(|e| e.to_string());
            }
        }
        
        // String
        if json.starts_with('"') && json.ends_with('"') {
            let inner = &json[1..json.len()-1];
            return Ok(Value::String(unescape_json(inner)));
        }
        
        // Array
        if json.starts_with('[') && json.ends_with(']') {
            let inner = json[1..json.len()-1].trim();
            if inner.is_empty() {
                return Ok(Value::Array(vec![]));
            }
            // Simple split (doesn't handle nested structures)
            let items: Result<Vec<Value>, String> = inner
                .split(',')
                .map(|s| from_json(s.trim()))
                .collect();
            return items.map(Value::Array);
        }
        
        // Object
        if json.starts_with('{') && json.ends_with('}') {
            let inner = json[1..json.len()-1].trim();
            if inner.is_empty() {
                return Ok(Value::Object(HashMap::new()));
            }
            let mut obj = HashMap::new();
            // Simple parse (doesn't handle complex nested structures)
            for pair in inner.split(',') {
                let pair = pair.trim();
                if let Some(colon_pos) = pair.find(':') {
                    let key = pair[..colon_pos].trim().trim_matches('"');
                    let value = from_json(pair[colon_pos+1..].trim())?;
                    obj.insert(key.to_string(), value);
                }
            }
            return Ok(Value::Object(obj));
        }
        
        Err(format!("Invalid JSON: {}", json))
    }
    
    fn unescape_json(s: &str) -> String {
        let mut result = String::new();
        let mut chars = s.chars().peekable();
        
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('"') => result.push('"'),
                    Some('\\') => result.push('\\'),
                    Some('n') => result.push('\n'),
                    Some('r') => result.push('\r'),
                    Some('t') => result.push('\t'),
                    Some(c) => {
                        result.push('\\');
                        result.push(c);
                    }
                    None => result.push('\\'),
                }
            } else {
                result.push(c);
            }
        }
        
        result
    }
}
