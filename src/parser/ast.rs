//! Veyra Abstract Syntax Tree
//! 
//! Strongly-typed representation of Veyra programs.

/// A complete Veyra program
#[derive(Debug, Clone)]
pub struct Program {
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub classes: Vec<Class>,
    pub statements: Vec<Stmt>,
}

/// Import statement
#[derive(Debug, Clone)]
pub struct Import {
    pub path: Vec<String>,
}

/// Function definition
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
    pub is_public: bool,
    pub is_async: bool,
}

/// Function parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: Type,
}

/// Class definition
#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<Function>,
    pub is_public: bool,
}

/// Class field
#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

/// Block of statements
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Stmt>,
}

/// Veyra types
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Integer types
    Int,      // Default integer (i64)
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    
    // Floating point types
    Float,    // Default float (f64)
    F32,
    F64,
    
    // Other primitives
    String,
    Bool,
    Void,
    Null,
    Any,
    
    // Compound types
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Optional(Box<Type>),
    
    // User-defined
    Custom(String),
    
    // Type inference placeholder
    Inferred,
}

impl Type {
    /// Get a human-readable name for the type
    pub fn name(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::I8 => "i8".to_string(),
            Type::I16 => "i16".to_string(),
            Type::I32 => "i32".to_string(),
            Type::I64 => "i64".to_string(),
            Type::U8 => "u8".to_string(),
            Type::U16 => "u16".to_string(),
            Type::U32 => "u32".to_string(),
            Type::U64 => "u64".to_string(),
            Type::Float => "float".to_string(),
            Type::F32 => "f32".to_string(),
            Type::F64 => "f64".to_string(),
            Type::String => "string".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::Null => "null".to_string(),
            Type::Any => "any".to_string(),
            Type::Array(inner) => format!("[{}]", inner.name()),
            Type::Tuple(types) => {
                let names: Vec<_> = types.iter().map(|t| t.name()).collect();
                format!("({})", names.join(", "))
            }
            Type::Function(params, ret) => {
                let param_names: Vec<_> = params.iter().map(|t| t.name()).collect();
                format!("fn({}) -> {}", param_names.join(", "), ret.name())
            }
            Type::Optional(inner) => format!("{}?", inner.name()),
            Type::Custom(name) => name.clone(),
            Type::Inferred => "_".to_string(),
        }
    }
    
    /// Check if types are compatible
    pub fn is_compatible(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }
        
        // Any is compatible with everything
        if matches!(self, Type::Any) || matches!(other, Type::Any) {
            return true;
        }
        
        // Inferred is compatible with everything
        if matches!(self, Type::Inferred) || matches!(other, Type::Inferred) {
            return true;
        }
        
        // Int types are compatible with each other
        if self.is_integer() && other.is_integer() {
            return true;
        }
        
        // Float types are compatible with each other
        if self.is_float() && other.is_float() {
            return true;
        }
        
        // Integers can be promoted to floats
        if self.is_integer() && other.is_float() {
            return true;
        }
        
        false
    }
    
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Int | Type::I8 | Type::I16 | Type::I32 | Type::I64 |
                      Type::U8 | Type::U16 | Type::U32 | Type::U64)
    }
    
    pub fn is_float(&self) -> bool {
        matches!(self, Type::Float | Type::F32 | Type::F64)
    }
    
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }
}

/// Statement nodes
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Variable declaration: let x = 5;
    Let {
        name: String,
        ty: Type,
        value: Option<Expr>,
        mutable: bool,
    },
    
    /// Constant declaration: const PI = 3.14;
    Const {
        name: String,
        ty: Type,
        value: Expr,
    },
    
    /// Assignment: x = 10;
    Assign {
        target: Expr,
        value: Expr,
    },
    
    /// Compound assignment: x += 5;
    CompoundAssign {
        target: Expr,
        op: BinaryOp,
        value: Expr,
    },
    
    /// If statement
    If {
        condition: Expr,
        then_block: Block,
        else_block: Option<Block>,
    },
    
    /// While loop
    While {
        condition: Expr,
        body: Block,
    },
    
    /// For loop
    For {
        var: String,
        iterable: Expr,
        body: Block,
    },
    
    /// Infinite loop
    Loop {
        body: Block,
    },
    
    /// Break from loop
    Break,
    
    /// Continue to next iteration
    Continue,
    
    /// Return from function
    Return(Option<Expr>),
    
    /// Try-catch
    Try {
        try_block: Block,
        error_var: String,
        catch_block: Block,
    },
    
    /// Match expression as statement
    Match {
        value: Expr,
        arms: Vec<MatchArm>,
    },
    
    /// Block of statements
    Block(Block),
    
    /// Expression statement
    Expr(Expr),
}

/// Match arm
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Expr,
    pub body: Stmt,
}

/// Expression nodes
#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
    
    // Identifier
    Identifier(String),
    
    // Binary operation
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    
    // Unary operation
    Unary {
        op: UnaryOp,
        operand: Box<Expr>,
    },
    
    // Function call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },
    
    // Method call
    MethodCall {
        object: Box<Expr>,
        method: String,
        args: Vec<Expr>,
    },
    
    // Field access
    FieldAccess {
        object: Box<Expr>,
        field: String,
    },
    
    // Index access
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    
    // Array literal
    Array(Vec<Expr>),
    
    // Object literal
    Object(Vec<(String, Expr)>),
    
    // Range
    Range {
        start: Box<Expr>,
        end: Box<Expr>,
    },
    
    // New instance
    New {
        class: String,
        args: Vec<Expr>,
    },
    
    // Lambda/anonymous function
    Lambda {
        params: Vec<Parameter>,
        body: Block,
    },
    
    // Grouped expression (parentheses)
    Grouped(Box<Expr>),
    
    // Ternary/conditional
    Ternary {
        condition: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    
    // Await expression
    Await(Box<Expr>),
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOp {
    // Arithmetic
    Add,    // +
    Sub,    // -
    Mul,    // *
    Div,    // /
    Mod,    // %
    Pow,    // **
    
    // Comparison
    Eq,     // ==
    NotEq,  // !=
    Lt,     // <
    Gt,     // >
    LtEq,   // <=
    GtEq,   // >=
    
    // Logical
    And,    // &&
    Or,     // ||
    
    // Bitwise
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
    Shl,    // <<
    Shr,    // >>
}

impl BinaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Pow => "**",
            BinaryOp::Eq => "==",
            BinaryOp::NotEq => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Gt => ">",
            BinaryOp::LtEq => "<=",
            BinaryOp::GtEq => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // !
    BitNot, // ~
}

impl UnaryOp {
    pub fn name(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        }
    }
}
