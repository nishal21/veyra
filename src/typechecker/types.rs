//! Typed AST nodes - AST after type checking

use crate::parser::{Type, Parameter, Import, Field, BinaryOp, UnaryOp};

/// Type-checked program
#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub imports: Vec<Import>,
    pub functions: Vec<TypedFunction>,
    pub classes: Vec<TypedClass>,
    pub statements: Vec<TypedStmt>,
}

/// Type-checked function
#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: TypedBlock,
    pub is_public: bool,
    pub is_async: bool,
}

/// Type-checked class
#[derive(Debug, Clone)]
pub struct TypedClass {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<TypedFunction>,
    pub is_public: bool,
}

/// Type-checked block
#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub statements: Vec<TypedStmt>,
}

/// Type-checked statement
#[derive(Debug, Clone)]
pub enum TypedStmt {
    Let {
        name: String,
        ty: Type,
        value: Option<TypedExpr>,
        mutable: bool,
    },
    Const {
        name: String,
        ty: Type,
        value: TypedExpr,
    },
    Assign {
        target: TypedExpr,
        value: TypedExpr,
    },
    CompoundAssign {
        target: TypedExpr,
        op: BinaryOp,
        value: TypedExpr,
    },
    If {
        condition: TypedExpr,
        then_block: TypedBlock,
        else_block: Option<TypedBlock>,
    },
    While {
        condition: TypedExpr,
        body: TypedBlock,
    },
    For {
        var: String,
        iterable: TypedExpr,
        body: TypedBlock,
    },
    Loop {
        body: TypedBlock,
    },
    Break,
    Continue,
    Return(Option<TypedExpr>),
    Try {
        try_block: TypedBlock,
        error_var: String,
        catch_block: TypedBlock,
    },
    Match {
        value: TypedExpr,
        arms: Vec<TypedMatchArm>,
    },
    Block(TypedBlock),
    Expr(TypedExpr),
}

/// Type-checked match arm
#[derive(Debug, Clone)]
pub struct TypedMatchArm {
    pub pattern: TypedExpr,
    pub body: TypedStmt,
}

/// Type-checked expression with resolved type
#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub kind: TypedExprKind,
    pub ty: Type,
}

impl TypedExpr {
    pub fn new(kind: TypedExprKind, ty: Type) -> Self {
        Self { kind, ty }
    }
}

/// Type-checked expression kinds
#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Null,
    Identifier(String),
    Binary {
        left: Box<TypedExpr>,
        op: BinaryOp,
        right: Box<TypedExpr>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<TypedExpr>,
    },
    Call {
        callee: Box<TypedExpr>,
        args: Vec<TypedExpr>,
    },
    MethodCall {
        object: Box<TypedExpr>,
        method: String,
        args: Vec<TypedExpr>,
    },
    FieldAccess {
        object: Box<TypedExpr>,
        field: String,
    },
    Index {
        object: Box<TypedExpr>,
        index: Box<TypedExpr>,
    },
    Array(Vec<TypedExpr>),
    Object(Vec<(String, TypedExpr)>),
    Range {
        start: Box<TypedExpr>,
        end: Box<TypedExpr>,
    },
    New {
        class: String,
        args: Vec<TypedExpr>,
    },
    Lambda {
        params: Vec<Parameter>,
        body: TypedBlock,
    },
    Grouped(Box<TypedExpr>),
    Ternary {
        condition: Box<TypedExpr>,
        then_expr: Box<TypedExpr>,
        else_expr: Box<TypedExpr>,
    },
    Await(Box<TypedExpr>),
}
