//! Veyra Token Definitions
//! 
//! All tokens that can appear in Veyra source code.

use logos::Logos;
use crate::error::Span;

/// A token with its kind, value, and source location
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub value: String,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, value: String, span: Span) -> Self {
        Self { kind, value, span }
    }
}

/// All token types in Veyra
#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\r]+")]
#[logos(skip r"//[^\n]*")]
pub enum TokenKind {
    // ============== KEYWORDS ==============
    #[token("fn")]
    Fn,
    
    #[token("let")]
    Let,
    
    #[token("mut")]
    Mut,
    
    #[token("const")]
    Const,
    
    #[token("if")]
    If,
    
    #[token("else")]
    Else,
    
    #[token("while")]
    While,
    
    #[token("for")]
    For,
    
    #[token("in")]
    In,
    
    #[token("loop")]
    Loop,
    
    #[token("break")]
    Break,
    
    #[token("continue")]
    Continue,
    
    #[token("return")]
    Return,
    
    #[token("true")]
    True,
    
    #[token("false")]
    False,
    
    #[token("null")]
    Null,
    
    #[token("class")]
    Class,
    
    #[token("struct")]
    Struct,
    
    #[token("enum")]
    Enum,
    
    #[token("impl")]
    Impl,
    
    #[token("trait")]
    Trait,
    
    #[token("pub")]
    Pub,
    
    #[token("import")]
    Import,
    
    #[token("export")]
    Export,
    
    #[token("async")]
    Async,
    
    #[token("await")]
    Await,
    
    #[token("try")]
    Try,
    
    #[token("catch")]
    Catch,
    
    #[token("throw")]
    Throw,
    
    #[token("new")]
    New,
    
    #[token("self")]
    SelfKw,
    
    #[token("match")]
    Match,
    
    // ============== TYPE KEYWORDS ==============
    #[token("int")]
    TypeInt,
    
    #[token("i8")]
    TypeI8,
    
    #[token("i16")]
    TypeI16,
    
    #[token("i32")]
    TypeI32,
    
    #[token("i64")]
    TypeI64,
    
    #[token("u8")]
    TypeU8,
    
    #[token("u16")]
    TypeU16,
    
    #[token("u32")]
    TypeU32,
    
    #[token("u64")]
    TypeU64,
    
    #[token("float")]
    TypeFloat,
    
    #[token("f32")]
    TypeF32,
    
    #[token("f64")]
    TypeF64,
    
    #[token("string")]
    TypeString,
    
    #[token("bool")]
    TypeBool,
    
    #[token("void")]
    TypeVoid,
    
    #[token("any")]
    TypeAny,
    
    // ============== LITERALS ==============
    #[regex(r"[0-9][0-9_]*")]
    IntLiteral,
    
    #[regex(r"0x[0-9a-fA-F][0-9a-fA-F_]*")]
    HexLiteral,
    
    #[regex(r"0b[01][01_]*")]
    BinaryLiteral,
    
    #[regex(r"0o[0-7][0-7_]*")]
    OctalLiteral,
    
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?")]
    FloatLiteral,
    
    #[regex(r#""([^"\\]|\\.)*""#)]
    StringLiteral,
    
    #[regex(r#"'([^'\\]|\\.)*'"#)]
    CharLiteral,
    
    #[regex(r"`[^`]*`")]
    TemplateString,
    
    // ============== IDENTIFIERS ==============
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,
    
    // ============== OPERATORS ==============
    #[token("+")]
    Plus,
    
    #[token("-")]
    Minus,
    
    #[token("*")]
    Star,
    
    #[token("/")]
    Slash,
    
    #[token("%")]
    Percent,
    
    #[token("**")]
    Power,
    
    #[token("=")]
    Assign,
    
    #[token("+=")]
    PlusAssign,
    
    #[token("-=")]
    MinusAssign,
    
    #[token("*=")]
    StarAssign,
    
    #[token("/=")]
    SlashAssign,
    
    #[token("==")]
    Eq,
    
    #[token("!=")]
    NotEq,
    
    #[token("<")]
    Lt,
    
    #[token(">")]
    Gt,
    
    #[token("<=")]
    LtEq,
    
    #[token(">=")]
    GtEq,
    
    #[token("&&")]
    And,
    
    #[token("||")]
    Or,
    
    #[token("!")]
    Not,
    
    #[token("&")]
    Ampersand,
    
    #[token("|")]
    Pipe,
    
    #[token("^")]
    Caret,
    
    #[token("~")]
    Tilde,
    
    #[token("<<")]
    ShiftLeft,
    
    #[token(">>")]
    ShiftRight,
    
    #[token("..")]
    Range,
    
    #[token("...")]
    Spread,
    
    #[token("->")]
    Arrow,
    
    #[token("=>")]
    FatArrow,
    
    #[token("?")]
    Question,
    
    #[token("??")]
    NullCoalesce,
    
    // ============== PUNCTUATION ==============
    #[token("(")]
    LParen,
    
    #[token(")")]
    RParen,
    
    #[token("{")]
    LBrace,
    
    #[token("}")]
    RBrace,
    
    #[token("[")]
    LBracket,
    
    #[token("]")]
    RBracket,
    
    #[token(";")]
    Semicolon,
    
    #[token(",")]
    Comma,
    
    #[token(".")]
    Dot,
    
    #[token(":")]
    Colon,
    
    #[token("::")]
    DoubleColon,
    
    #[token("@")]
    At,
    
    #[token("#")]
    Hash,
    
    // ============== SPECIAL ==============
    #[regex(r"\n")]
    Newline,
    
    Eof,
}

impl TokenKind {
    /// Get a human-readable name for the token
    pub fn name(&self) -> &'static str {
        match self {
            TokenKind::Fn => "fn",
            TokenKind::Let => "let",
            TokenKind::Mut => "mut",
            TokenKind::Const => "const",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::In => "in",
            TokenKind::Loop => "loop",
            TokenKind::Break => "break",
            TokenKind::Continue => "continue",
            TokenKind::Return => "return",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Null => "null",
            TokenKind::Class => "class",
            TokenKind::Struct => "struct",
            TokenKind::Enum => "enum",
            TokenKind::Impl => "impl",
            TokenKind::Trait => "trait",
            TokenKind::Pub => "pub",
            TokenKind::Import => "import",
            TokenKind::Export => "export",
            TokenKind::Async => "async",
            TokenKind::Await => "await",
            TokenKind::Try => "try",
            TokenKind::Catch => "catch",
            TokenKind::Throw => "throw",
            TokenKind::New => "new",
            TokenKind::SelfKw => "self",
            TokenKind::Match => "match",
            TokenKind::TypeInt => "int",
            TokenKind::TypeI8 => "i8",
            TokenKind::TypeI16 => "i16",
            TokenKind::TypeI32 => "i32",
            TokenKind::TypeI64 => "i64",
            TokenKind::TypeU8 => "u8",
            TokenKind::TypeU16 => "u16",
            TokenKind::TypeU32 => "u32",
            TokenKind::TypeU64 => "u64",
            TokenKind::TypeFloat => "float",
            TokenKind::TypeF32 => "f32",
            TokenKind::TypeF64 => "f64",
            TokenKind::TypeString => "string",
            TokenKind::TypeBool => "bool",
            TokenKind::TypeVoid => "void",
            TokenKind::TypeAny => "any",
            TokenKind::IntLiteral => "integer",
            TokenKind::HexLiteral => "hex literal",
            TokenKind::BinaryLiteral => "binary literal",
            TokenKind::OctalLiteral => "octal literal",
            TokenKind::FloatLiteral => "float",
            TokenKind::StringLiteral => "string",
            TokenKind::CharLiteral => "char",
            TokenKind::TemplateString => "template string",
            TokenKind::Identifier => "identifier",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Percent => "%",
            TokenKind::Power => "**",
            TokenKind::Assign => "=",
            TokenKind::PlusAssign => "+=",
            TokenKind::MinusAssign => "-=",
            TokenKind::StarAssign => "*=",
            TokenKind::SlashAssign => "/=",
            TokenKind::Eq => "==",
            TokenKind::NotEq => "!=",
            TokenKind::Lt => "<",
            TokenKind::Gt => ">",
            TokenKind::LtEq => "<=",
            TokenKind::GtEq => ">=",
            TokenKind::And => "&&",
            TokenKind::Or => "||",
            TokenKind::Not => "!",
            TokenKind::Ampersand => "&",
            TokenKind::Pipe => "|",
            TokenKind::Caret => "^",
            TokenKind::Tilde => "~",
            TokenKind::ShiftLeft => "<<",
            TokenKind::ShiftRight => ">>",
            TokenKind::Range => "..",
            TokenKind::Spread => "...",
            TokenKind::Arrow => "->",
            TokenKind::FatArrow => "=>",
            TokenKind::Question => "?",
            TokenKind::NullCoalesce => "??",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::LBracket => "[",
            TokenKind::RBracket => "]",
            TokenKind::Semicolon => ";",
            TokenKind::Comma => ",",
            TokenKind::Dot => ".",
            TokenKind::Colon => ":",
            TokenKind::DoubleColon => "::",
            TokenKind::At => "@",
            TokenKind::Hash => "#",
            TokenKind::Newline => "newline",
            TokenKind::Eof => "end of file",
        }
    }
    
    /// Check if this token is a keyword
    pub fn is_keyword(&self) -> bool {
        matches!(self,
            TokenKind::Fn | TokenKind::Let | TokenKind::Mut | TokenKind::Const |
            TokenKind::If | TokenKind::Else | TokenKind::While | TokenKind::For |
            TokenKind::In | TokenKind::Loop | TokenKind::Break | TokenKind::Continue |
            TokenKind::Return | TokenKind::True | TokenKind::False | TokenKind::Null |
            TokenKind::Class | TokenKind::Struct | TokenKind::Enum | TokenKind::Impl |
            TokenKind::Trait | TokenKind::Pub | TokenKind::Import | TokenKind::Export |
            TokenKind::Async | TokenKind::Await | TokenKind::Try | TokenKind::Catch |
            TokenKind::Throw | TokenKind::New | TokenKind::SelfKw | TokenKind::Match
        )
    }
    
    /// Check if this token is a type keyword
    pub fn is_type_keyword(&self) -> bool {
        matches!(self,
            TokenKind::TypeInt | TokenKind::TypeI8 | TokenKind::TypeI16 |
            TokenKind::TypeI32 | TokenKind::TypeI64 | TokenKind::TypeU8 |
            TokenKind::TypeU16 | TokenKind::TypeU32 | TokenKind::TypeU64 |
            TokenKind::TypeFloat | TokenKind::TypeF32 | TokenKind::TypeF64 |
            TokenKind::TypeString | TokenKind::TypeBool | TokenKind::TypeVoid |
            TokenKind::TypeAny
        )
    }
    
    /// Check if this token is a literal
    pub fn is_literal(&self) -> bool {
        matches!(self,
            TokenKind::IntLiteral | TokenKind::HexLiteral | TokenKind::BinaryLiteral |
            TokenKind::OctalLiteral | TokenKind::FloatLiteral | TokenKind::StringLiteral |
            TokenKind::CharLiteral | TokenKind::TemplateString | TokenKind::True |
            TokenKind::False | TokenKind::Null
        )
    }
    
    /// Check if this token is an operator
    pub fn is_operator(&self) -> bool {
        matches!(self,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash |
            TokenKind::Percent | TokenKind::Power | TokenKind::Eq | TokenKind::NotEq |
            TokenKind::Lt | TokenKind::Gt | TokenKind::LtEq | TokenKind::GtEq |
            TokenKind::And | TokenKind::Or | TokenKind::Not | TokenKind::Ampersand |
            TokenKind::Pipe | TokenKind::Caret | TokenKind::Tilde |
            TokenKind::ShiftLeft | TokenKind::ShiftRight
        )
    }
}
