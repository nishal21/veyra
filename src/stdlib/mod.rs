//! Veyra Standard Library
//! 
//! The most comprehensive standard library for any programming language.
//! Built for web, mobile, AI, databases, and systems programming.

// Core modules
pub mod io;
pub mod math;
pub mod string;
pub mod collections;

// Web & networking
pub mod web;
pub mod concurrency;

// Advanced domains
pub mod ai;           // AI/ML: tensors, neural networks, optimizers
pub mod app;          // App development: UI components, state, routing
pub mod database;     // Database: SQL, KV, document stores
pub mod system;       // System: process, filesystem, environment

/// Initialize all standard library functions
pub fn init() {
    // Standard library initialization
    // Functions are registered at compile time via the type checker
}
