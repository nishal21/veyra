//! Veyra Code Generator - LLVM Backend
//! 
//! Generates native machine code using LLVM.

use crate::error::VeyraError;
use crate::parser::{Type, BinaryOp, UnaryOp};
use crate::typechecker::{TypedProgram, TypedFunction, TypedBlock, TypedStmt, TypedExpr, TypedExprKind};

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{InitializationConfig, Target, TargetMachine, RelocMode, CodeModel, FileType};
use inkwell::types::{BasicType, BasicTypeEnum, BasicMetadataTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue, IntValue};
use inkwell::IntPredicate;
use inkwell::FloatPredicate;
use inkwell::OptimizationLevel;
use inkwell::AddressSpace;

use rustc_hash::FxHashMap;
use std::path::Path;

/// Code generator using LLVM
pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    
    /// Variables in current scope
    variables: FxHashMap<String, PointerValue<'ctx>>,
    
    /// Current function being compiled
    current_fn: Option<FunctionValue<'ctx>>,
    
    /// String constants pool
    string_pool: FxHashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        
        Self {
            context,
            module,
            builder,
            variables: FxHashMap::default(),
            current_fn: None,
            string_pool: FxHashMap::default(),
        }
    }
    
    /// Generate native code from typed AST
    pub fn generate(&mut self, program: TypedProgram, output_path: &str) -> Result<(), VeyraError> {
        // Declare external functions (printf, etc.)
        self.declare_externals();
        
        // Generate all functions
        for func in &program.functions {
            self.declare_function(func);
        }
        
        for func in program.functions {
            self.compile_function(func)?;
        }
        
        // Generate main wrapper if main exists
        if let Some(main_fn) = self.module.get_function("main") {
            self.generate_entry_point(main_fn);
        }
        
        // Emit object file
        self.emit_executable(output_path)?;
        
        Ok(())
    }
    
    /// JIT execute the program
    pub fn jit_execute(&mut self, program: TypedProgram) -> Result<i32, VeyraError> {
        // Declare external functions
        self.declare_externals();
        
        // Generate all functions
        for func in &program.functions {
            self.declare_function(func);
        }
        
        for func in program.functions {
            self.compile_function(func)?;
        }
        
        // Create execution engine
        let execution_engine = self.module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .map_err(|e| VeyraError::RuntimeError { 
                message: format!("Failed to create JIT engine: {}", e) 
            })?;
        
        // Get main function
        let main_fn = unsafe {
            execution_engine.get_function::<unsafe extern "C" fn() -> i32>("main")
        };
        
        match main_fn {
            Ok(main) => {
                let result = unsafe { main.call() };
                Ok(result)
            }
            Err(_) => {
                Err(VeyraError::RuntimeError {
                    message: "No main function found".to_string(),
                })
            }
        }
    }
    
    fn declare_externals(&self) {
        let i32_type = self.context.i32_type();
        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();
        let i8_ptr = self.context.i8_type().ptr_type(AddressSpace::default());
        
        // printf
        let printf_type = i32_type.fn_type(
            &[i8_ptr.into()],
            true, // variadic
        );
        self.module.add_function("printf", printf_type, None);
        
        // puts
        let puts_type = i32_type.fn_type(&[i8_ptr.into()], false);
        self.module.add_function("puts", puts_type, None);
        
        // malloc
        let malloc_type = i8_ptr.fn_type(&[i64_type.into()], false);
        self.module.add_function("malloc", malloc_type, None);
        
        // free
        let free_type = self.context.void_type().fn_type(&[i8_ptr.into()], false);
        self.module.add_function("free", free_type, None);
        
        // exit
        let exit_type = self.context.void_type().fn_type(&[i32_type.into()], false);
        self.module.add_function("exit", exit_type, None);
        
        // Math functions
        let f64_f64 = f64_type.fn_type(&[f64_type.into()], false);
        self.module.add_function("sin", f64_f64, None);
        self.module.add_function("cos", f64_f64, None);
        self.module.add_function("sqrt", f64_f64, None);
        self.module.add_function("floor", f64_f64, None);
        self.module.add_function("ceil", f64_f64, None);
        
        let pow_type = f64_type.fn_type(&[f64_type.into(), f64_type.into()], false);
        self.module.add_function("pow", pow_type, None);
    }
    
    fn declare_function(&mut self, func: &TypedFunction) {
        let ret_type = self.convert_type(&func.return_type);
        let param_types: Vec<BasicMetadataTypeEnum> = func.params
            .iter()
            .filter_map(|p| self.convert_type(&p.ty).map(|t| t.into()))
            .collect();
        
        let fn_type = match ret_type {
            Some(t) => t.fn_type(&param_types, false),
            None => self.context.void_type().fn_type(&param_types, false),
        };
        
        self.module.add_function(&func.name, fn_type, None);
    }
    
    fn compile_function(&mut self, func: TypedFunction) -> Result<(), VeyraError> {
        let function = self.module.get_function(&func.name).unwrap();
        self.current_fn = Some(function);
        
        // Create entry block
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        
        // Clear previous variables
        self.variables.clear();
        
        // Allocate space for parameters
        for (i, param) in func.params.iter().enumerate() {
            let param_value = function.get_nth_param(i as u32).unwrap();
            let param_type = self.convert_type(&param.ty).unwrap();
            let alloca = self.builder.build_alloca(param_type, &param.name).unwrap();
            self.builder.build_store(alloca, param_value).unwrap();
            self.variables.insert(param.name.clone(), alloca);
        }
        
        // Compile body
        self.compile_block(&func.body)?;
        
        // Add implicit return if needed
        if func.return_type == Type::Void {
            if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                self.builder.build_return(None).unwrap();
            }
        }
        
        Ok(())
    }
    
    fn compile_block(&mut self, block: &TypedBlock) -> Result<(), VeyraError> {
        for stmt in &block.statements {
            self.compile_statement(stmt)?;
        }
        Ok(())
    }
    
    fn compile_statement(&mut self, stmt: &TypedStmt) -> Result<(), VeyraError> {
        match stmt {
            TypedStmt::Let { name, ty, value, .. } => {
                let var_type = self.convert_type(ty).unwrap_or(self.context.i64_type().into());
                let alloca = self.builder.build_alloca(var_type, name).unwrap();
                
                if let Some(val) = value {
                    let compiled = self.compile_expression(val)?;
                    self.builder.build_store(alloca, compiled).unwrap();
                }
                
                self.variables.insert(name.clone(), alloca);
            }
            
            TypedStmt::Const { name, ty, value } => {
                let var_type = self.convert_type(ty).unwrap_or(self.context.i64_type().into());
                let alloca = self.builder.build_alloca(var_type, name).unwrap();
                let compiled = self.compile_expression(value)?;
                self.builder.build_store(alloca, compiled).unwrap();
                self.variables.insert(name.clone(), alloca);
            }
            
            TypedStmt::Assign { target, value } => {
                let val = self.compile_expression(value)?;
                if let TypedExprKind::Identifier(name) = &target.kind {
                    if let Some(ptr) = self.variables.get(name) {
                        self.builder.build_store(*ptr, val).unwrap();
                    }
                }
            }
            
            TypedStmt::CompoundAssign { target, op, value } => {
                if let TypedExprKind::Identifier(name) = &target.kind {
                    if let Some(ptr) = self.variables.get(name).cloned() {
                        let current = self.builder.build_load(
                            self.context.i64_type(),
                            ptr,
                            "load"
                        ).unwrap();
                        let val = self.compile_expression(value)?;
                        let result = self.compile_binary_op(current, *op, val);
                        self.builder.build_store(ptr, result).unwrap();
                    }
                }
            }
            
            TypedStmt::If { condition, then_block, else_block } => {
                let cond_val = self.compile_expression(condition)?;
                let cond_bool = self.to_bool(cond_val);
                
                let function = self.current_fn.unwrap();
                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "merge");
                
                self.builder.build_conditional_branch(cond_bool, then_bb, else_bb).unwrap();
                
                // Then block
                self.builder.position_at_end(then_bb);
                self.compile_block(then_block)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                
                // Else block
                self.builder.position_at_end(else_bb);
                if let Some(eb) = else_block {
                    self.compile_block(eb)?;
                }
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                }
                
                self.builder.position_at_end(merge_bb);
            }
            
            TypedStmt::While { condition, body } => {
                let function = self.current_fn.unwrap();
                let cond_bb = self.context.append_basic_block(function, "while.cond");
                let body_bb = self.context.append_basic_block(function, "while.body");
                let end_bb = self.context.append_basic_block(function, "while.end");
                
                self.builder.build_unconditional_branch(cond_bb).unwrap();
                
                // Condition
                self.builder.position_at_end(cond_bb);
                let cond_val = self.compile_expression(condition)?;
                let cond_bool = self.to_bool(cond_val);
                self.builder.build_conditional_branch(cond_bool, body_bb, end_bb).unwrap();
                
                // Body
                self.builder.position_at_end(body_bb);
                self.compile_block(body)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(cond_bb).unwrap();
                }
                
                self.builder.position_at_end(end_bb);
            }
            
            TypedStmt::For { var, iterable, body } => {
                // Simple range-based for loop
                let function = self.current_fn.unwrap();
                
                // Get start and end from range
                let (start, end) = if let TypedExprKind::Range { start: s, end: e } = &iterable.kind {
                    (self.compile_expression(s)?, self.compile_expression(e)?)
                } else {
                    // Fallback for other iterables
                    let zero = self.context.i64_type().const_int(0, false);
                    let ten = self.context.i64_type().const_int(10, false);
                    (zero.into(), ten.into())
                };
                
                // Allocate loop variable
                let var_alloca = self.builder.build_alloca(self.context.i64_type(), var).unwrap();
                self.builder.build_store(var_alloca, start).unwrap();
                self.variables.insert(var.clone(), var_alloca);
                
                let cond_bb = self.context.append_basic_block(function, "for.cond");
                let body_bb = self.context.append_basic_block(function, "for.body");
                let inc_bb = self.context.append_basic_block(function, "for.inc");
                let end_bb = self.context.append_basic_block(function, "for.end");
                
                self.builder.build_unconditional_branch(cond_bb).unwrap();
                
                // Condition: i < end
                self.builder.position_at_end(cond_bb);
                let current = self.builder.build_load(
                    self.context.i64_type(),
                    var_alloca,
                    "i"
                ).unwrap().into_int_value();
                let cond = self.builder.build_int_compare(
                    IntPredicate::SLT,
                    current,
                    end.into_int_value(),
                    "cond"
                ).unwrap();
                self.builder.build_conditional_branch(cond, body_bb, end_bb).unwrap();
                
                // Body
                self.builder.position_at_end(body_bb);
                self.compile_block(body)?;
                if self.builder.get_insert_block().unwrap().get_terminator().is_none() {
                    self.builder.build_unconditional_branch(inc_bb).unwrap();
                }
                
                // Increment
                self.builder.position_at_end(inc_bb);
                let current = self.builder.build_load(
                    self.context.i64_type(),
                    var_alloca,
                    "i"
                ).unwrap().into_int_value();
                let one = self.context.i64_type().const_int(1, false);
                let next = self.builder.build_int_add(current, one, "next").unwrap();
                self.builder.build_store(var_alloca, next).unwrap();
                self.builder.build_unconditional_branch(cond_bb).unwrap();
                
                self.builder.position_at_end(end_bb);
            }
            
            TypedStmt::Return(value) => {
                if let Some(val) = value {
                    let ret_val = self.compile_expression(val)?;
                    self.builder.build_return(Some(&ret_val)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
            }
            
            TypedStmt::Expr(expr) => {
                self.compile_expression(expr)?;
            }
            
            TypedStmt::Block(block) => {
                self.compile_block(block)?;
            }
            
            // TODO: Implement remaining statement types
            _ => {}
        }
        
        Ok(())
    }
    
    fn compile_expression(&mut self, expr: &TypedExpr) -> Result<BasicValueEnum<'ctx>, VeyraError> {
        match &expr.kind {
            TypedExprKind::Int(v) => {
                Ok(self.context.i64_type().const_int(*v as u64, true).into())
            }
            
            TypedExprKind::Float(v) => {
                Ok(self.context.f64_type().const_float(*v).into())
            }
            
            TypedExprKind::String(s) => {
                // Check if string is already in pool
                if let Some(ptr) = self.string_pool.get(s) {
                    return Ok((*ptr).into());
                }
                
                let string_val = self.builder.build_global_string_ptr(s, "str").unwrap();
                self.string_pool.insert(s.clone(), string_val.as_pointer_value());
                Ok(string_val.as_pointer_value().into())
            }
            
            TypedExprKind::Bool(b) => {
                Ok(self.context.bool_type().const_int(*b as u64, false).into())
            }
            
            TypedExprKind::Null => {
                Ok(self.context.i8_type().ptr_type(AddressSpace::default()).const_null().into())
            }
            
            TypedExprKind::Identifier(name) => {
                if let Some(ptr) = self.variables.get(name) {
                    let ty = self.convert_type(&expr.ty).unwrap_or(self.context.i64_type().into());
                    let val = self.builder.build_load(ty, *ptr, name).unwrap();
                    Ok(val)
                } else {
                    // Unknown variable, return 0
                    Ok(self.context.i64_type().const_int(0, false).into())
                }
            }
            
            TypedExprKind::Binary { left, op, right } => {
                let lhs = self.compile_expression(left)?;
                let rhs = self.compile_expression(right)?;
                Ok(self.compile_binary_op(lhs, *op, rhs))
            }
            
            TypedExprKind::Unary { op, operand } => {
                let val = self.compile_expression(operand)?;
                Ok(self.compile_unary_op(*op, val))
            }
            
            TypedExprKind::Call { callee, args } => {
                // Compile arguments
                let mut compiled_args: Vec<BasicMetadataTypeEnum> = Vec::new();
                let mut arg_values: Vec<BasicValueEnum> = Vec::new();
                
                for arg in args {
                    let val = self.compile_expression(arg)?;
                    arg_values.push(val);
                }
                
                // Get function name
                let fn_name = if let TypedExprKind::Identifier(name) = &callee.kind {
                    name.clone()
                } else {
                    return Ok(self.context.i64_type().const_int(0, false).into());
                };
                
                // Handle built-in functions
                match fn_name.as_str() {
                    "println" => {
                        self.compile_println(&arg_values)?;
                        Ok(self.context.i64_type().const_int(0, false).into())
                    }
                    _ => {
                        if let Some(function) = self.module.get_function(&fn_name) {
                            let args_meta: Vec<_> = arg_values.iter()
                                .map(|v| (*v).into())
                                .collect();
                            
                            let call = self.builder.build_call(function, &args_meta, "call").unwrap();
                            
                            match call.try_as_basic_value().left() {
                                Some(val) => Ok(val),
                                None => Ok(self.context.i64_type().const_int(0, false).into()),
                            }
                        } else {
                            Ok(self.context.i64_type().const_int(0, false).into())
                        }
                    }
                }
            }
            
            TypedExprKind::Array(elements) => {
                // For now, return a pointer to allocated array
                let elem_count = elements.len() as u64;
                let elem_size = 8u64; // 8 bytes per element
                let total_size = self.context.i64_type().const_int(elem_count * elem_size, false);
                
                let malloc = self.module.get_function("malloc").unwrap();
                let ptr = self.builder.build_call(malloc, &[total_size.into()], "arr").unwrap();
                
                Ok(ptr.try_as_basic_value().left().unwrap())
            }
            
            TypedExprKind::Range { start, end } => {
                // Return start for now (range is handled specially in for loops)
                self.compile_expression(start)
            }
            
            TypedExprKind::Grouped(inner) => {
                self.compile_expression(inner)
            }
            
            // TODO: Implement remaining expression types
            _ => Ok(self.context.i64_type().const_int(0, false).into()),
        }
    }
    
    fn compile_binary_op(
        &self,
        lhs: BasicValueEnum<'ctx>,
        op: BinaryOp,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        // Handle integer operations
        if lhs.is_int_value() && rhs.is_int_value() {
            let left = lhs.into_int_value();
            let right = rhs.into_int_value();
            
            let result: IntValue = match op {
                BinaryOp::Add => self.builder.build_int_add(left, right, "add").unwrap(),
                BinaryOp::Sub => self.builder.build_int_sub(left, right, "sub").unwrap(),
                BinaryOp::Mul => self.builder.build_int_mul(left, right, "mul").unwrap(),
                BinaryOp::Div => self.builder.build_int_signed_div(left, right, "div").unwrap(),
                BinaryOp::Mod => self.builder.build_int_signed_rem(left, right, "mod").unwrap(),
                BinaryOp::Eq => self.builder.build_int_compare(IntPredicate::EQ, left, right, "eq").unwrap(),
                BinaryOp::NotEq => self.builder.build_int_compare(IntPredicate::NE, left, right, "ne").unwrap(),
                BinaryOp::Lt => self.builder.build_int_compare(IntPredicate::SLT, left, right, "lt").unwrap(),
                BinaryOp::Gt => self.builder.build_int_compare(IntPredicate::SGT, left, right, "gt").unwrap(),
                BinaryOp::LtEq => self.builder.build_int_compare(IntPredicate::SLE, left, right, "le").unwrap(),
                BinaryOp::GtEq => self.builder.build_int_compare(IntPredicate::SGE, left, right, "ge").unwrap(),
                BinaryOp::And => self.builder.build_and(left, right, "and").unwrap(),
                BinaryOp::Or => self.builder.build_or(left, right, "or").unwrap(),
                BinaryOp::BitAnd => self.builder.build_and(left, right, "bitand").unwrap(),
                BinaryOp::BitOr => self.builder.build_or(left, right, "bitor").unwrap(),
                BinaryOp::BitXor => self.builder.build_xor(left, right, "xor").unwrap(),
                BinaryOp::Shl => self.builder.build_left_shift(left, right, "shl").unwrap(),
                BinaryOp::Shr => self.builder.build_right_shift(left, right, true, "shr").unwrap(),
                BinaryOp::Pow => {
                    // Convert to float and use pow
                    let lf = self.builder.build_signed_int_to_float(left, self.context.f64_type(), "lf").unwrap();
                    let rf = self.builder.build_signed_int_to_float(right, self.context.f64_type(), "rf").unwrap();
                    let pow_fn = self.module.get_function("pow").unwrap();
                    let result = self.builder.build_call(pow_fn, &[lf.into(), rf.into()], "pow").unwrap();
                    let float_result = result.try_as_basic_value().left().unwrap().into_float_value();
                    return self.builder.build_float_to_signed_int(float_result, self.context.i64_type(), "int").unwrap().into();
                }
            };
            
            return result.into();
        }
        
        // Handle float operations
        if lhs.is_float_value() || rhs.is_float_value() {
            let left = if lhs.is_float_value() {
                lhs.into_float_value()
            } else {
                self.builder.build_signed_int_to_float(
                    lhs.into_int_value(),
                    self.context.f64_type(),
                    "ftmp"
                ).unwrap()
            };
            
            let right = if rhs.is_float_value() {
                rhs.into_float_value()
            } else {
                self.builder.build_signed_int_to_float(
                    rhs.into_int_value(),
                    self.context.f64_type(),
                    "ftmp"
                ).unwrap()
            };
            
            let result: BasicValueEnum = match op {
                BinaryOp::Add => self.builder.build_float_add(left, right, "fadd").unwrap().into(),
                BinaryOp::Sub => self.builder.build_float_sub(left, right, "fsub").unwrap().into(),
                BinaryOp::Mul => self.builder.build_float_mul(left, right, "fmul").unwrap().into(),
                BinaryOp::Div => self.builder.build_float_div(left, right, "fdiv").unwrap().into(),
                BinaryOp::Eq => self.builder.build_float_compare(FloatPredicate::OEQ, left, right, "feq").unwrap().into(),
                BinaryOp::NotEq => self.builder.build_float_compare(FloatPredicate::ONE, left, right, "fne").unwrap().into(),
                BinaryOp::Lt => self.builder.build_float_compare(FloatPredicate::OLT, left, right, "flt").unwrap().into(),
                BinaryOp::Gt => self.builder.build_float_compare(FloatPredicate::OGT, left, right, "fgt").unwrap().into(),
                BinaryOp::LtEq => self.builder.build_float_compare(FloatPredicate::OLE, left, right, "fle").unwrap().into(),
                BinaryOp::GtEq => self.builder.build_float_compare(FloatPredicate::OGE, left, right, "fge").unwrap().into(),
                BinaryOp::Pow => {
                    let pow_fn = self.module.get_function("pow").unwrap();
                    let result = self.builder.build_call(pow_fn, &[left.into(), right.into()], "pow").unwrap();
                    result.try_as_basic_value().left().unwrap()
                }
                _ => left.into(),
            };
            
            return result;
        }
        
        // Default: return left
        lhs
    }
    
    fn compile_unary_op(&self, op: UnaryOp, operand: BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {
        match op {
            UnaryOp::Neg => {
                if operand.is_int_value() {
                    self.builder.build_int_neg(operand.into_int_value(), "neg").unwrap().into()
                } else {
                    self.builder.build_float_neg(operand.into_float_value(), "fneg").unwrap().into()
                }
            }
            UnaryOp::Not => {
                if operand.is_int_value() {
                    self.builder.build_not(operand.into_int_value(), "not").unwrap().into()
                } else {
                    operand
                }
            }
            UnaryOp::BitNot => {
                if operand.is_int_value() {
                    self.builder.build_not(operand.into_int_value(), "bitnot").unwrap().into()
                } else {
                    operand
                }
            }
        }
    }
    
    fn compile_println(&mut self, args: &[BasicValueEnum<'ctx>]) -> Result<(), VeyraError> {
        let printf = self.module.get_function("printf").unwrap();
        
        if args.is_empty() {
            // Print newline
            let fmt = self.builder.build_global_string_ptr("\n", "fmt").unwrap();
            self.builder.build_call(printf, &[fmt.as_pointer_value().into()], "").unwrap();
        } else {
            for arg in args {
                if arg.is_int_value() {
                    let fmt = self.builder.build_global_string_ptr("%lld\n", "fmt").unwrap();
                    self.builder.build_call(printf, &[fmt.as_pointer_value().into(), (*arg).into()], "").unwrap();
                } else if arg.is_float_value() {
                    let fmt = self.builder.build_global_string_ptr("%f\n", "fmt").unwrap();
                    self.builder.build_call(printf, &[fmt.as_pointer_value().into(), (*arg).into()], "").unwrap();
                } else if arg.is_pointer_value() {
                    let fmt = self.builder.build_global_string_ptr("%s\n", "fmt").unwrap();
                    self.builder.build_call(printf, &[fmt.as_pointer_value().into(), (*arg).into()], "").unwrap();
                }
            }
        }
        
        Ok(())
    }
    
    fn to_bool(&self, value: BasicValueEnum<'ctx>) -> IntValue<'ctx> {
        if value.is_int_value() {
            let int_val = value.into_int_value();
            let zero = int_val.get_type().const_int(0, false);
            self.builder.build_int_compare(IntPredicate::NE, int_val, zero, "tobool").unwrap()
        } else if value.is_float_value() {
            let float_val = value.into_float_value();
            let zero = float_val.get_type().const_float(0.0);
            self.builder.build_float_compare(FloatPredicate::ONE, float_val, zero, "tobool").unwrap()
        } else {
            self.context.bool_type().const_int(1, false)
        }
    }
    
    fn convert_type(&self, ty: &Type) -> Option<BasicTypeEnum<'ctx>> {
        match ty {
            Type::Int | Type::I64 => Some(self.context.i64_type().into()),
            Type::I8 | Type::U8 => Some(self.context.i8_type().into()),
            Type::I16 | Type::U16 => Some(self.context.i16_type().into()),
            Type::I32 | Type::U32 => Some(self.context.i32_type().into()),
            Type::U64 => Some(self.context.i64_type().into()),
            Type::Float | Type::F64 => Some(self.context.f64_type().into()),
            Type::F32 => Some(self.context.f32_type().into()),
            Type::Bool => Some(self.context.bool_type().into()),
            Type::String => Some(self.context.i8_type().ptr_type(AddressSpace::default()).into()),
            Type::Array(_) => Some(self.context.i8_type().ptr_type(AddressSpace::default()).into()),
            Type::Void => None,
            Type::Any | Type::Inferred => Some(self.context.i64_type().into()),
            _ => Some(self.context.i64_type().into()),
        }
    }
    
    fn generate_entry_point(&self, main_fn: FunctionValue<'ctx>) {
        // Create _start or entry point that calls main
        let i32_type = self.context.i32_type();
        let entry_type = i32_type.fn_type(&[], false);
        let entry_fn = self.module.add_function("_veyra_entry", entry_type, None);
        
        let entry_bb = self.context.append_basic_block(entry_fn, "entry");
        self.builder.position_at_end(entry_bb);
        
        // Call main
        let call = self.builder.build_call(main_fn, &[], "main_result").unwrap();
        
        // Return 0
        self.builder.build_return(Some(&i32_type.const_int(0, false))).unwrap();
    }
    
    fn emit_executable(&self, output_path: &str) -> Result<(), VeyraError> {
        // Initialize LLVM targets
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| VeyraError::RuntimeError { message: e.to_string() })?;
        
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| VeyraError::RuntimeError { message: e.to_string() })?;
        
        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| VeyraError::RuntimeError { 
                message: "Failed to create target machine".to_string() 
            })?;
        
        // Write object file
        let obj_path = format!("{}.o", output_path);
        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new(&obj_path))
            .map_err(|e| VeyraError::RuntimeError { message: e.to_string() })?;
        
        // Link to create executable
        #[cfg(target_os = "windows")]
        {
            std::process::Command::new("clang")
                .args([&obj_path, "-o", output_path])
                .output()
                .map_err(|e| VeyraError::RuntimeError { 
                    message: format!("Linking failed: {}", e) 
                })?;
        }
        
        #[cfg(not(target_os = "windows"))]
        {
            std::process::Command::new("clang")
                .args([&obj_path, "-o", output_path, "-lm"])
                .output()
                .map_err(|e| VeyraError::RuntimeError { 
                    message: format!("Linking failed: {}", e) 
                })?;
        }
        
        // Clean up object file
        let _ = std::fs::remove_file(&obj_path);
        
        Ok(())
    }
}
