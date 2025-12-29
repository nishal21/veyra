//! Veyra CLI - The Command Line Interface
//! 
//! Usage:
//!   veyra build <file.veyra> -o <output>
//!   veyra run <file.veyra>
//!   veyra repl
//!   veyra new <project-name>

use clap::{Parser, Subcommand};
use colored::*;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

#[derive(Parser)]
#[command(name = "veyra")]
#[command(version = veyra::VERSION)]
#[command(author = "Veyra Team")]
#[command(about = "🚀 Veyra - The World's Fastest Programming Language", long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// 🔨 Compile a Veyra source file to a native executable
    Build {
        /// Input source file (.veyra)
        input: String,
        
        /// Output executable name
        #[arg(short, long, default_value = "a.out")]
        output: String,
        
        /// Enable optimizations (release mode)
        #[arg(short = 'O', long)]
        optimize: bool,
    },
    
    /// ▶️ Run a Veyra source file directly (JIT compilation)
    Run {
        /// Source file to run
        file: String,
    },
    
    /// 💻 Start the interactive REPL (Read-Eval-Print Loop)
    Repl,
    
    /// 📦 Create a new Veyra project
    New {
        /// Project name
        name: String,
    },
    
    /// 📚 Show language documentation
    Docs {
        /// Topic to show docs for
        #[arg(default_value = "intro")]
        topic: String,
    },
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    
    match cli.command {
        Commands::Build { input, output, optimize } => {
            build_command(&input, &output, optimize)
        }
        Commands::Run { file } => {
            run_command(&file)
        }
        Commands::Repl => {
            repl_command()
        }
        Commands::New { name } => {
            new_project_command(&name)
        }
        Commands::Docs { topic } => {
            docs_command(&topic)
        }
    }
}

fn build_command(input: &str, output: &str, optimize: bool) -> ExitCode {
    println!("{}", "🚀 Veyra Compiler v1.0.0".bright_cyan().bold());
    println!();
    
    // Check input file exists
    if !Path::new(input).exists() {
        eprintln!("{} File not found: {}", "error:".red().bold(), input);
        return ExitCode::FAILURE;
    }
    
    // Read source file
    let source = match fs::read_to_string(input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} Failed to read file: {}", "error:".red().bold(), e);
            return ExitCode::FAILURE;
        }
    };
    
    // Add .exe extension on Windows if not present
    #[cfg(target_os = "windows")]
    let output_path = if !output.ends_with(".exe") {
        format!("{}.exe", output)
    } else {
        output.to_string()
    };
    
    #[cfg(not(target_os = "windows"))]
    let output_path = output.to_string();
    
    println!("  {} {}", "Compiling".green().bold(), input);
    
    let start = std::time::Instant::now();
    
    // Compile
    match veyra::compile(&source, &output_path) {
        Ok(()) => {
            let elapsed = start.elapsed();
            println!("  {} {} in {:.2}s", 
                "Finished".green().bold(), 
                if optimize { "release" } else { "debug" },
                elapsed.as_secs_f64()
            );
            println!();
            println!("  {} {}", "Output:".bright_white().bold(), output_path);
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!();
            eprintln!("{}", e);
            ExitCode::FAILURE
        }
    }
}

fn run_command(file: &str) -> ExitCode {
    // Check file exists
    if !Path::new(file).exists() {
        eprintln!("{} File not found: {}", "error:".red().bold(), file);
        return ExitCode::FAILURE;
    }
    
    // Read source file
    let source = match fs::read_to_string(file) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{} Failed to read file: {}", "error:".red().bold(), e);
            return ExitCode::FAILURE;
        }
    };
    
    // Run with JIT
    match veyra::run(&source) {
        Ok(exit_code) => ExitCode::from(exit_code as u8),
        Err(e) => {
            eprintln!("{}", e);
            ExitCode::FAILURE
        }
    }
}

fn repl_command() -> ExitCode {
    println!("{}", "🚀 Veyra REPL v1.0.0".bright_cyan().bold());
    println!("{}", "Type 'help' for commands, 'exit' to quit.".dimmed());
    println!();
    
    loop {
        print!("{} ", "veyra>".green().bold());
        use std::io::Write;
        std::io::stdout().flush().unwrap();
        
        let mut input = String::new();
        if std::io::stdin().read_line(&mut input).is_err() {
            break;
        }
        
        let input = input.trim();
        
        match input {
            "exit" | "quit" => {
                println!("{}", "Goodbye! 👋".bright_cyan());
                break;
            }
            "help" => {
                println!("{}", "Veyra REPL Commands:".bright_white().bold());
                println!("  {}  - Show this help", "help".green());
                println!("  {}  - Exit the REPL", "exit".green());
                println!("  {} - Clear the screen", "clear".green());
                println!();
                println!("Enter any Veyra code to evaluate it!");
            }
            "clear" => {
                print!("\x1B[2J\x1B[1;1H");
            }
            "" => continue,
            code => {
                // Wrap in main if it's just an expression
                let source = if code.contains("fn ") {
                    code.to_string()
                } else {
                    format!("fn main() {{ println({}); }}", code)
                };
                
                match veyra::run(&source) {
                    Ok(_) => {}
                    Err(e) => eprintln!("{}", e),
                }
            }
        }
    }
    
    ExitCode::SUCCESS
}

fn new_project_command(name: &str) -> ExitCode {
    println!("{}", "📦 Creating new Veyra project...".bright_cyan().bold());
    
    // Create project directory
    let project_dir = Path::new(name);
    if project_dir.exists() {
        eprintln!("{} Directory '{}' already exists", "error:".red().bold(), name);
        return ExitCode::FAILURE;
    }
    
    if let Err(e) = fs::create_dir_all(project_dir) {
        eprintln!("{} Failed to create directory: {}", "error:".red().bold(), e);
        return ExitCode::FAILURE;
    }
    
    // Create main.veyra
    let main_content = r#"// Welcome to Veyra! 🚀
// The world's fastest programming language

fn main() {
    println("Hello, Veyra!");
    
    // Variables with type inference
    let name = "World";
    let count = 42;
    
    // String interpolation
    println("Hello, " + name + "!");
    println("The answer is: " + count);
    
    // Functions
    let result = add(10, 20);
    println("10 + 20 = " + result);
}

fn add(a: int, b: int) -> int {
    return a + b;
}
"#;
    
    if let Err(e) = fs::write(project_dir.join("main.veyra"), main_content) {
        eprintln!("{} Failed to create main.veyra: {}", "error:".red().bold(), e);
        return ExitCode::FAILURE;
    }
    
    // Create veyra.toml
    let config_content = format!(r#"[project]
name = "{}"
version = "0.1.0"
authors = []

[build]
optimize = true
target = "native"
"#, name);
    
    if let Err(e) = fs::write(project_dir.join("veyra.toml"), config_content) {
        eprintln!("{} Failed to create veyra.toml: {}", "error:".red().bold(), e);
        return ExitCode::FAILURE;
    }
    
    println!();
    println!("  {} {}", "Created".green().bold(), name);
    println!();
    println!("  To get started:");
    println!("    {} {}", "cd".bright_white(), name);
    println!("    {} main.veyra", "veyra run".bright_white());
    
    ExitCode::SUCCESS
}

fn docs_command(topic: &str) -> ExitCode {
    match topic {
        "intro" => {
            println!("{}", "🚀 Welcome to Veyra!".bright_cyan().bold());
            println!();
            println!("Veyra is the world's fastest, simplest programming language.");
            println!();
            println!("{}", "Quick Start:".bright_white().bold());
            println!("  1. Create a new project: {}", "veyra new my-project".green());
            println!("  2. Navigate to it: {}", "cd my-project".green());
            println!("  3. Run your code: {}", "veyra run main.veyra".green());
            println!();
            println!("{}", "Topics:".bright_white().bold());
            println!("  {} - Variable declarations", "veyra docs variables".green());
            println!("  {} - Function definitions", "veyra docs functions".green());
            println!("  {} - Control flow", "veyra docs control".green());
            println!("  {} - Data types", "veyra docs types".green());
        }
        "variables" => {
            println!("{}", "📝 Variables in Veyra".bright_cyan().bold());
            println!();
            println!("Use 'let' to declare variables:");
            println!();
            println!("  {}", "// Type inference".dimmed());
            println!("  let name = \"Veyra\";");
            println!("  let count = 42;");
            println!("  let pi = 3.14159;");
            println!();
            println!("  {}", "// Explicit types".dimmed());
            println!("  let age: int = 25;");
            println!("  let price: float = 19.99;");
        }
        "functions" => {
            println!("{}", "🔧 Functions in Veyra".bright_cyan().bold());
            println!();
            println!("Use 'fn' to define functions:");
            println!();
            println!("  fn greet(name: string) {{");
            println!("      println(\"Hello, \" + name + \"!\");");
            println!("  }}");
            println!();
            println!("  fn add(a: int, b: int) -> int {{");
            println!("      return a + b;");
            println!("  }}");
        }
        _ => {
            eprintln!("{} Unknown topic: {}", "error:".red().bold(), topic);
            return ExitCode::FAILURE;
        }
    }
    
    ExitCode::SUCCESS
}
