//! I/O Operations
//! 
//! File reading, writing, and console I/O.

use std::fs;
use std::io::{self, Write, BufRead};

/// Read entire file contents as string
pub fn read_file(path: &str) -> Result<String, io::Error> {
    fs::read_to_string(path)
}

/// Write string to file
pub fn write_file(path: &str, content: &str) -> Result<(), io::Error> {
    fs::write(path, content)
}

/// Append to file
pub fn append_file(path: &str, content: &str) -> Result<(), io::Error> {
    use std::fs::OpenOptions;
    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)?;
    file.write_all(content.as_bytes())
}

/// Check if file exists
pub fn file_exists(path: &str) -> bool {
    std::path::Path::new(path).exists()
}

/// Read line from stdin
pub fn read_line() -> String {
    let stdin = io::stdin();
    let mut line = String::new();
    stdin.lock().read_line(&mut line).unwrap_or(0);
    line.trim().to_string()
}

/// Print to stdout
pub fn print(s: &str) {
    print!("{}", s);
    io::stdout().flush().unwrap();
}

/// Print line to stdout
pub fn println(s: &str) {
    println!("{}", s);
}

/// Read all lines from file
pub fn read_lines(path: &str) -> Result<Vec<String>, io::Error> {
    let file = fs::File::open(path)?;
    let reader = io::BufReader::new(file);
    reader.lines().collect()
}
