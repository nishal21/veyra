//! Veyra System Library
//! 
//! Low-level system programming: processes, filesystem, networking, OS access.

use std::collections::HashMap;
use std::process::{Command, Output, Stdio, Child};
use std::io::{Read, Write};
use std::path::Path;
use std::fs;
use std::env;
use std::time::{Duration, SystemTime, UNIX_EPOCH, Instant};

/// Process execution
pub mod process {
    use super::*;
    
    /// Run a command and get output
    pub fn run(cmd: &str) -> Result<String, String> {
        let output = if cfg!(target_os = "windows") {
            Command::new("cmd")
                .args(["/C", cmd])
                .output()
        } else {
            Command::new("sh")
                .args(["-c", cmd])
                .output()
        };
        
        match output {
            Ok(out) => {
                if out.status.success() {
                    Ok(String::from_utf8_lossy(&out.stdout).to_string())
                } else {
                    Err(String::from_utf8_lossy(&out.stderr).to_string())
                }
            }
            Err(e) => Err(e.to_string()),
        }
    }
    
    /// Run command with arguments
    pub fn exec(program: &str, args: &[&str]) -> Result<Output, String> {
        Command::new(program)
            .args(args)
            .output()
            .map_err(|e| e.to_string())
    }
    
    /// Spawn a background process
    pub fn spawn(cmd: &str) -> Result<Child, String> {
        if cfg!(target_os = "windows") {
            Command::new("cmd")
                .args(["/C", cmd])
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .map_err(|e| e.to_string())
        } else {
            Command::new("sh")
                .args(["-c", cmd])
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .map_err(|e| e.to_string())
        }
    }
    
    /// Get current process ID
    pub fn pid() -> u32 {
        std::process::id()
    }
    
    /// Exit with code
    pub fn exit(code: i32) -> ! {
        std::process::exit(code)
    }
    
    /// Abort the process
    pub fn abort() -> ! {
        std::process::abort()
    }
}

/// Environment variables
pub mod environment {
    use super::*;
    
    /// Get environment variable
    pub fn get(key: &str) -> Option<String> {
        env::var(key).ok()
    }
    
    /// Set environment variable
    pub fn set(key: &str, value: &str) {
        env::set_var(key, value);
    }
    
    /// Remove environment variable
    pub fn remove(key: &str) {
        env::remove_var(key);
    }
    
    /// Get all environment variables
    pub fn all() -> HashMap<String, String> {
        env::vars().collect()
    }
    
    /// Get current working directory
    pub fn cwd() -> String {
        env::current_dir()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default()
    }
    
    /// Change current directory
    pub fn chdir(path: &str) -> Result<(), String> {
        env::set_current_dir(path).map_err(|e| e.to_string())
    }
    
    /// Get home directory
    pub fn home() -> Option<String> {
        dirs_next().home_dir()
    }
    
    /// Get temp directory
    pub fn temp_dir() -> String {
        env::temp_dir().to_string_lossy().to_string()
    }
    
    /// Get command line arguments
    pub fn args() -> Vec<String> {
        env::args().collect()
    }
    
    fn dirs_next() -> DirHelper {
        DirHelper
    }
    
    struct DirHelper;
    impl DirHelper {
        fn home_dir(&self) -> Option<String> {
            #[cfg(target_os = "windows")]
            {
                env::var("USERPROFILE").ok()
            }
            #[cfg(not(target_os = "windows"))]
            {
                env::var("HOME").ok()
            }
        }
    }
}

/// Filesystem operations
pub mod filesystem {
    use super::*;
    
    /// Check if path exists
    pub fn exists(path: &str) -> bool {
        Path::new(path).exists()
    }
    
    /// Check if path is file
    pub fn is_file(path: &str) -> bool {
        Path::new(path).is_file()
    }
    
    /// Check if path is directory
    pub fn is_dir(path: &str) -> bool {
        Path::new(path).is_dir()
    }
    
    /// Create directory
    pub fn mkdir(path: &str) -> Result<(), String> {
        fs::create_dir(path).map_err(|e| e.to_string())
    }
    
    /// Create directory recursively
    pub fn mkdir_all(path: &str) -> Result<(), String> {
        fs::create_dir_all(path).map_err(|e| e.to_string())
    }
    
    /// Remove file
    pub fn remove_file(path: &str) -> Result<(), String> {
        fs::remove_file(path).map_err(|e| e.to_string())
    }
    
    /// Remove directory
    pub fn remove_dir(path: &str) -> Result<(), String> {
        fs::remove_dir(path).map_err(|e| e.to_string())
    }
    
    /// Remove directory recursively
    pub fn remove_dir_all(path: &str) -> Result<(), String> {
        fs::remove_dir_all(path).map_err(|e| e.to_string())
    }
    
    /// Copy file
    pub fn copy(from: &str, to: &str) -> Result<u64, String> {
        fs::copy(from, to).map_err(|e| e.to_string())
    }
    
    /// Rename/move file
    pub fn rename(from: &str, to: &str) -> Result<(), String> {
        fs::rename(from, to).map_err(|e| e.to_string())
    }
    
    /// List directory contents
    pub fn read_dir(path: &str) -> Result<Vec<String>, String> {
        fs::read_dir(path)
            .map_err(|e| e.to_string())?
            .map(|entry| {
                entry
                    .map_err(|e| e.to_string())
                    .map(|e| e.path().to_string_lossy().to_string())
            })
            .collect()
    }
    
    /// Get file size
    pub fn file_size(path: &str) -> Result<u64, String> {
        fs::metadata(path)
            .map(|m| m.len())
            .map_err(|e| e.to_string())
    }
    
    /// Get file metadata
    pub fn stat(path: &str) -> Result<FileStat, String> {
        let meta = fs::metadata(path).map_err(|e| e.to_string())?;
        
        Ok(FileStat {
            size: meta.len(),
            is_dir: meta.is_dir(),
            is_file: meta.is_file(),
            modified: meta.modified()
                .ok()
                .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
                .map(|d| d.as_secs()),
            created: meta.created()
                .ok()
                .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
                .map(|d| d.as_secs()),
        })
    }
    
    /// Walk directory tree
    pub fn walk(path: &str, callback: impl Fn(&str, bool)) {
        walk_recursive(Path::new(path), &callback);
    }
    
    fn walk_recursive(path: &Path, callback: &impl Fn(&str, bool)) {
        if let Ok(entries) = fs::read_dir(path) {
            for entry in entries.flatten() {
                let path = entry.path();
                let is_dir = path.is_dir();
                callback(&path.to_string_lossy(), is_dir);
                
                if is_dir {
                    walk_recursive(&path, callback);
                }
            }
        }
    }
    
    /// Glob pattern matching
    pub fn glob(pattern: &str) -> Vec<String> {
        // Simplified glob - just list matching files
        let base = Path::new(pattern).parent().unwrap_or(Path::new("."));
        let name_pattern = Path::new(pattern).file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_default();
        
        let mut matches = Vec::new();
        if let Ok(entries) = fs::read_dir(base) {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if glob_match(&name_pattern, &name) {
                    matches.push(entry.path().to_string_lossy().to_string());
                }
            }
        }
        matches
    }
    
    fn glob_match(pattern: &str, text: &str) -> bool {
        if pattern == "*" {
            return true;
        }
        
        if pattern.starts_with("*.") {
            let ext = &pattern[2..];
            return text.ends_with(&format!(".{}", ext));
        }
        
        pattern == text
    }
    
    #[derive(Debug)]
    pub struct FileStat {
        pub size: u64,
        pub is_dir: bool,
        pub is_file: bool,
        pub modified: Option<u64>,
        pub created: Option<u64>,
    }
}

/// Time and date operations
pub mod time {
    use super::*;
    
    /// Get current Unix timestamp (seconds)
    pub fn now() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs()
    }
    
    /// Get current Unix timestamp (milliseconds)
    pub fn now_ms() -> u128 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis()
    }
    
    /// Get current Unix timestamp (nanoseconds)
    pub fn now_ns() -> u128 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    }
    
    /// Sleep for milliseconds
    pub fn sleep(ms: u64) {
        std::thread::sleep(Duration::from_millis(ms));
    }
    
    /// Sleep for seconds
    pub fn sleep_secs(secs: u64) {
        std::thread::sleep(Duration::from_secs(secs));
    }
    
    /// Measure execution time
    pub fn measure<F, T>(f: F) -> (T, Duration)
    where F: FnOnce() -> T
    {
        let start = Instant::now();
        let result = f();
        (result, start.elapsed())
    }
    
    /// Format timestamp to string
    pub fn format(timestamp: u64, fmt: &str) -> String {
        // Simplified formatting
        let secs = timestamp;
        let mins = secs / 60;
        let hours = mins / 60;
        let days = hours / 24;
        
        fmt.replace("%s", &(secs % 60).to_string())
           .replace("%M", &format!("{:02}", mins % 60))
           .replace("%H", &format!("{:02}", hours % 24))
           .replace("%d", &days.to_string())
    }
}

/// Memory information
pub mod memory {
    /// Get approximate memory usage of current process
    #[cfg(target_os = "linux")]
    pub fn usage() -> Option<usize> {
        use std::fs;
        fs::read_to_string("/proc/self/statm")
            .ok()
            .and_then(|s| s.split_whitespace().next()?.parse().ok())
            .map(|pages: usize| pages * 4096) // 4KB pages
    }
    
    #[cfg(not(target_os = "linux"))]
    pub fn usage() -> Option<usize> {
        None
    }
}

/// Platform information
pub mod platform {
    use super::*;
    
    /// Get operating system name
    pub fn os() -> &'static str {
        env::consts::OS
    }
    
    /// Get architecture
    pub fn arch() -> &'static str {
        env::consts::ARCH
    }
    
    /// Get OS family
    pub fn family() -> &'static str {
        env::consts::FAMILY
    }
    
    /// Check if running on Windows
    pub fn is_windows() -> bool {
        cfg!(target_os = "windows")
    }
    
    /// Check if running on Linux
    pub fn is_linux() -> bool {
        cfg!(target_os = "linux")
    }
    
    /// Check if running on macOS
    pub fn is_macos() -> bool {
        cfg!(target_os = "macos")
    }
    
    /// Get number of CPU cores
    pub fn cpu_count() -> usize {
        std::thread::available_parallelism()
            .map(|p| p.get())
            .unwrap_or(1)
    }
}

/// Cryptographic operations
pub mod crypto {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    /// Simple hash function (not cryptographically secure)
    pub fn hash(data: &[u8]) -> u64 {
        let mut hasher = DefaultHasher::new();
        data.hash(&mut hasher);
        hasher.finish()
    }
    
    /// Hash string
    pub fn hash_str(s: &str) -> u64 {
        hash(s.as_bytes())
    }
    
    /// Generate random bytes (simple, not cryptographically secure)
    pub fn random_bytes(len: usize) -> Vec<u8> {
        use std::time::{SystemTime, UNIX_EPOCH};
        let seed = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_nanos();
        
        (0..len)
            .map(|i| {
                let a: u128 = 6364136223846793005;
                let c: u128 = 1442695040888963407;
                let result = a.wrapping_mul(seed + i as u128).wrapping_add(c);
                (result % 256) as u8
            })
            .collect()
    }
    
    /// Generate UUID v4
    pub fn uuid() -> String {
        let bytes = random_bytes(16);
        format!(
            "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            bytes[0], bytes[1], bytes[2], bytes[3],
            bytes[4], bytes[5],
            (bytes[6] & 0x0f) | 0x40, bytes[7],
            (bytes[8] & 0x3f) | 0x80, bytes[9],
            bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15]
        )
    }
    
    /// Base64 encode
    pub fn base64_encode(data: &[u8]) -> String {
        const CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
        let mut result = String::new();
        
        for chunk in data.chunks(3) {
            let b0 = chunk[0] as usize;
            let b1 = *chunk.get(1).unwrap_or(&0) as usize;
            let b2 = *chunk.get(2).unwrap_or(&0) as usize;
            
            result.push(CHARS[(b0 >> 2)] as char);
            result.push(CHARS[((b0 & 0x03) << 4) | (b1 >> 4)] as char);
            
            if chunk.len() > 1 {
                result.push(CHARS[((b1 & 0x0f) << 2) | (b2 >> 6)] as char);
            } else {
                result.push('=');
            }
            
            if chunk.len() > 2 {
                result.push(CHARS[b2 & 0x3f] as char);
            } else {
                result.push('=');
            }
        }
        
        result
    }
}

/// Signal handling
pub mod signals {
    use std::sync::atomic::{AtomicBool, Ordering};
    
    static INTERRUPTED: AtomicBool = AtomicBool::new(false);
    
    /// Check if interrupt signal received
    pub fn interrupted() -> bool {
        INTERRUPTED.load(Ordering::SeqCst)
    }
    
    /// Set interrupt flag
    pub fn set_interrupted() {
        INTERRUPTED.store(true, Ordering::SeqCst);
    }
    
    /// Clear interrupt flag
    pub fn clear_interrupted() {
        INTERRUPTED.store(false, Ordering::SeqCst);
    }
}
