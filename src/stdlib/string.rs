//! String Operations
//! 
//! String manipulation functions.

/// Get string length
pub fn len(s: &str) -> usize {
    s.len()
}

/// Check if string contains substring
pub fn contains(s: &str, sub: &str) -> bool {
    s.contains(sub)
}

/// Check if string starts with prefix
pub fn starts_with(s: &str, prefix: &str) -> bool {
    s.starts_with(prefix)
}

/// Check if string ends with suffix
pub fn ends_with(s: &str, suffix: &str) -> bool {
    s.ends_with(suffix)
}

/// Convert to uppercase
pub fn upper(s: &str) -> String {
    s.to_uppercase()
}

/// Convert to lowercase
pub fn lower(s: &str) -> String {
    s.to_lowercase()
}

/// Trim whitespace from both ends
pub fn trim(s: &str) -> String {
    s.trim().to_string()
}

/// Trim whitespace from left
pub fn trim_left(s: &str) -> String {
    s.trim_start().to_string()
}

/// Trim whitespace from right
pub fn trim_right(s: &str) -> String {
    s.trim_end().to_string()
}

/// Split string by delimiter
pub fn split(s: &str, delimiter: &str) -> Vec<String> {
    s.split(delimiter).map(|x| x.to_string()).collect()
}

/// Join strings with delimiter
pub fn join(parts: &[String], delimiter: &str) -> String {
    parts.join(delimiter)
}

/// Replace all occurrences
pub fn replace(s: &str, from: &str, to: &str) -> String {
    s.replace(from, to)
}

/// Get substring
pub fn substring(s: &str, start: usize, end: usize) -> String {
    s.chars().skip(start).take(end - start).collect()
}

/// Get character at index
pub fn char_at(s: &str, index: usize) -> Option<char> {
    s.chars().nth(index)
}

/// Find first occurrence of substring
pub fn index_of(s: &str, sub: &str) -> Option<usize> {
    s.find(sub)
}

/// Find last occurrence of substring
pub fn last_index_of(s: &str, sub: &str) -> Option<usize> {
    s.rfind(sub)
}

/// Repeat string n times
pub fn repeat(s: &str, n: usize) -> String {
    s.repeat(n)
}

/// Reverse string
pub fn reverse(s: &str) -> String {
    s.chars().rev().collect()
}

/// Pad left with character to width
pub fn pad_left(s: &str, width: usize, pad_char: char) -> String {
    if s.len() >= width {
        s.to_string()
    } else {
        let padding: String = std::iter::repeat(pad_char).take(width - s.len()).collect();
        format!("{}{}", padding, s)
    }
}

/// Pad right with character to width
pub fn pad_right(s: &str, width: usize, pad_char: char) -> String {
    if s.len() >= width {
        s.to_string()
    } else {
        let padding: String = std::iter::repeat(pad_char).take(width - s.len()).collect();
        format!("{}{}", s, padding)
    }
}

/// Convert string to integer
pub fn parse_int(s: &str) -> Result<i64, std::num::ParseIntError> {
    s.trim().parse()
}

/// Convert string to float
pub fn parse_float(s: &str) -> Result<f64, std::num::ParseFloatError> {
    s.trim().parse()
}

/// Check if string is empty
pub fn is_empty(s: &str) -> bool {
    s.is_empty()
}

/// Check if string is blank (empty or whitespace only)
pub fn is_blank(s: &str) -> bool {
    s.trim().is_empty()
}

/// Count occurrences of substring
pub fn count(s: &str, sub: &str) -> usize {
    s.matches(sub).count()
}

/// Capitalize first letter
pub fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().chain(chars).collect(),
    }
}

/// Title case (capitalize each word)
pub fn title_case(s: &str) -> String {
    s.split_whitespace()
        .map(|word| capitalize(word))
        .collect::<Vec<_>>()
        .join(" ")
}
