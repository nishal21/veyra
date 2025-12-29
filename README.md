# 🚀 Veyra

**The World's Fastest, Simplest Programming Language**

Veyra is a modern, professional-grade programming language with LLVM native compilation and a full-featured interpreter. It's designed to be fast, simple, and powerful.

## ⚡ Features

- **LLVM-Powered Native Compilation** - Compile to blazing-fast executables
- **Full-Featured Interpreter** - Run code instantly with `veyra run`
- **100+ Built-in Functions** - Date/time, crypto, file I/O, web servers, AI/ML, and more
- **Simple Syntax** - Easy to learn, hard to mess up
- **Cross-Platform** - Windows, macOS, Linux support

## 📦 Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/nishal21/veyra.git
cd veyra/veyra-native

# Build (requires Rust and LLVM 17)
cargo build --release

# The executable is at target/release/veyra
```

### Requirements
- Rust 1.70+
- LLVM 17 (for native compilation)

## 🚀 Quick Start

### Hello World

```veyra
fn main() {
    println("Hello, Veyra!");
}
```

### Run it

```bash
# Interpreter mode (full features)
veyra run hello.veyra

# Compile to native executable
veyra build hello.veyra -o hello
./hello.exe  # Windows
./hello      # Linux/macOS
```

## 📖 Language Guide

### Variables

```veyra
let name = "Veyra";
let count = 42;
let pi = 3.14159;
let active = true;
let items = [1, 2, 3, 4, 5];
```

### Functions

```veyra
fn greet(name: string) {
    println("Hello, " + name + "!");
}

fn add(a: int, b: int) -> int {
    return a + b;
}

fn main() {
    greet("World");
    let result = add(10, 20);
    println(result);  // 30
}
```

### Control Flow

```veyra
// If/else
if score >= 90 {
    println("A");
} else if score >= 80 {
    println("B");
} else {
    println("C");
}

// While loop
while i < 10 {
    println(i);
    i = i + 1;
}

// For loop
for i in 0..10 {
    println(i);
}
```

### Built-in Functions

```veyra
// String operations
println(upper("hello"));        // HELLO
println(lower("WORLD"));        // world
println(trim("  hi  "));        // hi
println(replace("foo", "o", "a")); // faa

// Math
println(sqrt(16.0));            // 4
println(pow(2.0, 10.0));        // 1024
println(abs(-42));              // 42

// Arrays
let arr = [3, 1, 4, 1, 5];
println(sort(arr));             // [1, 1, 3, 4, 5]
println(sum(arr));              // 14
println(mean(arr));             // 2.8

// Date/Time
println(date());                // 2025-01-15
println(time());                // 14:30:45
println(timestamp());           // 1705332645

// File I/O
write_file("test.txt", "Hello!");
let content = read_file("test.txt");
println(file_exists("test.txt")); // true

// Base64
let encoded = base64_encode("Hello");
let decoded = base64_decode(encoded);

// UUID
println(uuid());  // 550e8400-e29b-41d4-a716-446655440000

// Export
export_csv(data, "output.csv");
export_json(obj, "data.json");
export_html(content, "page.html");
```

### Web Server

```veyra
fn main() {
    let page = html(
        h1("Welcome to Veyra!") +
        p("A fast, modern language.")
    );
    
    serve_html(page, 8080);
    // Open http://localhost:8080
}
```

### AI/ML Functions

```veyra
// Activation functions
println(sigmoid(0.0));   // 0.5
println(relu(-5.0));     // 0
println(tanh(1.0));      // 0.76

// Linear algebra
let a = [1.0, 2.0, 3.0];
let b = [4.0, 5.0, 6.0];
println(dot(a, b));      // 32
println(matrix_add(a, b)); // [5, 7, 9]

// Statistics
println(sum([1,2,3,4,5]));  // 15
println(mean([1,2,3,4,5])); // 3
println(softmax([1,2,3]));  // [0.09, 0.24, 0.67]
```

## 📚 Complete Function Reference

### I/O Functions
`println`, `print`, `printf`, `sprintf`, `read_file`, `write_file`, `append_file`, `read_bytes`, `write_bytes`

### String Functions
`len`, `trim`, `upper`, `lower`, `replace`, `contains`, `split`, `join`, `starts_with`, `ends_with`, `format`, `pad_left`, `pad_right`, `repeat`, `char_at`, `char_code`, `from_char_code`, `substring`, `index_of`, `last_index_of`

### Math Functions
`sqrt`, `pow`, `sin`, `cos`, `abs`, `floor`, `ceil`, `round`, `rand`, `rand_int`, `min`, `max`

### Array Functions
`len`, `push`, `pop`, `sort`, `reverse`, `unique`, `range`, `sum`, `mean`, `merge`, `clone`

### Date/Time Functions
`date`, `time`, `timestamp`, `now`, `sleep`, `format_date`

### File System Functions
`file_exists`, `is_file`, `is_dir`, `list_dir`, `mkdir`, `rmdir`, `delete_file`, `copy_file`, `move_file`, `file_size`

### Encoding Functions
`base64_encode`, `base64_decode`, `url_encode`, `url_decode`, `json_parse`, `json_stringify`

### Crypto Functions
`hash`, `md5`, `uuid`, `random_bytes`

### AI/ML Functions
`sigmoid`, `relu`, `tanh`, `softmax`, `dot`, `matrix_add`, `matrix_mul`

### Web Functions
`html`, `h1`, `h2`, `p`, `div`, `span`, `button`, `input`, `link`, `img`, `style`, `script`, `serve_html`, `http_get`, `fetch`

### System Functions
`cwd`, `env`, `set_env`, `exec`, `spawn`, `args`, `exit`

### Type Functions
`type_of`, `to_string`, `to_int`, `to_float`, `is_null`, `is_int`, `is_float`, `is_string`, `is_array`, `is_object`, `is_bool`

### Export Functions
`export_json`, `export_csv`, `export_html`

## 🛠️ CLI Commands

```bash
veyra build <file.veyra> -o <output>   # Compile to native
veyra run <file.veyra>                  # Run with interpreter
veyra repl                              # Interactive REPL
veyra new <project>                     # Create new project
veyra docs                              # Show documentation
```

## 📄 License

MIT License - See [LICENSE](LICENSE) for details.

## 🤝 Contributing

Contributions are welcome! Please read our contributing guidelines before submitting PRs.

## 🌟 Star History

If you find Veyra useful, please give it a star! ⭐

---

**Made with ❤️ by the Veyra Team**
