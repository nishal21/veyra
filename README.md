# Veyra Programming Language

[![CI](https://github.com/nishal21/veyra/workflows/CI/badge.svg)](https://github.com/nishal21/veyra/actions)
[![PyPI](https://img.shields.io/pypi/v/veyra)](https://pypi.org/project/veyra/)
[![License](https://img.shields.io/github/license/nishal21/veyra)](https://github.com/nishal21/veyra/blob/main/LICENSE)

Veyra is a modern programming language designed for web development and rapid prototyping, featuring built-in web capabilities and a clean, readable syntax.

## ✨ Features

- 🚀 **Fast Development**: Simple syntax with powerful built-ins
- 🌐 **Web-Native**: Built-in HTML, CSS, and web server functionality
- 📦 **Package Management**: Easy dependency management with `veyra-pm`
- 🎯 **AI-Ready**: Integrated machine learning functions
- 🔧 **Extensible**: Easy to add new features and libraries
- 📚 **Rich Standard Library**: Math, string manipulation, and utility functions

## 📦 Installation

### From PyPI (Recommended)
```bash
pip install veyra
```

### From Source
```bash
git clone https://github.com/nishal21/veyra.git
cd veyra
pip install -e .
```

### Requirements
- Python 3.8+

## 🚀 Quick Start

Create a file `hello.veyra`:
```veyra
fn main() {
    println("Hello, Veyra!");
}
```

Run it:
```bash
veyra hello.veyra
```

## 🌐 Web Development Example

```veyra
fn main() {
    let page = create_page(
        "My App",
        html_element("h1", "Welcome to Veyra!"),
        css_style("body", "background: #f0f0f0;")
    );
    web_serve(page, 8080);
}
```

Visit `http://localhost:8080` in your browser!

## 📚 Language Features

### Variables and Functions
```veyra
let message = "Hello";
let count = 42;

fn greet(name) {
    return "Hello, " + name + "!";
}
```

### Control Flow
```veyra
if count > 10 {
    println("Count is high");
} else {
    println("Count is low");
}

while count > 0 {
    println(count);
    count = count - 1;
}
```

### Arrays and Objects
```veyra
let numbers = [1, 2, 3, 4, 5];
let person = {"name": "Alice", "age": 30};
```

## 📖 Standard Library

### Math Functions
- `abs(x)` - Absolute value
- `sqrt(x)` - Square root
- `pow(x, y)` - Power function
- `max(...)` - Maximum value
- `min(...)` - Minimum value

### String Functions
- `upper(s)` - Convert to uppercase
- `lower(s)` - Convert to lowercase
- `split(s, sep)` - Split string
- `join(arr, sep)` - Join array elements
- `len(obj)` - Get length

### Web Functions
- `html_element(tag, content, attrs)` - Create HTML elements
- `css_style(selector, rules)` - Generate CSS
- `create_page(title, body, css)` - Build complete HTML pages
- `web_serve(content, port)` - Start web server

## 📦 Package Management

Install packages:
```bash
veyra pm install math
veyra pm list
```

## 📁 Project Structure

```
veyra/
├── src/veyra/           # Core language implementation
│   ├── __init__.py
│   ├── cli.py          # Command line interface
│   ├── lexer.py        # Lexical analysis
│   ├── parser.py       # Syntax parsing
│   ├── interpreter.py  # Runtime execution
│   ├── pm.py           # Package manager
│   └── lib/            # Installed packages
├── examples/           # Example programs
├── tests/              # Test suite
├── docs/               # Documentation
└── README.md
```

## 🧪 Running Tests

```bash
pip install -e .[test]
pytest tests/
```

## 📚 Documentation

- [Language Guide](docs/language.md)
- [Standard Library Reference](docs/standard_library.md)
- [API Documentation](docs/api.md)
- [Contributing Guide](CONTRIBUTING.md)

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Built with Python
- Inspired by modern programming languages
- Community-driven development

## 📞 Contact

- GitHub: [@nishal21](https://github.com/nishal21/veyra)
- Issues: [GitHub Issues](https://github.com/nishal21/veyra/issues)

---

**Veyra** - Making web development fun and accessible! 🎉