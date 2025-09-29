# Veyra API Documentation

## Overview

This document describes the internal API for extending and embedding Veyra. The Veyra interpreter is built as a modular Python package that can be extended with new features.

## Core Components

### Lexer (`veyra.lexer`)

The lexer tokenizes Veyra source code into tokens.

#### `class Token`
Represents a lexical token.

```python
class Token:
    def __init__(self, type_, value, line=1):
        self.type = type_    # str: token type
        self.value = value   # any: token value
        self.line = line     # int: line number
```

#### `class Lexer`
Tokenizes source code.

```python
class Lexer:
    def __init__(self, text):
        # Initialize with source text

    def tokenize(self):
        # Returns list of Token objects
```

### Parser (`veyra.parser`)

The parser converts tokens into an Abstract Syntax Tree (AST).

#### Main Classes

```python
class Program:
    def __init__(self):
        self.functions = []  # List of Function objects
        self.statements = [] # List of statement objects

class Function:
    def __init__(self, name, params, body):
        self.name = name      # str: function name
        self.params = params  # list: parameter names
        self.body = body      # Block: function body
```

#### `class Parser`
Parses tokens into AST.

```python
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def parse(self):
        # Returns Program object
```

### Interpreter (`veyra.interpreter`)

The interpreter executes the AST.

#### `class Interpreter`
Executes Veyra programs.

```python
class Interpreter:
    def __init__(self):
        self.global_env = {}  # Global variables
        self.functions = {}   # Defined functions

    def interpret(self, program):
        # Execute a Program object

    def call_function(self, name, args):
        # Call a function by name with arguments
```

## Extending Veyra

### Adding Built-in Functions

To add new built-in functions, modify the `call_function` method in `Interpreter`:

```python
elif name == 'my_function':
    # Your implementation here
    return result
```

### Adding Syntax

To add new syntax:

1. Update the lexer to recognize new tokens
2. Update the parser to handle new grammar rules
3. Update the interpreter to execute new constructs

### Creating Packages

Packages are regular Veyra files that define functions. Place them in the `lib/` directory:

```veyra
// lib/mypackage.veyra
fn my_function() {
    return "Hello from my package!";
}
```

## AST Node Types

### Expressions
- `Number` - Numeric literals
- `String` - String literals
- `Boolean` - Boolean literals
- `Variable` - Variable references
- `BinaryOp` - Binary operations (+, -, *, /, etc.)
- `UnaryOp` - Unary operations (-, !)
- `FunctionCall` - Function calls
- `Array` - Array literals
- `Index` - Array indexing

### Statements
- `LetStatement` - Variable declarations
- `AssignmentStatement` - Variable assignments
- `FunctionCallStatement` - Function calls as statements
- `IfStatement` - Conditional statements
- `WhileStatement` - While loops
- `ForStatement` - For loops
- `ReturnStatement` - Return statements
- `PrintStatement` - Print statements
- `ImportStatement` - Import statements

## Error Handling

Veyra uses Python exceptions for error handling. Common exceptions:

- `ValueError` - Syntax and semantic errors
- `KeyError` - Undefined variables/functions
- `TypeError` - Type mismatches

## Testing

Use pytest for testing:

```python
def test_my_feature():
    code = "my_function();"
    # Test implementation
    assert result == expected
```

## Performance Considerations

- The interpreter uses a simple tree-walking approach
- For better performance, consider:
  - Bytecode compilation
  - JIT compilation
  - C extension modules

## Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for contribution guidelines.