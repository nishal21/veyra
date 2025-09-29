# Veyra Language Guide

## Introduction

Veyra is a modern programming language designed for web development and rapid prototyping. It features a clean, readable syntax with built-in web capabilities.

## Basic Syntax

### Variables

```veyra
let message = "Hello, World!";
let count = 42;
let pi = 3.14159;
let is_active = true;
```

### Functions

```veyra
fn greet(name) {
    return "Hello, " + name + "!";
}

fn add(a, b) {
    return a + b;
}
```

### Control Flow

#### If/Else Statements

```veyra
if age >= 18 {
    println("Adult");
} else {
    println("Minor");
}
```

#### While Loops

```veyra
let i = 0;
while i < 5 {
    println(i);
    i = i + 1;
}
```

#### For Loops

```veyra
for i in 0..5 {
    println(i);
}
```

### Data Types

#### Arrays

```veyra
let numbers = [1, 2, 3, 4, 5];
let first = numbers[0];
```

#### Dictionaries

```veyra
let person = {
    "name": "Alice",
    "age": 30,
    "city": "New York"
};
```

### Expressions

Veyra supports standard arithmetic and comparison operators:

```veyra
let sum = 2 + 3;
let product = 4 * 5;
let is_equal = (2 + 2) == 4;
let is_greater = 5 > 3;
```

### Comments

```veyra
// This is a single-line comment

/*
This is a multi-line comment
*/
```

## Advanced Features

### Pattern Matching

```veyra
match value {
    1 => "one",
    2 => "two",
    _ => "other"
}
```

### Concurrency

```veyra
let chan = channel();
send(chan, "hello");
let message = receive(chan);
```

## Error Handling

Veyra provides clear error messages with line numbers to help with debugging.

## Best Practices

1. Use descriptive variable and function names
2. Keep functions small and focused
3. Use comments to explain complex logic
4. Follow consistent indentation (4 spaces recommended)

## Next Steps

- Check out the [Standard Library Reference](standard_library.md)
- Look at [examples](../examples/) for more code samples
- Read the [API Documentation](api.md) for extending Veyra