# Veyra Tutorials: Learn by Doing

Welcome to Veyra! These interactive tutorials will teach you everything from basics to advanced topics.

## Tutorial 1: Hello World
```veyra
fn main() {
    println("Hello, Veyra!");
}
```
Run: `python main.py hello.veyra`

## Tutorial 2: Variables and Types
```veyra
fn main() {
    let x = 42;
    let pi = 3.14;
    let name = "Veyra";
    println(x);
    println(pi);
    println(name);
}
```

## Tutorial 3: Functions
```veyra
fn add(a, b) {
    return a + b;
}

fn main() {
    let result = add(5, 3);
    println(result);
}
```

## Tutorial 4: Control Flow
```veyra
fn main() {
    let x = 10;
    if x > 5 {
        println("x is big");
    } else {
        println("x is small");
    }

    for i in 0 .. 3 {
        println(i);
    }
}
```

## Tutorial 5: Web Development
```veyra
fn main() {
    let html = html_element("p", "Hello Web!");
    println(html);
    web_serve(3000);
}
```

## Tutorial 6: AI Basics
```veyra
fn main() {
    let X = [1, 2, 3];
    let y = [2, 4, 6];
    let model = ai_linear_regression(X, y);
    println(model);
}
```

## Tutorial 7: UI Apps
```veyra
fn main() {
    ui_button("Start", "App started!");
}
```

## Advanced Topics
- Pattern Matching
- Concurrency with Channels
- Building Full Apps

Each tutorial builds on the last. Practice and experiment!

For interactive playground, visit [veyra.dev/playground](https://veyra.dev/playground) (coming soon).
