# Veyra Standard Library Reference

## Math Functions

### `abs(x)`
Returns the absolute value of x.

```veyra
let result = abs(-5);  // result = 5
```

### `sqrt(x)`
Returns the square root of x.

```veyra
let result = sqrt(16);  // result = 4
```

### `pow(x, y)`
Returns x raised to the power of y.

```veyra
let result = pow(2, 3);  // result = 8
```

### `max(...)`
Returns the maximum value from the arguments.

```veyra
let result = max(1, 5, 3);  // result = 5
```

### `min(...)`
Returns the minimum value from the arguments.

```veyra
let result = min(1, 5, 3);  // result = 1
```

## String Functions

### `upper(s)`
Converts string s to uppercase.

```veyra
let result = upper("hello");  // result = "HELLO"
```

### `lower(s)`
Converts string s to lowercase.

```veyra
let result = lower("HELLO");  // result = "hello"
```

### `split(s, sep)`
Splits string s by separator sep.

```veyra
let result = split("a,b,c", ",");  // result = ["a", "b", "c"]
```

### `join(arr, sep)`
Joins array elements with separator sep.

```veyra
let result = join(["a", "b", "c"], "-");  // result = "a-b-c"
```

### `len(obj)`
Returns the length of obj (string, array, or dict).

```veyra
let str_len = len("hello");     // str_len = 5
let arr_len = len([1,2,3]);     // arr_len = 3
```

## Web Functions

### `html_element(tag, content, attrs)`
Creates an HTML element.

```veyra
let div = html_element("div", "Hello", {"class": "greeting"});
// <div class="greeting">Hello</div>
```

### `css_style(selector, rules)`
Generates CSS rules.

```veyra
let style = css_style("body", "background: blue; color: white;");
// body { background: blue; color: white; }
```

### `create_page(title, body, css)`
Creates a complete HTML page.

```veyra
let page = create_page("My Site", "<h1>Hello</h1>", "body{background:red;}");
```

### `web_serve(content, port)`
Starts a web server serving the content on the specified port.

```veyra
web_serve(page, 8080);  // Serves on http://localhost:8080
```

## UI Functions

### `ui_button(text, callback)`
Creates a UI button.

```veyra
let button = ui_button("Click me", "alert('Clicked!')");
```

### `ui_layout(direction, children)`
Creates a layout container.

```veyra
let layout = ui_layout("column", [button1, button2]);
```

### `ui_form(type, placeholder)`
Creates a form input.

```veyra
let input = ui_form("text", "Enter your name");
```

### `ui_animation(element, anim_type)`
Adds animation to an element.

```veyra
let animated = ui_animation(".button", "fadeIn");
```

### `ui_accessible(element, label)`
Creates an accessible UI element.

```veyra
let accessible = ui_accessible("button", "Submit Form");
```

## Utility Functions

### `println(value)`
Prints value to the console.

```veyra
println("Hello, World!");
println(42);
```

## AI/ML Functions

### `ai_linear_regression(X, y)`
Performs simple linear regression.

```veyra
let result = ai_linear_regression([[1,2],[3,4]], [2,4]);
```

## Networking Functions

### `http_get(url)`
Makes an HTTP GET request.

```veyra
let response = http_get("https://api.example.com/data");
```

## Concurrency Functions

### `channel()`
Creates a communication channel.

```veyra
let chan = channel();
```

### `send(channel, value)`
Sends a value through a channel.

```veyra
send(chan, "message");
```

### `receive(channel)`
Receives a value from a channel.

```veyra
let message = receive(chan);
```

## Package Management

### Installing Packages

```bash
veyra pm install math
veyra pm list
```

### Using Imported Functions

```veyra
import "lib/math.veyra";

fn main() {
    let result = factorial(5);
    println(result);
}
```