# pyisheval

`pyisheval` is a Rust library that allows you to evaluate Python-like expressions.  
It's not a full Python interpreter, but it supports a subset of Python-like syntax:

- Arithmetic operations: `+`, `-`, `*`, `/`, `%`, `**`
- Variables and assignments
- Lambda expressions (`lambda x: x + 1`)
- Built-in functions: `abs`, `max`, `min`, `int`, `float`
- List and dictionary literals

No classes, functions (def), or control structures are supported.

## Installation

```sh
cargo add pyisheval
```

## Example

```rust
use pyisheval::Interpreter;

fn main() {
    let mut interp = Interpreter::new();

    // Assign variables
    interp.eval("x = 10").unwrap();
    interp.eval("y = 20").unwrap();

    // Arithmetic
    let val = interp.eval("x + y * 2").unwrap();
    println!("{}", val); // 50

    // Lambda
    interp.eval("inc = lambda a: a + 1").unwrap();
    let val = interp.eval("inc(x)").unwrap();
    println!("{}", val); // 11
}
```

## Why
This library aims to provide a lightweight and embedded Python-like expression evaluator for scenarios where you want to let users provide arithmetic expressions or simple lambdas without embedding a full Python interpreter.
