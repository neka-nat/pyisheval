# pyisheval

`pyisheval` is a Rust library that allows you to evaluate Python-like expressions.  
It's not a full Python interpreter, but it supports a subset of Python-like syntax:

- Arithmetic operations: `+`, `-`, `*`, `/`, `//`, `%`, `**`, `>`, `<`, `>=`, `<=`, `==`, `!=`
- Variables and assignments
- Lambda expressions (`lambda x: x + 1`)
- Built-in functions: `abs`, `max`, `min`, `int`, `float`, `len`, `sum`
- List and dictionary literals
- List comprehensions: `[y * 2 for y in x]`
- Conditional expressions: `x if x > 5 else 0`
- String method: `str.upper()`, `str.lower()`, `str.splitlines()`, etc.
- List method: `list.append()`, `list.clear()`, etc.
- Dict method: `dict.clear()`, `dict.get()`, `dict.items()`, `dict.keys()`, `dict.values()`, etc.

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

    // Conditional expression
    let val = interp.eval("x if x > y else y").unwrap();
    println!("{}", val); // 10

    // List comprehension
    let val = interp.eval("[y * 2 for y in x]").unwrap();
    println!("{}", val); // [2, 4, 6, 8, 10]

    // Dict comprehension
    let val = interp.eval("{y: y * 2 for y in x}").unwrap();
    println!("{}", val); // {1: 2, 2: 4, 3: 6, 4: 8, 5: 10}

    // String method
    let val = interp.eval("'hello'.upper()").unwrap();
    println!("{}", val); // HELLO

    // List method
    interp.eval("x = [1, 2, 3]").unwrap();
    let val = interp.eval("x.append(4)").unwrap();
    println!("{}", val); // [1, 2, 3, 4]

    // Dict method
    interp.eval("x = {'a': 1, 'b': 2}").unwrap();
    let val = interp.eval("x.items()").unwrap();
    println!("{}", val); // [(a, 1), (b, 2)]
}
```

## Why
This library aims to provide a lightweight and embedded Python-like expression evaluator for scenarios where you want to let users provide arithmetic expressions or simple lambdas without embedding a full Python interpreter.
