use pyisheval::Interpreter;
use pyisheval::Value;
use std::collections::HashMap;

fn main() {
    let mut interp = Interpreter::new();
    interp.eval("x = 10").unwrap();
    interp.eval("y = 20").unwrap();
    let val = interp
        .eval("[max(x + y * 2, x * 2 - y), abs(x - y), x**2]")
        .unwrap();
    println!("{}", val); // 50

    let context = HashMap::from([("x".to_string(), Value::Number(10.0))]);
    let val = interp.eval_with_context("x + 1", &context).unwrap();
    println!("{}", val); // 11

    let val = interp.eval_boolean("x + 1").unwrap();
    println!("{}", val); // true
}
