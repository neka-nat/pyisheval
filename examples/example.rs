use pyisheval::Interpreter;

fn main() {
    let mut interp = Interpreter::new();
    interp.eval("x = 10").unwrap();
    interp.eval("y = 20").unwrap();
    let val = interp
        .eval("[max(x + y * 2, x * 2 - y), abs(x - y), x**2]")
        .unwrap();
    println!("{}", val); // 50
}
