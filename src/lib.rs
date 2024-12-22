mod ast;
mod eval;
mod parser;

pub use eval::{EvalError, Interpreter, Value};

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_arithmetic() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("1+2").unwrap().to_string(), "3");
        assert_eq!(interp.eval("2*3+4").unwrap().to_string(), "10");
        assert_eq!(interp.eval("10/2").unwrap().to_string(), "5");
    }

    #[test]
    fn test_variables() {
        let mut interp = Interpreter::new();
        interp.eval("x=10").unwrap();
        interp.eval("y=20").unwrap();
        assert_eq!(interp.eval("x+y").unwrap().to_string(), "30");
        interp.eval("x=x+5").unwrap();
        assert_eq!(interp.eval("x").unwrap().to_string(), "15");
    }

    #[test]
    fn test_lambda() {
        let mut interp = Interpreter::new();
        interp.eval("inc = lambda a: a+1").unwrap();
        assert_eq!(interp.eval("inc(10)").unwrap().to_string(), "11");
        interp.eval("double = lambda a: a*2").unwrap();
        assert_eq!(interp.eval("double(5)").unwrap().to_string(), "10");
    }

    #[test]
    fn test_comparison() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("1 > 2").unwrap().to_string(), "0");
        assert_eq!(interp.eval("1 < 2").unwrap().to_string(), "1");
        assert_eq!(interp.eval("1 >= 2").unwrap().to_string(), "0");
        assert_eq!(interp.eval("1 <= 2").unwrap().to_string(), "1");
        assert_eq!(interp.eval("1 == 2").unwrap().to_string(), "0");
        assert_eq!(interp.eval("1 != 2").unwrap().to_string(), "1");
    }

    #[test]
    fn test_initialize_iterable() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(interp.eval("x[0]").unwrap().to_string(), "1");
        interp.eval("y = (1, 2, 3)").unwrap();
        assert_eq!(interp.eval("y[0]").unwrap().to_string(), "1");
        interp.eval("z = {1, 2, 3}").unwrap();
        assert_eq!(interp.eval("z[0]").unwrap().to_string(), "1");
        interp.eval("w = {'a': 2, 'b': 3}").unwrap();
        assert_eq!(interp.eval("w['a']").unwrap().to_string(), "2");
    }

    #[test]
    fn test_builtin_len_value() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(interp.eval("len(x)").unwrap().to_string(), "3");
        assert_eq!(interp.eval("len('abc')").unwrap().to_string(), "3");
    }

    #[test]
    fn test_builtin_sum_value() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(interp.eval("sum(x)").unwrap().to_string(), "6");
    }

    #[test]
    fn test_list_comp() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3, 4, 5]").unwrap();
        assert_eq!(
            interp.eval("[y * 2 for y in x]").unwrap().to_string(),
            "[2, 4, 6, 8, 10]"
        );
        assert_eq!(
            interp
                .eval("[y * 2 for y in x if y > 2]")
                .unwrap()
                .to_string(),
            "[6, 8, 10]"
        );
    }
}
