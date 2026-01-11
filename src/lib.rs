mod ast;
mod eval;
mod parser;

pub use eval::{EvalError, Interpreter, Value};

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

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
    fn test_eval_with_context() {
        let interp = Interpreter::new();
        let context = HashMap::from([("x".to_string(), Value::Number(10.0))]);
        assert_eq!(
            interp.eval_with_context("x + 1", &context).unwrap().to_string(),
            "11"
        );
    }

    #[test]
    fn test_list() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(
            interp.eval("x + [4, 5, 6]").unwrap().to_string(),
            "[1, 2, 3, 4, 5, 6]"
        );
        assert_eq!(
            interp.eval("x[0] == 1").unwrap().to_string(),
            "1"
        );
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
    fn test_builtin() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(interp.eval("len(x)").unwrap().to_string(), "3");
        assert_eq!(interp.eval("len('abc')").unwrap().to_string(), "3");
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(interp.eval("sum(x)").unwrap().to_string(), "6");
        assert_eq!(interp.eval("abs(-10)").unwrap().to_string(), "10");
        assert_eq!(interp.eval("max(1, 2, 3)").unwrap().to_string(), "3");
        assert_eq!(interp.eval("min(1, 2, 3)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("int(1.5)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("float(1)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("str(1)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("str(1.5)").unwrap().to_string(), "1.5");
        assert_eq!(
            interp.eval("str([1, 2, 3])").unwrap().to_string(),
            "[1, 2, 3]"
        );
        assert_eq!(
            interp.eval("str({'a': 1, 'b': 2})").unwrap().to_string(),
            "{a: 1, b: 2}"
        );
        assert_eq!(
            interp.eval("dict({'a': 1, 'b': 2})").unwrap().to_string(),
            "{a: 1, b: 2}"
        );
        assert_eq!(
            interp.eval("list([1, 2, 3])").unwrap().to_string(),
            "[1, 2, 3]"
        );
        assert_eq!(
            interp.eval("set([1, 2, 3])").unwrap().to_string(),
            "{1, 2, 3}"
        );
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

    #[test]
    fn test_dict_comp() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3, 4, 5]").unwrap();
        assert_eq!(
            interp.eval("{y: y * 2 for y in x}").unwrap().to_string(),
            "{1: 2, 2: 4, 3: 6, 4: 8, 5: 10}"
        );
    }

    #[test]
    fn test_if_expr() {
        let mut interp = Interpreter::new();
        interp.eval("x = 10").unwrap();
        assert_eq!(interp.eval("x if x > 5 else 0").unwrap().to_string(), "10");
        interp.eval("y = 20").unwrap();
        assert_eq!(
            interp
                .eval("'big' if x > y else 'small'")
                .unwrap()
                .to_string(),
            "small"
        );
    }

    #[test]
    fn test_string_method() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("'hello'.upper()").unwrap().to_string(), "HELLO");
        assert_eq!(interp.eval("'Hello'.lower()").unwrap().to_string(), "hello");
        interp.eval("x = 'a\nb\nc'").unwrap();
        assert_eq!(
            interp.eval("x.splitlines()").unwrap().to_string(),
            "[a, b, c]"
        );
    }

    #[test]
    fn test_list_method() {
        let mut interp = Interpreter::new();
        interp.eval("x = [1, 2, 3]").unwrap();
        assert_eq!(
            interp.eval("x.append(4)").unwrap().to_string(),
            "[1, 2, 3, 4]"
        );
        assert_eq!(interp.eval("x.clear()").unwrap().to_string(), "[]");
    }

    #[test]
    fn test_dict_method() {
        let mut interp = Interpreter::new();
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        assert_eq!(interp.eval("x.clear()").unwrap().to_string(), "{}");
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        assert_eq!(interp.eval("x.get('a')").unwrap().to_string(), "1");
        assert_eq!(interp.eval("x.get('c', 0)").unwrap().to_string(), "0");
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        assert_eq!(
            interp.eval("x.items()").unwrap().to_string(),
            "[(a, 1), (b, 2)]"
        );
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        assert_eq!(interp.eval("x.keys()").unwrap().to_string(), "[a, b]");
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        assert_eq!(interp.eval("x.values()").unwrap().to_string(), "[1, 2]");
    }

    #[test]
    fn test_strings_non_empty_single_quote() {
        let interp = Interpreter::new();
        assert_eq!(
            interp
                .eval_with_context("'a'", &Default::default())
                .unwrap()
                .to_string(),
            "a"
        );
        assert_eq!(
            interp
                .eval_with_context("'hello'", &Default::default())
                .unwrap()
                .to_string(),
            "hello"
        );
        assert!(interp.eval_boolean("'a' == 'a'").unwrap());
    }

    #[test]
    fn test_strings_single_quote_empty() {
        let interp = Interpreter::new();
        assert_eq!(
            interp
                .eval_with_context("''", &Default::default())
                .unwrap()
                .to_string(),
            ""
        );
        assert!(interp.eval_boolean("'' == ''").unwrap());
        assert_eq!(
            interp
                .eval_with_context("len('')", &Default::default())
                .unwrap()
                .to_string(),
            "0"
        );
    }

    #[test]
    fn test_strings_empty_comparison() {
        let mut interp = Interpreter::new();
        interp.eval("x = ''").unwrap();
        assert!(interp.eval_boolean("x == ''").unwrap());
        assert!(interp.eval_boolean("x != 'foo'").unwrap());
    }

    #[test]
    fn test_leading_dot_decimals() {
        let mut interp = Interpreter::new();
        // Basic leading dot cases
        assert_eq!(interp.eval(".02").unwrap().to_string(), "0.02");
        assert_eq!(interp.eval(".38").unwrap().to_string(), "0.38");
        assert_eq!(interp.eval(".021").unwrap().to_string(), "0.021");
        assert_eq!(interp.eval(".5").unwrap().to_string(), "0.5");

        // With unary minus
        assert_eq!(interp.eval("-.021").unwrap().to_string(), "-0.021");
        assert_eq!(interp.eval("-.5").unwrap().to_string(), "-0.5");
    }

    #[test]
    fn test_trailing_dot_decimals() {
        let mut interp = Interpreter::new();
        // Python accepts trailing dot as valid float syntax
        assert_eq!(interp.eval("4.").unwrap().to_string(), "4");
        assert_eq!(interp.eval("10.").unwrap().to_string(), "10");
        assert_eq!(interp.eval("123.").unwrap().to_string(), "123");
    }

    #[test]
    fn test_scientific_notation() {
        let mut interp = Interpreter::new();
        // Basic scientific notation
        assert_eq!(interp.eval("1e-3").unwrap().to_string(), "0.001");
        assert_eq!(interp.eval("1e3").unwrap().to_string(), "1000");
        assert_eq!(interp.eval("2.5e-2").unwrap().to_string(), "0.025");
        assert_eq!(interp.eval("1e+3").unwrap().to_string(), "1000");

        // Uppercase E
        assert_eq!(interp.eval("1E-3").unwrap().to_string(), "0.001");
        assert_eq!(interp.eval("1E3").unwrap().to_string(), "1000");

        // Leading dot with exponent
        assert_eq!(interp.eval(".5e2").unwrap().to_string(), "50");
        assert_eq!(interp.eval(".1e-2").unwrap().to_string(), "0.001");
    }

    #[test]
    fn test_number_parsing_in_expressions() {
        let mut interp = Interpreter::new();
        interp.eval("width = 10.0").unwrap();
        interp.eval("weight = 100.0").unwrap();
        interp.eval("reflect = 1.0").unwrap();
        interp.eval("pi = 3.14159").unwrap();

        // Leading dots example
        assert_eq!(
            interp.eval("reflect*(width+.02)").unwrap().to_string(),
            "10.02"
        );
        assert_eq!(
            interp.eval(".38 * weight").unwrap().to_string(),
            "38"
        );
        // Floating point precision: -0.0145 - 0.021 = -0.0355 (with minor precision error)
        let result = interp.eval("-0.0145 - .021").unwrap();
        let n = match result {
            Value::Number(n) => n,
            other => panic!("Expected Number, got {:?}", other),
        };
        assert!((n - (-0.0355)).abs() < 1e-10, "Expected -0.0355, got {}", n);

        // Scientific notation example
        assert_eq!(
            interp.eval("1e-3 * weight").unwrap().to_string(),
            "0.1"
        );

        // Trailing dot example
        // -pi*3/4. = -3.14159*3/4 = -2.3561925
        let result = interp.eval("-pi*3/4.").unwrap();
        let n = match result {
            Value::Number(n) => n,
            other => panic!("Expected Number, got {:?}", other),
        };
        let expected = -3.14159 * 3.0 / 4.0;
        assert!((n - expected).abs() < 1e-5, "Expected {}, got {}", expected, n);
    }

    #[test]
    fn test_number_parsing_error_location() {
        let mut interp = Interpreter::new();

        // Test that parse errors report sensible locations
        // Invalid: number followed by invalid character should fail
        let result = interp.eval("123..456");
        assert!(
            matches!(&result, Err(EvalError::ParseError(_))),
            "Expected a ParseError for '123..456', but got {:?}",
            result
        );

        // Invalid: starts with valid float syntax but has issues
        let result = interp.eval(".e5");
        assert!(
            matches!(&result, Err(EvalError::ParseError(_))),
            "Expected a ParseError for '.e5', but got {:?}",
            result
        );
    }
}
