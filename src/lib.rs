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
        // str() returns StringLit, which now outputs quoted for valid Python syntax
        assert_eq!(interp.eval("str(1)").unwrap().to_string(), "'1'");
        assert_eq!(interp.eval("str(1.5)").unwrap().to_string(), "'1.5'");
        assert_eq!(
            interp.eval("str([1, 2, 3])").unwrap().to_string(),
            "'[1, 2, 3]'"
        );
        // str() converts dict to StringLit, which then escapes quotes
        assert_eq!(
            interp.eval("str({'a': 1, 'b': 2})").unwrap().to_string(),
            "'{\\'a\\': 1, \\'b\\': 2}'"
        );
        // dict() returns Dict directly, which quotes string keys
        assert_eq!(
            interp.eval("dict({'a': 1, 'b': 2})").unwrap().to_string(),
            "{'a': 1, 'b': 2}"
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
        // Dict keys are always quoted (even numeric-looking ones) for valid Python syntax
        assert_eq!(
            interp.eval("{y: y * 2 for y in x}").unwrap().to_string(),
            "{'1': 2, '2': 4, '3': 6, '4': 8, '5': 10}"
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
            "'small'"
        );
    }

    #[test]
    fn test_chained_conditional() {
        let mut interp = Interpreter::new();
        // Python: 1 if False else 2 if False else 3 → evaluates to 3
        assert_eq!(interp.eval("1 if 0 else 2 if 0 else 3").unwrap().to_string(), "3");
        // Python: 1 if True else 2 if False else 3 → evaluates to 1
        assert_eq!(interp.eval("1 if 1 else 2 if 0 else 3").unwrap().to_string(), "1");
        // Python: 1 if False else 2 if True else 3 → evaluates to 2
        assert_eq!(interp.eval("1 if 0 else 2 if 1 else 3").unwrap().to_string(), "2");
    }

    #[test]
    fn test_string_method() {
        let mut interp = Interpreter::new();
        // String methods return StringLit, which now quotes output
        assert_eq!(interp.eval("'hello'.upper()").unwrap().to_string(), "'HELLO'");
        assert_eq!(interp.eval("'Hello'.lower()").unwrap().to_string(), "'hello'");
        interp.eval("x = 'a\nb\nc'").unwrap();
        // splitlines() returns a list of StringLits, which are now quoted
        assert_eq!(
            interp.eval("x.splitlines()").unwrap().to_string(),
            "['a', 'b', 'c']"
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
        // items() returns list of tuples with StringLit keys
        assert_eq!(
            interp.eval("x.items()").unwrap().to_string(),
            "[('a', 1), ('b', 2)]"
        );
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        // keys() returns list of StringLits
        assert_eq!(interp.eval("x.keys()").unwrap().to_string(), "['a', 'b']");
        interp.eval("x = {'a': 1, 'b': 2}").unwrap();
        assert_eq!(interp.eval("x.values()").unwrap().to_string(), "[1, 2]");
    }

    #[test]
    fn test_strings_non_empty_single_quote() {
        let interp = Interpreter::new();
        // StringLit now outputs quoted strings for valid Python syntax
        assert_eq!(
            interp
                .eval_with_context("'a'", &Default::default())
                .unwrap()
                .to_string(),
            "'a'"
        );
        assert_eq!(
            interp
                .eval_with_context("'hello'", &Default::default())
                .unwrap()
                .to_string(),
            "'hello'"
        );
        assert!(interp.eval_boolean("'a' == 'a'").unwrap());
    }

    #[test]
    fn test_strings_single_quote_empty() {
        let interp = Interpreter::new();
        // Empty string now outputs as '' for valid Python syntax
        assert_eq!(
            interp
                .eval_with_context("''", &Default::default())
                .unwrap()
                .to_string(),
            "''"
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

    #[test]
    fn test_not_operator_numbers() {
        let mut interp = Interpreter::new();
        // not 0 is True (1 in pyisheval)
        assert_eq!(interp.eval("not 0").unwrap().to_string(), "1");
        // not 1 is False (0)
        assert_eq!(interp.eval("not 1").unwrap().to_string(), "0");
        // not 5 is False
        assert_eq!(interp.eval("not 5").unwrap().to_string(), "0");
        // not -1 is False
        assert_eq!(interp.eval("not -1").unwrap().to_string(), "0");
        // not 0.0 is True
        assert_eq!(interp.eval("not 0.0").unwrap().to_string(), "1");
    }

    #[test]
    fn test_not_operator_strings() {
        let mut interp = Interpreter::new();
        // not '' is True
        assert_eq!(interp.eval("not ''").unwrap().to_string(), "1");
        // not 'foo' is False
        assert_eq!(interp.eval("not 'foo'").unwrap().to_string(), "0");
        // not 'hello' is False
        assert_eq!(interp.eval("not 'hello'").unwrap().to_string(), "0");
    }

    #[test]
    fn test_not_operator_collections() {
        let mut interp = Interpreter::new();
        // not [] is True
        assert_eq!(interp.eval("not []").unwrap().to_string(), "1");
        // not [1, 2] is False
        assert_eq!(interp.eval("not [1, 2]").unwrap().to_string(), "0");
        // not {} is True
        assert_eq!(interp.eval("not {}").unwrap().to_string(), "1");
        // not {'a': 1} is False
        assert_eq!(interp.eval("not {'a': 1}").unwrap().to_string(), "0");
        // not (1, 2) is False
        assert_eq!(interp.eval("not (1, 2)").unwrap().to_string(), "0");
        // not () is True
        assert_eq!(interp.eval("not ()").unwrap().to_string(), "1");
    }

    #[test]
    fn test_not_operator_chaining() {
        let mut interp = Interpreter::new();
        // Double negation
        assert_eq!(interp.eval("not not 0").unwrap().to_string(), "0");
        assert_eq!(interp.eval("not not 1").unwrap().to_string(), "1");
        assert_eq!(interp.eval("not not 5").unwrap().to_string(), "1");
    }

    #[test]
    fn test_not_operator_with_variables() {
        let mut interp = Interpreter::new();
        interp.eval("x = 0").unwrap();
        assert_eq!(interp.eval("not x").unwrap().to_string(), "1");

        interp.eval("y = 10").unwrap();
        assert_eq!(interp.eval("not y").unwrap().to_string(), "0");

        interp.eval("empty = ''").unwrap();
        assert_eq!(interp.eval("not empty").unwrap().to_string(), "1");
    }

    #[test]
    fn test_not_operator_in_expressions() {
        let mut interp = Interpreter::new();
        // not in comparisons
        assert_eq!(interp.eval("not (1 > 2)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("not (1 < 2)").unwrap().to_string(), "0");

        // not with arithmetic
        interp.eval("x = 5").unwrap();
        assert_eq!(interp.eval("not (x - 5)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("not (x * 2)").unwrap().to_string(), "0");
    }

    #[test]
    fn test_not_operator_precedence() {
        let mut interp = Interpreter::new();
        // not has lower precedence than comparisons in Python
        // "not 1 == 0" should parse as "not (1 == 0)" → not False → True
        assert_eq!(interp.eval("not 1 == 0").unwrap().to_string(), "1");

        // Critical test case: not 2 == 1 should be not (2 == 1) → not False → True
        assert_eq!(interp.eval("not 2 == 1").unwrap().to_string(), "1");

        // But we want to ensure parentheses work correctly too
        assert_eq!(interp.eval("(not 1) == 0").unwrap().to_string(), "1");
        assert_eq!(interp.eval("(not 2) == 1").unwrap().to_string(), "0");
    }

    #[test]
    fn test_builtin_constants() {
        let mut interp = Interpreter::new();
        // Test True constant
        assert_eq!(interp.eval("True").unwrap().to_string(), "1");
        // Test False constant
        assert_eq!(interp.eval("False").unwrap().to_string(), "0");
    }

    #[test]
    fn test_builtin_constants_comparisons() {
        let mut interp = Interpreter::new();
        // True == 1
        assert_eq!(interp.eval("True == 1").unwrap().to_string(), "1");
        // False == 0
        assert_eq!(interp.eval("False == 0").unwrap().to_string(), "1");
        // True != False
        assert_eq!(interp.eval("True != False").unwrap().to_string(), "1");
        // True == False should be false
        assert_eq!(interp.eval("True == False").unwrap().to_string(), "0");
    }

    #[test]
    fn test_builtin_constants_with_not() {
        let mut interp = Interpreter::new();
        // not True is False
        assert_eq!(interp.eval("not True").unwrap().to_string(), "0");
        // not False is True
        assert_eq!(interp.eval("not False").unwrap().to_string(), "1");
    }

    #[test]
    fn test_builtin_constants_in_expressions() {
        let mut interp = Interpreter::new();
        // Test variable comparison with True/False
        interp.eval("foo = 0").unwrap();
        assert_eq!(interp.eval("foo == True").unwrap().to_string(), "0");
        assert_eq!(interp.eval("foo == False").unwrap().to_string(), "1");

        interp.eval("foo = 1").unwrap();
        assert_eq!(interp.eval("foo == True").unwrap().to_string(), "1");
        assert_eq!(interp.eval("foo == False").unwrap().to_string(), "0");

        // Test arithmetic operations with True/False
        assert_eq!(interp.eval("True + 1").unwrap().to_string(), "2");
        assert_eq!(interp.eval("False + 1").unwrap().to_string(), "1");
        assert_eq!(interp.eval("True * 5").unwrap().to_string(), "5");
        assert_eq!(interp.eval("False * 5").unwrap().to_string(), "0");
    }

    #[test]
    fn test_and_operator_basic() {
        let mut interp = Interpreter::new();
        // Basic and operations
        assert_eq!(interp.eval("1 and 2").unwrap().to_string(), "2");
        assert_eq!(interp.eval("0 and 2").unwrap().to_string(), "0");
        assert_eq!(interp.eval("1 and 0").unwrap().to_string(), "0");
        assert_eq!(interp.eval("0 and 0").unwrap().to_string(), "0");

        // Boolean values
        assert_eq!(interp.eval("True and True").unwrap().to_string(), "1");
        assert_eq!(interp.eval("True and False").unwrap().to_string(), "0");
        assert_eq!(interp.eval("False and True").unwrap().to_string(), "0");
        assert_eq!(interp.eval("False and False").unwrap().to_string(), "0");
    }

    #[test]
    fn test_or_operator_basic() {
        let mut interp = Interpreter::new();
        // Basic or operations
        assert_eq!(interp.eval("1 or 2").unwrap().to_string(), "1");
        assert_eq!(interp.eval("0 or 2").unwrap().to_string(), "2");
        assert_eq!(interp.eval("1 or 0").unwrap().to_string(), "1");
        assert_eq!(interp.eval("0 or 0").unwrap().to_string(), "0");

        // Boolean values
        assert_eq!(interp.eval("True or True").unwrap().to_string(), "1");
        assert_eq!(interp.eval("True or False").unwrap().to_string(), "1");
        assert_eq!(interp.eval("False or True").unwrap().to_string(), "1");
        assert_eq!(interp.eval("False or False").unwrap().to_string(), "0");
    }

    #[test]
    fn test_and_operator_with_comparisons() {
        let mut interp = Interpreter::new();
        // And with comparison expressions
        assert_eq!(interp.eval("(5 > 3) and (10 < 20)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("(5 > 10) and (10 < 20)").unwrap().to_string(), "0");
        assert_eq!(interp.eval("(5 > 3) and (10 > 20)").unwrap().to_string(), "0");

        // With variables
        interp.eval("x = 5").unwrap();
        interp.eval("y = 10").unwrap();
        assert_eq!(interp.eval("(x > 3) and (y < 20)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("(x > 10) and (y < 20)").unwrap().to_string(), "0");
    }

    #[test]
    fn test_or_operator_with_comparisons() {
        let mut interp = Interpreter::new();
        // Or with comparison expressions
        assert_eq!(interp.eval("(5 > 10) or (10 < 20)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("(5 > 10) or (10 > 20)").unwrap().to_string(), "0");
        assert_eq!(interp.eval("(5 > 3) or (10 > 20)").unwrap().to_string(), "1");

        // With variables
        interp.eval("x = 5").unwrap();
        interp.eval("y = 10").unwrap();
        assert_eq!(interp.eval("(x > 10) or (y < 20)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("(x > 10) or (y > 20)").unwrap().to_string(), "0");
    }

    #[test]
    fn test_and_or_with_strings() {
        let mut interp = Interpreter::new();
        // And with strings - StringLits now output quoted
        assert_eq!(interp.eval("'hello' and 'world'").unwrap().to_string(), "'world'");
        assert_eq!(interp.eval("'' and 'world'").unwrap().to_string(), "''");
        assert_eq!(interp.eval("'hello' and ''").unwrap().to_string(), "''");

        // Or with strings - StringLits now output quoted
        assert_eq!(interp.eval("'hello' or 'world'").unwrap().to_string(), "'hello'");
        assert_eq!(interp.eval("'' or 'world'").unwrap().to_string(), "'world'");
        assert_eq!(interp.eval("'hello' or ''").unwrap().to_string(), "'hello'");
    }

    #[test]
    fn test_and_or_combined() {
        let mut interp = Interpreter::new();
        // Combined and/or (or has lower precedence)
        assert_eq!(interp.eval("1 and 2 or 0").unwrap().to_string(), "2");
        assert_eq!(interp.eval("0 and 2 or 3").unwrap().to_string(), "3");
        assert_eq!(interp.eval("1 or 2 and 0").unwrap().to_string(), "1");
        assert_eq!(interp.eval("0 or 2 and 3").unwrap().to_string(), "3");
    }

    #[test]
    fn test_and_or_with_not() {
        let mut interp = Interpreter::new();
        // not has higher precedence than and/or
        assert_eq!(interp.eval("not 0 and 1").unwrap().to_string(), "1");
        assert_eq!(interp.eval("not 1 and 1").unwrap().to_string(), "0");
        assert_eq!(interp.eval("not 0 or 0").unwrap().to_string(), "1");
        assert_eq!(interp.eval("not 1 or 1").unwrap().to_string(), "1");
    }

    #[test]
    fn test_real_world_xacro_patterns() {
        let mut interp = Interpreter::new();
        // Pattern: check non-empty and non-default value
        interp.eval("robot_ns = 'my_robot'").unwrap();
        assert_eq!(
            interp.eval("(robot_ns != '') and (robot_ns != '/')").unwrap().to_string(),
            "1"
        );

        interp.eval("robot_ns = ''").unwrap();
        assert_eq!(
            interp.eval("(robot_ns != '') and (robot_ns != '/')").unwrap().to_string(),
            "0"
        );

        // Pattern: match any of several values
        interp.eval("x = 'a'").unwrap();
        assert_eq!(
            interp.eval("(x == 'a') or (x == 'b')").unwrap().to_string(),
            "1"
        );

        interp.eval("x = 'c'").unwrap();
        assert_eq!(
            interp.eval("(x == 'a') or (x == 'b')").unwrap().to_string(),
            "0"
        );

        // Pattern: multiple boolean flags
        interp.eval("use_fake_hardware = 1").unwrap();
        interp.eval("sim_ignition = 0").unwrap();
        interp.eval("sim_isaac = 0").unwrap();
        assert_eq!(
            interp.eval("use_fake_hardware or sim_ignition or sim_isaac").unwrap().to_string(),
            "1"
        );
    }

    #[test]
    fn test_mixed_type_eq_returns_false() {
        let mut interp = Interpreter::new();
        // Number == String should return False (0)
        assert_eq!(interp.eval("1.0 == '/'").unwrap().to_string(), "0");
        assert_eq!(interp.eval("42 == 'hello'").unwrap().to_string(), "0");
        assert_eq!(interp.eval("0 == ''").unwrap().to_string(), "0");

        // With variables
        interp.eval("x = 1.0").unwrap();
        assert_eq!(interp.eval("x == '/'").unwrap().to_string(), "0");

        interp.eval("s = '/'").unwrap();
        assert_eq!(interp.eval("1.0 == s").unwrap().to_string(), "0");
    }

    #[test]
    fn test_mixed_type_ne_returns_true() {
        let mut interp = Interpreter::new();
        // Number != String should return True (1)
        assert_eq!(interp.eval("1.0 != '/'").unwrap().to_string(), "1");
        assert_eq!(interp.eval("42 != 'hello'").unwrap().to_string(), "1");
        assert_eq!(interp.eval("0 != ''").unwrap().to_string(), "1");

        // With variables
        interp.eval("x = 1.0").unwrap();
        assert_eq!(interp.eval("x != '/'").unwrap().to_string(), "1");

        interp.eval("s = '/'").unwrap();
        assert_eq!(interp.eval("1.0 != s").unwrap().to_string(), "1");
    }

    #[test]
    fn test_mixed_type_comparison_commutative() {
        let mut interp = Interpreter::new();
        // String == Number should also return False (commutative)
        interp.eval("s = '/'").unwrap();
        assert_eq!(interp.eval("s == 1.0").unwrap().to_string(), "0");

        // String != Number should return True
        assert_eq!(interp.eval("s != 1.0").unwrap().to_string(), "1");

        // Both orderings should give same result
        interp.eval("num = 42").unwrap();
        interp.eval("str = 'test'").unwrap();
        assert_eq!(interp.eval("num == str").unwrap().to_string(), "0");
        assert_eq!(interp.eval("str == num").unwrap().to_string(), "0");
        assert_eq!(interp.eval("num != str").unwrap().to_string(), "1");
        assert_eq!(interp.eval("str != num").unwrap().to_string(), "1");
    }

    #[test]
    fn test_mixed_type_falsy_values() {
        let mut interp = Interpreter::new();
        // Empty string and 0 are both "falsy" but not equal
        interp.eval("empty = ''").unwrap();
        assert_eq!(interp.eval("empty != 0").unwrap().to_string(), "1");
        assert_eq!(interp.eval("empty == 0").unwrap().to_string(), "0");

        // This is important: different types are never equal
        // even if both would be falsy in boolean context
        assert_eq!(interp.eval("'' == 0").unwrap().to_string(), "0");
        assert_eq!(interp.eval("0 == ''").unwrap().to_string(), "0");
    }

    #[test]
    fn test_mixed_type_in_boolean_expressions() {
        let mut interp = Interpreter::new();
        // Check if prefix equals any of several string values
        // When prefix is a number (e.g. True → 1.0), all comparisons should be False
        interp.eval("prefix = 1.0").unwrap();

        // Each individual comparison returns False (0)
        assert_eq!(interp.eval("prefix == '/'").unwrap().to_string(), "0");
        assert_eq!(interp.eval("prefix == ''").unwrap().to_string(), "0");
        assert_eq!(interp.eval("prefix == ' '").unwrap().to_string(), "0");

        // Overall OR expression should be False (0)
        assert_eq!(
            interp.eval("prefix == '/' or prefix == '' or prefix == ' '").unwrap().to_string(),
            "0"
        );
    }

    #[test]
    fn test_same_type_comparisons_still_work() {
        let mut interp = Interpreter::new();
        // Ensure same-type comparisons are unaffected

        // Number == Number
        assert_eq!(interp.eval("1.0 == 1.0").unwrap().to_string(), "1");
        assert_eq!(interp.eval("1.0 != 2.0").unwrap().to_string(), "1");

        // String == String
        interp.eval("s1 = 'hello'").unwrap();
        interp.eval("s2 = 'hello'").unwrap();
        assert_eq!(interp.eval("s1 == s2").unwrap().to_string(), "1");

        interp.eval("s3 = 'world'").unwrap();
        assert_eq!(interp.eval("s1 != s3").unwrap().to_string(), "1");

        // Empty strings
        assert_eq!(interp.eval("'' == ''").unwrap().to_string(), "1");
    }

    #[test]
    fn test_collection_equality() {
        let mut interp = Interpreter::new();

        // Lists
        assert_eq!(interp.eval("[1, 2] == [1, 2]").unwrap().to_string(), "1");
        assert_eq!(interp.eval("[1, 2] != [1, 3]").unwrap().to_string(), "1");
        assert_eq!(interp.eval("[] == []").unwrap().to_string(), "1");

        // Tuples
        assert_eq!(interp.eval("(1, 2) == (1, 2)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("(1, 2) != (1, 3)").unwrap().to_string(), "1");
        assert_eq!(interp.eval("() == ()").unwrap().to_string(), "1");

        // Dicts
        assert_eq!(interp.eval("{'a': 1} == {'a': 1}").unwrap().to_string(), "1");
        assert_eq!(interp.eval("{'a': 1} != {'a': 2}").unwrap().to_string(), "1");
        assert_eq!(interp.eval("{} == {}").unwrap().to_string(), "1");

        // Sets
        assert_eq!(interp.eval("set([1, 2]) == set([1, 2])").unwrap().to_string(), "1");
        assert_eq!(interp.eval("set([1, 2]) == set([2, 1])").unwrap().to_string(), "1"); // Order shouldn't matter
        assert_eq!(interp.eval("set([1, 2]) != set([1, 3])").unwrap().to_string(), "1");
        assert_eq!(interp.eval("set() == set()").unwrap().to_string(), "1");

        // Mixed-type (collection vs non-collection) should be False
        assert_eq!(interp.eval("[1, 2] == 5").unwrap().to_string(), "0");
        assert_eq!(interp.eval("[1, 2] != 5").unwrap().to_string(), "1");
        assert_eq!(interp.eval("set([1, 2]) == 5").unwrap().to_string(), "0");
    }

    #[test]
    fn test_set_equality_with_duplicates() {
        let mut interp = Interpreter::new();

        // Sets deduplicate properly, so duplicates in input don't affect equality.
        // Our multiset comparison handles the edge case where Value::Set might
        // contain duplicates (though builtin_set_value should prevent this).

        // Both deduplicate to {1, 2} -> equal
        assert_eq!(interp.eval("set([1, 1, 2]) == set([1, 2, 2])").unwrap().to_string(), "1");

        // Different elements after dedup -> not equal
        assert_eq!(interp.eval("set([1, 2]) == set([1, 3])").unwrap().to_string(), "0");

        // Same elements, order doesn't matter -> equal
        assert_eq!(interp.eval("set([2, 1]) == set([1, 2])").unwrap().to_string(), "1");
    }

    #[test]
    fn test_lambda_comparison() {
        let mut interp = Interpreter::new();

        // Lambda comparisons throw TypeError - they require identity tracking
        // that we don't have (would need Rc wrappers + ptr_eq).
        interp.eval("f1 = lambda x: 1").unwrap();
        interp.eval("f2 = lambda x: 1").unwrap();

        // All lambda comparisons throw TypeError
        assert!(matches!(
            interp.eval("f1 == f2"),
            Err(crate::EvalError::TypeError)
        ));

        assert!(matches!(
            interp.eval("f1 == f1"),
            Err(crate::EvalError::TypeError)
        ));

        assert!(matches!(
            interp.eval("f1 != f2"),
            Err(crate::EvalError::TypeError)
        ));
    }

    #[test]
    fn test_builtin_comparison() {
        let mut interp = Interpreter::new();

        // Builtins can be compared (by name)
        interp.eval("f1 = len").unwrap();
        interp.eval("f2 = len").unwrap();

        assert_eq!(interp.eval("f1 == f2").unwrap().to_string(), "1");
        assert_eq!(interp.eval("f1 != f2").unwrap().to_string(), "0");
        assert_eq!(interp.eval("len == sum").unwrap().to_string(), "0");
        assert_eq!(interp.eval("len != sum").unwrap().to_string(), "1");
    }

    #[test]
    fn test_var_stringlit_cross_type_equality() {
        let mut interp = Interpreter::new();

        // Dict iteration creates Var values, not StringLit
        interp.eval("d = {'a': 1, 'b': 2}").unwrap();
        interp.eval("keys = [k for k in d]").unwrap();

        // Var and StringLit should compare as equal when content matches
        assert_eq!(interp.eval("keys[0] == 'a'").unwrap().to_string(), "1");
        assert_eq!(interp.eval("keys[1] == 'b'").unwrap().to_string(), "1");

        // List of Var should equal list of StringLit
        assert_eq!(interp.eval("keys == ['a', 'b']").unwrap().to_string(), "1");

        // Reverse comparison should also work
        assert_eq!(interp.eval("'a' == keys[0]").unwrap().to_string(), "1");
    }

    #[test]
    fn test_dict_display_round_trip_string_keys() {
        let mut interp = Interpreter::new();
        let input = "{'key': 'value'}";
        let parsed = interp.eval(input).unwrap();
        let output = parsed.to_string();

        // Should be able to parse the output back
        let reparsed = interp.eval(&output).unwrap();
        assert_eq!(parsed, reparsed);
    }

    #[test]
    fn test_dict_display_round_trip_nested() {
        let mut interp = Interpreter::new();
        let input = "{'outer': {'inner': 'value', 'num': 42}}";
        let parsed = interp.eval(input).unwrap();
        let output = parsed.to_string();

        // Output should have quotes
        assert!(output.contains("'outer'"));
        assert!(output.contains("'inner'"));
        assert!(output.contains("'value'"));

        // Should be able to parse back
        let reparsed = interp.eval(&output).unwrap();
        assert_eq!(parsed, reparsed);
    }

    #[test]
    fn test_dict_display_mixed_value_types() {
        let mut interp = Interpreter::new();
        let input = "{'str': 'hello', 'num': 42, 'float': 3.14}";
        let parsed = interp.eval(input).unwrap();
        let output = parsed.to_string();

        // String values should be quoted, numbers should not
        assert!(output.contains("'str': 'hello'"));
        assert!(output.contains("'num': 42"));
        assert!(output.contains("'float': 3.14"));

        let reparsed = interp.eval(&output).unwrap();
        assert_eq!(parsed, reparsed);
    }

    #[test]
    fn test_list_display_round_trip() {
        let mut interp = Interpreter::new();
        let input = "['a', 'b', 42, 3.14]";
        let parsed = interp.eval(input).unwrap();
        let output = parsed.to_string();

        // Should be able to parse back
        let reparsed = interp.eval(&output).unwrap();
        assert_eq!(parsed, reparsed);
    }

    #[test]
    fn test_dict_display_numeric_string_keys() {
        let mut interp = Interpreter::new();
        // Dict keys are always strings in pyisheval - always quoted
        let input = "{'1': 'one', '2': 'two', '3.5': 'three-point-five'}";
        let parsed = interp.eval(input).unwrap();
        let output = parsed.to_string();

        // All string keys should be quoted (even numeric-looking ones)
        assert!(output.contains("'1': 'one'"));
        assert!(output.contains("'2': 'two'"));
        assert!(output.contains("'3.5': 'three-point-five'"));

        let reparsed = interp.eval(&output).unwrap();
        assert_eq!(parsed, reparsed);
    }

    #[test]
    fn test_dict_display_round_trip_simple() {
        let mut interp = Interpreter::new();
        // Simple round-trip test without special characters
        let input = "{'name': 'test', 'count': 42}";
        let parsed = interp.eval(input).unwrap();
        let output = parsed.to_string();

        // Should be able to reparse the output
        let reparsed = interp.eval(&output).unwrap();
        assert_eq!(parsed, reparsed);

        // Output should contain quoted keys
        assert!(output.contains("'name'"));
        assert!(output.contains("'count'"));
    }

    #[test]
    fn test_str_on_string_literal() {
        use crate::Value;
        let mut interp = Interpreter::new();
        // str() should extract raw value without Display's quotes
        let result = interp.eval("str('36_11')").unwrap();
        if let Value::StringLit(s) = result {
            assert_eq!(s, "36_11"); // Underlying value has no extra quotes
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_str_on_variable() {
        use crate::Value;
        let mut interp = Interpreter::new();
        interp.eval("x = 'hello'").unwrap();
        let result = interp.eval("str(x)").unwrap();
        if let Value::StringLit(s) = result {
            assert_eq!(s, "hello");
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_str_on_number() {
        use crate::Value;
        let mut interp = Interpreter::new();
        let result = interp.eval("str(42.5)").unwrap();
        if let Value::StringLit(s) = result {
            assert_eq!(s, "42.5");
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_str_on_list() {
        use crate::Value;
        let mut interp = Interpreter::new();
        let result = interp.eval("str([1, 2, 3])").unwrap();
        if let Value::StringLit(s) = result {
            // List's Display output includes quotes for string elements
            assert_eq!(s, "[1, 2, 3]");
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_str_on_dict() {
        use crate::Value;
        let mut interp = Interpreter::new();
        let result = interp.eval("str({'a': 1})").unwrap();
        if let Value::StringLit(s) = result {
            // Dict's Display output includes quoted keys
            assert_eq!(s, "{'a': 1}");
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_str_empty() {
        use crate::Value;
        let mut interp = Interpreter::new();
        let result = interp.eval("str()").unwrap();
        if let Value::StringLit(s) = result {
            assert_eq!(s, "");
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_str_in_expression() {
        use crate::Value;
        let mut interp = Interpreter::new();
        interp.eval("family = '36_11'").unwrap();

        // str() should not add extra quotes that would interfere with string operations
        // (The xacro use case - str() should produce clean values for further processing)
        let result = interp.eval("str(family)").unwrap();
        assert_eq!(result, Value::StringLit("36_11".to_string()));
    }

    #[test]
    fn test_str_with_replace_method() {
        use crate::Value;
        let mut interp = Interpreter::new();
        interp.eval("family = '36_11'").unwrap();

        // The xacro use case: str(value).replace()
        let result = interp.eval("str(family).replace('_', '-')").unwrap();
        if let Value::StringLit(s) = result {
            assert_eq!(s, "36-11");  // Should work without quote pollution
        } else {
            panic!("Expected StringLit");
        }
    }

    #[test]
    fn test_lambda_equality_behavior() {
        let mut interp = Interpreter::new();

        // Direct lambda == lambda still throws TypeError (caught in eval_expr)
        let result = interp.eval("(lambda x: 1) == (lambda x: 1)");
        assert!(matches!(result, Err(EvalError::TypeError)));

        // TODO: Lambdas in collections always compare as false (trade-off, would require object id tracking)
        // Even though these are syntactically identical, we can't track identity
        assert_eq!(
            interp.eval("[lambda x: 1] == [lambda x: 1]").unwrap().to_string(),
            "0"
        );

        // Different lambdas in collections also return false
        assert_eq!(
            interp.eval("[lambda x: 1] == [lambda x: 2]").unwrap().to_string(),
            "0"
        );

        // Lists with mixed content: lambdas make them unequal
        assert_eq!(
            interp.eval("['a', lambda x: 1] == ['a', lambda x: 1]").unwrap().to_string(),
            "0"
        );
    }

    // ===== in / not in operator tests =====

    #[test]
    fn test_in_string_basic() {
        let mut interp = Interpreter::new();
        // Single character search
        assert_eq!(interp.eval("'x' in 'xyz'").unwrap(), Value::Number(1.0));
        // Substring search
        assert_eq!(interp.eval("'ab' in 'xabz'").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("'az' in 'baz'").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("'hello' in 'say hello world'").unwrap(), Value::Number(1.0));
        // Not found
        assert_eq!(interp.eval("'q' in 'xyz'").unwrap(), Value::Number(0.0));
        assert_eq!(interp.eval("'za' in 'baz'").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_in_string_empty() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("'' in 'anything'").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("'' in ''").unwrap(), Value::Number(1.0));
    }

    #[test]
    fn test_in_list() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("2 in [1, 2, 3]").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("4 in [1, 2, 3]").unwrap(), Value::Number(0.0));
        assert_eq!(interp.eval("'a' in ['a', 'b']").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("1 in []").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_in_tuple() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("2 in (1, 2, 3)").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("4 in (1, 2, 3)").unwrap(), Value::Number(0.0));
        assert_eq!(interp.eval("1 in ()").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_in_set() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("2 in {1, 2, 3}").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("4 in {1, 2, 3}").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_in_dict_keys() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("'a' in {'a': 1, 'b': 2}").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("'c' in {'a': 1, 'b': 2}").unwrap(), Value::Number(0.0));
        // Values are NOT checked, only keys
        assert_eq!(interp.eval("1 in {'a': 1}").unwrap(), Value::Number(0.0));
        assert_eq!(interp.eval("'a' in {}").unwrap(), Value::Number(0.0));
        // Number keys are converted to strings (consistent with extract_key for indexing)
        assert_eq!(interp.eval("2 in {'2': 'a'}").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("3 in {'2': 'a'}").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_not_in_string() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("'q' not in 'xyz'").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("'x' not in 'xyz'").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_not_in_list() {
        let mut interp = Interpreter::new();
        assert_eq!(interp.eval("4 not in [1, 2, 3]").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("2 not in [1, 2, 3]").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_in_type_error_non_iterable() {
        let mut interp = Interpreter::new();
        // Cannot check membership in a number (haystack not iterable)
        assert!(matches!(
            interp.eval("2 in 42"),
            Err(EvalError::TypeError)
        ));
        assert!(matches!(
            interp.eval("'x' in 123"),
            Err(EvalError::TypeError)
        ));
        assert!(matches!(
            interp.eval("3 in 7"),
            Err(EvalError::TypeError)
        ));
        assert!(matches!(
            interp.eval("3 in 9"),
            Err(EvalError::TypeError)
        ));
    }

    #[test]
    fn test_in_string_requires_string_needle() {
        let mut interp = Interpreter::new();
        // Python: 'in <string>' requires string as left operand
        assert!(matches!(
            interp.eval("2 in 'foo'"),
            Err(EvalError::TypeError)
        ));
        assert!(matches!(
            interp.eval("2 in '2'"),
            Err(EvalError::TypeError)
        ));
        // String needle is ok
        assert_eq!(interp.eval("'2' in '123'").unwrap(), Value::Number(1.0));
        assert_eq!(interp.eval("'x' in 'xyz'").unwrap(), Value::Number(1.0));
    }

    #[test]
    fn test_in_type_mismatch_returns_false() {
        let mut interp = Interpreter::new();
        // String not in list of numbers -> false
        assert_eq!(interp.eval("'x' in [1, 2, 3]").unwrap(), Value::Number(0.0));
        // Number not in list of strings -> false
        assert_eq!(interp.eval("1 in ['a', 'b']").unwrap(), Value::Number(0.0));
    }

    #[test]
    fn test_in_with_variables() {
        let mut interp = Interpreter::new();
        interp.eval("x = 'hello'").unwrap();
        interp.eval("items = ['hello', 'world']").unwrap();
        assert_eq!(interp.eval("x in items").unwrap(), Value::Number(1.0));
    }

    #[test]
    fn test_in_with_logical_operators() {
        let mut interp = Interpreter::new();
        // Precedence: in binds tighter than and/or
        assert_eq!(
            interp.eval("2 in [1, 2, 3] and 'x' in 'xyz'").unwrap(),
            Value::Number(1.0)
        );
        assert_eq!(
            interp.eval("4 in [1, 2, 3] or 'x' in 'xyz'").unwrap(),
            Value::Number(1.0)
        );
    }

    #[test]
    fn test_comprehension_still_works_with_in() {
        let mut interp = Interpreter::new();
        // Ensure comprehensions didn't break with "in" operator addition
        let result = interp.eval("[x for x in [1, 2, 3]]").unwrap();
        assert_eq!(result, Value::List(vec![
            Value::Number(1.0),
            Value::Number(2.0),
            Value::Number(3.0),
        ]));
    }

    #[test]
    fn test_identifier_boundary_not_split() {
        let mut interp = Interpreter::new();
        // Identifiers like "index" should not be split into "in" + "dex"
        interp.eval("index = 5").unwrap();
        interp.eval("integer = 10").unwrap();
        interp.eval("notice = 'warning'").unwrap();

        assert_eq!(interp.eval("index").unwrap(), Value::Number(5.0));
        assert_eq!(interp.eval("integer").unwrap(), Value::Number(10.0));
        assert_eq!(interp.eval("notice").unwrap(), Value::StringLit("warning".to_string()));
    }

    #[test]
    fn test_not_in_is_compound_operator() {
        let mut interp = Interpreter::new();
        // "not in" is a single compound operator, not "not (x in y)"
        // These should give same result but parsed differently

        // Compound: x not in y
        let result1 = interp.eval("3 not in [1, 2]").unwrap();

        // Prefix: not (x in y)
        let result2 = interp.eval("not 3 in [1, 2]").unwrap();

        // Both should be true (3 is not in [1, 2])
        assert_eq!(result1, Value::Number(1.0));
        assert_eq!(result2, Value::Number(1.0));
    }

    #[test]
    fn test_in_xacro_patterns() {
        let mut interp = Interpreter::new();
        // Real-world xacro patterns from corpus

        // Pattern: ${'-x' in snapto}
        interp.eval("snapto = '-x'").unwrap();
        assert_eq!(interp.eval("'-x' in snapto").unwrap(), Value::Number(1.0));

        // Pattern: ${var not in [list]}
        interp.eval("var = 'foo'").unwrap();
        assert_eq!(interp.eval("var not in ['bar', 'baz']").unwrap(), Value::Number(1.0));
    }
}
