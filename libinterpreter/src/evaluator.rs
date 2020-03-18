#[macro_use]
mod object;
mod environment;

use crate::{
    evaluator::{
        environment::Environment,
        object::{EvalError, Object},
    },
    parser::ast::{
        Arguments, BlockStatement, Expression, Ident, Infix, Literal, Prefix, Program, Statement,
    },
};
use std::{cell::RefCell, mem, rc::Rc};

#[derive(Default)]
pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn eval(&mut self, program: Program) -> Object {
        let mut result = Object::Null;
        for statement in program.statements() {
            result = self.eval_statement(statement);
            match result {
                Object::Return(return_value) => {
                    result = *return_value;
                    break;
                }
                Object::Error(_) => break,
                _ => continue,
            }
        }
        result
    }

    fn eval_statements(&mut self, statements: &[Statement]) -> Object {
        let mut result = Object::Null;
        for statement in statements {
            result = self.eval_statement(statement);
            // bubble return statements/errors up the call stack
            match result {
                Object::Return(_) | Object::Error(_) => break,
                _ => (),
            }
        }
        result
    }

    fn eval_statement(&mut self, statement: &Statement) -> Object {
        match statement {
            Statement::Expression(expr) => self.eval_expression(expr),
            Statement::Return(expr) => {
                let ret_obj = self.eval_expression(expr);
                if ret_obj.is_error() {
                    return ret_obj;
                }
                Object::Return(Box::new(ret_obj))
            }
            Statement::Let(Ident(id), expr) => {
                let val = self.eval_expression(expr);
                if val.is_error() {
                    return val;
                }
                self.add_to_env(id, &val);
                val
            }
        }
    }

    fn eval_expression(&mut self, expression: &Expression) -> Object {
        match expression {
            Expression::Prefix(pre, exp) => {
                let right = self.eval_expression(exp);
                if right.is_error() {
                    return right;
                }
                self.eval_prefix_expression(pre, right)
            }
            Expression::Infix(left_exp, op, right_exp) => {
                let left = self.eval_expression(left_exp);
                if left.is_error() {
                    return left;
                }

                let right = self.eval_expression(right_exp);
                if right.is_error() {
                    return right;
                }

                self.eval_infix_expression(op, left, right)
            }
            Expression::Literal(Literal::Integer(i)) => Object::Integer(*i as i128),
            Expression::Literal(Literal::Bool(b)) => Object::Bool(*b),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expression(condition, consequence, alternative),
            Expression::Ident(Ident(id)) => {
                if let Some(val) = self.env.borrow().get(id) {
                    val
                } else {
                    Object::build_identifier_not_found(id)
                }
            }
            Expression::Function { parameters, body } => Object::Function {
                parameters: parameters.to_vec(),
                body: body.to_vec(),
                env: Rc::clone(&self.env),
            },
            Expression::Call {
                function,
                arguments,
            } => self.eval_call_expression(function, arguments),
        }
    }

    fn eval_prefix_expression(&self, prefix: &Prefix, right: Object) -> Object {
        match prefix {
            Prefix::Bang => match right {
                Object::Bool(b) => Object::Bool(!b),
                Object::Null => Object::Bool(true),
                _ => Object::Bool(false),
            },
            Prefix::Minus => match right {
                Object::Integer(i) => Object::Integer(-i),
                _ => Object::build_prefix_mismatch(right),
            },
        }
    }

    fn eval_infix_expression(&self, infix: &Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => self.eval_infix_integer(infix, l, r),
            (Object::Bool(l), Object::Bool(r)) => self.eval_infix_bool(infix, l, r),
            (o_left, o_right) => Object::build_type_mismatch(infix, o_left, o_right),
        }
    }

    fn eval_infix_integer(&self, infix: &Infix, left: i128, right: i128) -> Object {
        match infix {
            Infix::Plus => obj_int!(left + right),
            Infix::Minus => obj_int!(left - right),
            Infix::Asterisk => obj_int!(left * right),
            Infix::Slash => obj_int!(left / right),
            Infix::GreaterThan => obj_bool!(left > right),
            Infix::LessThan => obj_bool!(left < right),
            Infix::Equal => obj_bool!(left == right),
            Infix::NotEqual => obj_bool!(left != right),
        }
    }

    fn eval_infix_bool(&self, infix: &Infix, left: bool, right: bool) -> Object {
        match infix {
            Infix::Equal => obj_bool!(left == right),
            Infix::NotEqual => obj_bool!(left != right),
            _ => Object::build_operator_unknown(infix, left, right),
        }
    }

    fn eval_if_expression(
        &mut self,
        cond_expression: &Expression,
        consequence: &[Statement],
        alternative: &Option<BlockStatement>,
    ) -> Object {
        let condition = self.eval_expression(cond_expression);
        if condition.is_error() {
            return condition;
        }

        if condition.is_truthy() {
            self.eval_statements(consequence)
        } else if let Some(alt_block) = alternative {
            self.eval_statements(alt_block)
        } else {
            Object::Null
        }
    }

    fn eval_call_expression(&mut self, function: &Expression, arguments: &Arguments) -> Object {
        let function = self.eval_expression(function);
        if function.is_error() {
            return function;
        }

        let arguments = self.eval_expressions(arguments);
        if arguments.len() == 1 && arguments[0].is_error() {
            return arguments[0].clone();
        }

        self.apply_function(function, &arguments)
    }

    // TODO find more idiomatic way
    fn eval_expressions(&mut self, arguments: &Arguments) -> Vec<Object> {
        let mut result = vec![];
        for expr in arguments {
            let evaluated = self.eval_expression(expr);
            if evaluated.is_error() {
                return vec![evaluated];
            }
            result.push(evaluated);
        }
        result
    }

    fn apply_function(&mut self, function: Object, args: &[Object]) -> Object {
        let (parameters, body, fn_env) = match function {
            Object::Function {
                parameters,
                body,
                env,
            } => (parameters, body, env),
            obj => return Object::Error(EvalError::NotAFunction(Box::new(obj))),
        };

        let function_scope_env = Environment::new_with_outer(&fn_env);
        let cur_env = mem::replace(&mut self.env, Rc::new(RefCell::new(function_scope_env)));

        for (ident, arg) in parameters.iter().zip(args) {
            let Ident(id) = ident;
            self.add_to_env(id, &arg);
        }

        let evaluated = self.eval_statements(&body);
        self.env = cur_env;
        match evaluated {
            Object::Return(retval) => *retval,
            _ => evaluated,
        }
    }

    fn add_to_env(&self, key: &String, value: &Object) {
        self.env.borrow_mut().insert(key.to_string(), value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    macro_rules! obj_true {
        () => {
            obj_bool!(true)
        };
    }

    macro_rules! obj_false {
        () => {
            obj_bool!(false)
        };
    }

    macro_rules! obj_null {
        () => {
            Object::Null
        };
    }

    fn eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator::default();

        evaluator.eval(program)
    }

    fn assert_eval(inputs: Vec<(&str, Object)>) {
        for (input, expected) in inputs {
            let output = eval(input);
            assert_eq!(output, expected, "input is: {}", input);
        }
    }

    #[test]
    fn test_eval_integer() {
        let inputs = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-9", Object::Integer(-9)),
            ("5 + 5 + 5 - 3", Object::Integer(12)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("18/3 - 2", Object::Integer(4)),
        ];

        assert_eval(inputs);
    }

    #[test]
    fn test_eval_bool() {
        let inputs = vec![
            ("true", obj_true!()),
            ("false", obj_false!()),
            ("false == false", obj_true!()),
            ("true == false", obj_false!()),
            ("3 > 4", obj_false!()),
            ("5 < 500", obj_true!()),
            ("(1 > 2) == true", obj_false!()),
            ("(1 < 2) != false", obj_true!()),
        ];

        assert_eval(inputs);
    }

    #[test]
    fn test_bang_operator() {
        let inputs = vec![
            ("!true", obj_false!()),
            ("!false", obj_true!()),
            ("!!true", obj_true!()),
            ("!5", obj_false!()),
        ];

        assert_eval(inputs);
    }

    #[test]
    fn test_ifelse_expression() {
        let inputs = vec![
            ("if (false) { 10 }", obj_null!()),
            ("if (true) {10}", obj_int!(10)),
            ("if (1){20}", obj_int!(20)),
            ("if (1 < 2) { 10 }", obj_int!(10)),
            ("if (1 > 2) { 10 }", obj_null!()),
            ("if (1 > 2) { 10 } else { 20 }", obj_int!(20)),
            ("if (1 < 2) { 10 } else { 20 }", obj_int!(10)),
        ];

        assert_eval(inputs);
    }

    #[test]
    fn test_return_statements() {
        let inputs = vec![
            ("return 10;", obj_int!(10)),
            ("return 10; 9;", obj_int!(10)),
            ("return 2 * 5; 9;", obj_int!(10)),
            ("9; return 2 * 5; 9;", obj_int!(10)),
            (
                "
if (10 > 1) {
  if (10 > 1) {
    return 10;
  }

  return 1;
}
",
                obj_int!(10),
            ),
        ];

        assert_eval(inputs);
    }

    #[test]
    fn test_error_handling() {
        let inputs = vec![
            (
                "5 + true;",
                Object::build_type_mismatch(&Infix::Plus, obj_int!(5), obj_true!()),
            ),
            (
                "5; true + false; 5",
                Object::build_operator_unknown(&Infix::Plus, true, false),
            ),
            ("5; -true; 5", Object::build_prefix_mismatch(obj_true!())),
            ("foobar", Object::build_identifier_not_found("foobar")),
        ];
        assert_eval(inputs);
    }

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            ("let a = 5; a;", obj_int!(5)),
            ("let a = 5 * 5; a;", obj_int!(25)),
            ("let a = 5; let b = a; let c = a + b + 5; c;", obj_int!(15)),
        ];
        assert_eval(inputs);
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            ("let three = fn() { 3; }; three();", obj_int!(3)),
            ("let identity = fn(x) { x; }; identity(34);", obj_int!(34)),
            ("fn(x) { x * 3; }(8);", obj_int!(24)),
            ("let y = 4; fn(x) { x * y; }(2);", obj_int!(8)),
            (
                "
let newAdder = fn(x) {
  fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);",
                obj_int!(4),
            ),
            ("let add = fn(a,b){a+b;}; add(2, fn(){3;}());", obj_int!(5)),
        ];
        assert_eval(inputs);
    }
}
