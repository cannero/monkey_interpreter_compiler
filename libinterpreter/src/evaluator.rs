#[macro_use]
mod object;
use crate::{
    evaluator::object::Object,
    parser::ast::{BlockStatement, Expression, Infix, Literal, Prefix, Program, Statement},
};

#[derive(Default)]
pub struct Evaluator {}

impl Evaluator {
    pub fn eval(&self, program: Program) -> Object {
        self.eval_statements(program.statements())
    }

    fn eval_statements(&self, statements: &[Statement]) -> Object {
        self.eval_statement(&statements[0])
    }

    fn eval_statement(&self, statement: &Statement) -> Object {
        match statement {
            Statement::Expression(expr) => self.eval_expression(expr),
            _ => Object::Null,
        }
    }

    fn eval_expression(&self, expression: &Expression) -> Object {
        match expression {
            Expression::Prefix(pre, exp) => {
                let right = self.eval_expression(exp);
                self.eval_prefix_expression(pre, right)
            }
            Expression::Infix(left_exp, op, right_exp) => {
                let left = self.eval_expression(left_exp);
                let right = self.eval_expression(right_exp);
                self.eval_infix_expression(op, left, right)
            }
            Expression::Literal(Literal::Integer(i)) => Object::Integer(*i as i128),
            Expression::Literal(Literal::Bool(b)) => Object::Bool(*b),
            Expression::If {
                condition,
                consequence,
                alternative,
            } => self.eval_if_expression(condition, consequence, alternative),
            _ => Object::Null,
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
                _ => Object::Null,
            },
        }
    }

    fn eval_infix_expression(&self, infix: &Infix, left: Object, right: Object) -> Object {
        match (left, right) {
            (Object::Integer(l), Object::Integer(r)) => self.eval_infix_integer(infix, l, r),
            (Object::Bool(l), Object::Bool(r)) => self.eval_infix_bool(infix, l, r),
            _ => Object::Null,
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
            _ => Object::Null,
        }
    }

    fn eval_if_expression(
        &self,
        cond_expression: &Expression,
        consequence: &[Statement],
        alternative: &Option<BlockStatement>,
    ) -> Object {
        let condition = self.eval_expression(cond_expression);

        if condition.is_truthy() {
            self.eval_statements(consequence)
        } else if let Some(alt_block) = alternative {
            self.eval_statements(alt_block)
        } else {
            Object::Null
        }
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

    fn assert_eval(inputs: Vec<(&str, Object)>) {
        for (input, expected) in inputs {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            let evaluator = Evaluator::default();

            let output = evaluator.eval(program);

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
}
