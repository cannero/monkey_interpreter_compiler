mod object;
use crate::{
    evaluator::object::Object,
    parser::ast::{BlockStatement, Expression, Literal, Program, Statement},
};

struct Evaluator {}

impl Evaluator {
    fn new() -> Self {
        Evaluator {}
    }

    fn eval(&self, program: Program) -> Object {
        self.eval_statements(program.statements())
    }

    fn eval_statements(&self, statements: &BlockStatement) -> Object {
        self.eval_statement(&statements[0])
    }

    fn eval_statement(&self, statement: &Statement) -> Object {
        match statement {
            Statement::Expression(Expression::Literal(Literal::Integer(i))) => Object::Integer(*i),
            _ => Object::Integer(0),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer::Lexer, parser::Parser};

    #[test]
    fn test_eval_integer() {
        let expected = vec![Object::Integer(5), Object::Integer(10)];
        let inputs = vec!["5", "10"];

        for (i, input) in inputs.iter().enumerate() {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse();
            let evaluator = Evaluator::new();

            let output = evaluator.eval(program);

            assert_eq!(output, expected[i]);
        }
    }
}
