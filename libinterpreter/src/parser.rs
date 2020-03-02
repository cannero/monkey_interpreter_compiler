use crate::{
    ast::{Expression, Ident, Program, Statement},
    lexer::Lexer,
    token::Token,
};

enum Precedence {
    Lowest,
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser<'_> {
    fn new(mut lexer: Lexer) -> Parser {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    fn parse(&mut self) -> Program {
        let mut program = Program::new();
        while self.current_token != Token::EndOfFile {
            match self.parse_statement() {
                Statement::None => (),
                stmt => program.push(stmt),
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Statement {
        let identifier = match self.peek_token.clone() {
            Token::Ident(id) => id,
            _ => {
                self.peek_error(Token::dummy_ident());
                return Statement::None;
            }
        };
        self.next_token();

        if !self.expect_peek(Token::Assign) {
            return Statement::None;
        }

        while self.current_token != Token::SemiColon {
            self.next_token();
        }
        Statement::Let(Ident(identifier))
    }

    fn parse_return_statement(&mut self) -> Statement {
        self.next_token();

        while self.current_token != Token::SemiColon {
            self.next_token();
        }
        Statement::Return
    }

    fn parse_expression_statement(&mut self) -> Statement {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }

        if let Some(expression) = expression {
            Statement::Expression(expression)
        } else {
            Statement::None
        }
    }

    fn parse_expression(&self, precedence: Precedence) -> Option<Expression> {
        match &self.current_token {
            Token::Ident(_) => self.parse_identifier(),
            _ => None,
        }
    }

    fn parse_identifier(&self) -> Option<Expression> {
        match self.current_token.clone() {
            Token::Ident(id) => Some(Expression::Ident(Ident(id))),
            _ => None,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            self.next_token();
            true
        } else {
            self.peek_error(token);
            false
        }
    }

    fn errors(&self) -> Vec<String> {
        self.errors.clone()
    }

    fn peek_error(&mut self, token: Token) {
        self.errors.push(format!(
            "expected next token to be {:?}, is {:?} (current {:?})",
            token, self.peek_token, self.current_token
        ));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! let_stmt {
        ($literal:expr) => {
            Statement::Let(Ident($literal.to_string()))
        };
    }

    macro_rules! ret_stmt {
        () => {
            Statement::Return
        };
    }

    macro_rules! expr_stmt {
        ($literal:expr) => {
            Statement::Expression(Expression::Ident(Ident($literal.to_string())))
        };
    }

    fn assert_no_errors(parser: Parser) {
        assert!(parser.errors().len() == 0, "{:?}", parser.errors());
    }

    fn assert_statements(expected: Vec<Statement>, input: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        let statements = program.statements();

        assert_eq!(statements.len(), expected.len());
        for (i, stmt) in expected.iter().enumerate() {
            assert_eq!(&statements[i], stmt);
        }
        assert_no_errors(parser);
    }

    #[test]
    fn test_let_statement() {
        let expected_statements = vec![let_stmt!("x"), let_stmt!("y"), let_stmt!("foobar")];
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_parser_errors() {
        let input = "
let x = 5;
let y: 10;
let foobar = 838383;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse();
        assert!(parser.errors().len() == 1);
    }

    #[test]
    fn test_return_statement() {
        let expected_statements = vec![ret_stmt!(), ret_stmt!()];
        let input = "
return 5;
return x+y;
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_expression_statement() {
        let expected_statements = vec![expr_stmt!("foobar")];
        let input = "foobar;";

        assert_statements(expected_statements, input);
    }
}
