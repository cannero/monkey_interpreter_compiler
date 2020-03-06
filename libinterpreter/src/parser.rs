use crate::{
    ast::{Expression, Ident, Infix, Prefix, Program, Statement},
    lexer::Lexer,
    token::Token,
};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn get_precedence(token: &Token) -> Precedence {
    match token {
        Token::Equal => Precedence::Equals,
        Token::NotEqual => Precedence::Equals,
        Token::LessThan => Precedence::LessGreater,
        Token::GreaterThan => Precedence::LessGreater,
        Token::Plus => Precedence::Sum,
        Token::Minus => Precedence::Sum,
        Token::Slash => Precedence::Product,
        Token::Asterisk => Precedence::Product,
        Token::LParen => Precedence::Call,
        _ => Precedence::Lowest,
    }
}

type ExpressionResult = Result<Expression, ()>;

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

        if let Ok(expression) = expression {
            Statement::Expression(expression)
        } else {
            Statement::None
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ExpressionResult {
        let mut left_expression = match self.current_token.clone() {
            Token::Ident(id) => Expression::Ident(Ident(id)),
            Token::Integer(i) => Expression::Integer(i),
            Token::Bang => self.parse_prefix()?,
            Token::Minus => self.parse_prefix()?,
            _ => return self.expression_error("left expression"),
        };

        while self.peek_token != Token::SemiColon && precedence < self.peek_precedence() {
            match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Equal
                | Token::NotEqual
                | Token::GreaterThan
                | Token::LessThan => {
                    self.next_token();
                    if let Ok(expression) = self.parse_infix(left_expression.clone()) {
                        left_expression = expression;
                    } else {
                        return Ok(left_expression);
                    }
                }
                _ => return Ok(left_expression),
            }
        }
        Ok(left_expression)
    }

    fn parse_identifier(&mut self) -> ExpressionResult {
        match self.current_token.clone() {
            Token::Ident(id) => Ok(Expression::Ident(Ident(id))),
            _ => self.expression_error("identifier"),
        }
    }

    fn parse_prefix(&mut self) -> ExpressionResult {
        let prefix = match self.current_token {
            Token::Bang => Prefix::Bang,
            Token::Minus => Prefix::Minus,
            _ => return self.expression_error("prefix operator"),
        };

        self.next_token();
        if let Ok(expression) = self.parse_expression(Precedence::Prefix) {
            Ok(Expression::Prefix(prefix, Box::new(expression)))
        } else {
            self.expression_error("prefix expression")
        }
    }

    fn parse_infix(&mut self, left: Expression) -> ExpressionResult {
        let precedence = self.cur_precedence();
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Slash,
            Token::Asterisk => Infix::Asterisk,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            Token::LessThan => Infix::LessThan,
            Token::GreaterThan => Infix::GreaterThan,
            _ => return self.expression_error("infix operator"),
        };

        self.next_token();
        if let Ok(expression) = self.parse_expression(precedence) {
            Ok(Expression::Infix(
                Box::new(left),
                infix,
                Box::new(expression),
            ))
        } else {
            self.expression_error("infix")
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

    fn cur_precedence(&self) -> Precedence {
        get_precedence(&self.current_token)
    }

    fn peek_precedence(&self) -> Precedence {
        get_precedence(&self.peek_token)
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

    fn expression_error<S: Into<String>>(&mut self, expr: S) -> ExpressionResult {
        self.errors.push(format!(
            "could not parse {}, current {:?}, peek {:?}",
            expr.into(),
            self.current_token,
            self.peek_token
        ));
        Err(())
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

    macro_rules! ident_expr {
        ($literal:expr) => {
            Expression::Ident(Ident($literal.to_string()))
        };
    }

    macro_rules! integer_expr {
        ($integer:expr) => {
            Expression::Integer($integer)
        };
    }

    macro_rules! infix_expr {
        ($expr_left:expr, $infix:expr, $expr_right:expr) => {
            Expression::Infix(Box::new($expr_left), $infix, Box::new($expr_right))
        };
    }

    macro_rules! ident_expr_stmt {
        ($literal:expr) => {
            Statement::Expression(ident_expr!($literal))
        };
    }

    macro_rules! integer_expr_stmt {
        ($literal:expr) => {
            Statement::Expression(Expression::Integer($literal))
        };
    }

    macro_rules! prefix_expr_stmt {
        ($prefix:expr, $expr:expr) => {
            Statement::Expression(Expression::Prefix($prefix, $expr))
        };
    }

    macro_rules! infix_expr_stmt {
        ($expr_left:expr, $infix:expr, $expr_right:expr) => {
            Statement::Expression(infix_expr!($expr_left, $infix, $expr_right))
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
        assert!(parser.errors().len() == 2, parser.errors().connect(":"));
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
    fn test_ident_expression_statement() {
        let expected_statements = vec![ident_expr_stmt!("foobar")];
        let input = "foobar;";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_integer_expression_statement() {
        let expected_statements = vec![integer_expr_stmt!(5)];
        let input = "5;";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_prefix_expression_statement() {
        let expected_statements = vec![
            prefix_expr_stmt!(Prefix::Bang, Box::new(Expression::Integer(5))),
            prefix_expr_stmt!(Prefix::Minus, Box::new(Expression::Integer(15))),
        ];
        let input = "
!5;
-15;
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_infix_expression_statement() {
        let expected_statements = vec![
            infix_expr_stmt!(integer_expr!(5), Infix::Plus, integer_expr!(5)),
            infix_expr_stmt!(integer_expr!(5), Infix::Minus, integer_expr!(5)),
        ];
        let input = "
5 + 5;
5 - 5;
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_multiple_operators_parsing() {
        let expected_statements = vec![
            infix_expr_stmt!(
                infix_expr!(
                    infix_expr!(
                        ident_expr!("a"),
                        Infix::Plus,
                        infix_expr!(ident_expr!("b"), Infix::Asterisk, ident_expr!("c"))
                    ),
                    Infix::Plus,
                    infix_expr!(ident_expr!("d"), Infix::Slash, ident_expr!("e"))
                ),
                Infix::Minus,
                ident_expr!("f")
            ),
            infix_expr_stmt!(
                infix_expr!(
                    integer_expr!(3),
                    Infix::Plus,
                    infix_expr!(integer_expr!(4), Infix::Asterisk, integer_expr!(5))
                ),
                Infix::Equal,
                infix_expr!(
                    infix_expr!(integer_expr!(3), Infix::Asterisk, integer_expr!(1)),
                    Infix::Plus,
                    infix_expr!(integer_expr!(4), Infix::Asterisk, integer_expr!(5))
                )
            ),
        ];
        let input = "
a + b * c + d / e - f;
3 + 4 * 5 == 3 * 1 + 4 * 5;
";

        assert_statements(expected_statements, input);
    }
}
