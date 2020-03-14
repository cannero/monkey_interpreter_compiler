pub mod ast;

use crate::{
    lexer::token::Token,
    lexer::Lexer,
    parser::ast::{BlockStatement, Expression, Ident, Infix, Literal, Prefix, Program, Statement},
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
type StatementResult = Result<Statement, ()>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser<'_> {
    pub fn new(mut lexer: Lexer) -> Parser {
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        while self.current_token != Token::EndOfFile {
            match self.parse_statement() {
                Err(_) => (),
                Ok(stmt) => program.push(stmt),
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> StatementResult {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> StatementResult {
        let identifier = match &self.peek_token {
            Token::Ident(id) => id.clone(),
            _ => {
                self.peek_error(Token::dummy_ident());
                return Err(());
            }
        };
        self.next_token();

        if !self.expect_peek(Token::Assign) {
            return Err(());
        }
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }
        Ok(Statement::Let(Ident(identifier), expression))
    }

    fn parse_return_statement(&mut self) -> StatementResult {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }
        Ok(Statement::Return(expression))
    }

    fn parse_expression_statement(&mut self) -> StatementResult {
        let expression = self.parse_expression(Precedence::Lowest);

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }

        if let Ok(expression) = expression {
            Ok(Statement::Expression(expression))
        } else {
            Err(())
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> ExpressionResult {
        let mut left_expression = match &self.current_token {
            Token::Ident(id) => Expression::Ident(Ident(id.clone())),
            Token::Integer(i) => Expression::Literal(Literal::Integer(*i)),
            t @ Token::False | t @ Token::True => {
                Expression::Literal(Literal::Bool(t == &Token::True))
            }
            Token::LParen => self.parse_grouped_expression()?,
            Token::Bang | Token::Minus => self.parse_prefix()?,
            Token::If => self.parse_if_expression()?,
            Token::Function => self.parse_fn_expression()?,
            _ => return self.expression_error("expression"),
        };

        while self.peek_token != Token::SemiColon && precedence < self.peek_precedence() {
            let expression = match self.peek_token {
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Equal
                | Token::NotEqual
                | Token::GreaterThan
                | Token::LessThan => {
                    self.next_token();
                    self.parse_infix(left_expression.clone())?
                }
                Token::LParen => {
                    self.next_token();
                    self.parse_call_expression(left_expression.clone())?
                }
                _ => self.expression_error("unknown left expression")?,
            };
            left_expression = expression;
        }
        Ok(left_expression)
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

    fn parse_grouped_expression(&mut self) -> ExpressionResult {
        self.next_token();

        if let Ok(expression) = self.parse_expression(Precedence::Lowest) {
            if !self.expect_peek(Token::RParen) {
                self.expression_error("missing RParen grouped expression")
            } else {
                Ok(expression)
            }
        } else {
            self.expression_error("grouped expression")
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

    fn parse_if_expression(&mut self) -> ExpressionResult {
        if !self.expect_peek(Token::LParen) {
            return self.expression_error("if lparen");
        }
        self.next_token();
        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);

        if !self.expect_peek(Token::RParen) {
            return self.expression_error("if rparen");
        }
        if !self.expect_peek(Token::LBrace) {
            return self.expression_error("if lbrace");
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token == Token::Else {
            self.next_token();
            if !self.expect_peek(Token::LBrace) {
                return self.expression_error("else lbrace");
            }
            Some(self.parse_block_statement())
        } else {
            None
        };

        Ok(Expression::If {
            condition,
            consequence,
            alternative,
        })
    }

    fn parse_fn_expression(&mut self) -> ExpressionResult {
        if !self.expect_peek(Token::LParen) {
            return self.expression_error("fn lparen");
        }

        let parameters = self.parse_comma_separated(&Self::parse_fn_parameter);

        if !self.expect_peek(Token::LBrace) {
            return self.expression_error("fn lbrace");
        }

        let body = self.parse_block_statement();

        Ok(Expression::Function { parameters, body })
    }

    fn parse_fn_parameter(&mut self) -> Result<Ident, ()> {
        match &self.current_token {
            Token::Ident(id) => Ok(Ident(id.clone())),
            _ => Err(()),
        }
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        self.next_token();
        let mut result = vec![];
        while self.current_token != Token::RBrace && self.current_token != Token::EndOfFile {
            match self.parse_statement() {
                Err(_) => (),
                Ok(stmt) => result.push(stmt),
            }
            self.next_token();
        }
        result
    }

    fn parse_call_expression(&mut self, function: Expression) -> ExpressionResult {
        Ok(Expression::Call {
            function: Box::new(function),
            arguments: self.parse_comma_separated(&Self::parse_call_argument),
        })
    }

    fn parse_comma_separated<T, F>(&mut self, parse_fn: &F) -> Vec<T>
    where
        F: Fn(&mut Self) -> Result<T, ()>,
    {
        if self.peek_token == Token::RParen {
            self.next_token();
            return vec![];
        }

        let mut values = vec![];
        loop {
            self.next_token();
            match parse_fn(self) {
                Ok(value) => values.push(value),
                _ => (),
            }
            if self.peek_token != Token::Comma {
                break;
            }
            self.next_token();
        }

        if !self.expect_peek(Token::RParen) {
            vec![]
        } else {
            values
        }
    }

    fn parse_call_argument(&mut self) -> ExpressionResult {
        match self.parse_expression(Precedence::Lowest) {
            Ok(expression) => Ok(expression),
            _ => Err(()),
        }
    }

    fn next_token(&mut self) {
        self.current_token = std::mem::replace(&mut self.peek_token, self.lexer.next_token());
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

    pub fn errors(&self) -> Vec<String> {
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

    macro_rules! ident {
        ($literal:expr) => {
            Ident($literal.to_string())
        };
    }

    macro_rules! let_stmt {
        ($literal:expr, $expr:expr) => {
            Statement::Let(ident!($literal), $expr)
        };
    }

    macro_rules! ret_stmt {
        ($expr:expr) => {
            Statement::Return($expr)
        };
    }

    macro_rules! ident_expr {
        ($literal:expr) => {
            Expression::Ident(ident!($literal))
        };
    }

    macro_rules! boolean_expr {
        ($bool:expr) => {
            Expression::Literal(Literal::Bool($bool))
        };
    }

    macro_rules! integer_expr {
        ($integer:expr) => {
            Expression::Literal(Literal::Integer($integer))
        };
    }

    macro_rules! infix_expr {
        ($expr_left:expr, $infix:expr, $expr_right:expr) => {
            Expression::Infix(Box::new($expr_left), $infix, Box::new($expr_right))
        };
    }

    macro_rules! if_expr {
        ($condition:expr, $consequence:expr, $alternative:expr) => {
            Expression::If {
                condition: Box::new($condition),
                consequence: block_stmt!($consequence),
                alternative: if let Some(alt) = $alternative {
                    Some(block_stmt!(alt))
                } else {
                    None
                },
            }
        };
    }

    macro_rules! fn_expr {
        ($parameters:expr, $($body:expr),*) => {
            Expression::Function {
                parameters: $parameters,
                body: block_stmt!($($body),*),
            }
        };
    }

    macro_rules! call_expr {
        ($function:expr, $($argument:expr),*) => {
            Expression::Call {
                function: Box::new($function),
                arguments: vec![$($argument),*],
            }
        };
    }

    macro_rules! block_stmt {
        ($($expr:expr),*) => {
            vec![$(Statement::Expression($expr)),*]
        };
    }

    macro_rules! ident_expr_stmt {
        ($literal:expr) => {
            Statement::Expression(ident_expr!($literal))
        };
    }

    macro_rules! integer_expr_stmt {
        ($literal:expr) => {
            Statement::Expression(integer_expr!($literal))
        };
    }

    macro_rules! boolean_expr_stmt {
        ($bool:expr) => {
            Statement::Expression(boolean_expr!($bool))
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

    macro_rules! if_expr_stmt {
        ($condition:expr, $consequence:expr, $alternative:expr) => {
            Statement::Expression(if_expr!($condition, $consequence, $alternative))
        };
    }

    macro_rules! fn_expr_stmt {
        ($parameters:expr, $($body:expr),*) => {
            Statement::Expression(fn_expr!($parameters, $($body),*))
        };
    }

    fn assert_no_errors(parser: Parser) {
        assert!(parser.errors().len() == 0, "{:?}", parser.errors());
    }

    fn assert_statements(expected: BlockStatement, input: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        let statements = program.statements();

        assert_no_errors(parser);
        assert_eq!(statements.len(), expected.len());
        for (i, stmt) in expected.iter().enumerate() {
            assert_eq!(&statements[i], stmt);
        }
    }

    #[test]
    fn test_let_statement() {
        let expected_statements = vec![
            let_stmt!("x", integer_expr!(5)),
            let_stmt!(
                "y",
                infix_expr!(integer_expr!(3), Infix::Asterisk, ident_expr!("a"))
            ),
        ];
        let input = "
let x = 5;
let y = 3 * a;
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
        assert!(parser.errors().len() == 2, parser.errors().join(";"));
    }

    #[test]
    fn test_return_statement() {
        let expected_statements = vec![
            ret_stmt!(integer_expr!(5)),
            ret_stmt!(infix_expr!(ident_expr!("x"), Infix::Plus, ident_expr!("y"))),
        ];
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
            prefix_expr_stmt!(Prefix::Bang, Box::new(integer_expr!(5))),
            prefix_expr_stmt!(Prefix::Minus, Box::new(integer_expr!(15))),
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
    fn test_expression_with_boolean_statement() {
        let expected_statements = vec![
            boolean_expr_stmt!(true),
            infix_expr_stmt!(
                infix_expr!(integer_expr!(3), Infix::LessThan, integer_expr!(5)),
                Infix::NotEqual,
                boolean_expr!(false)
            ),
        ];
        let input = "true;
3 < 5 != false";

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

    #[test]
    fn test_grouped_expression_parsing() {
        let expected_statements = vec![
            infix_expr_stmt!(
                ident_expr!("a"),
                Infix::Minus,
                infix_expr!(ident_expr!("b"), Infix::Slash, ident_expr!("c"))
            ),
            infix_expr_stmt!(
                infix_expr!(ident_expr!("a"), Infix::Minus, ident_expr!("b")),
                Infix::Slash,
                ident_expr!("c")
            ),
        ];

        let input = "
a - b / c;
(a - b) / c;
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_if_expression() {
        let expected_statements = vec![if_expr_stmt!(
            infix_expr!(ident_expr!("x"), Infix::LessThan, ident_expr!("y")),
            ident_expr!("x"),
            None
        )];

        let input = "
if (x < y){
    x
}
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_if_else_expression() {
        let expected_statements = vec![if_expr_stmt!(
            infix_expr!(ident_expr!("x"), Infix::LessThan, ident_expr!("y")),
            ident_expr!("x"),
            Some(ident_expr!("y"))
        )];

        let input = "
if (x < y){
    x
} else {
    y
}
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_fn_expression() {
        let expected_statements = vec![
            fn_expr_stmt!(vec![], ident_expr!("y"), ident_expr!("x")),
            fn_expr_stmt!(
                vec![ident!("x"), ident!("y")],
                infix_expr!(ident_expr!("x"), Infix::Plus, ident_expr!("y"))
            ),
        ];

        let input = "
fn (){
    y;
    x;
}
fn (x,y){
    x+y;
}
";

        assert_statements(expected_statements, input);
    }

    #[test]
    fn test_call_expression() {
        let expected_statements = vec![
            Statement::Expression(call_expr!(
                ident_expr!("add"),
                integer_expr!(1),
                integer_expr!(2)
            )),
            Statement::Expression(call_expr!(
                ident_expr!("add"),
                integer_expr!(1),
                infix_expr!(integer_expr!(2), Infix::Asterisk, integer_expr!(3)),
                call_expr!(
                    ident_expr!("subtract"),
                    infix_expr!(integer_expr!(4), Infix::Slash, ident_expr!("a"))
                )
            )),
        ];

        let input = "
add(1, 2);
add(1, 2 * 3, subtract(4 / a));
";
        assert_statements(expected_statements, input);
    }
}
