use crate::{
    ast::{Ident, Program, Statement},
    lexer::Lexer,
    token::Token,
};

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
}

impl Parser<'_> {
    fn new(mut lexer: Lexer) -> Parser {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();
        Parser {
            lexer: lexer,
            current_token: cur_token,
            peek_token: peek_token,
            errors: Vec::new(),
        }
    }

    fn parse(&mut self) -> Program {
        let mut program = Program::new();
        while self.current_token != Token::EndOfFile {
            match self.parse_statement() {
                Statement::None => (),
                stmt @ _ => program.push(stmt),
            }
            self.next_token();
        }
        program
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token {
            Token::Let => self.parse_let_statement(),
            _ => Statement::None,
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

    fn assert_no_errors(parser: Parser) {
        assert!(parser.errors().len() == 0, "{:?}", parser.errors());
    }

    #[test]
    fn test_let_statement() {
        let expected_statements = vec![let_stmt!("x"), let_stmt!("y"), let_stmt!("foobar")];
        let input = "
let x = 5;
let y = 10;
let foobar = 838383;
";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse();
        let statements = program.statements();
        assert_eq!(statements.len(), expected_statements.len());
        for (i, stmt) in expected_statements.iter().enumerate() {
            assert_eq!(&statements[i], stmt);
        }
        assert_no_errors(parser);
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

    //     #[test]
    //     fn test_return_statement() {
    //         let expected_statements = vec![let_stmt!("x"), let_stmt!("y"), let_stmt!("foobar")];
    //         let input = "
    // let x = 5;
    // let y = 10;
    // let hhu;
    // let foobar = 838383;
    // ";
    //         let lexer = Lexer::new(input);
    //         let mut parser = Parser::new(lexer);

    //         let program = parser.parse();
    //         let statements = program.statements();
    //         assert_eq!(statements.len(), expected_statements.len());
    //         for (i, stmt) in expected_statements.iter().enumerate() {
    //             assert_eq!(&statements[i], stmt);
    //         }
    //     }
}
