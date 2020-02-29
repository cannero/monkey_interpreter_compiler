use crate::token::Token;
use std::{iter::Peekable, str::Chars};

fn is_identifier(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl Lexer<'_> {
    fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn next(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn read_string<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut res = String::new();
        loop {
            match self.peek() {
                Some(&ch) if f(ch) => res.push(ch),
                _ => return res,
            }
            self.next();
        }
    }

    fn tokenize_number(&mut self) -> Token {
        let num_str = self.read_string(|c| c.is_digit(10));
        Token::Integer(num_str.parse().unwrap())
    }

    fn tokenize_identifier(&mut self) -> Token {
        let identifier = self.read_string(|c| is_identifier(c));
        match identifier.as_ref() {
            "let" => Token::Let,
            "fn" => Token::Function,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "return" => Token::Return,
            _ => Token::Ident(identifier),
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.next();
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        match self.peek() {
            Some(&c) => match c {
                _ if is_identifier(c) => self.tokenize_identifier(),
                _ if c.is_digit(10) => self.tokenize_number(),
                _ => self.next_token_not_identifier_or_number(),
            },
            _ => self.next_token_not_identifier_or_number(),
        }
    }

    fn next_token_not_identifier_or_number(&mut self) -> Token {
        match self.next() {
            Some(c) => match c {
                '=' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }
                ';' => Token::SemiColon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '+' => Token::Plus,
                '-' => Token::Minus,
                '!' => {
                    if self.peek() == Some(&'=') {
                        self.next();
                        Token::NotEqual
                    } else {
                        Token::Bang
                    }
                }
                '/' => Token::Slash,
                '*' => Token::Asterisk,
                '<' => Token::LessThan,
                '>' => Token::GreaterThan,
                ',' => Token::Comma,
                ch @ _ => Token::Illegal(ch),
            },
            None => Token::EndOfFile,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_tokens(expected: Vec<Token>, input: &str) {
        let mut l = Lexer::new(input);
        for t in expected {
            let tok = l.next_token();
            assert_eq!(tok, t);
        }
    }

    #[test]
    fn test_lexer_delimiters() {
        let input = "=+(){},;";
        let expected_results = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::SemiColon,
            Token::EndOfFile,
        ];

        assert_tokens(expected_results, input);
    }

    #[test]
    fn test_small_program() {
        let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
";
        let expected_results = vec![
            //first line
            Token::Let,
            Token::Ident("five".to_string()),
            Token::Assign,
            Token::Integer(5),
            Token::SemiColon,
            //second line
            Token::Let,
            Token::Ident("ten".to_string()),
            Token::Assign,
            Token::Integer(10),
            Token::SemiColon,
            //third line
            Token::Let,
            Token::Ident("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_string()),
            Token::Comma,
            Token::Ident("y".to_string()),
            Token::RParen,
            Token::LBrace,
            //fourth line
            Token::Ident("x".to_string()),
            Token::Plus,
            Token::Ident("y".to_string()),
            Token::SemiColon,
            //fifth line
            Token::RBrace,
            Token::SemiColon,
            //sixth line
            Token::Let,
            Token::Ident("result".to_string()),
            Token::Assign,
            Token::Ident("add".to_string()),
            Token::LParen,
            Token::Ident("five".to_string()),
            Token::Comma,
            Token::Ident("ten".to_string()),
            Token::RParen,
            Token::SemiColon,
            Token::EndOfFile,
        ];
        assert_tokens(expected_results, input);
    }
}
