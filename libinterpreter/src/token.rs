#[derive(Debug, PartialEq)]
pub enum Token {
    Illegal(char),
    EndOfFile,
    // identifiers/literals
    Ident(String),
    Integer(u64),
    // operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    //delimiters
    Comma,
    SemiColon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    //keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}
