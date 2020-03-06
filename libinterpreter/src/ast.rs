pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }

    pub fn statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn push(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    None,
    Let(Ident),
    Return,
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Ident(Ident),
    Integer(u64),
    Prefix(Prefix, Box<Expression>),
    Infix(Box<Expression>, Infix, Box<Expression>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    Minus,
    Bang,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
}
