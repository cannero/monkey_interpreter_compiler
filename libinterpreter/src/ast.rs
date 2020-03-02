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

#[derive(PartialEq, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug)]
pub enum Expression {
    Ident(Ident),
}
