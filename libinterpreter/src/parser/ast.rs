pub struct Program {
    statements: BlockStatement,
}

impl Program {
    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }

    pub fn push(&mut self, statement: Statement) {
        self.statements.push(statement);
    }
}

impl Default for Program {
    fn default() -> Self {
        Program {
            statements: Vec::new(),
        }
    }
}

pub type BlockStatement = Vec<Statement>;

#[derive(PartialEq, Debug, Clone)]
pub enum Statement {
    Let(Ident, Expression),
    Return(Expression),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident(pub String);

pub type Identifiers = Vec<Ident>;
pub type Arguments = Vec<Expression>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expression {
    Ident(Ident),
    Literal(Literal),
    Prefix(Prefix, Box<Expression>),
    Infix(Box<Expression>, Infix, Box<Expression>),
    If {
        condition: Box<Expression>,
        consequence: BlockStatement,
        alternative: Option<BlockStatement>,
    },
    Function {
        parameters: Identifiers,
        body: BlockStatement,
    },
    Call {
        function: Box<Expression>,
        arguments: Arguments,
    },
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

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Integer(u64),
    Bool(bool),
}
