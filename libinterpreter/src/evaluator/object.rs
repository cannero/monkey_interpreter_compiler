use crate::{
    evaluator::environment::Environment,
    parser::ast::{BlockStatement, Identifiers, Infix},
};
use std::{cell::RefCell, fmt, rc::Rc};
use EvalError::*;
use Object::*;

macro_rules! obj_int {
    ($val:expr) => {
        Object::Integer($val)
    };
}

macro_rules! obj_bool {
    ($val:expr) => {
        Object::Bool($val)
    };
}

#[derive(PartialEq, Debug, Clone)]
pub enum EvalError {
    TypeMismatch(Box<Object>, Infix, Box<Object>),
    UnknownOperator(Box<Object>, Infix, Box<Object>),
    PrefixMismatch(Box<Object>),
    IdentifierNotFound(String),
    NotAFunction(Box<Object>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(i128),
    Bool(bool),
    Null,
    Return(Box<Object>),
    Error(EvalError),
    Function {
        parameters: Identifiers,
        body: BlockStatement,
        env: Rc<RefCell<Environment>>,
    },
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Bool(false) | Null => false,
            _ => true,
        }
    }

    pub fn build_type_mismatch(infix: &Infix, left: Object, right: Object) -> Object {
        Error(TypeMismatch(Box::new(left), infix.clone(), Box::new(right)))
    }

    pub fn build_operator_unknown(infix: &Infix, left: bool, right: bool) -> Object {
        Error(UnknownOperator(
            Box::new(obj_bool!(left)),
            infix.clone(),
            Box::new(obj_bool!(right)),
        ))
    }

    pub fn build_prefix_mismatch(right: Object) -> Object {
        Error(PrefixMismatch(Box::new(right)))
    }

    pub fn build_identifier_not_found(id: &str) -> Object {
        Error(IdentifierNotFound(id.to_string()))
    }

    pub fn is_error(&self) -> bool {
        match self {
            Error(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Function {
                parameters,
                body,
                env,
            } => write!(
                f,
                "Function: {} parameters, {:?}, {}",
                parameters.len(),
                body,
                env.borrow()
            ),
            _ => write!(f, "{:?}", self),
        }
    }
}
