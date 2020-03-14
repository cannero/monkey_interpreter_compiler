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

#[derive(PartialEq, Debug)]
pub enum Object {
    Integer(i128),
    Bool(bool),
    Null,
}
use Object::*;

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            Bool(false) | Null => false,
            _ => true,
        }
    }
}
