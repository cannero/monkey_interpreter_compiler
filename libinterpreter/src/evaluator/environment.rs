use crate::evaluator::object::Object;
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

#[derive(Default, Debug, PartialEq)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new_with_outer(outer: &Rc<RefCell<Self>>) -> Self {
        Self {
            store: Default::default(),
            outer: Some(Rc::clone(outer)),
        }
    }

    pub fn get(&self, key: &str) -> Option<Object> {
        match self.store.get(key) {
            Some(obj) => Some(obj.clone()),
            None => match &self.outer {
                Some(parent) => parent.borrow().get(key),
                None => None,
            },
        }
    }

    pub fn insert(&mut self, key: String, value: &Object) {
        self.store.insert(key, value.clone());
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Environment: ")?;
        for (k, v) in &self.store {
            match v {
                Object::Function { .. } => write!(f, "{}: fn/", k)?,
                _ => write!(f, "{}: {}/", k, v)?,
            }
        }
        if let Some(outer) = &self.outer {
            writeln!(f, "outer: {}", outer.borrow())?;
        }
        Ok(())
    }
}
