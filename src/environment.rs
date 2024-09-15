use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, Result};
use crate::scanner::Token;
use crate::value::Value;
use crate::{wrap, SharedRef};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment {
    parent: Option<SharedRef<Environment>>,
    values: HashMap<String, Value>,
}

/// Create a new local scope with the given scope as parent.
pub fn new_block(current: SharedRef<Environment>) -> SharedRef<Environment> {
    wrap(Environment::new(Some(current)))
}

/// Remove the current local scope and return the parent scope.
pub fn remove_block(env: SharedRef<Environment>) -> SharedRef<Environment> {
    env.borrow().parent.clone().expect("removed global scope")
}

fn ancestor(env: SharedRef<Environment>, depth: usize) -> SharedRef<Environment> {
    let mut env = env;
    for _ in 0..depth {
        let parent = env.borrow().parent.as_ref().map(Rc::clone).unwrap();
        env = parent;
    }
    env
}

impl Environment {
    pub fn new(parent: Option<SharedRef<Environment>>) -> Self {
        Self {
            parent,
            values: HashMap::new(),
        }
    }

    /// Define a new local variable in the current environment.
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn try_get(&self, name: &Token) -> Result<Value> {
        self.values
            .get(&name.lexeme)
            .cloned()
            .ok_or_else(|| Error::runtime_err(name, "Undefined variable."))
    }

    /// Get a local variable at the given depth.
    pub fn get(&self, depth: usize, name: &Token) -> Value {
        match depth {
            0 => self
                .values
                .get(&name.lexeme)
                .expect("resolver should catch undefined local")
                .clone(),
            _ => {
                let env = ancestor(self.parent.as_ref().unwrap().clone(), depth - 1);
                let value = env
                    .borrow()
                    .values
                    .get(&name.lexeme)
                    .expect("resolver should catch undefined local")
                    .clone();
                value
            }
        }
    }

    /// Assign a local variable at the given depth.
    pub fn assign(&mut self, depth: usize, name: &Token, value: &Value) {
        match depth {
            0 => {
                *self
                    .values
                    .get_mut(&name.lexeme)
                    .expect("resolver should catch undefined local") = value.clone()
            }
            _ => {
                let env = ancestor(self.parent.as_ref().unwrap().clone(), depth - 1);
                *env.borrow_mut()
                    .values
                    .get_mut(&name.lexeme)
                    .expect("resolver should catch undefined local") = value.clone();
            }
        }
    }
}
