use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, Result};
use crate::scanner::Token;
use crate::value::Value;
use crate::{wrap, SharedRef};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment {
    parent: Option<EnvironmentRef>,
    values: HashMap<String, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnvironmentRef(SharedRef<Environment>);

impl EnvironmentRef {
    pub fn new() -> Self {
        Self(wrap(Environment::default()))
    }

    pub fn with_parent(parent: EnvironmentRef) -> Self {
        Self(wrap(Environment {
            parent: Some(parent),
            ..Default::default()
        }))
    }

    pub fn borrow(&self) -> std::cell::Ref<Environment> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> std::cell::RefMut<Environment> {
        self.0.borrow_mut()
    }

    pub fn get_at(&self, depth: usize, name: &Token) -> Value {
        self.ancestor(depth)
            .0
            .borrow()
            .try_get(name)
            .expect("unresolved local")
    }

    pub fn assign_at(&self, depth: usize, name: &Token, value: &Value) {
        self.ancestor(depth).0.borrow_mut().assign(name, value);
    }

    fn ancestor(&self, depth: usize) -> Self {
        let mut env = Self(Rc::clone(&self.0));
        for _ in 0..depth {
            let parent = env
                .0
                .borrow()
                .parent
                .as_ref()
                .map(|p| Rc::clone(&p.0))
                .expect("too deep");
            env = Self(parent);
        }
        env
    }
}

impl Environment {
    /// Define a new local variable in the current environment.
    pub fn define(&mut self, name: String, value: Value) {
        self.values.insert(name, value);
    }

    pub fn assign(&mut self, name: &Token, value: &Value) {
        match self.values.get_mut(&name.lexeme) {
            Some(v) => *v = value.clone(),
            None => unreachable!(),
        }
    }

    pub fn try_get(&self, name: &Token) -> Result<Value> {
        self.values
            .get(&name.lexeme)
            .cloned()
            .ok_or_else(|| Error::runtime_err(name, "Undefined variable."))
    }
}
