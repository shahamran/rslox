use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::interpreter::Literal;
use crate::lex::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct Environment(Vec<HashMap<String, Literal>>);

impl Environment {
    pub fn new_block(&mut self) {
        self.0.push(Default::default());
    }

    pub fn remove_block(&mut self) {
        assert!(self.0.len() > 1, "cannot remove global scope");
        self.0.pop();
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.0.last_mut().unwrap().insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<&Literal> {
        for values in self.0.iter().rev() {
            if let Some(v) = values.get(&name.lexeme) {
                return Ok(v);
            }
        }
        Err(Error::runtime_err(name, "Undefined variable."))
    }

    pub fn assign(&mut self, name: &Token, value: &Literal) -> Result<()> {
        let v = self.get_mut(name)?;
        *v = value.clone();
        Ok(())
    }

    fn get_mut(&mut self, name: &Token) -> Result<&mut Literal> {
        for values in self.0.iter_mut().rev() {
            if let Some(v) = values.get_mut(&name.lexeme) {
                return Ok(v);
            }
        }
        Err(Error::runtime_err(name, "Undefined variable."))
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self(vec![Default::default()])
    }
}
