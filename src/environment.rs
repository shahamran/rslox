use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, Result};
use crate::interpreter::Literal;
use crate::lex::Token;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment {
    parent: Option<Rc<Environment>>,
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: Literal) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &Token) -> Result<&Literal> {
        match self.values.get(&name.lexeme) {
            Some(v) => Ok(v),
            None => Err(Error::runtime_err(name, "Undefined variable.")),
        }
    }

    pub fn assign(&mut self, name: &Token, value: &Literal) -> Result<()> {
        match self.values.get_mut(&name.lexeme) {
            Some(v) => *v = value.clone(),
            None => return Err(Error::runtime_err(name, "Undefined variable.")),
        }
        Ok(())
    }
}
