use std::collections::HashMap;

use crate::error::{Error, Result};
use crate::interpreter::Literal;
use crate::scanner::Token;

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

    pub fn get(&self, depth: Option<usize>, name: &Token) -> Result<&Literal> {
        let depth = depth.unwrap_or(self.0.len() - 1);
        assert!(depth < self.0.len());
        let i = self.0.len() - depth - 1;
        match self.0[i].get(&name.lexeme) {
            Some(Literal::Undefined) | None => Err(Error::runtime_err(name, "Undefined variable.")),
            Some(lit) => Ok(lit),
        }
    }

    pub fn assign(&mut self, depth: Option<usize>, name: &Token, value: &Literal) -> Result<()> {
        let depth = depth.unwrap_or(self.0.len() - 1);
        assert!(depth < self.0.len());
        let i = self.0.len() - depth - 1;
        match self.0[i].get_mut(&name.lexeme) {
            Some(v) => *v = value.clone(),
            None => return Err(Error::runtime_err(name, "Undefined variable.")),
        }
        Ok(())
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self(vec![Default::default()])
    }
}
