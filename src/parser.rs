use crate::error::{Error, Result};
use crate::scanner::{Token, TokenType};
use crate::Lox;

#[derive(Debug, PartialEq)]
pub struct Parser<'a> {
    pub(crate) lox: &'a mut Lox,
    pub(crate) tokens: Vec<Token>,
    current: usize,
}

/// General impl.
impl<'a> Parser<'a> {
    pub fn new(lox: &'a mut Lox, tokens: Vec<Token>) -> Self {
        Self {
            lox,
            tokens,
            current: 0,
        }
    }

    pub(crate) fn matches(&mut self, ts: &[TokenType]) -> bool {
        for token_type in ts {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    pub(crate) fn advance(&mut self) -> &Token {
        if !self.is_eof() && self.current < self.tokens.len() {
            self.current += 1;
        }
        self.previous()
    }

    pub(crate) fn check(&self, token_type: &TokenType) -> bool {
        &self.peek().token_type == token_type
    }

    pub(crate) fn is_eof(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    pub(crate) fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub(crate) fn previous(&self) -> &Token {
        // TODO: not sure this is right.
        &self.tokens[self.current.saturating_sub(1)]
    }

    pub(crate) fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token> {
        if self.check(&token_type) {
            Ok(self.advance())
        } else {
            Err(Error::syntax_err(self.peek(), message))
        }
    }

    pub(crate) fn synchronize(&mut self) {
        use TokenType::*;
        self.advance();
        while !self.is_eof() {
            if self.previous().token_type == Semicolon {
                return;
            }
            if matches!(
                self.peek().token_type,
                Class | Fun | Var | For | If | While | Print | Return
            ) {
                return;
            }
            self.advance();
        }
    }
}
