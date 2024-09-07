use crate::error::{Error, Result};
use crate::lex::{Token, TokenType};
use crate::Lox;

#[derive(Debug, PartialEq)]
pub struct Parser<'a> {
    lox: &'a mut Lox,
    tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Token),
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Variable(Token),
    Assign {
        name: Token,
        value: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    Expression(Expr),
    Print(Expr),
    Block(Vec<Stmt>),
}

impl Expr {
    pub fn accept<V: Visitor<Expr>>(&self, visitor: &V) -> V::Return {
        visitor.visit(self)
    }
}

impl Stmt {
    pub fn accept<V: Visitor<Stmt>>(&self, visitor: &V) -> V::Return {
        visitor.visit(self)
    }
}

pub trait Visitor<T> {
    type Return;

    fn visit(&self, value: &T) -> Self::Return;
}

impl<'a> Parser<'a> {
    pub fn new(lox: &'a mut Lox, tokens: Vec<Token>) -> Self {
        Self {
            lox,
            tokens,
            current: 0,
        }
    }

    pub fn parse(mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while !self.is_eof() {
            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.synchronize();
                    self.lox.report(err);
                }
            }
        }
        stmts
    }

    fn declaration(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        self.consume(&TokenType::Identifier, "Expected variable name.")?;
        let name = self.previous().clone();
        let initializer = match self.matches(&[TokenType::Equal]) {
            true => Some(self.expression()?),
            false => None,
        };
        self.consume(
            &TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenType::Print]) {
            self.print_statement()
        } else if self.matches(&[TokenType::LeftBrace]) {
            Ok(Stmt::Block(self.block()?))
        } else {
            self.expression_statement()
        }
    }

    fn block(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_eof() {
            statements.push(self.declaration()?);
        }
        self.consume(&TokenType::RightBrace, "Expected '}' after block.")?;
        Ok(statements)
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Print(expr))
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(&TokenType::Semicolon, "Expected ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.equality()?;
        if self.matches(&[TokenType::Equal]) {
            let equals = self.previous().clone();
            let value = self.assignment()?;
            if let Expr::Variable(t) = expr {
                let name = t;
                let value = Box::new(value);
                return Ok(Expr::Assign { name, value });
            } else {
                self.lox
                    .report(Error::runtime_err(&equals, "Invalid assignment target."));
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        use TokenType::{BangEqual, EqualEqual};
        let mut expr = self.comparison()?;
        while self.matches(&[BangEqual, EqualEqual]) {
            let op = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        use TokenType::{Greater, GreaterEqual, Less, LessEqual};
        let mut expr = self.term()?;
        while self.matches(&[Greater, GreaterEqual, Less, LessEqual]) {
            let op = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;
        while self.matches(&[TokenType::Minus, TokenType::Plus]) {
            let op = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;
        while self.matches(&[TokenType::Star, TokenType::Slash]) {
            let op = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        Ok(if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            Expr::Unary {
                op: self.previous().clone(),
                expr: Box::new(self.unary()?),
            }
        } else {
            self.primary()?
        })
    }

    fn primary(&mut self) -> Result<Expr> {
        use TokenType::*;
        Ok(match &self.advance().token_type {
            False | True | Nil | String(_) | Number(_) => Expr::Literal(self.previous().clone()),
            LeftParen => {
                let expr = self.expression()?;
                self.consume(&RightParen, "Expected ')' after expression.")?;
                Expr::Grouping(Box::new(expr))
            }
            Identifier => Expr::Variable(self.previous().clone()),
            _ => {
                return Err(Error::syntax_err(self.previous(), "Expected expression."));
            }
        })
    }

    fn matches(&mut self, ts: &[TokenType]) -> bool {
        for token_type in ts {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) -> &Token {
        if !self.is_eof() && self.current < self.tokens.len() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, token_type: &TokenType) -> bool {
        &self.peek().token_type == token_type
    }

    fn is_eof(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        // TODO: not sure this is right.
        &self.tokens[self.current.saturating_sub(1)]
    }

    fn consume(&mut self, token_type: &TokenType, message: &'static str) -> Result<&Token> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(Error::syntax_err(self.peek(), message))
        }
    }

    fn synchronize(&mut self) {
        use TokenType::*;
        self.advance();
        while !self.is_eof() {
            if self.previous().token_type == Semicolon {
                return;
            }
            if matches!(
                &self.peek().token_type,
                Class | Fun | Var | For | If | While | Print | Return
            ) {
                return;
            }
            self.advance();
        }
    }
}
