use core::fmt;

use crate::error::{Error, Result};
use crate::expr::Expr;
use crate::parser::Parser;
use crate::scanner::{Token, TokenType};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    Expression(Expr),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print(Expr),
    ReplExpression(Expr),
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Block(Vec<Stmt>),
    Function(Function),
    Return {
        keyword: Token,
        value: Option<Expr>,
    },
    Class {
        name: Token,
        methods: Vec<Function>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionType {
    Function,
    Method,
}

/// Statements parsing.
impl Parser<'_> {
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
        if self.matches(&[TokenType::Class]) {
            self.class()
        } else if self.matches(&[TokenType::Fun]) {
            self.function(FunctionType::Function)
        } else if self.matches(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class(&mut self) -> Result<Stmt> {
        let name = self
            .consume(TokenType::Identifier, "Expected class name.")?
            .clone();
        self.consume(TokenType::LeftBrace, "Expected '{' before class body.")?;
        let mut methods = Vec::new();
        while !self.check(&TokenType::RightBrace) && !self.is_eof() {
            match self.function(FunctionType::Method)? {
                Stmt::Function(func) => methods.push(func),
                _ => unreachable!(),
            }
        }
        self.consume(TokenType::RightBrace, "Expected '}' after class body.")?;
        Ok(Stmt::Class { name, methods })
    }

    fn function(&mut self, kind: FunctionType) -> Result<Stmt> {
        let name = self
            .consume(TokenType::Identifier, &format!("Expected {kind} name."))?
            .clone();
        self.consume(
            TokenType::LeftParen,
            &format!("Expected '(' after {kind} name."),
        )?;
        let mut params = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                if params.len() >= 255 {
                    self.lox.report(Error::syntax_err(
                        self.peek(),
                        "Can't have more than 255 parameters.",
                    ));
                }
                params.push(
                    self.consume(TokenType::Identifier, "Expected parameter name.")?
                        .clone(),
                );
                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expected ')' after parameters.")?;
        self.consume(
            TokenType::LeftBrace,
            &format!("Expected '{{' before {kind} body."),
        )?;
        let body = self.block()?;
        Ok(Stmt::Function(Function { name, params, body }))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        self.consume(TokenType::Identifier, "Expected variable name.")?;
        let name = self.previous().clone();
        let initializer = match self.matches(&[TokenType::Equal]) {
            true => Some(self.expression()?),
            false => None,
        };
        self.consume(
            TokenType::Semicolon,
            "Expected ';' after variable declaration.",
        )?;
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.matches(&[TokenType::For]) {
            self.for_statement()
        } else if self.matches(&[TokenType::If]) {
            self.if_statement()
        } else if self.matches(&[TokenType::Print]) {
            self.print_statement()
        } else if self.matches(&[TokenType::Return]) {
            self.return_statement()
        } else if self.matches(&[TokenType::While]) {
            self.while_statement()
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
        self.consume(TokenType::RightBrace, "Expected '}' after block.")?;
        Ok(statements)
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '(' after 'for'.")?;
        let initializer = if self.matches(&[TokenType::Semicolon]) {
            None
        } else if self.matches(&[TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        let condition = match self.check(&TokenType::Semicolon) {
            true => Expr::Literal(Value::Boolean(true)),
            false => self.expression()?,
        };
        self.consume(TokenType::Semicolon, "Expected ';' after loop condition.")?;
        let increment = match self.check(&TokenType::RightParen) {
            true => None,
            false => Some(self.expression()?),
        };
        self.consume(TokenType::RightParen, "Expected ')' after for clause.")?;
        let mut body = self.statement()?;
        if let Some(expr) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(expr)]);
        }
        body = Stmt::While {
            condition,
            body: Box::new(body),
        };
        if let Some(stmt) = initializer {
            body = Stmt::Block(vec![stmt, body]);
        }
        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after if condition.")?;
        let then_branch = Box::new(self.statement()?);
        let else_branch = if self.matches(&[TokenType::Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expected ';' after value.")?;
        Ok(Stmt::Print(expr))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenType::LeftParen, "Expected '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expected ')' after condition.")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While { condition, body })
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        if self.lox.in_repl() && self.matches(&[TokenType::Eof]) {
            return Ok(Stmt::ReplExpression(expr));
        }
        self.consume(TokenType::Semicolon, "Expected ';' after expression.")?;
        Ok(Stmt::Expression(expr))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous().clone();
        let value = match self.check(&TokenType::Semicolon) {
            true => None,
            false => Some(self.expression()?),
        };
        self.consume(TokenType::Semicolon, "Expected ';' after return value.")?;
        Ok(Stmt::Return { keyword, value })
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionType::Function => write!(f, "function"),
            FunctionType::Method => write!(f, "method"),
        }
    }
}
