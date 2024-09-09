use std::fmt;

use crate::error::{Error, Result};
use crate::interpreter::Literal;
use crate::scanner::{Token, TokenType};
use crate::Lox;

#[derive(Debug, PartialEq)]
pub struct Parser<'a> {
    lox: &'a mut Lox,
    pub(crate) tokens: Vec<Token>,
    current: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable(Token),
    Grouping(Box<Expr>),
    Call {
        callee: Box<Expr>,
        closing_paren: Token,
        arguments: Vec<Expr>,
    },
    Unary {
        op: Token,
        expr: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: Token,
        right: Box<Expr>,
    },
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum FunctionType {
    Function,
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
        if self.matches(&[TokenType::Fun]) {
            self.function(FunctionType::Function)
        } else if self.matches(&[TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
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
            true => Expr::Literal(Literal::Boolean(true)),
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

/// Expression parsing.
impl Parser<'_> {
    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;
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

    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;
        while self.matches(&[TokenType::Or]) {
            let op = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;
        while self.matches(&[TokenType::And]) {
            let op = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            };
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
            self.call()?
        })
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;
        loop {
            if self.matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, expr: Expr) -> Result<Expr> {
        let mut arguments = vec![];
        if !self.check(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    // TODO: this reports the same error for each argument after the 255'th.
                    self.lox.report(Error::syntax_err(
                        self.peek(),
                        "Can't have more than 255 arguments.",
                    ));
                }
                arguments.push(self.expression()?);
                if !self.matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }
        let closing_paren = self
            .consume(TokenType::RightParen, "Expected ')' after arguments.")?
            .clone();
        Ok(Expr::Call {
            callee: Box::new(expr),
            closing_paren,
            arguments,
        })
    }

    fn primary(&mut self) -> Result<Expr> {
        use TokenType::*;
        Ok(match self.advance().token_type {
            False | True | Nil | String | Number => Expr::Literal(self.previous().clone().into()),
            LeftParen => {
                let expr = self.expression()?;
                self.consume(RightParen, "Expected ')' after expression.")?;
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

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token> {
        if self.check(&token_type) {
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
                self.peek().token_type,
                Class | Fun | Var | For | If | While | Print | Return
            ) {
                return;
            }
            self.advance();
        }
    }
}

impl fmt::Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FunctionType::Function => write!(f, "function"),
        }
    }
}
