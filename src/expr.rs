use crate::error::{Error, Result};
use crate::parser::Parser;
use crate::scanner::{Token, TokenType};
use crate::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Value),
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
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    This(Token),
}

/// Expression parsing.
impl Parser<'_> {
    pub fn expression(&mut self) -> Result<Expr> {
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
            } else if let Expr::Get { object, name } = expr {
                return Ok(Expr::Set {
                    object,
                    name,
                    value: Box::new(value),
                });
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
            } else if self.matches(&[TokenType::Dot]) {
                expr = Expr::Get {
                    name: self
                        .consume(TokenType::Identifier, "Expected property name after '.'.")?
                        .clone(),
                    object: Box::new(expr),
                };
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
            This => Expr::This(self.previous().clone()),
            Super => {
                let keyword = self.previous().clone();
                self.consume(TokenType::Dot, "Expeted '.' after 'super'.")?;
                let method = self
                    .consume(TokenType::Identifier, "Expected superclass method name.")?
                    .clone();
                Expr::Super { keyword, method }
            }
            Identifier => Expr::Variable(self.previous().clone()),
            _ => {
                return Err(Error::syntax_err(self.previous(), "Expected expression."));
            }
        })
    }
}
