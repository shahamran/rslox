use crate::{
    lex::{Token, TokenType},
    Context,
};

#[derive(Debug, PartialEq)]
pub struct Parser<'a> {
    ctx: &'a mut Context,
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
}

impl Expr {
    pub fn accept<V: Visitor>(&self, visitor: &V) -> V::Return {
        visitor.visit(self)
    }
}

pub trait Visitor {
    type Return;

    fn visit(&self, expr: &Expr) -> Self::Return;
}

impl<'a> Parser<'a> {
    pub fn new(ctx: &'a mut Context, tokens: Vec<Token>) -> Self {
        Self {
            ctx,
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Option<Expr> {
        self.expression().ok()
    }

    fn expression(&mut self) -> Result<Expr, ()> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ()> {
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

    fn comparison(&mut self) -> Result<Expr, ()> {
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

    fn term(&mut self) -> Result<Expr, ()> {
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

    fn factor(&mut self) -> Result<Expr, ()> {
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

    fn unary(&mut self) -> Result<Expr, ()> {
        Ok(if self.matches(&[TokenType::Bang, TokenType::Minus]) {
            Expr::Unary {
                op: self.previous().clone(),
                expr: Box::new(self.unary()?),
            }
        } else {
            self.primary()?
        })
    }

    fn primary(&mut self) -> Result<Expr, ()> {
        use TokenType::*;
        Ok(match &self.advance().token_type {
            False | True | Nil | String(_) | Number(_) => Expr::Literal(self.previous().clone()),
            LeftParen => {
                let expr = self.expression()?;
                self.consume(&RightParen, "Expected ')' after expression.")?;
                Expr::Grouping(Box::new(expr))
            }
            _ => {
                let t = self.previous();
                let (start, end) = (t.offset, t.offset + t.length);
                self.ctx.report_error(start, end, "Expected expression.");
                return Err(());
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
        if !self.is_eof() {
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
        &self.tokens[self.current - 1]
    }

    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&Token, ()> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let t = self.peek();
            let (start, end) = (t.offset, t.offset + t.length);
            self.ctx.report_error(start, end, message);
            Err(())
        }
    }
}
