use std::cell::RefCell;

use crate::environment::Environment;
use crate::error::{Error, Result};
use crate::lex::{Token, TokenType};
use crate::parser::{Expr, Stmt, Visitor};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Interpreter {
    environment: RefCell<Environment>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Visitor<Expr> for Interpreter {
    type Return = Result<Literal>;

    fn visit(&self, expr: &Expr) -> Self::Return {
        match expr {
            Expr::Literal(t) => Ok(t.clone().into()),
            Expr::Unary { op, expr } => self.eval_unary(op, expr),
            Expr::Binary { left, op, right } => self.eval_binary(left, op, right),
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Variable(name) => self.environment.borrow().get(name).cloned(),
        }
    }
}

impl Interpreter {
    pub fn execute(&self, stmt: &Stmt) -> Result<()> {
        stmt.accept(self)
    }

    pub fn evaluate(&self, expr: &Expr) -> Result<Literal> {
        expr.accept(self)
    }

    fn eval_unary(&self, op: &Token, expr: &Expr) -> Result<Literal> {
        let right = self.evaluate(expr)?;
        match &op.token_type {
            TokenType::Minus => match right {
                Literal::Number(n) => Ok(Literal::Number(-n)),
                _ => Err(Error::runtime_err(op, "Operand must be a number.")),
            },
            TokenType::Bang => Ok(Literal::Boolean(!self.is_truthy(right))),
            _ => unreachable!("unary expr parsing error"),
        }
    }

    fn eval_binary(&self, left: &Expr, op: &Token, right: &Expr) -> Result<Literal> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        let num_operands = || Error::runtime_err(op, "Operands must be numbers.");
        match &op.token_type {
            TokenType::Minus => num_op(|a, b| a - b, left, right).ok_or_else(num_operands),
            TokenType::Slash => num_op(|a, b| a / b, left, right).ok_or_else(num_operands),
            TokenType::Star => num_op(|a, b| a * b, left, right).ok_or_else(num_operands),
            TokenType::Plus => match (left, right) {
                (Literal::Number(n1), Literal::Number(n2)) => Ok(Literal::Number(n1 + n2)),
                (Literal::String(s1), Literal::String(s2)) => Ok(Literal::String(s1 + &s2)),
                _ => Err(Error::runtime_err(
                    op,
                    "Operands must both be numbers or strings.",
                )),
            },
            TokenType::Greater => num_cmp(|a, b| a > b, left, right).ok_or_else(num_operands),
            TokenType::GreaterEqual => num_cmp(|a, b| a >= b, left, right).ok_or_else(num_operands),
            TokenType::Less => num_cmp(|a, b| a < b, left, right).ok_or_else(num_operands),
            TokenType::LessEqual => num_cmp(|a, b| a <= b, left, right).ok_or_else(num_operands),
            TokenType::EqualEqual => Ok(Literal::Boolean(left == right)),
            TokenType::BangEqual => Ok(Literal::Boolean(left != right)),
            _ => unreachable!("binary expr parsing error"),
        }
    }

    fn is_truthy(&self, literal: Literal) -> bool {
        match literal {
            Literal::Boolean(b) => b,
            Literal::Nil => false,
            _ => true,
        }
    }
}

impl Visitor<Stmt> for Interpreter {
    type Return = Result<()>;

    fn visit(&self, stmt: &Stmt) -> Self::Return {
        match stmt {
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Literal::Nil,
                };
                let name = name.lexeme.clone();
                self.environment.borrow_mut().define(name, value);
                Ok(())
            }
            Stmt::Expression(expr) => self.evaluate(expr).map(|_| ()),
            Stmt::Print(expr) => {
                let literal = self.evaluate(expr)?;
                println!("{}", literal.to_string());
                Ok(())
            }
        }
    }
}

fn num_op<F>(op: F, left: Literal, right: Literal) -> Option<Literal>
where
    F: FnOnce(f64, f64) -> f64,
{
    use Literal::Number;
    match (left, right) {
        (Number(left), Number(right)) => Some(Number(op(left, right))),
        _ => None,
    }
}

fn num_cmp<F>(op: F, left: Literal, right: Literal) -> Option<Literal>
where
    F: FnOnce(f64, f64) -> bool,
{
    use Literal::{Boolean, Number};
    match (left, right) {
        (Number(left), Number(right)) => Some(Boolean(op(left, right))),
        _ => None,
    }
}

impl From<Token> for Literal {
    fn from(token: Token) -> Self {
        use TokenType::*;
        match token.token_type {
            Nil => Self::Nil,
            False => Self::Boolean(false),
            True => Self::Boolean(true),
            Number(n) => Self::Number(n),
            String(s) => Self::String(s),
            _ => unreachable!("literal parsing error"),
        }
    }
}

impl ToString for Literal {
    fn to_string(&self) -> String {
        match self {
            Literal::Nil => "nil".to_string(),
            Literal::Boolean(b) => b.to_string(),
            Literal::Number(n) => n.to_string(),
            Literal::String(s) => s.clone(),
        }
    }
}
