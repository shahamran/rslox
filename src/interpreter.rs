use crate::lex::{Token, TokenType};
use crate::parser::{Expr, Visitor};

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Visitor for Interpreter {
    type Return = Literal;

    fn visit(&self, expr: &Expr) -> Self::Return {
        match expr {
            Expr::Literal(t) => t.token_type.clone().try_into().expect("invalid literal"),
            Expr::Unary { op, expr } => self.eval_unary(op, expr),
            Expr::Binary { left, op, right } => self.eval_binary(left, op, right),
            Expr::Grouping(expr) => self.evaluate(expr),
        }
    }
}

impl Interpreter {
    pub fn evaluate(&self, expr: &Expr) -> Literal {
        expr.accept(self)
    }

    fn eval_unary(&self, op: &Token, expr: &Expr) -> Literal {
        let right = self.evaluate(expr);
        match &op.token_type {
            TokenType::Minus => {
                if let Literal::Number(n) = right {
                    Literal::Number(-n)
                } else {
                    todo!();
                }
            }
            TokenType::Bang => Literal::Boolean(!self.is_truthy(right)),
            _ => todo!(),
        }
    }

    fn eval_binary(&self, left: &Expr, op: &Token, right: &Expr) -> Literal {
        let left = self.evaluate(left);
        let right = self.evaluate(right);
        match &op.token_type {
            TokenType::Minus => numeric_op(|a, b| a - b, left, right).unwrap(),
            TokenType::Slash => numeric_op(|a, b| a / b, left, right).unwrap(),
            TokenType::Star => numeric_op(|a, b| a * b, left, right).unwrap(),
            TokenType::Plus => match (left, right) {
                (Literal::Number(n1), Literal::Number(n2)) => Literal::Number(n1 + n2),
                (Literal::String(s1), Literal::String(s2)) => Literal::String(s1 + &s2),
                _ => todo!(),
            },
            TokenType::Greater => numeric_cmp(|a, b| a > b, left, right).unwrap(),
            TokenType::GreaterEqual => numeric_cmp(|a, b| a >= b, left, right).unwrap(),
            TokenType::Less => numeric_cmp(|a, b| a < b, left, right).unwrap(),
            TokenType::LessEqual => numeric_cmp(|a, b| a <= b, left, right).unwrap(),
            TokenType::EqualEqual => Literal::Boolean(left == right),
            TokenType::BangEqual => Literal::Boolean(left != right),
            _ => todo!(),
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

fn numeric_op<F>(op: F, left: Literal, right: Literal) -> Option<Literal>
where
    F: FnOnce(f64, f64) -> f64,
{
    use Literal::Number;
    match (left, right) {
        (Number(left), Number(right)) => Some(Number(op(left, right))),
        _ => None,
    }
}

fn numeric_cmp<F>(op: F, left: Literal, right: Literal) -> Option<Literal>
where
    F: FnOnce(f64, f64) -> bool,
{
    use Literal::{Boolean, Number};
    match (left, right) {
        (Number(left), Number(right)) => Some(Boolean(op(left, right))),
        _ => None,
    }
}

impl TryFrom<TokenType> for Literal {
    type Error = ();

    fn try_from(token_type: TokenType) -> Result<Self, Self::Error> {
        use TokenType::*;
        match token_type {
            Nil => Ok(Self::Nil),
            False => Ok(Self::Boolean(false)),
            True => Ok(Self::Boolean(true)),
            Number(n) => Ok(Self::Number(n)),
            String(s) => Ok(Self::String(s)),
            _ => Err(()),
        }
    }
}
