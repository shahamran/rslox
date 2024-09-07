use crate::environment::Environment;
use crate::error::{Error, Result};
use crate::lex::{Token, TokenType};
use crate::parser::{Expr, Stmt};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Interpreter {
    environment: Environment,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Undefined,
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Interpreter {
    pub fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Literal::Undefined,
                };
                let name = name.lexeme.clone();
                self.environment.define(name, value);
            }
            Stmt::Expression(expr) => self.evaluate(expr).map(|_| ())?,
            Stmt::Print(expr) | Stmt::ReplExpression(expr) => {
                let literal = self.evaluate(expr)?;
                println!("{literal}");
            }
            Stmt::Block(statements) => self.execute_block(statements)?,
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition = self.evaluate(condition)?;
                if self.is_truthy(condition) {
                    self.execute(&then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }
        }
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Literal> {
        match expr {
            Expr::Literal(t) => Ok(t.clone().into()),
            Expr::Unary { op, expr } => self.eval_unary(op, expr),
            Expr::Binary { left, op, right } => self.eval_binary(left, op, right),
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Variable(name) => self.environment.get(name).cloned(),
            Expr::Assign { name, value } => self.eval_assignment(name, value),
        }
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<()> {
        self.environment.new_block();
        for stmt in statements {
            if let Err(e) = self.execute(stmt) {
                self.environment.remove_block();
                return Err(e);
            }
        }
        self.environment.remove_block();
        Ok(())
    }

    fn eval_unary(&mut self, op: &Token, expr: &Expr) -> Result<Literal> {
        let right = self.evaluate(expr)?;
        match op.token_type {
            TokenType::Minus => match right {
                Literal::Number(n) => Ok(Literal::Number(-n)),
                _ => Err(Error::runtime_err(op, "Operand must be a number.")),
            },
            TokenType::Bang => Ok(Literal::Boolean(!self.is_truthy(right))),
            _ => unreachable!("unary expr parsing error"),
        }
    }

    fn eval_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Literal> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        let num_operands = || Error::runtime_err(op, "Operands must be numbers.");
        match op.token_type {
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

    fn eval_assignment(&mut self, name: &Token, value: &Expr) -> Result<Literal> {
        let value = self.evaluate(value)?;
        self.environment.assign(name, &value)?;
        Ok(value)
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
            Number => Self::Number(token.lexeme.parse().unwrap()),
            String => Self::String(token.lexeme[1..token.lexeme.len() - 1].to_string()),
            _ => unreachable!("literal parsing error"),
        }
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Undefined => write!(f, "undefined"),
            Literal::Nil => write!(f, "nil"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Number(n) => write!(f, "{n}"),
            Literal::String(s) => write!(f, "{s}"),
        }
    }
}
