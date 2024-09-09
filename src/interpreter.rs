use std::collections::HashMap;

use crate::environment::Environment;
use crate::error::{Error, ErrorKind, Result};
use crate::parser::{Expr, Function, Stmt};
use crate::scanner::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    environment: Environment,
    locals: HashMap<Token, usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Undefined,
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Clock,
    Function(Function),
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
                if is_truthy(&self.evaluate(condition)?) {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
            }
            Stmt::While { condition, body } => {
                while is_truthy(&self.evaluate(condition)?) {
                    self.execute(body)?;
                }
            }
            Stmt::Function(fun) => self.eval_function_decl(fun)?,
            Stmt::Return { value, .. } => {
                let value = match value {
                    Some(expr) => self.evaluate(expr)?,
                    None => Literal::Nil,
                };
                return Err(Error::return_value(value));
            }
        }
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Literal> {
        match expr {
            Expr::Literal(t) => Ok(t.clone()),
            Expr::Logical { left, op, right } => self.eval_logical(left, op, right),
            Expr::Unary { op, expr } => self.eval_unary(op, expr),
            Expr::Binary { left, op, right } => self.eval_binary(left, op, right),
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Variable(name) => self.look_up_variable(name).cloned(),
            Expr::Assign { name, value } => self.eval_assignment(name, value),
            expr @ Expr::Call { .. } => self.eval_call(expr),
        }
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<()> {
        self.environment.new_block();
        let result = self.execute_all(statements);
        self.environment.remove_block();
        result
    }

    fn execute_all(&mut self, statements: &[Stmt]) -> Result<()> {
        for stmt in statements {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn eval_unary(&mut self, op: &Token, expr: &Expr) -> Result<Literal> {
        let right = self.evaluate(expr)?;
        match op.token_type {
            TokenType::Minus => match right {
                Literal::Number(n) => Ok(Literal::Number(-n)),
                _ => Err(Error::runtime_err(op, "Operand must be a number.")),
            },
            TokenType::Bang => Ok(Literal::Boolean(!is_truthy(&right))),
            _ => unreachable!("unary expr parsing error"),
        }
    }

    fn eval_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Literal> {
        let left = self.evaluate(left)?;
        match (op.token_type, is_truthy(&left)) {
            (TokenType::Or, true) => Ok(left),
            (TokenType::And, false) => Ok(left),
            _ => self.evaluate(right),
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

    fn eval_assignment(&mut self, name: &Token, value: &Expr) -> Result<Literal> {
        let value = self.evaluate(value)?;
        self.environment
            .assign(self.locals.get(name).copied(), name, &value)?;
        Ok(value)
    }

    fn eval_call(&mut self, expr: &Expr) -> Result<Literal> {
        let Expr::Call {
            callee,
            closing_paren,
            arguments,
        } = expr
        else {
            unreachable!()
        };
        let callee = self.evaluate(callee)?;
        let mut args = Vec::with_capacity(arguments.len());
        for arg in arguments {
            args.push(self.evaluate(arg)?);
        }
        match callee {
            Literal::Callable(mut f) => {
                if f.arity() == args.len() {
                    f.call(self, args)
                } else {
                    Err(Error::runtime_err(
                        closing_paren,
                        &format!("Expected {} arguments but got {}.", f.arity(), args.len()),
                    ))
                }
            }
            _ => Err(Error::runtime_err(
                closing_paren,
                "Can only call functions and classes.",
            )),
        }
    }

    fn eval_function_decl(&mut self, fun: &Function) -> Result<()> {
        self.environment
            .define(fun.name.lexeme.clone(), fun.clone().into());
        Ok(())
    }

    pub(crate) fn resolve(&mut self, variable: &Token, depth: usize) {
        self.locals.insert(variable.clone(), depth);
    }

    fn look_up_variable(&self, variable: &Token) -> Result<&Literal> {
        self.environment
            .get(self.locals.get(variable).copied(), variable)
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

const fn is_truthy(literal: &Literal) -> bool {
    match literal {
        Literal::Boolean(b) => *b,
        Literal::Nil => false,
        _ => true,
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut environment = Environment::default();
        environment.define("clock".to_string(), Literal::Callable(Callable::Clock));
        let locals = Default::default();
        Self {
            environment,
            locals,
        }
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Clock => write!(f, "<native fn>"),
            Self::Function(func) => write!(f, "<fn {}>", &func.name.lexeme),
        }
    }
}

impl Callable {
    fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<Literal>) -> Result<Literal> {
        match self {
            Callable::Clock => {
                let now = std::time::SystemTime::now();
                let elapsed = now
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .expect("uh oh");
                Ok(Literal::Number(elapsed.as_millis() as f64 / 1000.0))
            }
            Callable::Function(fun) => {
                interpreter.environment.new_block();
                assert_eq!(arguments.len(), fun.params.len());
                for (i, value) in arguments.into_iter().enumerate() {
                    let name = fun.params[i].lexeme.clone();
                    interpreter.environment.define(name, value);
                }
                let result = match interpreter.execute_all(&fun.body) {
                    Ok(()) => Ok(Literal::Nil),
                    Err(Error {
                        kind: ErrorKind::Return(value),
                        ..
                    }) => Ok(value),
                    Err(err) => Err(err),
                };
                interpreter.environment.remove_block();
                result
            }
        }
    }

    fn arity(&self) -> usize {
        match self {
            Callable::Clock => 0,
            Callable::Function(fun) => fun.params.len(),
        }
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

impl From<Function> for Literal {
    fn from(fun: Function) -> Self {
        Literal::Callable(Callable::Function(fun))
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
            Literal::Callable(func) => write!(f, "{func}"),
        }
    }
}
