use std::collections::HashMap;
use std::rc::Rc;

use crate::environment::Environment;
use crate::error::{Error, Result};
use crate::parser::{Expr, Function, Stmt};
use crate::scanner::{Token, TokenType};
use crate::types::{Callable, Class, Literal};

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    pub environment: Environment,
    locals: HashMap<Token, usize>,
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
            Stmt::Class { name, methods } => self.eval_class_decl(name, methods)?,
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
            Expr::Get { object, name } => self.eval_get(object, name),
            Expr::Set {
                object,
                name,
                value,
            } => self.eval_set(object, name, value),
            Expr::This(keyword) => self.look_up_variable(keyword).cloned(),
        }
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<()> {
        self.environment.new_block();
        let result = self.execute_all(statements);
        self.environment.remove_block();
        result
    }

    pub fn execute_all(&mut self, statements: &[Stmt]) -> Result<()> {
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

    fn eval_get(&mut self, object: &Expr, name: &Token) -> Result<Literal> {
        match self.evaluate(object)? {
            Literal::ClassInstance(inst) => match inst.borrow().get(name)? {
                Literal::Callable(f @ Callable::Function { .. }) => {
                    Ok(f.bind(Rc::clone(&inst)).into())
                }
                lit => Ok(lit),
            },
            _ => Err(Error::runtime_err(name, "Only instances have properties.")),
        }
    }

    fn eval_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Result<Literal> {
        let inst = match self.evaluate(object)? {
            Literal::ClassInstance(inst) => inst,
            _ => return Err(Error::runtime_err(name, "Only instances have fields.")),
        };
        let value = self.evaluate(value)?;
        inst.borrow_mut()
            .fields
            .insert(name.lexeme.clone(), value.clone());
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
                    dbg!(&self.locals);
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

    fn eval_class_decl(&mut self, name: &Token, methods: &[Function]) -> Result<()> {
        let class_name = name.lexeme.clone();
        self.environment
            .define(class_name.clone(), Literal::Undefined);
        let methods = methods
            .iter()
            .map(|f| (f.name.lexeme.clone(), f.clone()))
            .collect();
        let class = Class {
            name: class_name,
            methods,
        };
        self.environment.assign(Some(0), name, &class.into())?;
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
        environment.define("clock".to_string(), Callable::Clock.into());
        Self {
            environment,
            locals: Default::default(),
        }
    }
}
