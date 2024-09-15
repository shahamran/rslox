use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, Result};
use crate::expr::Expr;
use crate::scanner::{Token, TokenType};
use crate::stmt::{self, Stmt};
use crate::value::{self, Value};
use crate::SharedRef;
use crate::{environment as env, wrap};

pub type EnvironmentRef = SharedRef<env::Environment>;

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    pub environment: EnvironmentRef,
    globals: EnvironmentRef,
    locals: HashMap<Token, usize>,
}

impl Interpreter {
    pub fn execute(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate(expr)?,
                    None => Value::Undefined,
                };
                let name = name.lexeme.clone();
                self.environment.borrow_mut().define(name, value);
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
                    None => Value::Nil,
                };
                return Err(Error::return_value(value));
            }
            Stmt::Class { name, methods } => self.eval_class_decl(name, methods)?,
        }
        Ok(())
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Literal(t) => Ok(t.clone()),
            Expr::Logical { left, op, right } => self.eval_logical(left, op, right),
            Expr::Unary { op, expr } => self.eval_unary(op, expr),
            Expr::Binary { left, op, right } => self.eval_binary(left, op, right),
            Expr::Grouping(expr) => self.evaluate(expr),
            Expr::Variable(name) => self.look_up_variable(name),
            Expr::Assign { name, value } => self.eval_assignment(name, value),
            expr @ Expr::Call { .. } => self.eval_call(expr),
            Expr::Get { object, name } => self.eval_get(object, name),
            Expr::Set {
                object,
                name,
                value,
            } => self.eval_set(object, name, value),
            Expr::This(keyword) => self.look_up_variable(keyword),
        }
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<()> {
        self.environment = env::new_block(self.environment.clone());
        let result = self.execute_all(statements);
        self.environment = env::remove_block(self.environment.clone());
        result
    }

    pub fn execute_all(&mut self, statements: &[Stmt]) -> Result<()> {
        for stmt in statements {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn eval_unary(&mut self, op: &Token, expr: &Expr) -> Result<Value> {
        let right = self.evaluate(expr)?;
        match op.token_type {
            TokenType::Minus => match right {
                Value::Number(n) => Ok(Value::Number(-n)),
                _ => Err(Error::runtime_err(op, "Operand must be a number.")),
            },
            TokenType::Bang => Ok(Value::Boolean(!is_truthy(&right))),
            _ => unreachable!("unary expr parsing error"),
        }
    }

    fn eval_logical(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value> {
        let left = self.evaluate(left)?;
        match (op.token_type, is_truthy(&left)) {
            (TokenType::Or, true) => Ok(left),
            (TokenType::And, false) => Ok(left),
            _ => self.evaluate(right),
        }
    }

    fn eval_binary(&mut self, left: &Expr, op: &Token, right: &Expr) -> Result<Value> {
        let left = self.evaluate(left)?;
        let right = self.evaluate(right)?;
        let num_operands = || Error::runtime_err(op, "Operands must be numbers.");
        match op.token_type {
            TokenType::Minus => num_op(|a, b| a - b, left, right).ok_or_else(num_operands),
            TokenType::Slash => num_op(|a, b| a / b, left, right).ok_or_else(num_operands),
            TokenType::Star => num_op(|a, b| a * b, left, right).ok_or_else(num_operands),
            TokenType::Plus => match (left, right) {
                (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
                (Value::String(s1), Value::String(s2)) => Ok(Value::String(s1 + &s2)),
                _ => Err(Error::runtime_err(
                    op,
                    "Operands must both be numbers or strings.",
                )),
            },
            TokenType::Greater => num_cmp(|a, b| a > b, left, right).ok_or_else(num_operands),
            TokenType::GreaterEqual => num_cmp(|a, b| a >= b, left, right).ok_or_else(num_operands),
            TokenType::Less => num_cmp(|a, b| a < b, left, right).ok_or_else(num_operands),
            TokenType::LessEqual => num_cmp(|a, b| a <= b, left, right).ok_or_else(num_operands),
            TokenType::EqualEqual => Ok(Value::Boolean(left == right)),
            TokenType::BangEqual => Ok(Value::Boolean(left != right)),
            _ => unreachable!("binary expr parsing error"),
        }
    }

    fn eval_assignment(&mut self, name: &Token, value: &Expr) -> Result<Value> {
        let value = self.evaluate(value)?;
        match self.locals.get(name) {
            Some(&distance) => self.environment.borrow_mut().assign(distance, name, &value),
            None => self.globals.borrow_mut().assign(0, name, &value),
        }
        Ok(value)
    }

    fn eval_get(&mut self, object: &Expr, name: &Token) -> Result<Value> {
        match self.evaluate(object)? {
            Value::ClassInstance(inst) => match inst.borrow().get(name)? {
                Value::Callable(value::Callable::Function(fun)) => {
                    Ok(fun.bind(Rc::clone(&inst)).into())
                }
                lit => Ok(lit),
            },
            _ => Err(Error::runtime_err(name, "Only instances have properties.")),
        }
    }

    fn eval_set(&mut self, object: &Expr, name: &Token, value: &Expr) -> Result<Value> {
        let inst = match self.evaluate(object)? {
            Value::ClassInstance(inst) => inst,
            _ => return Err(Error::runtime_err(name, "Only instances have fields.")),
        };
        let value = self.evaluate(value)?;
        inst.borrow_mut()
            .fields
            .insert(name.lexeme.clone(), value.clone());
        Ok(value)
    }

    fn eval_call(&mut self, expr: &Expr) -> Result<Value> {
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
            Value::Callable(mut f) => {
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

    fn eval_function_decl(&mut self, fun: &stmt::Function) -> Result<()> {
        let name = fun.name.lexeme.clone();
        let fun = value::Function {
            declaration: fun.clone(),
            closure: Some(Rc::clone(&self.environment)),
            this: None,
        };
        self.environment.borrow_mut().define(name, fun.into());
        Ok(())
    }

    fn eval_class_decl(&mut self, name: &Token, methods: &[stmt::Function]) -> Result<()> {
        let class_name = name.lexeme.clone();
        self.environment
            .borrow_mut()
            .define(class_name.clone(), Value::Undefined);
        let methods = methods
            .iter()
            .map(|f| (f.name.lexeme.clone(), value::Function::from(f.clone())))
            .collect();
        let class = value::Class {
            name: class_name,
            methods,
        };
        self.environment.borrow_mut().assign(0, name, &class.into());
        Ok(())
    }

    pub(crate) fn resolve(&mut self, variable: &Token, depth: usize) {
        self.locals.insert(variable.clone(), depth);
    }

    fn look_up_variable(&self, variable: &Token) -> Result<Value> {
        match self.locals.get(variable) {
            Some(&depth) => Ok(self.environment.borrow().get(depth, variable)),
            None => self.globals.borrow().try_get(variable),
        }
    }
}

fn num_op<F>(op: F, left: Value, right: Value) -> Option<Value>
where
    F: FnOnce(f64, f64) -> f64,
{
    use Value::Number;
    match (left, right) {
        (Number(left), Number(right)) => Some(Number(op(left, right))),
        _ => None,
    }
}

fn num_cmp<F>(op: F, left: Value, right: Value) -> Option<Value>
where
    F: FnOnce(f64, f64) -> bool,
{
    use Value::{Boolean, Number};
    match (left, right) {
        (Number(left), Number(right)) => Some(Boolean(op(left, right))),
        _ => None,
    }
}

const fn is_truthy(literal: &Value) -> bool {
    match literal {
        Value::Boolean(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        let mut environment = env::Environment::new(None);
        environment.define("clock".to_string(), value::Callable::Clock.into());
        let environment: EnvironmentRef = wrap(environment);
        let globals = environment.clone();
        Self {
            environment,
            globals,
            locals: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lox, SourceId};

    use super::*;

    #[test]
    fn test_class() {
        let source = r#"
            class Cake {
              taste() {
                var adjective = "delicious";
                return "The " + this.flavor + " cake is " + adjective + "!";
              }
            }

            var cake = Cake();
            cake.flavor = "German chocolate";
            "#;
        // cake.taste(); // Prints "The German chocolate cake is delicious!".
        let mut lox = Lox::new(SourceId::Test, "".to_string());
        assert_eq!(lox.test_run(source), Ok(()));
        assert_eq!(
            lox.test_eval("cake.taste()"),
            Ok(Value::String(
                "The German chocolate cake is delicious!".into()
            ))
        );
        let source = r#"
            fun notTaste() {
                var adjective = "Uh oh!";
                fun inner() {
                    return adjective;
                }
                return inner;
            }
            cake.taste = notTaste();
            "#;
        assert_eq!(lox.test_run(source), Ok(()));
        assert_eq!(
            lox.test_eval("cake.taste()"),
            Ok(Value::String("Uh oh!".into()))
        );
    }
}
