use std::collections::HashMap;

use crate::error::Error;
use crate::expr::Expr;
use crate::scanner::Token;
use crate::stmt::{Function, Stmt};
use crate::Lox;

pub struct Resolver<'a> {
    lox: &'a mut Lox,
    scopes: Vec<Scope>,
}

type Scope = HashMap<String, Variable>;

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    token: Option<Token>,
    status: Status,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Status {
    Declared,
    Defined,
    Used,
}

impl<'a> Resolver<'a> {
    pub fn new(lox: &'a mut Lox) -> Self {
        Self {
            lox,
            scopes: Default::default(),
        }
    }

    pub fn resolve(&mut self, statements: &[Stmt]) {
        for stmt in statements {
            self.resolve_stmt(stmt);
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Var { name, initializer } => {
                self.declare(name);
                if let Some(expr) = initializer {
                    self.resolve_expr(expr);
                }
                self.define(name);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition);
                self.resolve_stmt(then_branch);
                if let Some(stmt) = else_branch {
                    self.resolve_stmt(stmt);
                }
            }
            Stmt::Print(expr) | Stmt::ReplExpression(expr) | Stmt::Expression(expr) => {
                self.resolve_expr(expr)
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition);
                self.resolve_stmt(body);
            }
            Stmt::Block(statements) => {
                self.begin_scope();
                self.resolve(statements);
                self.end_scope();
            }
            Stmt::Function(fun) => {
                self.declare(&fun.name);
                self.define(&fun.name);
                self.resolve_function(fun);
            }
            Stmt::Return { value, .. } => {
                if let Some(expr) = value {
                    self.resolve_expr(expr);
                }
            }
            Stmt::Class { name, methods } => {
                self.declare(name);
                self.define(name);
                let scope = self.begin_scope();
                scope.insert(
                    "this".to_string(),
                    Variable {
                        token: None,
                        status: Status::Used,
                    },
                );
                for method in methods {
                    self.resolve_function(method);
                }
                self.end_scope();
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(_) => {}
            Expr::Variable(token) => self.resolve_variable(token),
            Expr::Grouping(expr) | Expr::Unary { expr, .. } => self.resolve_expr(expr),
            Expr::Call {
                callee, arguments, ..
            } => {
                self.resolve_expr(callee);
                for arg in arguments {
                    self.resolve_expr(arg);
                }
            }
            Expr::Binary { left, right, .. } | Expr::Logical { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::Assign { name, value } => {
                self.resolve_expr(value);
                self.resolve_local(name);
            }
            Expr::Get { object, .. } => self.resolve_expr(object),
            Expr::Set { object, value, .. } => {
                self.resolve_expr(object);
                self.resolve_expr(value)
            }
            Expr::This(keyword) => self.resolve_local(keyword),
        }
    }

    fn resolve_local(&mut self, token: &Token) {
        for (i, scope) in self.scopes.iter_mut().rev().enumerate() {
            if let Some(v) = scope.get_mut(&token.lexeme) {
                // mark this local as being used to later report unused variables.
                v.status = Status::Used;
                self.lox.interpreter.resolve(token, i);
                break;
            }
        }
    }

    fn resolve_variable(&mut self, token: &Token) {
        if let Some(scope) = self.scopes.last() {
            if let Some(&Variable {
                status: Status::Declared,
                ..
            }) = scope.get(&token.lexeme)
            {
                self.lox.report(Error::syntax_err(
                    token,
                    "Can't read local variable in its own initializer.",
                ))
            }
        }
        self.resolve_local(token);
    }

    fn resolve_function(&mut self, fun: &Function) {
        self.begin_scope();
        for param in &fun.params {
            self.declare(param);
            self.define(param);
        }
        self.resolve(&fun.body);
        self.end_scope();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            match scope.contains_key(&name.lexeme) {
                true => {
                    if name.lexeme != "_" {
                        self.lox.report(Error::syntax_err(
                            name,
                            "Variable with the same name exists in this scope.",
                        ));
                    }
                }
                false => {
                    let status = Status::Declared;
                    let token = Some(name.clone());
                    scope.insert(name.lexeme.clone(), Variable { status, token });
                }
            }
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            let status = Status::Defined;
            let token = Some(name.clone());
            scope.insert(name.lexeme.clone(), Variable { status, token });
        }
    }

    fn begin_scope(&mut self) -> &mut Scope {
        self.scopes.push(Default::default());
        self.scopes.last_mut().unwrap()
    }

    fn end_scope(&mut self) {
        let popped = self.scopes.pop();
        if self.lox.in_repl() {
            return;
        }
        if let Some(values) = popped {
            for (name, variable) in values {
                if !name.starts_with('_') && variable.status != Status::Used {
                    let token = variable.token.as_ref().unwrap();
                    self.lox.warn(Error::syntax_err(token, "Unused variable."));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Lox, SourceId};

    #[test]
    fn test_undefined_global() {
        let mut lox = Lox::new(SourceId::Test, "print a;".into());
        let stmts = lox.parser().parse();
        Resolver::new(&mut lox).resolve(&stmts);
        assert_eq!(lox.error, None);
    }
}
