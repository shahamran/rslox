use std::collections::HashMap;

use crate::error::Error;
use crate::parser::{Expr, Function, Stmt};
use crate::scanner::Token;
use crate::Lox;

pub struct Resolver<'a> {
    lox: &'a mut Lox,
    scopes: Vec<HashMap<String, Variable>>,
}

#[derive(Debug, Clone, PartialEq)]
struct Variable {
    token: Token,
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
                    self.define(name);
                }
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
                    let token = name.clone();
                    scope.insert(name.lexeme.clone(), Variable { status, token });
                }
            }
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            let status = Status::Defined;
            let token = name.clone();
            scope.insert(name.lexeme.clone(), Variable { status, token });
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn end_scope(&mut self) {
        let popped = self.scopes.pop();
        if self.lox.in_repl() {
            return;
        }
        if let Some(values) = popped {
            for (name, variable) in values {
                if !name.starts_with('_') && variable.status != Status::Used {
                    self.lox
                        .report(Error::syntax_err(&variable.token, "Unused variable."))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ErrorKind;
    use crate::scanner::TokenType;
    use crate::{Lox, SourceId};

    #[test]
    fn test_undefined_global() {
        let mut lox = Lox::new(SourceId::Test, "print a;".into());
        let stmts = lox.parser().parse();
        Resolver::new(&mut lox).resolve(&stmts);
        assert_eq!(lox.error, None);
    }

    #[test]
    fn test_undefined_local() {
        let mut lox = Lox::new(SourceId::Test, "{print a;}".into());
        let stmts = lox.parser().parse();
        Resolver::new(&mut lox).resolve(&stmts);
        let t = token(TokenType::Identifier, "a", 7, 1);
        assert_eq!(lox.error, Some(error(t, "Undefined local.")));
    }

    #[test]
    fn test_unused_local() {
        let mut lox = Lox::new(SourceId::Test, "{var a = 3;}".into());
        let parser = lox.parser();
        let t = parser
            .tokens
            .iter()
            .find(|&t| t.lexeme == "a")
            .cloned()
            .unwrap();
        let stmts = parser.parse();
        Resolver::new(&mut lox).resolve(&stmts);
        assert_eq!(lox.error, Some(error(t, "Unused variable.")));
    }

    fn error(token: Token, message: &str) -> Error {
        Error {
            kind: ErrorKind::SyntaxError,
            token: Some(token),
            message: message.to_string(),
        }
    }

    fn token(token_type: TokenType, lexeme: &str, offset: usize, length: usize) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            offset,
            length,
        }
    }
}
