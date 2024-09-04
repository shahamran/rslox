use crate::lex::TokenType;
use crate::parser::{Expr, Visitor};

pub struct AstPrinter;

impl Visitor for AstPrinter {
    type Return = String;

    fn visit(&self, expr: &Expr) -> Self::Return {
        match expr {
            Expr::Literal(t) => match &t.token_type {
                TokenType::Identifier(x) => x.clone(),
                TokenType::String(s) => s.clone(),
                TokenType::Number(n) => n.to_string(),
                TokenType::False => "false".to_string(),
                TokenType::True => "true".to_string(),
                TokenType::Nil => "nil".to_string(),
                _ => panic!("invalid literal: {t:?}"),
            },
            Expr::Unary { op, expr } => self.parenthesize(&op.lexeme, &[expr]),
            Expr::Binary { left, op, right } => self.parenthesize(&op.lexeme, &[left, right]),
            Expr::Grouping(t) => self.parenthesize("group", &[t]),
        }
    }
}

impl AstPrinter {
    pub fn print(&self, expr: &Expr) -> String {
        self.visit(expr)
    }

    fn parenthesize(&self, name: &str, exprs: &[&Expr]) -> String {
        let mut s = String::with_capacity(2 + name.len() + exprs.len() * 2);
        s.push('(');
        s.push_str(name);
        for &expr in exprs {
            s.push(' ');
            s.push_str(&expr.accept(self));
        }
        s.push(')');
        s
    }
}

#[cfg(test)]
mod tests {
    use crate::lex::{Token, TokenType};
    use TokenType::*;

    use super::*;

    fn token(token_type: TokenType) -> Token {
        let lexeme = match token_type {
            Star => "*",
            Plus => "+",
            Minus => "-",
            Slash => "/",
            _ => "",
        }
        .to_string();
        Token {
            token_type,
            lexeme,
            offset: 0,
            length: 0,
        }
    }

    fn literal(token_type: TokenType) -> Expr {
        Expr::Literal(token(token_type))
    }

    #[test]
    fn test_literal() {
        let printer = AstPrinter;
        assert_eq!(&printer.print(&literal(Nil)), "nil");
        assert_eq!(&printer.print(&literal(False)), "false");
        assert_eq!(&printer.print(&literal(True)), "true");
        assert_eq!(&printer.print(&literal(Number(12.34))), "12.34");
        assert_eq!(&printer.print(&literal(String("foo".into()))), "foo");
    }

    #[test]
    fn test_book_example() {
        let printer = AstPrinter;
        let expr = Expr::Binary {
            left: Box::new(Expr::Unary {
                op: token(Minus),
                expr: Box::new(literal(Number(123.0))),
            }),
            op: token(Star),
            right: Box::new(Expr::Grouping(Box::new(literal(Number(45.67))))),
        };
        assert_eq!(&printer.print(&expr), "(* (- 123) (group 45.67))");
    }

    #[test]
    fn test_binary() {
        let printer = AstPrinter;
        let expr = Expr::Binary {
            left: Box::new(literal(Number(1.0))),
            op: token(Plus),
            right: Box::new(literal(Number(2.0))),
        };
        assert_eq!(&printer.print(&expr), "(+ 1 2)");
    }
}
