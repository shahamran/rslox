use std::io;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::interpreter::Literal;
use crate::scanner::Token;
use crate::Lox;

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub token: Option<Token>,
    pub message: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    UsageError,
    IoError,
    SyntaxError,
    RuntimeError,
    Return(Literal),
}

impl Error {
    pub fn usage_err(prog: &str) -> Self {
        Self {
            kind: ErrorKind::UsageError,
            token: None,
            message: format!("Usage: {prog} [script]"),
        }
    }

    pub fn syntax_err(token: &Token, message: &str) -> Self {
        Self {
            kind: ErrorKind::SyntaxError,
            token: Some(token.clone()),
            message: message.to_string(),
        }
    }

    pub fn runtime_err(token: &Token, message: &str) -> Self {
        Self {
            kind: ErrorKind::RuntimeError,
            token: Some(token.clone()),
            message: message.to_string(),
        }
    }

    pub fn return_value(value: Literal) -> Self {
        Self {
            kind: ErrorKind::Return(value),
            token: None,
            message: "".to_string(),
        }
    }

    pub fn exit_code(&self) -> i32 {
        match self.kind {
            ErrorKind::UsageError => 64,
            ErrorKind::IoError => 74,
            ErrorKind::SyntaxError => 65,
            ErrorKind::RuntimeError => 70,
            _ => unreachable!(),
        }
    }
}

impl Lox {
    pub fn report(&mut self, err: Error) {
        let err = self.error.insert(err);
        let (start, end) = match &err.token {
            Some(t) => (t.offset, t.offset + t.length),
            None => return eprintln!("{:?}: {}", err.kind, &err.message),
        };
        let source = &self.source;
        let src_id = self.src_id.as_ref();
        Report::build(ReportKind::Error, src_id, start)
            .with_label(
                Label::new((src_id, start..end))
                    .with_color(Color::Red)
                    .with_message(format!("{:?}: {}", err.kind, &err.message)),
            )
            .finish()
            .eprint((src_id, Source::from(source)))
            .unwrap();
    }
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error {
            kind: ErrorKind::IoError,
            token: None,
            message: err.to_string(),
        }
    }
}
