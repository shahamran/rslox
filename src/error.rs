use std::io;

use annotate_snippets::{Level, Renderer, Snippet};

use crate::scanner::Token;
use crate::value::Value;
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
    Return(Value),
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

    pub fn return_value(value: Value) -> Self {
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
        self.annotate_err(Level::Error, err);
    }

    pub fn warn(&mut self, err: Error) {
        self.annotate_err(Level::Warning, err);
    }

    fn annotate_err(&mut self, level: Level, err: Error) {
        let err = match level {
            Level::Error => self.error.insert(err),
            _ => &err,
        };
        let title = err.message.to_lowercase();
        let mut message = level.title(&title).id(err.kind.as_ref());
        if let Some(t) = &err.token {
            message = message.snippet(
                Snippet::source(&self.source)
                    .origin(self.src_id.as_ref())
                    .fold(true)
                    .annotation(level.span(t.offset..t.offset + t.length)),
            );
        }
        eprintln!("{}", Renderer::styled().render(message));
    }
}

impl AsRef<str> for ErrorKind {
    fn as_ref(&self) -> &str {
        match self {
            ErrorKind::UsageError => "usage",
            ErrorKind::IoError => "io",
            ErrorKind::SyntaxError => "syntax",
            ErrorKind::RuntimeError => "runtime",
            ErrorKind::Return(_) => unreachable!(),
        }
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
