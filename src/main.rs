// mod ast_printer;
mod environment;
mod error;
mod expr;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod value;

use std::cell::RefCell;
use std::env;
use std::io::{self, Write};
use std::process;
use std::rc::Rc;

use error::{Error, Result};
use interpreter::Interpreter;
use resolver::Resolver;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let res = match args.len() {
        1 => run_prompt(),
        2 => run_file(args[1].clone()),
        _ => Err(Error::usage_err(&args[0])),
    };
    if let Err(e) = res {
        use error::ErrorKind::*;
        match e.kind {
            UsageError | IoError => eprintln!("{:?}: {}", e.kind, e.message),
            _ => {}
        }
        process::exit(e.exit_code());
    }
}

fn run_file(path: String) -> Result<()> {
    let content = std::fs::read_to_string(&path)?;
    let mut lox = Lox::new(SourceId::File(path), content);
    lox.run();
    match lox.error {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

fn run_prompt() -> Result<()> {
    let mut lox = Lox::new(SourceId::Prompt, String::new());
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buf = String::new();
        let num_read = io::stdin().read_line(&mut buf)?;
        lox.set_context(SourceId::Prompt, buf);
        if num_read == 0 {
            break;
        }
        lox.run();
    }
    println!();
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
struct Lox {
    src_id: SourceId,
    source: String,
    error: Option<Error>,
    interpreter: Interpreter,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceId {
    Prompt,
    File(String),
    #[cfg(test)]
    Test,
}

impl Lox {
    fn new(src_id: SourceId, source: String) -> Self {
        Self {
            src_id,
            source,
            error: None,
            interpreter: Default::default(),
        }
    }

    fn run(&mut self) {
        let stmts = self.parser().parse();
        Resolver::new(self).resolve(&stmts);
        if self.error.is_some() {
            return;
        }
        for stmt in stmts {
            if let Err(e) = self.interpreter.execute(&stmt) {
                self.report(e);
                break;
            }
        }
    }

    fn set_context(&mut self, src_id: SourceId, source: String) {
        self.src_id = src_id;
        self.source = source;
        self.error = None;
    }

    fn in_repl(&self) -> bool {
        self.src_id == SourceId::Prompt
    }

    fn scan(&mut self) -> Vec<scanner::Token> {
        scanner::Scanner::new(self).all_tokens()
    }

    fn parser(&mut self) -> parser::Parser<'_> {
        let tokens = self.scan();
        parser::Parser::new(self, tokens)
    }

    #[cfg(test)]
    fn test_eval(&mut self, source: &str) -> Result<value::Value> {
        self.set_context(SourceId::Test, source.to_string());
        let expr = self.parser().expression()?;
        self.interpreter.evaluate(&expr)
    }

    #[cfg(test)]
    fn test_run(&mut self, source: &str) -> Result<()> {
        self.set_context(SourceId::Test, source.to_string());
        self.run();
        match &self.error {
            Some(e) => Err(e.clone()),
            None => Ok(()),
        }
    }
}

impl AsRef<str> for SourceId {
    fn as_ref(&self) -> &str {
        match self {
            SourceId::Prompt => "<prompt>",
            SourceId::File(f) => f,
            #[cfg(test)]
            SourceId::Test => "<test>",
        }
    }
}

type SharedRef<T> = Rc<RefCell<T>>;

fn wrap<T>(t: T) -> SharedRef<T> {
    Rc::new(RefCell::new(t))
}
