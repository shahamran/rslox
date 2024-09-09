// mod ast_printer;
mod environment;
mod error;
mod interpreter;
mod parser;
mod resolver;
mod scanner;

use std::env;
use std::io;
use std::io::Write;
use std::process;

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
            UsageError | IoError => eprintln!("{}", e.message),
            _ => {}
        }
        process::exit(e.exit_code());
    }
}

fn run_file(path: String) -> Result<()> {
    let content = std::fs::read_to_string(&path)?;
    let mut lox = Lox::new(SourceId::File(path), content);
    run(&mut lox);
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
        run(&mut lox);
    }
    println!();
    Ok(())
}

fn run(lox: &mut Lox) {
    let stmts = lox.parser().parse();
    Resolver::new(lox).resolve(&stmts);
    if lox.error.is_some() {
        return;
    }
    for stmt in stmts {
        if let Err(e) = lox.interpreter.execute(&stmt) {
            lox.report(e);
            break;
        }
    }
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
