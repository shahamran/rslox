// mod ast_printer;
mod environment;
mod error;
mod interpreter;
mod lex;
mod parser;

use std::env;
use std::io;
use std::io::Write;
use std::process;

use interpreter::Interpreter;
use error::{Error, Result};

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let res = if args.len() > 2 {
        Err(Error::usage_err(&args[0]))
    } else if args.len() == 2 {
        run_file(args[1].clone())
    } else {
        run_prompt()
    };
    if let Err(e) = res {
        eprint!("{}", e.message);
        process::exit(e.exit_code());
    }
}

fn run_file(path: String) -> Result<()> {
    let content = std::fs::read_to_string(&path)?;
    let mut ctx = Context::new(SourceId::File(path), content);
    run(&mut ctx);
    match ctx.error {
        Some(e) => Err(e),
        None => Ok(()),
    }
}

fn run_prompt() -> Result<()> {
    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut buf = String::new();
        let num_read = io::stdin().read_line(&mut buf)?;
        if num_read == 0 {
            break;
        }
        let mut ctx = Context::new(SourceId::Prompt, buf);
        run(&mut ctx);
    }
    println!();
    Ok(())
}

fn run(ctx: &mut Context) {
    let mut scanner = lex::Lexer::new(ctx);
    let mut tokens = Vec::new();
    loop {
        let token = scanner.next_token();
        tokens.push(token);
        if tokens.last().unwrap().is_eof() {
            break;
        }
    }
    let stmts = {
        let mut parser = parser::Parser::new(ctx, tokens);
        parser.parse()
    };
    if ctx.error.is_some() {
        return;
    }
    let interpreter = Interpreter::default();
    for stmt in stmts {
        match interpreter.execute(&stmt) {
            Ok(()) => {}
            Err(e) => ctx.report(e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Context {
    src_id: SourceId,
    source: String,
    error: Option<Error>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceId {
    Prompt,
    File(String),
}

impl Context {
    fn new(src_id: SourceId, source: String) -> Self {
        Self {
            src_id,
            source,
            error: None,
        }
    }
}

impl AsRef<str> for SourceId {
    fn as_ref(&self) -> &str {
        match self {
            SourceId::Prompt => "<prompt>",
            SourceId::File(f) => f,
        }
    }
}
