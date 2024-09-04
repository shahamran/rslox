mod ast_printer;
mod lex;
mod parser;

use std::env;
use std::io;
use std::io::Write;
use std::process;

use ariadne::Color;
use ariadne::{Label, Report, ReportKind, Source};
use ast_printer::AstPrinter;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let res = if args.len() > 2 {
        Err(Error::usage_error("Usage: rslox [script]\n"))
    } else if args.len() == 2 {
        run_file(args[1].clone())
    } else {
        run_prompt()
    };
    if let Err(e) = res {
        eprint!("{}", e.message);
        process::exit(e.code);
    }
}

fn run_file(path: String) -> Result<(), Error> {
    let content = std::fs::read_to_string(&path)
        .map_err(|e| Error::io_error(format!("failed to read script {path:?}: {e}")))?;
    let mut ctx = Context::new(SourceId::File(path), content);
    run(&mut ctx);
    match ctx.has_error {
        true => Err(Error::value_error("".into())),
        false => Ok(()),
    }
}

fn run_prompt() -> Result<(), Error> {
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
    let tree = {
        let mut parser = parser::Parser::new(ctx, tokens);
        parser.parse()
    };
    if ctx.has_error {
        return;
    }
    if let Some(t) = tree {
        println!("{}", AstPrinter.print(&t))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    src_id: SourceId,
    source: String,
    has_error: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum SourceId {
    Prompt,
    File(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Error {
    code: i32,
    message: String,
}

impl Context {
    fn new(src_id: SourceId, source: String) -> Self {
        let has_error = false;
        Self {
            src_id,
            source,
            has_error,
        }
    }

    fn report_error(&mut self, start: usize, end: usize, message: &str) {
        self.has_error = true;
        let source = &self.source;
        let src_id = self.src_id.as_ref();
        Report::build(ReportKind::Error, src_id, start)
            .with_label(
                Label::new((src_id, start..end))
                    .with_color(Color::Red)
                    .with_message(message),
            )
            .finish()
            .eprint((src_id, Source::from(source)))
            .unwrap();
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

impl Error {
    fn usage_error(usage: &str) -> Self {
        Self {
            code: 64,
            message: usage.into(),
        }
    }

    fn value_error(message: String) -> Self {
        let code = 65;
        Self { code, message }
    }

    fn io_error(message: String) -> Self {
        let code = 74;
        Self { code, message }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Self::io_error(e.to_string())
    }
}
