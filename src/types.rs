use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::{Error, ErrorKind, Result};
use crate::interpreter::Interpreter;
use crate::parser::Function;
use crate::scanner::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Undefined,
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
    ClassInstance(Rc<RefCell<ClassInstance>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Clock,
    Function {
        fun: Function,
        this: Option<Rc<RefCell<ClassInstance>>>,
    },
    Class(Rc<Class>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInstance {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Literal>,
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Clock => write!(f, "<native fn>"),
            Self::Function { fun, .. } => write!(f, "<fn {}>", &fun.name.lexeme),
            Self::Class(class) => write!(f, "<class {}>", class.name),
        }
    }
}

impl Callable {
    pub fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal> {
        match self {
            Callable::Clock => {
                let now = std::time::SystemTime::now();
                let elapsed = now
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .expect("uh oh");
                Ok(Literal::Number(elapsed.as_millis() as f64 / 1000.0))
            }
            Callable::Function { fun, this } => {
                interpreter.environment.new_block();
                if let Some(this) = this {
                    interpreter
                        .environment
                        .define("this".to_string(), Literal::ClassInstance(Rc::clone(this)));
                }
                assert_eq!(arguments.len(), fun.params.len());
                for (i, value) in arguments.into_iter().enumerate() {
                    let name = fun.params[i].lexeme.clone();
                    interpreter.environment.define(name, value);
                }
                let result = match interpreter.execute_all(&fun.body) {
                    Ok(()) => Ok(Literal::Nil),
                    Err(Error {
                        kind: ErrorKind::Return(value),
                        ..
                    }) => Ok(value),
                    Err(err) => Err(err),
                };
                interpreter.environment.remove_block();
                result
            }
            Callable::Class(class) => {
                let inst = Literal::ClassInstance(Rc::new(RefCell::new(ClassInstance::new(
                    Rc::clone(class),
                ))));
                Ok(inst)
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Callable::Clock => 0,
            Callable::Function { fun, .. } => fun.params.len(),
            Callable::Class { .. } => 0,
        }
    }

    pub fn bind(self, instance: Rc<RefCell<ClassInstance>>) -> Self {
        match self {
            Callable::Function { fun, mut this, .. } => {
                this = Some(instance);
                Callable::Function { fun, this }
            }
            _ => unreachable!(),
        }
    }
}

impl ClassInstance {
    fn new(class: Rc<Class>) -> Self {
        let fields = HashMap::new();
        Self { class, fields }
    }

    pub fn get(&self, name: &Token) -> Result<Literal> {
        let prop_name = &name.lexeme;
        if let Some(lit) = self.fields.get(prop_name).cloned() {
            Ok(lit)
        } else if let Some(method) = self.class.methods.get(prop_name) {
            Ok(method.clone().into())
        } else {
            Err(Error::runtime_err(
                name,
                &format!("Undefined property '{prop_name}'."),
            ))
        }
    }
}

impl From<Callable> for Literal {
    fn from(callable: Callable) -> Self {
        Self::Callable(callable)
    }
}

impl From<Token> for Literal {
    fn from(token: Token) -> Self {
        use TokenType::*;
        match token.token_type {
            Nil => Self::Nil,
            False => Self::Boolean(false),
            True => Self::Boolean(true),
            Number => Self::Number(token.lexeme.parse().unwrap()),
            String => Self::String(token.lexeme[1..token.lexeme.len() - 1].to_string()),
            _ => unreachable!("literal parsing error"),
        }
    }
}

impl From<Function> for Literal {
    fn from(fun: Function) -> Self {
        let this = None;
        Callable::Function { fun, this }.into()
    }
}

impl From<Class> for Literal {
    fn from(class: Class) -> Self {
        Callable::Class(Rc::new(class)).into()
    }
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Undefined => write!(f, "undefined"),
            Literal::Nil => write!(f, "nil"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Number(n) => write!(f, "{n}"),
            Literal::String(s) => write!(f, "{s}"),
            Literal::Callable(func) => write!(f, "{func}"),
            Literal::ClassInstance(inst) => write!(f, "<{} instance>", inst.borrow().class.name),
        }
    }
}
