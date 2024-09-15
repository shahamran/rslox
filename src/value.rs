use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::environment as env;
use crate::environment::Environment;
use crate::error::{Error, ErrorKind, Result};
use crate::interpreter::Interpreter;
use crate::scanner::{Token, TokenType};
use crate::stmt;
use crate::SharedRef;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Undefined,
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
    ClassInstance(SharedRef<ClassInstance>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callable {
    Clock,
    Function(Function),
    Class(Rc<Class>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub declaration: stmt::Function,
    pub closure: Option<SharedRef<Environment>>,
    pub this: Option<SharedRef<ClassInstance>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClassInstance {
    pub class: Rc<Class>,
    pub fields: HashMap<String, Value>,
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Clock => write!(f, "<native fn>"),
            Self::Function(fun) => write!(f, "<fn {}>", &fun.declaration.name.lexeme),
            Self::Class(class) => write!(f, "<class {}>", class.name),
        }
    }
}

impl Callable {
    pub fn call(&mut self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Result<Value> {
        match self {
            Callable::Clock => {
                let now = std::time::SystemTime::now();
                let elapsed = now
                    .duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .expect("uh oh");
                Ok(Value::Number(elapsed.as_millis() as f64 / 1000.0))
            }
            Callable::Function(fun) => {
                interpreter.environment = env::new_block(interpreter.environment.clone());
                if let Some(this) = &fun.this {
                    interpreter
                        .environment
                        .borrow_mut()
                        .define("this".to_string(), Value::ClassInstance(Rc::clone(this)));
                }
                assert_eq!(arguments.len(), fun.declaration.params.len());
                for (i, value) in arguments.into_iter().enumerate() {
                    let name = fun.declaration.params[i].lexeme.clone();
                    interpreter.environment.borrow_mut().define(name, value);
                }
                let result = match interpreter.execute_all(&fun.declaration.body) {
                    Ok(()) => Ok(Value::Nil),
                    Err(Error {
                        kind: ErrorKind::Return(value),
                        ..
                    }) => Ok(value),
                    Err(err) => Err(err),
                };
                interpreter.environment = env::remove_block(interpreter.environment.clone());
                result
            }
            Callable::Class(class) => {
                let inst = Value::ClassInstance(Rc::new(RefCell::new(ClassInstance::new(
                    Rc::clone(class),
                ))));
                Ok(inst)
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Callable::Clock => 0,
            Callable::Function(fun) => fun.declaration.params.len(),
            Callable::Class { .. } => 0,
        }
    }
}

impl Function {
    pub fn bind(self, instance: Rc<RefCell<ClassInstance>>) -> Self {
        Self {
            this: Some(instance),
            ..self
        }
    }
}

impl ClassInstance {
    fn new(class: Rc<Class>) -> Self {
        let fields = HashMap::new();
        Self { class, fields }
    }

    pub fn get(&self, name: &Token) -> Result<Value> {
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

impl From<Callable> for Value {
    fn from(callable: Callable) -> Self {
        Self::Callable(callable)
    }
}

impl From<Token> for Value {
    fn from(token: Token) -> Self {
        use TokenType::*;
        match token.token_type {
            Nil => Self::Nil,
            False => Self::Boolean(false),
            True => Self::Boolean(true),
            Number => Self::Number(token.lexeme.parse().unwrap()),
            String => Self::String(token.lexeme[1..token.lexeme.len() - 1].to_string()),
            _ => unreachable!("value parsing error"),
        }
    }
}

impl From<Function> for Value {
    fn from(fun: Function) -> Self {
        Callable::Function(fun).into()
    }
}

impl From<Class> for Value {
    fn from(class: Class) -> Self {
        Callable::Class(Rc::new(class)).into()
    }
}

impl From<stmt::Function> for Function {
    fn from(fun: stmt::Function) -> Self {
        Self {
            declaration: fun,
            closure: None,
            this: None,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Undefined => write!(f, "undefined"),
            Value::Nil => write!(f, "nil"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
            Value::Callable(func) => write!(f, "{func}"),
            Value::ClassInstance(inst) => write!(f, "<{} instance>", inst.borrow().class.name),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lox, SourceId};

    use super::*;

    #[test]
    fn test_closure() {
        let mut lox = Lox::new(SourceId::Test, "".to_string());
        let res = lox.test_run(
            r#"
            fun makeCounter() {
                var i = 0;
                fun count() {
                    i = i + 1;
                    return i;
                }
                return count;
            }
            var counter = makeCounter();
            "#,
        );
        assert_eq!(res, Ok(()));
        assert_eq!(lox.test_eval("counter()"), Ok(Value::Number(1.0)));
        assert_eq!(lox.test_eval("counter()"), Ok(Value::Number(2.0)));
    }
}
