use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::environment::EnvironmentRef;
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
    closure: EnvironmentRef,
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
                let environment = EnvironmentRef::with_parent(fun.closure.clone());
                assert_eq!(arguments.len(), fun.declaration.params.len());
                for (i, value) in arguments.into_iter().enumerate() {
                    let name = fun.declaration.params[i].lexeme.clone();
                    environment.borrow_mut().define(name, value);
                }
                let fun_kind = fun.declaration.kind;
                let get_this = || {
                    fun.closure
                        .get_by_name("this")
                        .expect("initializer without 'this' local.")
                };
                match interpreter.execute_block(&fun.declaration.body, environment) {
                    Ok(()) => match fun_kind {
                        stmt::FunctionType::Initializer => Ok(get_this()),
                        _ => Ok(Value::Nil),
                    },
                    Err(Error {
                        kind: ErrorKind::Return(value),
                        ..
                    }) => match fun_kind {
                        stmt::FunctionType::Initializer => Ok(get_this()),
                        _ => Ok(value),
                    },
                    Err(err) => Err(err),
                }
            }
            Callable::Class(class) => {
                let inst = Rc::new(RefCell::new(ClassInstance::new(Rc::clone(class))));
                if let Some(initializer) = class.methods.get("init") {
                    Callable::Function(initializer.clone().bind(Rc::clone(&inst)))
                        .call(interpreter, arguments)?;
                }
                Ok(Value::ClassInstance(Rc::clone(&inst)))
            }
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Callable::Clock => 0,
            Callable::Function(fun) => fun.declaration.params.len(),
            Callable::Class(cls) => match cls.methods.get("init") {
                Some(init) => init.declaration.params.len(),
                None => 0,
            },
        }
    }
}

impl Function {
    pub fn new(declaration: stmt::Function, closure: EnvironmentRef) -> Self {
        Self {
            declaration,
            closure,
        }
    }

    pub fn bind(self, instance: Rc<RefCell<ClassInstance>>) -> Self {
        let closure = EnvironmentRef::with_parent(self.closure);
        closure
            .borrow_mut()
            .define("this".to_string(), Value::ClassInstance(instance));
        Self { closure, ..self }
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

    #[test]
    fn test_method() {
        let source = r#"
            class Bacon {
                eat() {
                    return "Crunch crunch crunch!";
                }
            }
            "#;
        let mut lox = Lox::new(SourceId::Test, "".to_string());
        assert_eq!(lox.test_run(source), Ok(()));
        assert_eq!(
            lox.test_eval("Bacon().eat()"),
            Ok(Value::String("Crunch crunch crunch!".into()))
        );
    }

    #[test]
    fn test_bound_method() {
        let source = r#"
            class Cake {
              taste() {
                var adjective = "delicious";
                return "The " + this.flavor + " cake is " + adjective + "!";
              }
            }

            var cake = Cake();
            cake.flavor = "German chocolate";
            "#;
        let mut lox = Lox::new(SourceId::Test, "".to_string());
        assert_eq!(lox.test_run(source), Ok(()));
        assert_eq!(
            lox.test_eval("cake.taste()"),
            Ok(Value::String(
                "The German chocolate cake is delicious!".into()
            ))
        );
    }

    #[test]
    fn test_method_complex() {
        let source = r#"
            class Thing {
              getCallback() {
                fun localFunction() {
                  return this;
                }

                return localFunction;
              }
            }

            var callback = Thing().getCallback();
            "#;
        let mut lox = Lox::new(SourceId::Test, "".to_string());
        assert_eq!(lox.test_run(source), Ok(()));
        assert!(matches!(
            lox.test_eval("callback()"),
            Ok(Value::ClassInstance(..))
        ));
    }
}
