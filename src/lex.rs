use crate::Context;

#[derive(Debug, PartialEq, Eq)]
pub struct Lexer<'a, 'b> {
    ctx: &'a mut Context<'b>,

    // offset in which lexeme starts
    start: usize,

    // current stream offset
    current: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    token_type: TokenType,
    offset: usize,
    length: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    String(String),
    Number(f64),

    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

impl<'a, 'b> Lexer<'a, 'b> {
    pub fn new(ctx: &'a mut Context<'b>) -> Self {
        Self {
            ctx,
            start: 0,
            current: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        use TokenType::*;
        self.start = self.current;
        while let Some(c) = self.advance() {
            match c {
                b'(' => return self.new_token(LeftParen),
                b')' => return self.new_token(RightParen),
                b'{' => return self.new_token(LeftBrace),
                b'}' => return self.new_token(RightBrace),
                b',' => return self.new_token(Comma),
                b'.' => return self.new_token(Dot),
                b'-' => return self.new_token(Minus),
                b'+' => return self.new_token(Plus),
                b';' => return self.new_token(Semicolon),
                b'*' => return self.new_token(Star),
                b'!' => {
                    return if self.matches(b'=') {
                        self.new_token(BangEqual)
                    } else {
                        self.new_token(Bang)
                    };
                }
                b'=' => {
                    return if self.matches(b'=') {
                        self.new_token(EqualEqual)
                    } else {
                        self.new_token(Equal)
                    };
                }
                b'<' => {
                    return if self.matches(b'=') {
                        self.new_token(LessEqual)
                    } else {
                        self.new_token(Less)
                    };
                }
                b'>' => {
                    return if self.matches(b'=') {
                        self.new_token(GreaterEqual)
                    } else {
                        self.new_token(Greater)
                    };
                }
                b'/' => {
                    if self.matches(b'/') {
                        while !matches!(self.peek(), Some(b'\n') | None) {
                            self.advance();
                        }
                    } else {
                        return self.new_token(Slash);
                    }
                }
                b' ' | b'\r' | b'\t' | b'\n' => {}
                b'"' => {
                    if let Some(t) = self.string() {
                        return t;
                    }
                }
                c if is_digit(c) => return self.number(),
                c if is_alpha(c) => return self.identifier(),
                _ => self
                    .ctx
                    .report_error(self.start, self.current, "Unexpected character."),
            };
            self.start = self.current;
        }
        Token {
            token_type: Eof,
            offset: self.current - 1,
            length: 0,
        }
    }

    fn advance(&mut self) -> Option<u8> {
        self.current += 1;
        self.ctx.source.as_bytes().get(self.current - 1).copied()
    }

    fn matches(&mut self, c: u8) -> bool {
        if self.peek() == Some(c) {
            self.current += 1;
            true
        } else {
            false
        }
    }

    fn peek(&self) -> Option<u8> {
        self.ctx.source.as_bytes().get(self.current).copied()
    }

    fn peek_next(&self) -> Option<u8> {
        self.ctx.source.as_bytes().get(self.current + 1).copied()
    }

    fn new_token(&self, token_type: TokenType) -> Token {
        Token {
            token_type,
            offset: self.start,
            length: self.current - self.start,
        }
    }

    fn string(&mut self) -> Option<Token> {
        while !matches!(self.peek(), Some(b'"') | None) {
            self.advance();
        }
        if self.peek().is_none() {
            self.ctx
                .report_error(self.start, self.current, "Unterminated string.");
            return None;
        }
        self.advance(); // closing ".
        let lexeme = self.read_lexeme();
        Some(self.new_token(TokenType::String(lexeme[1..lexeme.len() - 1].to_string())))
    }

    fn read_lexeme(&self) -> &str {
        &self.ctx.source[self.start..self.current]
    }

    fn number(&mut self) -> Token {
        while matches!(self.peek(), Some(b'0'..=b'9')) {
            self.advance();
        }
        if self.peek() == Some(b'.') && matches!(self.peek_next(), Some(b'0'..=b'9')) {
            self.advance();
            while matches!(self.peek(), Some(b'0'..=b'9')) {
                self.advance();
            }
        }
        let lexeme = self.read_lexeme();
        self.new_token(TokenType::Number(lexeme.parse().unwrap()))
    }

    fn identifier(&mut self) -> Token {
        use TokenType::*;
        while self.peek().is_some_and(is_alphanumeric) {
            self.advance();
        }
        let lexeme = self.read_lexeme();
        let token_type = match lexeme {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Identifier(lexeme.to_string()),
        };
        self.new_token(token_type)
    }
}

impl Token {
    pub fn is_eof(&self) -> bool {
        self.token_type == TokenType::Eof
    }
}

const fn is_digit(c: u8) -> bool {
    matches!(c, b'0'..=b'9')
}

const fn is_alpha(c: u8) -> bool {
    matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

const fn is_alphanumeric(c: u8) -> bool {
    is_alpha(c) || is_digit(c)
}
