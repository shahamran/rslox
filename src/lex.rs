use crate::Context;

#[derive(Debug, PartialEq, Eq)]
pub struct Lexer<'a> {
    ctx: &'a mut Context,

    // offset in which lexeme starts
    start: usize,

    // current stream offset
    current: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub offset: usize,
    pub length: usize,
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

impl<'a> Lexer<'a> {
    pub fn new(ctx: &'a mut Context) -> Self {
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
                b'!' => match self.matches(b'=') {
                    true => return self.new_token(BangEqual),
                    false => return self.new_token(Bang),
                },
                b'=' => match self.matches(b'=') {
                    true => return self.new_token(EqualEqual),
                    false => return self.new_token(Equal),
                },
                b'<' => match self.matches(b'=') {
                    true => return self.new_token(LessEqual),
                    false => return self.new_token(Less),
                },
                b'>' => match self.matches(b'=') {
                    true => return self.new_token(GreaterEqual),
                    false => return self.new_token(Greater),
                },
                b'/' => match self.slash() {
                    Some(t) => return t,
                    _ => (),
                },
                b' ' | b'\r' | b'\t' | b'\n' => {}
                b'"' => match self.string() {
                    Some(t) => return t,
                    _ => (),
                },
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
            lexeme: "".to_string(),
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
            lexeme: self.ctx.source[self.start..self.current].to_string(),
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

    fn slash(&mut self) -> Option<Token> {
        match self.peek() {
            Some(b'/') => {
                self.advance();
                while self.peek()? != b'\n' {
                    self.advance();
                }
            }
            Some(b'*') => {
                self.advance();
                while (self.peek()?, self.peek_next()?) != (b'*', b'/') {
                    self.advance();
                }
                self.current += 2;
            }
            _ => return Some(self.new_token(TokenType::Slash)),
        }
        None
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

#[cfg(test)]
mod tests {
    use crate::SourceId;

    use super::TokenType::*;
    use super::*;

    fn all_tokens(source: &str) -> Vec<Token> {
        let source = source.to_string();
        let mut ctx = Context::new(SourceId::Prompt, source);
        let mut lexer = Lexer::new(&mut ctx);
        let mut tokens = Vec::new();
        let mut is_eof = false;
        while !is_eof {
            let t = lexer.next_token();
            is_eof = t.is_eof();
            tokens.push(t);
        }
        tokens
    }

    fn first_token(source: &str) -> Token {
        all_tokens(source)
            .into_iter()
            .next()
            .expect("source has no tokens")
    }

    #[test]
    fn test_slash() {
        assert_eq!(
            all_tokens("1 / 2")[1],
            token(Slash, "/", Offset(2), Length(1))
        );
        assert!(all_tokens("1 + () //")
            .into_iter()
            .all(|t| t.token_type != Slash));
        assert_eq!(first_token("/"), token(Slash, "/", Offset(0), Length(1)));
        assert_eq!(first_token("//"), eof(Offset(2)));
        assert_eq!(first_token("// \n"), eof(Offset(4)));
        assert_eq!(first_token("/**/"), eof(Offset(4)));
        assert_eq!(
            all_tokens("/*/*/*/"),
            vec![
                token(Star, "*", Offset(5), Length(1)),
                token(Slash, "/", Offset(6), Length(1)),
                eof(Offset(7)),
            ]
        );
    }

    fn token(token_type: TokenType, lexeme: &str, offset: Offset, length: Length) -> Token {
        Token {
            token_type,
            lexeme: lexeme.to_string(),
            offset: offset.0,
            length: length.0,
        }
    }

    fn eof(offset: Offset) -> Token {
        token(Eof, "", offset, Length(0))
    }

    struct Offset(usize);
    struct Length(usize);
}
