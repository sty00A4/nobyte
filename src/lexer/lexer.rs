use crate::*;
use crate::position::*;
use crate::error::*;
use super::token::*;

pub struct Lexer {
    pub path: Option<String>,
    text: String,
    idx: usize, ln: usize, col: usize
}
impl Lexer {
    pub fn new(path: Option<String>, text: String) -> Self {
        Self { path, text, idx: 0, ln: 0, col: 0 }
    }
    pub fn get(&self) -> Option<char> {
        self.text.get(self.idx..self.idx+1)?.chars().next()
    }
    pub fn pos(&self) -> Position {
        Position::new(self.ln..self.ln+1, self.col..self.col+1)
    }
    pub fn advance(&mut self) {
        if self.get() == Some('\n') {
            self.ln += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        self.idx += 1;
    }

    pub fn next(&mut self) -> Result<Option<Located<Token>>, Error> {
        while let Some(c) = self.get() {
            if !c.is_whitespace() { break }
            self.advance();
        }
        while let Some(c) = self.get() {
            if c != ';' { break }
            self.advance();
            while let Some(c) = self.get() {
                if c == '\n' { break }
                self.advance();
            }
            while let Some(c) = self.get() {
                if !c.is_whitespace() { break }
                self.advance();
            }
        }
        let mut pos = self.pos();
        match self.get() {
            Some(c) => match c {
                '(' => {
                    self.advance();
                    Ok(Some(Located::new(Token::CallIn, pos)))
                }
                ')' => {
                    self.advance();
                    Ok(Some(Located::new(Token::CallOut, pos)))
                }
                '[' => {
                    self.advance();
                    Ok(Some(Located::new(Token::VecIn, pos)))
                }
                ']' => {
                    self.advance();
                    Ok(Some(Located::new(Token::VecOut, pos)))
                }
                '{' => {
                    self.advance();
                    Ok(Some(Located::new(Token::BodyIn, pos)))
                }
                '}' => {
                    self.advance();
                    Ok(Some(Located::new(Token::BodyOut, pos)))
                }
                '#' => {
                    self.advance();
                    Ok(Some(Located::new(Token::Closure, pos)))
                }
                '@' => {
                    self.advance();
                    let mut word = String::new();
                    while let Some(c) = self.get() {
                        if c.is_whitespace() || SYMBOLS.contains(&c) { break }
                        word.push(c);
                        pos.extend(&self.pos());
                        self.advance();
                    }
                    Ok(Some(Located::new(Token::Key(word), pos)))
                }
                '\'' => {
                    self.advance();
                    let Some(mut c) = self.get() else {
                        return expected_char_error!(self.path.clone(), self.pos())
                    };
                    match c {
                        '\\' => {
                            self.advance();
                            let Some(esc) = self.get() else {
                                return expected_char_error!(self.path.clone(), self.pos())
                            };
                            c = match esc {
                                'n' => '\n',
                                't' => '\t',
                                'r' => '\r',
                                _ => esc,
                            };
                        }                           
                        _ => {}
                    }
                    self.advance();
                    if let Some(c) = self.get() {
                        if c != '\'' {
                            return unclosed_char_but_error!(c, self.path.clone(), self.pos())
                        }
                    } else {
                        return unclosed_char_error!(self.path.clone(), self.pos())
                    }
                    pos.extend(&self.pos());
                    self.advance();
                    Ok(Some(Located::new(Token::Char(c), pos)))
                }
                '"' => {
                    self.advance();
                    let mut string = String::new();
                    while let Some(c) = self.get() {
                        if c == '"' { break }
                        match c {
                            '\\' => {
                                self.advance();
                                let Some(mut c) = self.get() else {
                                    return expected_char_error!(self.path.clone(), self.pos())
                                };
                                c = match c {
                                    'n' => '\n',
                                    't' => '\t',
                                    'r' => '\r',
                                    _ => c
                                };
                                string.push(c);
                                self.advance();
                            }                           
                            c => {
                                string.push(c);
                                self.advance();
                            }
                        }
                    }
                    if self.get() == None {
                        return unclosed_string_error!(self.path.clone(), self.pos())
                    }
                    pos.extend(&self.pos());
                    self.advance();
                    Ok(Some(Located::new(Token::String(string), pos)))
                }
                c if c.is_digit(10) => {
                    let mut number = String::from(c);
                    self.advance();
                    while let Some(c) = self.get() {
                        if !c.is_digit(10) { break }
                        number.push(c);
                        pos.extend(&self.pos());
                        self.advance();
                    }
                    if self.get() == Some('.') {
                        number.push('.');
                        pos.extend(&self.pos());
                        self.advance();
                        while let Some(c) = self.get() {
                            if !c.is_digit(10) { break }
                            number.push(c);
                            pos.extend(&self.pos());
                            self.advance();
                        }
                        match number.parse() {
                            Ok(number) => Ok(Some(Located::new(Token::Float(number), pos))),
                            Err(err) => parse_float_error!(number, err, self.path.clone(), pos)
                        }
                    } else {
                        match number.parse() {
                            Ok(number) => Ok(Some(Located::new(Token::Int(number), pos))),
                            Err(err) => parse_int_error!(number, err, self.path.clone(), pos)
                        }
                    }
                }
                c => {
                    let mut word = String::from(c);
                    self.advance();
                    while let Some(c) = self.get() {
                        if c.is_whitespace() || SYMBOLS.contains(&c) { break }
                        word.push(c);
                        pos.extend(&self.pos());
                        self.advance();
                    }
                    Ok(Some(Located::new(Token::from_word(word), pos)))
                }
            }
            None => Ok(None)
        }
    }
    pub fn lex(&mut self) -> Result<Vec<Located<Token>>, Error> {
        let mut tokens = vec![];
        while let Some(token) = self.next()? {
            tokens.push(token);
        }
        Ok(tokens)
    }
}