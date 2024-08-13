pub mod token;
use std::iter::{Iterator, Peekable};
use std::str::Chars;
use token::Token;

#[derive(Debug, Clone)]
pub struct Program {
    tokens: Vec<Token>,
    cursor: usize,
}

impl From<Vec<Token>> for Program {
    fn from(value: Vec<Token>) -> Self {
        Program {
            tokens: value,
            cursor: 0,
        }
    }
}

impl Iterator for Program {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.tokens.len() {
            return None;
        }

        let token_at = self.tokens[self.cursor].clone();
        self.cursor += 1;
        Some(token_at)
    }
}

fn consume_integer<'a>(c: char, stream: &mut Peekable<Chars<'a>>) -> i32 {
    let mut number = vec![c as u8];
    while let Some(c) = stream.next_if(|c| c.is_numeric()) {
        number.push(c as u8);
    }
    String::from_utf8(number).unwrap().parse::<i32>().unwrap()
}

fn consume_current_lexeme<'a>(c: char, stream: &mut Peekable<Chars<'a>>) -> String {
    let mut lexeme = vec![c as u8];
    while let Some(c) = stream.next_if(|c| c.is_alphanumeric()) {
        lexeme.push(c as u8);
    }
    String::from_utf8(lexeme).unwrap()
}

fn tokenize<'a>(stream: &mut Peekable<Chars<'a>>) -> Vec<Token> {
    let mut tokens = vec![];

    while let Some(c) = stream.next() {
        // if it is a number then advance until it is not anymore
        // this should be expansible to deal with float numbers
        // where you can advance until you find a `.` and if so
        // consume the decimal places
        if c.is_numeric() {
            tokens.push(Token::Integer(consume_integer(c, stream)));
            continue;
        }

        match c {
            ' ' | '\0' | '\r' | '\n' => continue,
            ';' => tokens.push(Token::SemiColon),
            ':' => tokens.push(Token::Colon),
            ',' => tokens.push(Token::Comma),
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => tokens.push(Token::Slash),
            '(' => tokens.push(Token::LeftParen),
            ')' => tokens.push(Token::RightParen),
            '{' => tokens.push(Token::LeftBraces),
            '}' => tokens.push(Token::RightBraces),
            '=' => tokens.push(Token::Assign),
            _ => {
                let current_lexeme = consume_current_lexeme(c, stream);
                let token = match current_lexeme.as_str() {
                    "var" => Token::Var,
                    "func" => Token::Func,
                    "int" => Token::I32Type,
                    "void" => Token::Void,
                    "return" => Token::Return,
                    _ => Token::Ident(current_lexeme.clone()),
                };

                tokens.push(token);
            }
        }
    }

    tokens
}

pub fn read_from_str(source: &'static str) -> Vec<Token> {
    let mut char_iter = source.chars().peekable();
    tokenize(&mut char_iter)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_stream_of_chars_into_tokens() {
        let cases: Vec<(&'static str, Vec<Token>)> = vec![(
            "var a = 150;",
            vec![
                Token::Var,
                Token::Ident(String::from("a")),
                Token::Assign,
                Token::Integer(150),
                Token::SemiColon,
            ],
        )];

        for (source, expected) in cases {
            let tokens = read_from_str(source);
            assert_eq!(expected, tokens);
        }
    }
}
