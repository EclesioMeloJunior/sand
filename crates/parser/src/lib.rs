mod ast;
mod pratt_parser;

use std::iter::Iterator;
use std::convert::From;
use tokenizer::token::Token;

#[derive(Debug, Clone)]
pub struct Program {
    tokens: Vec<Token>,
    cursor: usize,
}

impl From<Vec<Token>> for Program {
    fn from(value: Vec<Token>) -> Self {
        Program{
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
        return Some(token_at);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_program_from_token_set() {
        let tokens = vec![
            Token::Var, 
            Token::Ident(String::from("a")),
            Token::Assign,
            Token::Integer(10),
            Token::Plus,
            Token::Integer(10),
            Token::Star,
            Token::Integer(4),
        ];

        let mut program = Program::from(tokens);
        for t in program.clone().into_iter() {   
            println!("{:?}", t);
        }
        
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();
        println!("{:?}", stmts); 
    }
}

