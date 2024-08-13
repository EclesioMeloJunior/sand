use std::fmt::Display;

use crate::pratt_parser::Error;
use tokenizer::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    I32,
    I64,
    U32,
    U64,
    Void,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::Void => write!(f, "void"),
        }
    }
}

impl TryFrom<Token> for Type {
    type Error = Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::I32Type => Ok(Type::I32),
            Token::Void => Ok(Type::Void),
            _ => Err(Error::UnexpectedTokenAsType(value)),
        }
    }
}
