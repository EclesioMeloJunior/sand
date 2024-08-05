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

impl TryFrom<Token> for Type {
    type Error = Error;

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::I32Type => Ok(Type::I32),
            _ => Err(Error::UnexpectedTokenAsType(value)),
        }
    }
}
