use std::string::ToString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Const,
    Var,
    
    ExprType,
    IntegerType,
    Integer(i32),

    Ident(String),
    Func,

    Colon,
    Comma,
    Assign,

    Plus, Minus,
    Star, Slash,

    LeftParen,
    RightParen
}

impl Token {
    pub fn is_ident(&self) -> bool {
        match self {
            Token::Ident(_) => true,
            _ => false,
        }
    }
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self {
            Token::Ident(s) => s.clone(),
            _ => String::from("<?>")
        }
    }
}
