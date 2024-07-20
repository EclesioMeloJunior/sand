use std::string::ToString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Const,
    Var,
    
    I32Type,
    Integer(i32),

    Ident(String),
    Func,
    Return,

    Colon,
    SemiColon,
    Comma,
    Assign,

    Plus, Minus,
    Star, Slash,

    LeftParen,
    RightParen,
    LeftBraces,
    RightBraces,
}

impl Token {
    pub fn is_ident(&self) -> bool {
        match self {
            Token::Ident(_) => true,
            _ => false,
        }
    }

    pub fn is_type(&self) -> bool {
        match self {
            Token::I32Type => true,
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
