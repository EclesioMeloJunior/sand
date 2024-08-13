use std::fmt::Display;

use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::Sub => write!(f, "-"),
            Self::Mul => write!(f, "*"),
            Self::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Integer(i32),
    Unary(Operator, Box<Expression>),
    Call(String, Box<Vec<Expression>>),
    BinaryExpression(Box<Expression>, Operator, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    VariableAssignment {
        ident: String,
        ty: Type,
        expr: Expression,
    },
    FunctionDefinition {
        name: String,
        args: Vec<(String, Type)>,
        return_ty: Type,
        body: Box<Vec<Statement>>,
    },
    FunctionCall(Expression),
    Return(Expression),
}
