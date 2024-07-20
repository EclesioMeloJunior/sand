use crate::types::Type;

#[derive(Debug, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Ident(String),
    Integer(i32),
    Unary(Box<Expression>),
    Call(Box<Expression>, Box<Vec<Expression>>),
    BinaryExpression(Box<Expression>, Operator, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    VariableAssignment {
        ident: String,
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
