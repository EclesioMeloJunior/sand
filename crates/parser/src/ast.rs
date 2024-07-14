#[derive(Debug)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug)]
pub enum Expression {
    Integer(i32),
    Unary(Box<Expression>),
    BinaryExpression(Box<Expression>, Operator, Box<Expression>),
}

#[derive(Debug)]
pub enum Statement {
    VariableAssignment {
        ident: String,
        expr: Box<Expression>
    }
}
