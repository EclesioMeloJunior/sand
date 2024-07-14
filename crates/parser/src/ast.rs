pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum Expression {
    Integer(i32),
    Unary(Box<Expression>),
    BinaryExpression(Box<Expression>, Operator, Box<Expression>),
}

pub enum Statement {
    VariableAssignment {
        ident: String,
        expr: Box<Expression>
    }
}
