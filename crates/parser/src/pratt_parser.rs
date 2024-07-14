use std::iter::{Peekable, Iterator};
use tokenizer::token::Token;
use crate::ast::{Expression, Statement, Operator};

#[derive(Debug)]
pub enum Error {
    UnexpectedEnd,
    ExpectedIdent,
    EmptyExpression,
    MissingAssignmentToken,
    HeadHandlerNotImplemented(Token),
    TailHandlerNotImplemented(Token),
    NonexistentPrecedence(Token),
}

type ParseResult<T> = Result<T, Error>;

pub trait TokenHandler<I> where I: Iterator<Item = Token> {
    fn head_handler(&self, program: &mut Peekable<I>) -> ParseResult<Expression>;
    fn tail_handler(&self, program: &mut Peekable<I>, lhs: Expression) -> ParseResult<Expression>;
    fn precedence(self) -> ParseResult<usize>;
}

impl<I> TokenHandler<I> for Token where I: Iterator<Item = Token> {
    fn head_handler(&self, program: &mut Peekable<I>) -> ParseResult<Expression> {
        match self {
            Token::Integer(int_value) => Ok(Expression::Integer(int_value.clone())),
            _ => Err(Error::HeadHandlerNotImplemented(self.clone())),
        }
    }
    
    fn tail_handler(&self, program: &mut Peekable<I>, lhs: Expression) -> ParseResult<Expression> {
        match self {
            Token::Plus => parse_binary_expression(program, lhs, Operator::Add),
            Token::Minus => parse_binary_expression(program, lhs, Operator::Sub),
            Token::Star => parse_binary_expression(program, lhs, Operator::Mul),
            Token::Slash => parse_binary_expression(program, lhs, Operator::Div),
            _ => Err(Error::TailHandlerNotImplemented(self.clone())),
        }
    }

    fn precedence(self) -> ParseResult<usize> {
        match self {
            Token::Plus | Token::Minus => Ok(1),
            Token::Star | Token::Slash => Ok(2),
            _ => Err(Error::NonexistentPrecedence(self.clone()))
        }
    }
}

pub fn parse_statements<I>(program: &mut Peekable<I>) -> ParseResult<Vec<Statement>> where I: Iterator<Item = Token> {
    let mut stmts = Vec::new();
    while let Some(token) = program.next() {
        match token {
            Token::Var => stmts.push(parse_var_assignment(program)?),
            _ => todo!("statement not supported yet: {:?}", token),
        }
    }

    Ok(stmts)
}

pub fn parse_expression<I>(program: &mut Peekable<I>, curr_precedence: usize) -> ParseResult<Expression> where I: Iterator<Item = Token> {
    let mut curr_token = program.next().ok_or(Error::EmptyExpression)?;
    let mut lhs = curr_token.head_handler(program)?;
    
    while program.peek().map_or(0, |tok| <Token as TokenHandler<I>>::precedence(tok.clone()).unwrap_or(0)) > curr_precedence {
        curr_token = program.next().ok_or(Error::UnexpectedEnd)?;
        lhs = curr_token.tail_handler(program, lhs)?;
    }

    Ok(lhs)
}

fn parse_var_assignment<I>(program: &mut Peekable<I>) -> Result<Statement, Error> where I: Iterator<Item = Token> {
    let ident = program.next_if(|tok| tok.is_ident()).ok_or(Error::ExpectedIdent)?;
    program.next_if_eq(&Token::Assign).ok_or(Error::MissingAssignmentToken)?;
    
    let expr = parse_expression(program, 0)?;

    Ok(Statement::VariableAssignment{
        ident: ident.to_string(),
        expr: Box::new(expr),
    })
}

fn parse_binary_expression<I>(program: &mut Peekable<I>, lhs: Expression, op: Operator) -> ParseResult<Expression>
    where I: Iterator<Item = Token> {
    let rhs = parse_expression(program, 0)?;
    Ok(Expression::BinaryExpression(Box::new(lhs), op, Box::new(rhs)))
}
