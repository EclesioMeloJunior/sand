use std::iter::{Peekable, Iterator};
use tokenizer::token::Token;
use crate::ast::{Expression, Statement};

#[derive(Debug)]
pub enum Error {
    UnexpectedEnd,
    ExpectedIdent,
    EmptyExpression,
    MissingAssignmentToken
}

type ParseResult<T> = Result<Option<T>, Error>;

pub trait TokenHandler<I> where I: Iterator<Item = Token> {
    fn head_handler(&self, program: &mut Peekable<I>) -> ParseResult<Expression>;
    fn tail_handler(&self, program: &mut Peekable<I>, lhs: ParseResult<Expression>) -> ParseResult<Expression>;
    fn precedence(self) -> usize;
}

impl<I> TokenHandler<I> for Token where I: Iterator<Item = Token> {
    fn head_handler(&self, program: &mut Peekable<I>) -> ParseResult<Expression> {
        match self {
            Token::Integer(int_value) => Ok(Some(Expression::Integer(int_value.clone()))),
            _ => todo!("missing head_handler for :{:?}", self),
        }
    }
    
    fn tail_handler(&self, program: &mut Peekable<I>, lhs: ParseResult<Expression>) -> ParseResult<Expression> {
        todo!("implement tail_handler");
    }

    fn precedence(self) -> usize {
        0 
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

    Ok(Some(stmts))
}

pub fn parse_expression<I>(program: &mut Peekable<I>, curr_precedence: usize) -> ParseResult<Expression> where I: Iterator<Item = Token> {
    let mut curr_token = program.next();
    if curr_token.is_none() { return Ok(None); }
    
    let mut lhs = curr_token.unwrap().head_handler(program);
    
    while program.peek().map_or(0, |tok| <Token as TokenHandler<I>>::precedence(tok.clone())) > curr_precedence {
        curr_token = Some(program.next().map_or_else(|| Err(Error::UnexpectedEnd) , |tok| Ok(tok))?);
        lhs = curr_token.unwrap().tail_handler(program, lhs);
    }

    lhs
}

fn parse_var_assignment<I>(program: &mut Peekable<I>) -> Result<Statement, Error> where I: Iterator<Item = Token> {
    let ident = program.next_if(|tok| tok.is_ident()).ok_or(Error::ExpectedIdent)?;
    program.next_if_eq(&Token::Assign).ok_or(Error::MissingAssignmentToken)?;
    
    let expr = parse_expression(program, 0)?;
       

    Ok(Statement::VariableAssignment{
        ident: ident.to_string(),
        expr: Box::new(expr.ok_or(Error::EmptyExpression)?),
    })
}
