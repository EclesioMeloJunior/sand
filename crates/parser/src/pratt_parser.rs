use crate::ast::{Expression, Operator, Statement};
use crate::types::Type;
use crate::Parser;
use std::any::TypeId;
use std::iter::{Iterator, Peekable};
use tokenizer::token::Token;

#[derive(Debug)]
pub enum Error {
    UnexpectedEnd,
    ExpectedIdent,
    ExpectedColon,
    ExpectedSemiColon,
    ExpectedParamType,
    ExpectedVarType,
    ExpectedReturnType,
    ExpectedFunctionCall,
    EmptyExpression,
    MissingAssignmentToken,
    MissingLeftParentheses,
    MissingRightParentheses,
    MissingFunctionCallIdentifier,
    MissingLeftBraces,
    UnexpectedTokenAsType(Token),
    HeadHandlerNotImplemented(Token),
    TailHandlerNotImplemented(Token),
    NonexistentPrecedence(Token),
}

type ParseResult<T> = Result<T, Error>;

pub struct PrattParser;

impl<I> Parser<Vec<Statement>, I> for PrattParser
where
    I: Iterator<Item = Token>,
{
    type Error = Error;

    fn parse(program: &mut Peekable<I>) -> Result<Vec<Statement>, Self::Error> {
        parse_statements(program)
    }
}

pub trait TokenHandler<I>
where
    I: Iterator<Item = Token>,
{
    fn head_handler(&self, program: &mut Peekable<I>) -> ParseResult<Expression>;
    fn tail_handler(&self, program: &mut Peekable<I>, lhs: Expression) -> ParseResult<Expression>;
    fn precedence(self) -> ParseResult<usize>;
}

impl<I> TokenHandler<I> for Token
where
    I: Iterator<Item = Token>,
{
    fn head_handler(&self, program: &mut Peekable<I>) -> ParseResult<Expression> {
        match self {
            Token::Ident(s) => Ok(Expression::Ident(s.to_string())),
            Token::Integer(int_value) => Ok(Expression::Integer(int_value.clone())),
            Token::LeftParen => parse_grouped_expression(program),
            _ => Err(Error::HeadHandlerNotImplemented(self.clone())),
        }
    }

    fn tail_handler(&self, program: &mut Peekable<I>, lhs: Expression) -> ParseResult<Expression> {
        match self {
            Token::Plus => parse_binary_expression(program, lhs, Operator::Add),
            Token::Minus => parse_binary_expression(program, lhs, Operator::Sub),
            Token::Star => parse_binary_expression(program, lhs, Operator::Mul),
            Token::Slash => parse_binary_expression(program, lhs, Operator::Div),
            Token::LeftParen => parse_call_expression(program, lhs),
            _ => Err(Error::TailHandlerNotImplemented(self.clone())),
        }
    }

    fn precedence(self) -> ParseResult<usize> {
        match self {
            Token::Comma | Token::RightParen | Token::SemiColon => Ok(0),
            Token::Plus | Token::Minus => Ok(1),
            Token::Star | Token::Slash => Ok(2),
            Token::LeftParen => Ok(3),
            _ => Err(Error::NonexistentPrecedence(self.clone())),
        }
    }
}

pub fn parse_statements<I>(program: &mut Peekable<I>) -> ParseResult<Vec<Statement>>
where
    I: Iterator<Item = Token>,
{
    let mut stmts = Vec::new();
    while let Some(token) = program.peek() {
        match *token {
            Token::Func => stmts.push(parse_func_definition(program)?),
            Token::Var => stmts.push(parse_var_assignment(program)?),
            Token::RightBraces => break,
            Token::Ident(_) => stmts.push(parse_function_call_stmt(program)?),
            Token::Return => {
                // consume the return token
                program.next().unwrap();
                stmts.push(Statement::Return(parse_expression(program, 0)?));
                program
                    .next_if_eq(&Token::SemiColon)
                    .ok_or(Error::ExpectedSemiColon)?;
            }
            _ => todo!("statement not supported yet: {:?}", token),
        }
    }

    Ok(stmts)
}

pub fn parse_expression<I>(
    program: &mut Peekable<I>,
    curr_precedence: usize,
) -> ParseResult<Expression>
where
    I: Iterator<Item = Token>,
{
    let mut curr_token = program.next().ok_or(Error::EmptyExpression)?;
    let mut lhs = curr_token.head_handler(program)?;

    while program.peek().map_or(0, |tok| {
        <Token as TokenHandler<I>>::precedence(tok.clone()).unwrap_or(0)
    }) > curr_precedence
    {
        curr_token = program.next().ok_or(Error::UnexpectedEnd)?;
        lhs = curr_token.tail_handler(program, lhs)?;
    }

    Ok(lhs)
}

fn parse_func_definition<I>(program: &mut Peekable<I>) -> ParseResult<Statement>
where
    I: Iterator<Item = Token>,
{
    program.next_if_eq(&Token::Func).unwrap();
    let ident = program
        .next_if(|tok| tok.is_ident())
        .ok_or(Error::ExpectedIdent)?;
    let fn_args = parse_func_args(program)?;
    let return_ty = Type::try_from(
        program
            .next_if(|tok| tok.is_type())
            .ok_or(Error::ExpectedReturnType)?,
    )?;

    program
        .next_if_eq(&Token::LeftBraces)
        .ok_or(Error::MissingLeftBraces)?;
    let body = parse_statements(program)?;

    Ok(Statement::FunctionDefinition {
        name: ident.to_string(),
        args: fn_args,
        return_ty,
        body: Box::new(body),
    })
}

fn parse_func_args<I>(program: &mut Peekable<I>) -> Result<Vec<(String, Type)>, Error>
where
    I: Iterator<Item = Token>,
{
    program
        .next_if_eq(&Token::LeftParen)
        .ok_or(Error::MissingLeftParentheses)?;
    let mut args: Vec<(String, Type)> = vec![];
    let mut should_end = false;
    loop {
        if let Some(_) = program.next_if_eq(&Token::RightParen) {
            break;
        } else if should_end {
            return Err(Error::MissingRightParentheses);
        }

        let param_name = program
            .next_if(|tok| tok.is_ident())
            .ok_or(Error::ExpectedIdent)?;
        program
            .next_if_eq(&Token::Colon)
            .ok_or(Error::ExpectedColon)?;
        let param_type = program
            .next_if(|tok| tok.is_type())
            .ok_or(Error::ExpectedParamType)?;
        args.push((param_name.to_string(), Type::try_from(param_type)?));

        // if the next token is Token::Comma then we can
        // iterate again, otherwise if the next token is
        // not a Token::Comma then the next token should
        // be a right paren ending the args session
        if program.next_if_eq(&Token::Comma).is_none() {
            should_end = true;
        }
    }

    Ok(args)
}

fn parse_var_assignment<I>(program: &mut Peekable<I>) -> ParseResult<Statement>
where
    I: Iterator<Item = Token>,
{
    program.next_if_eq(&Token::Var).unwrap();
    let ident = program
        .next_if(|tok| tok.is_ident())
        .ok_or(Error::ExpectedIdent)?;

    program
        .next_if_eq(&Token::Colon)
        .ok_or(Error::ExpectedVarType)?;

    let var_type = Type::try_from(
        program
            .next_if(|tok| tok.is_type())
            .ok_or(Error::ExpectedVarType)?,
    )?;

    program
        .next_if_eq(&Token::Assign)
        .ok_or(Error::MissingAssignmentToken)?;

    let expr = parse_expression(program, 0)?;
    program
        .next_if_eq(&Token::SemiColon)
        .ok_or(Error::ExpectedSemiColon)?;

    Ok(Statement::VariableAssignment {
        ident: ident.to_string(),
        ty: var_type,
        expr,
    })
}

fn parse_function_call_stmt<I>(program: &mut Peekable<I>) -> ParseResult<Statement>
where
    I: Iterator<Item = Token>,
{
    let expr = parse_expression(program, 0)?;
    match expr {
        Expression::Call(..) => {}
        _ => return Err(Error::ExpectedFunctionCall),
    }
    program.next_if_eq(&Token::SemiColon);

    Ok(Statement::FunctionCall(expr))
}

fn parse_binary_expression<I>(
    program: &mut Peekable<I>,
    lhs: Expression,
    op: Operator,
) -> ParseResult<Expression>
where
    I: Iterator<Item = Token>,
{
    let rhs = parse_expression(program, 0)?;
    Ok(Expression::BinaryExpression(
        Box::new(lhs),
        op,
        Box::new(rhs),
    ))
}

fn parse_grouped_expression<I>(program: &mut Peekable<I>) -> ParseResult<Expression>
where
    I: Iterator<Item = Token>,
{
    let expr = parse_expression(program, 0);
    program
        .next_if_eq(&Token::RightParen)
        .ok_or(Error::MissingRightParentheses)?;
    expr
}

fn parse_call_expression<I>(program: &mut Peekable<I>, lhs: Expression) -> ParseResult<Expression>
where
    I: Iterator<Item = Token>,
{
    // ensure that before the open paren there is
    // an identifier and not other kind of expression
    let fn_name = match lhs {
        Expression::Ident(fn_name) => fn_name,
        _ => return Err(Error::MissingFunctionCallIdentifier),
    };

    let mut call_params: Vec<Expression> = vec![];
    let mut should_end = false;
    loop {
        if let Some(_) = program.next_if_eq(&Token::RightParen) {
            break;
        } else if should_end {
            return Err(Error::MissingRightParentheses);
        }

        call_params.push(parse_expression(program, 0)?);
        if program.next_if_eq(&Token::Comma).is_none() {
            should_end = true;
        }
    }

    Ok(Expression::Call(fn_name, Box::new(call_params)))
}
