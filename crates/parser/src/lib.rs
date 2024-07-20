mod ast;
mod types;
mod pratt_parser;

use std::iter::Iterator;
use std::convert::From;
use tokenizer::token::Token;
use types::Type;
use ast::{Statement, Operator, Expression};

#[derive(Debug, Clone)]
pub struct Program {
    tokens: Vec<Token>,
    cursor: usize,
}

impl From<Vec<Token>> for Program {
    fn from(value: Vec<Token>) -> Self {
        Program{
            tokens: value,
            cursor: 0,
        }
    }
}

impl Iterator for Program {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.tokens.len() {
            return None;
        }

        let token_at = self.tokens[self.cursor].clone();
        self.cursor += 1;
        Some(token_at)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_program_from_token_set() {
        // var a = 10 + 10 * 4
        let tokens = vec![
            Token::Var, 
            Token::Ident(String::from("a")),
            Token::Assign,
            Token::Integer(10),
            Token::Plus,
            Token::Integer(10),
            Token::Star,
            Token::Integer(4),
            Token::SemiColon,
        ];

        let mut program = Program::from(tokens);
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();

        let expected = vec![
            Statement::VariableAssignment{
                ident: String::from("a"),
                expr: Expression::BinaryExpression(
                    Box::new(Expression::Integer(10)),
                    Operator::Add,
                    Box::new(Expression::BinaryExpression(
                        Box::new(Expression::Integer(10)),
                        Operator::Mul,
                        Box::new(Expression::Integer(4)),
                    )),
                ),
            }
        ];
        assert_eq!(expected, stmts);
    }

    #[test]
    fn parse_parenthesis_precedence() {
        // var b = 10 * (5 + 2)
        let tokens = vec![
            Token::Var,
            Token::Ident(String::from("b")),
            Token::Assign,
            Token::Integer(10),
            Token::Star,
            Token::LeftParen,
            Token::Integer(5),
            Token::Plus,
            Token::Integer(2),
            Token::RightParen,
            Token::SemiColon,
        ];

        let mut program = Program::from(tokens);
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();
        
        let expected = vec![
            Statement::VariableAssignment{
                ident: String::from("b"),
                expr: Expression::BinaryExpression(
                    Box::new(Expression::Integer(10)),
                    Operator::Mul,
                    Box::new(Expression::BinaryExpression(
                            Box::new(Expression::Integer(5)),
                            Operator::Add,
                            Box::new(Expression::Integer(2)),
                    )),
                ),
            },
        ];

        assert_eq!(expected, stmts);
    }

    #[test]
    fn parse_function_definition() {
        /*
         * func main() int {
         *  var b = 5 + 2;
         *  println(b);
         *  return 0;
         * }
         */

        let tokens = vec![
            Token::Func,
            Token::Ident(String::from("main")),
            Token::LeftParen,
            Token::RightParen,
            Token::I32Type,
            Token::LeftBraces,
            Token::Var,
            Token::Ident(String::from("b")),
            Token::Assign,
            Token::Integer(5),
            Token::Plus,
            Token::Integer(2),
            Token::SemiColon,
            Token::Ident(String::from("println")),
            Token::LeftParen,
            Token::Ident(String::from("b")),
            Token::RightParen,
            Token::SemiColon,
            Token::Return,
            Token::Integer(0),
            Token::SemiColon,
            Token::RightBraces,
        ];

        let expected = vec![
            Statement::FunctionDefinition{
                name: String::from("main"),
                args: vec![],
                return_ty: types::Type::I32,
                body: Box::new(vec![
                    Statement::VariableAssignment{
                        ident: String::from("b"),
                        expr: Expression::BinaryExpression(
                            Box::new(Expression::Integer(5)),
                            Operator::Add,
                            Box::new(Expression::Integer(2)),
                        ),
                    },
                    Statement::FunctionCall(
                        Expression::Call(
                            Box::new(Expression::Ident(String::from("println"))),
                            Box::new(vec![
                                Expression::Ident(String::from("b")),
                            ]),
                        )
                    ),
                    Statement::Return(Expression::Integer(0)),
                ]),
            }
        ];

        let program = Program::from(tokens);
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();
        assert_eq!(expected, stmts);
    }
}

