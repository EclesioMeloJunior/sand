pub mod ast;
pub mod pratt_parser;
pub mod types;

use std::iter::Peekable;

use ast::{Expression, Operator, Statement};
use tokenizer::{token::Token, Program};
use types::Type;

pub trait Parser<T, I>
where
    I: Iterator<Item = Token>,
{
    type Error;
    fn parse(program: &mut Peekable<I>) -> Result<T, Self::Error>;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_program_from_token_set() {
        let tokens = tokenizer::read_from_str("var a: int = 10 + 10 * 4;");
        let program = Program::from(tokens);
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();

        let expected = vec![Statement::VariableAssignment {
            ident: String::from("a"),
            ty: Type::I32,
            expr: Expression::BinaryExpression(
                Box::new(Expression::Integer(10)),
                Operator::Add,
                Box::new(Expression::BinaryExpression(
                    Box::new(Expression::Integer(10)),
                    Operator::Mul,
                    Box::new(Expression::Integer(4)),
                )),
            ),
        }];
        assert_eq!(expected, stmts);
    }

    #[test]
    fn parse_parenthesis_precedence() {
        // var b = 10 * (5 + 2)
        let tokens = tokenizer::read_from_str("var b: int = 10 * (5 + 2);");
        let program = Program::from(tokens);
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();

        let expected = vec![Statement::VariableAssignment {
            ident: String::from("b"),
            ty: Type::I32,
            expr: Expression::BinaryExpression(
                Box::new(Expression::Integer(10)),
                Operator::Mul,
                Box::new(Expression::BinaryExpression(
                    Box::new(Expression::Integer(5)),
                    Operator::Add,
                    Box::new(Expression::Integer(2)),
                )),
            ),
        }];

        assert_eq!(expected, stmts);
    }

    #[test]
    fn parse_function_definition() {
        let tokens = tokenizer::read_from_str(
            r#"
            func main() int {
                var b: int = 5 + 2;
                println(b);
                return 0;
            }
        "#,
        );

        let expected = vec![Statement::FunctionDefinition {
            name: String::from("main"),
            args: vec![],
            return_ty: types::Type::I32,
            body: Box::new(vec![
                Statement::VariableAssignment {
                    ident: String::from("b"),
                    ty: Type::I32,
                    expr: Expression::BinaryExpression(
                        Box::new(Expression::Integer(5)),
                        Operator::Add,
                        Box::new(Expression::Integer(2)),
                    ),
                },
                Statement::FunctionCall(Expression::Call(
                    String::from("println"),
                    Box::new(vec![Expression::Ident(String::from("b"))]),
                )),
                Statement::Return(Expression::Integer(0)),
            ]),
        }];

        let program = Program::from(tokens);
        let mut peekable_program = program.peekable();
        let stmts = pratt_parser::parse_statements(&mut peekable_program).unwrap();
        assert_eq!(expected, stmts);
    }
}
