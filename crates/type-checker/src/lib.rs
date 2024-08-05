#![feature(map_try_insert)]

mod graph;
use std::{boxed, collections::HashMap, path::Iter};

use parser::{
    ast::{Expression, Statement},
    types::Type,
};

#[derive(Debug)]
enum CheckError {
    VariableAlreadyDefined,
}

#[derive(Debug, Clone)]
enum Scope {
    Global,
    Local,
}

#[derive(Debug)]
enum Symbol {
    Function {
        args: Vec<Type>,
        return_ty: Type,
        scope: Scope,
    },
    Variable {
        ty: Type,
        scope: Scope,
    },
}

#[derive(Debug)]
struct Checker {
    symbol_table: HashMap<String, Symbol>,
}

impl Checker {
    fn add_symbol(&mut self, name: String, sym: Symbol) {
        self.symbol_table.insert(name, sym);
    }
}

pub fn default_checker(stmts: Vec<Statement>) -> Result<(), CheckError> {
    let mut checker = Checker {
        symbol_table: HashMap::new(),
    };

    build_symbol_table(&stmts, Scope::Global, &mut checker)?;

    println!("{:?}", checker.symbol_table);

    verify_statements(&stmts, &mut checker)?;
    Ok(())
}

fn build_symbol_table(
    stmts: &[Statement],
    scope: Scope,
    checker: &mut Checker,
) -> Result<(), CheckError> {
    for stmt in stmts {
        match stmt {
            Statement::VariableAssignment { ident, ty, .. } => checker.add_symbol(
                ident.to_string(),
                Symbol::Variable {
                    ty: ty.clone(),
                    scope: scope.clone(),
                },
            ),
            Statement::FunctionDefinition {
                name,
                args,
                return_ty,
                body,
            } => {
                checker.add_symbol(
                    name.to_string(),
                    Symbol::Function {
                        args: args.into_iter().map(|arg| arg.1.clone()).collect(),
                        return_ty: return_ty.clone(),
                        scope: scope.clone(),
                    },
                );

                build_symbol_table(&body, Scope::Local, checker)?;
            }
            _ => continue,
        }
    }

    Ok(())
}

fn verify_statements(stmts: &[Statement], checker: &mut Checker) -> Result<(), CheckError> {
    for stmt in stmts {
        match stmt {
            Statement::FunctionDefinition {
                name,
                args,
                return_ty,
                body,
            } => {
                // Check if the returned type matches the func return type
            }
            _ => todo!(),
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use parser::*;
    use tokenizer::*;

    use crate::default_checker;

    #[test]
    fn function_calls_for_existing_functions() {
        let source = r"
            var a: int = sum(1, 2);

            func sum(a: int, b: int) int {
                var i: int = 2;
                return a + b + i;
            }
        ";

        let tokens = read_from_str(source);
        let mut program = Program::from(tokens).peekable();
        let raw_ast = pratt_parser::PrattParser::parse(&mut program).unwrap();

        default_checker(raw_ast).unwrap();
    }
}
