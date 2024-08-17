#![feature(map_try_insert)]

use std::{
    boxed,
    collections::{hash_map::OccupiedEntry, hash_set::SymmetricDifference, HashMap},
    env::args,
    fmt::{write, Display},
    path::Iter,
};

use parser::{
    ast::{Expression, Operator, Statement},
    types::Type,
};

#[derive(Debug)]
pub enum CheckError {
    ReturnOutsideScope,
    VariableAlreadyDefined(String, Symbol),
    // 0: variable's type
    // 1: expression type
    VariableAssignmentTypeMismatch(String, Type, Type),
    MismatchUnaryOperatorType(Operator, Type),
    MismatchBinaryOperatorType(Operator, Type, Type),
    SymbolNotFound(String),
    // 0: type passed
    // 1: type expected
    ReturnTypeMismatch(String, Type, Type),
    NotCallable(String),
    // 0: param length passed
    // 1: param length defined
    WrongParamLen(String, usize, usize),
    // 0: types passed
    // 1: types defined
    WrongParamsType(String, Vec<Type>, Vec<Type>),
}

impl Display for CheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReturnOutsideScope => write!(f, "return must be inside a function"),
            Self::VariableAlreadyDefined(var_name, sym) => {
                write!(f, "variable {} already defined as: {}", var_name, sym)
            }
            Self::VariableAssignmentTypeMismatch(var_name, corr, wrong) => write!(
                f,
                "type mismatch for variable {}, expected {}, got {}",
                var_name, corr, wrong
            ),
            Self::MismatchUnaryOperatorType(op, ty) => {
                write!(f, "type {} not supported by unary operator {}", ty, op)
            }
            Self::MismatchBinaryOperatorType(op, lhs, rhs) => {
                write!(
                    f,
                    "types {}, {} not supported by binary operator {}",
                    lhs, rhs, op
                )
            }
            Self::SymbolNotFound(name) => write!(f, "symbol not found for {}", name),
            Self::ReturnTypeMismatch(fn_name, wrong, corr) => write!(
                f,
                "return type mismatch at function {}, expected {}, got {}",
                fn_name, corr, wrong
            ),
            Self::NotCallable(name) => write!(f, "symbol {} is not callable", name),
            Self::WrongParamLen(fn_name, wrong, corr) => write!(
                f,
                "wrong num of args at function {}, expected {}, got {}",
                fn_name, corr, wrong
            ),
            Self::WrongParamsType(fn_name, wrong, corr) => write!(
                f,
                "wrong num of args at function {}, expected {}, got {}",
                fn_name,
                corr.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(" "),
                wrong
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(" "),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Scope {
    Global,
    Local(String),
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Scope::Global => write!(f, "Global"),
            Scope::Local(fn_name) => write!(f, "{}", fn_name),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Function { args: Vec<Type>, return_ty: Type },
    Variable { ty: Type },
}

impl Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Function { args, return_ty } => {
                write!(f, "func ( ")?;
                for ty in args {
                    write!(f, "{}", ty)?;
                }
                write!(f, " ) ")?;
                write!(f, "{}", return_ty)
            }
            Symbol::Variable { ty } => write!(f, "var {}", ty),
        }
    }
}

impl Symbol {
    fn get_type(&self) -> Type {
        match self {
            Self::Function { return_ty, .. } => return_ty.clone(),
            Self::Variable { ty, .. } => ty.clone(),
        }
    }
}

pub trait Lookup {
    fn lookup(&self, scope: &Scope, sym_name: &String) -> Option<&Symbol> {
        match scope {
            Scope::Global => self.global_lookup(sym_name),
            Scope::Local(scope_name) => self.scoped_lookup(scope_name, sym_name),
        }
    }

    fn global_lookup(&self, sym_name: &String) -> Option<&Symbol>;
    fn scoped_lookup(&self, scope: &String, sym_name: &String) -> Option<&Symbol>;
}

type SymbolTable = HashMap<String, Symbol>;

#[derive(Debug, Default, Clone)]
pub struct Symbols {
    global_table: SymbolTable,
    scope_table: HashMap<String, SymbolTable>,
}

impl Symbols {
    fn add_global_symbol(&mut self, name: String, sym: Symbol) -> Result<(), CheckError> {
        match self.global_table.try_insert(name.clone(), sym) {
            Ok(_) => Ok(()),
            Err(occ) => Err(CheckError::VariableAlreadyDefined(name, occ.value)),
        }
    }

    fn add_scoped_symbol(
        &mut self,
        scope: String,
        name: String,
        sym: Symbol,
    ) -> Result<(), CheckError> {
        if let Some(scope_table) = self.scope_table.get_mut(&scope) {
            return match scope_table.try_insert(name.clone(), sym) {
                Ok(_) => Ok(()),
                Err(occ) => Err(CheckError::VariableAlreadyDefined(name, occ.value)),
            };
        }

        let mut fresh_scope_table = HashMap::new();
        assert!(fresh_scope_table.insert(name, sym).is_none());
        self.scope_table.insert(scope.clone(), fresh_scope_table);

        return Ok(());
    }
}

impl Lookup for Symbols {
    fn global_lookup(&self, name: &String) -> Option<&Symbol> {
        self.global_table.get(name)
    }

    fn scoped_lookup(&self, scope: &String, sym_name: &String) -> Option<&Symbol> {
        self.scope_table
            .get(scope)
            .and_then(|inner| inner.get(sym_name))
    }
}

pub fn default_checker(stmts: &Vec<Statement>) -> Result<Symbols, CheckError> {
    let mut checker: Symbols = Default::default();
    build_symbol_table(&stmts, Scope::Global, &mut checker)?;
    verify_statements(&stmts, &mut checker, None, Scope::Global)?;
    Ok(checker.clone())
}

fn build_symbol_table(
    stmts: &[Statement],
    scope: Scope,
    checker: &mut Symbols,
) -> Result<(), CheckError> {
    for stmt in stmts {
        match stmt {
            Statement::VariableAssignment { ident, ty, .. } => match &scope {
                Scope::Global => checker
                    .add_global_symbol(ident.to_string(), Symbol::Variable { ty: ty.clone() })?,
                Scope::Local(scope_name) => checker.add_scoped_symbol(
                    scope_name.clone(),
                    ident.to_string(),
                    Symbol::Variable { ty: ty.clone() },
                )?,
            },
            Statement::FunctionDefinition {
                name,
                args,
                return_ty,
                body,
            } => {
                let fn_sym = Symbol::Function {
                    args: args.into_iter().map(|arg| arg.1.clone()).collect(),
                    return_ty: return_ty.clone(),
                };

                match &scope {
                    Scope::Global => checker.add_global_symbol(name.to_string(), fn_sym)?,
                    Scope::Local(scope_name) => {
                        checker.add_scoped_symbol(scope_name.clone(), name.to_string(), fn_sym)?
                    }
                }

                for (arg_name, arg_type) in args {
                    let arg_sym = Symbol::Variable {
                        ty: arg_type.clone(),
                    };

                    checker.add_scoped_symbol(name.clone(), arg_name.to_string(), arg_sym)?;
                }

                build_symbol_table(&body, Scope::Local(name.to_string()), checker)?;
            }
            _ => continue,
        }
    }

    Ok(())
}

fn verify_statements(
    stmts: &[Statement],
    checker: &Symbols,
    return_ty: Option<&Type>,
    scope: Scope,
) -> Result<(), CheckError> {
    for stmt in stmts {
        match stmt {
            Statement::FunctionDefinition {
                name,
                args,
                return_ty,
                body,
            } => {
                verify_statements(
                    body.as_ref(),
                    checker,
                    Some(return_ty),
                    Scope::Local(name.to_string()),
                )?;

                continue;
            }
            Statement::VariableAssignment { ident, ty, expr } => {
                let expr_type = type_of(expr, checker, &scope)?;
                if !expr_type.eq(ty) {
                    return Err(CheckError::VariableAssignmentTypeMismatch(
                        ident.clone(),
                        ty.clone(),
                        expr_type,
                    ));
                }
                continue;
            }
            Statement::Return(expr) => {
                return match &scope {
                    Scope::Local(scope_name) => {
                        let expr_type = type_of(expr, checker, &scope)?;
                        if &expr_type == return_ty.unwrap() {
                            continue;
                        }

                        Err(CheckError::ReturnTypeMismatch(
                            scope_name.clone(),
                            expr_type.clone(),
                            return_ty.unwrap().clone(),
                        ))
                    }
                    _ => Err(CheckError::ReturnOutsideScope),
                };
            }
            _ => todo!(),
        }
    }

    Ok(())
}

fn type_of(expr: &Expression, checker: &impl Lookup, scope: &Scope) -> Result<Type, CheckError> {
    match expr {
        Expression::Integer(_) => Ok(Type::I32),
        Expression::Ident(name) => {
            if let Some(symbol) = checker.lookup(scope, &name) {
                return Ok(symbol.get_type());
            }

            return Err(CheckError::SymbolNotFound(name.clone()));
        }
        Expression::Unary(op, expr) => {
            let expr_type = type_of(expr, checker, scope)?;
            match op {
                Operator::Add | Operator::Div | Operator::Mul | Operator::Sub => match expr_type {
                    Type::I32 => return Ok(Type::I32),
                    _ => return Err(CheckError::MismatchUnaryOperatorType(op.clone(), expr_type)),
                },
                _ => unreachable!(),
            }
        }
        Expression::BinaryExpression(lhs, op, rhs) => {
            let lhs_type = type_of(lhs, checker, scope)?;
            let rhs_type = type_of(rhs, checker, scope)?;

            match op {
                Operator::Add | Operator::Div | Operator::Mul | Operator::Sub => {
                    match (&lhs_type, &rhs_type) {
                        (Type::I32, Type::I32) => Ok(Type::I32),
                        _ => {
                            return Err(CheckError::MismatchBinaryOperatorType(
                                op.clone(),
                                lhs_type,
                                rhs_type,
                            ))
                        }
                    }
                }
            }
        }
        Expression::Call(fn_name, call_args) => {
            if let Some(symbol) = checker.lookup(scope, fn_name) {
                match symbol {
                    Symbol::Function {
                        args: params,
                        return_ty,
                        ..
                    } => {
                        if call_args.len() != params.len() {
                            return Err(CheckError::WrongParamLen(
                                fn_name.clone(),
                                call_args.len(),
                                params.len(),
                            ));
                        }

                        let mut args_type: Vec<Type> = Vec::with_capacity(call_args.len());
                        for arg in call_args.as_ref() {
                            args_type.push(type_of(arg, checker, scope)?);
                        }

                        if !params.eq(&args_type) {
                            return Err(CheckError::WrongParamsType(
                                fn_name.clone(),
                                args_type.clone(),
                                params.clone(),
                            ));
                        }
                        return Ok(return_ty.clone());
                    }
                    _ => return Err(CheckError::NotCallable(fn_name.clone())),
                };
            }

            Err(CheckError::SymbolNotFound(fn_name.clone()))
        }
    }
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
            var b: int = sum(3, 5);

            func sum(a: int, b: int) int {
                return a + b;
            }
        ";

        let tokens = read_from_str(source);
        let mut program = Program::from(tokens).peekable();
        let raw_ast = pratt_parser::PrattParser::parse(&mut program).unwrap();

        match default_checker(raw_ast) {
            Err(err) => {
                println!("{}", err);
                assert!(false);
            }
            _ => {}
        }
    }
}
