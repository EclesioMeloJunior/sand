use parser::{ast::Expression, ast::Operator, ast::Statement, types::Type};

#[derive(Debug)]
enum LLVMError {
    MissingMainFunction,
}

struct LLVMContainer {
    ast: Vec<Statement>,
}

fn main_function_exists(stmts: &Vec<Statement>) -> bool {
    for stmt in stmts.into_iter() {
        match stmt {
            Statement::FunctionDefinition { name, .. } if name.eq(&String::from("main")) => true,
            _ => continue,
        };
    }

    false
}

impl TryFrom<Vec<Statement>> for LLVMContainer {
    type Error = LLVMError;
    fn try_from(stmts: Vec<Statement>) -> Result<Self, Self::Error> {
        if !main_function_exists(&stmts) {
            return Err(LLVMError::MissingMainFunction);
        }

        Ok(LLVMContainer { ast: stmts })
    }
}

#[cfg(test)]
mod tests {
    use parser::{ast::Expression, ast::Statement, types::Type};

    use crate::{LLVMContainer, LLVMError};

    #[test]
    fn from_ast_to_llvm() {
        // current ast is:
        // func main(argc: int32) int32 {
        //   printf(argc);
        //   return 0;
        // }
        let ast = vec![Statement::FunctionDefinition {
            name: String::from("main"),
            args: vec![(String::from("argc"), Type::I32)],
            return_ty: Type::I32,
            body: Box::new(vec![
                Statement::FunctionCall(Expression::Call(
                    Box::new(Expression::Ident(String::from("printf"))),
                    Box::new(vec![Expression::Ident(String::from("argc"))]),
                )),
                Statement::Return(Expression::Integer(0)),
            ]),
        }];

        let container = LLVMContainer::try_from(ast);

        Arm64::generate(container);

        println!("from llvm")
    }
}
