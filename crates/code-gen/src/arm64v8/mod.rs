use std::ops::Deref;

use parser::{
    ast::{Expression, Statement},
    types::Type,
};
use type_checker::Lookup;

// This module should generate the AMR64v8 instructions for
// the sand source code.

// 1. The naive approach,
// - using the symbol table to discover all the global variables and functions
// - lookup for the main fn symbol so we can define the entry point of the binary

pub struct Arm64v8;

#[derive(Debug)]
pub enum GenError {
    MissingMainFn,
    MainShouldReturnI32(Type),
}

struct FunctionDefinition {
    name: String,
    args: Vec<(String, Type)>,
    return_ty: Type,
    body: Box<Vec<Statement>>,
}

enum Section {
    Data,
    BSS,
    Text(TextMetadata, Vec<LabeledInstruction>),
}

#[derive(Default)]
pub struct TextMetadata {
    global: String,
    align: usize,
}

pub struct LabeledInstruction {
    label: String,
    instruction: String,
}

#[derive(Default)]
pub struct TextSection {
    metadata: TextMetadata,
    labeled_inst: Vec<LabeledInstruction>,
}

pub struct Module<L: Lookup> {
    ast: Vec<Statement>,
    symbol_table: L,
    text_section: TextSection,
}

impl<L> Module<L>
where
    L: Lookup,
{
    pub fn new(ast: Vec<Statement>, sym_table: L) -> Self {
        Module {
            ast,
            symbol_table: sym_table,
            text_section: Default::default(),
        }
    }

    pub fn generate(&mut self) -> Result<(), GenError> {
        self.start_from_main()
    }

    fn start_from_main(&mut self) -> Result<(), GenError> {
        let stmts = &self.ast;
        let main_fn = stmts.into_iter().find_map(|p| match p {
            Statement::FunctionDefinition {
                name,
                args,
                return_ty,
                body,
            } if name.eq(&String::from("main")) => {
                return Some(FunctionDefinition {
                    name: name.to_string(),
                    args: args.clone(),
                    return_ty: return_ty.clone(),
                    body: body.clone(),
                })
            }
            _ => None,
        });

        if let Some(main) = main_fn {
            self.text_section.metadata = TextMetadata {
                global: String::from("_start"),
                align: 4,
            };

            // TODO: generate machine code to capture the CLI args
            return self.generate_code_for(main);
        }

        Err(GenError::MissingMainFn)
    }

    fn generate_code_for(&mut self, fn_def: FunctionDefinition) -> Result<(), GenError> {
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use parser::*;
    use tokenizer::*;
    use type_checker::*;

    use super::Module;

    #[test]
    fn generate_raw_machine_code() {
        let source = r"
            func sum(a: int, b: int) int {
                return a + b;
            }

            func main() int {
                return sum(1, 2);
            }
        ";

        let tokens = read_from_str(source);
        let mut program = Program::from(tokens).peekable();
        let raw_ast = pratt_parser::PrattParser::parse(&mut program).unwrap();

        let symbols = default_checker(&raw_ast).unwrap();
        let mut module = Module::new(raw_ast, symbols);

        module.generate().unwrap()
    }
}
