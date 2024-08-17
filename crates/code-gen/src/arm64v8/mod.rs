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

enum GenError {
    MissingMainFn,
    MainShouldReturnI32(Type),
}

enum Section {
    Data,
    BSS,
    Text(Vec<LabeledInstruction>),
}

pub struct LabeledInstruction {}

pub struct Module<L: Lookup> {
    ast: Vec<Statement>,
    symbol_table: L,
    sections: Vec<Section>,
}

struct FunctionDefinition {
    name: String,
    args: Vec<(String, Type)>,
    return_ty: Type,
    body: Box<Vec<Statement>>,
}

impl From<Statement> for FunctionDefinition {
    fn from(value: Statement) -> Self {}
}

impl<L> Module<L>
where
    L: Lookup,
{
    pub fn new(ast: Vec<Statement>, sym_table: L) -> Self {
        Module {
            ast,
            symbol_table: sym_table,
            sections: Vec::new(),
        }
    }

    pub fn generate(&mut self) -> Result<(), GenError> {
        self.start_from_main()
    }

    fn start_from_main(&mut self) -> Result<(), GenError> {
        let stmts = &self.ast;
        let main_fn = stmts.into_iter().find_map(|p| match p {
            Statement::FunctionDefinition { name, .. } => return Some(FunctionDefinition { name }),
            _ => None,
        });

        match main_fn {
            Some(main_fn_definition) => main_fn_definition,
            None => return Err(GenError::MissingMainFn),
        }

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
        let module = Module::new(raw_ast, symbols);

        module.generate();
    }
}
