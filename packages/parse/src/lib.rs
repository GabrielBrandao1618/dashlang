use ast::Program;
use parser::{DashlangParser, Rule};

mod body;
#[cfg(test)]
mod examples_tests;
mod expression;
mod instruction;
mod literal;
mod parser;
mod program;
mod statement;
mod utils;

use program::parse_program;

pub fn parse(input: &str) -> Program {
    parse_program(input)
}
