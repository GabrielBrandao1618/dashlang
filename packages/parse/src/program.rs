use ast::Program;
use errors::DashlangResult;
use pest::Parser;

use crate::{
    parser::{DashlangParser, Rule},
    statement::parse_statement,
    utils::get_pair_location,
};

pub fn parse_program(input: &str) -> DashlangResult<Program> {
    let mut program: Program = vec![];
    let ast = DashlangParser::parse(Rule::program, input)
        .expect("Could not parse program")
        .next()
        .expect("Could not parse program");
    for stmt in ast.into_inner() {
        let (start, _end) = get_pair_location(&stmt);
        program.push(parse_statement(stmt.as_str(), start)?);
    }
    Ok(program)
}

#[cfg(test)]
mod tests {

    use ast::{AssignmentExpr, Expr, Int, Literal, Location, Stmt};

    use super::*;

    #[test]
    fn test_parse_program() {
        assert_eq!(
            parse_program("age = 5 count = 1"),
            Ok(vec![
                Stmt::Expr(Expr::Assignment(AssignmentExpr {
                    symbol: String::from("age"),
                    value: Box::new(Expr::Literal(Literal::Int(Int {
                        value: 5,
                        location: Location::new(6, 7)
                    }))),
                    location: Location::new(0, 8),
                })),
                Stmt::Expr(Expr::Assignment(AssignmentExpr {
                    symbol: String::from("count"),
                    value: Box::new(Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(16, 17)
                    }))),
                    location: Location::new(8, 17),
                }))
            ])
        )
    }
}
