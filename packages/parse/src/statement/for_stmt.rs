use ast::For;
use pest::Parser;

use crate::{
    body::parse_body,
    expression::parse_expression,
    instruction::parse_instruction,
    parser::{DashlangParser, Rule},
};

pub fn parse_for_stmt(input: &str) -> For {
    let ast = DashlangParser::parse(Rule::for_stmt, input)
        .expect("Could not parse for statement")
        .next()
        .expect("Could not get for statement");
    let mut inner_ast = ast.into_inner();
    let init_instruction = inner_ast
        .next()
        .expect("Could not get init instruction from for statement");
    let cond_expr = inner_ast
        .next()
        .expect("Could not get condition expr from for statement");
    let iteration_instruction = inner_ast
        .next()
        .expect("Could not get iteration instruction from for statement");
    let for_body = inner_ast.next().expect("Could not get for statement body");

    For {
        cond: parse_expression(cond_expr.as_str()),
        body: parse_body(for_body.as_str()),
        init: parse_instruction(init_instruction.as_str()),
        iteration: parse_instruction(iteration_instruction.as_str()),
    }
}

#[cfg(test)]
mod tests {
    use ast::{Assignment, BinaryExpr, BinaryOperator, Expr, Instruction, Literal};

    use super::*;
    #[test]
    fn test_for_stmt() {
        assert_eq!(
            parse_for_stmt("for n = 1; n < 10; n += 1 {}"),
            For {
                cond: Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Symbol(String::from("n")),
                    right: Expr::Literal(Literal::Int(10)),
                    operator: BinaryOperator::Lt
                })),
                body: vec![],
                init: Instruction::Expr(Expr::Assignment(Assignment {
                    symbol: String::from("n"),
                    value: Box::new(Expr::Literal(Literal::Int(1)))
                })),
                iteration: Instruction::Expr(Expr::Assignment(Assignment {
                    symbol: String::from("n"),
                    value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                        left: Expr::Symbol(String::from("n")),
                        right: Expr::Literal(Literal::Int(1)),
                        operator: BinaryOperator::Add
                    })))
                }))
            }
        );
        assert_eq!(
            parse_for_stmt("for n = 10; n > 0; n -= 1 {}"),
            For {
                cond: Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Symbol(String::from("n")),
                    right: Expr::Literal(Literal::Int(0)),
                    operator: BinaryOperator::Gt
                })),
                body: vec![],
                init: Instruction::Expr(Expr::Assignment(Assignment {
                    symbol: String::from("n"),
                    value: Box::new(Expr::Literal(Literal::Int(10)))
                })),
                iteration: Instruction::Expr(Expr::Assignment(Assignment {
                    symbol: String::from("n"),
                    value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                        left: Expr::Symbol(String::from("n")),
                        right: Expr::Literal(Literal::Int(1)),
                        operator: BinaryOperator::Sub
                    })))
                }))
            }
        );
    }
}