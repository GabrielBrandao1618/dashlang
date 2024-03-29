use ast::{Expr, Stmt};
use pest::Parser;

use crate::{
    expression::parse_expression,
    parser::{DashlangParser, Rule},
    value::parse_values,
};

pub fn parse_return_stmt(input: &str) -> Stmt {
    let ast = DashlangParser::parse(Rule::return_stmt, input)
        .expect("Could not parse return statement")
        .next()
        .expect("Could not parse return statement");
    let return_stmt = ast
        .into_inner()
        .next()
        .expect("Could not get return statement");
    let return_value = match return_stmt.as_rule() {
        Rule::value => {
            let value = return_stmt
                .into_inner()
                .next()
                .expect("Could not get value");
            Expr::Literal(parse_values(value.as_str()))
        }
        Rule::expression => parse_expression(return_stmt.as_str()),
        _ => unreachable!(),
    };
    Stmt::Return(return_value)
}

#[cfg(test)]
mod tests {
    use ast::{BinaryOp, BinaryOpType, Expr, Literal};

    use super::*;
    #[test]
    fn test_return_value() {
        assert_eq!(
            parse_return_stmt("return 1"),
            Stmt::Return(Expr::Literal(Literal::Int(1)))
        );
    }
    #[test]
    fn test_return_expression() {
        assert_eq!(
            parse_return_stmt("return 1 + 1"),
            Stmt::Return(Expr::BinaryOp(Box::new(BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::Literal(Literal::Int(1)),
                op_type: BinaryOpType::Add
            })))
        );
        assert_eq!(
            parse_return_stmt("return 2 * (2 + 2)"),
            Stmt::Return(Expr::BinaryOp(Box::new(BinaryOp {
                left: Expr::Literal(Literal::Int(2)),
                right: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::Literal(Literal::Int(2)),
                    right: Expr::Literal(Literal::Int(2)),
                    op_type: BinaryOpType::Add
                })),
                op_type: BinaryOpType::Mul
            })))
        );
    }
}
