use ast::{Expr, Location, Return, Stmt};
use pest::Parser;

use crate::{
    expression::parse_expression,
    literal::parse_literal,
    parser::{DashlangParser, Rule},
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
        Rule::literal => {
            let value = return_stmt
                .into_inner()
                .next()
                .expect("Could not get value");
            Expr::Literal(parse_literal(value.as_str()))
        }
        Rule::expression => parse_expression(return_stmt.as_str()),
        _ => unreachable!(),
    };
    Stmt::Return(Return {
        value: return_value,
        location: Location::default(),
    })
}

#[cfg(test)]
mod tests {
    use ast::{BinaryExpr, BinaryOperator, Expr, Int, Literal, Location, Return};

    use super::*;
    #[test]
    fn test_return_value() {
        assert_eq!(
            parse_return_stmt("return 1"),
            Stmt::Return(Return {
                value: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                location: Location::default()
            })
        );
    }
    #[test]
    fn test_return_expression() {
        assert_eq!(
            parse_return_stmt("return 1 + 1"),
            Stmt::Return(Return {
                value: Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(0, 1)
                    })),
                    right: Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(0, 1)
                    })),
                    operator: BinaryOperator::Add,
                    location: Location::default(),
                })),
                location: Location::default()
            })
        );
        assert_eq!(
            parse_return_stmt("return 2 * (2 + 2)"),
            Stmt::Return(Return {
                value: Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Int(Int {
                        value: 2,
                        location: Location::new(0, 1)
                    })),
                    right: Expr::BinaryExpr(Box::new(BinaryExpr {
                        left: Expr::Literal(Literal::Int(Int {
                            value: 2,
                            location: Location::new(0, 1)
                        })),
                        right: Expr::Literal(Literal::Int(Int {
                            value: 2,
                            location: Location::new(0, 1)
                        })),
                        operator: BinaryOperator::Add,
                        location: Location::default(),
                    })),
                    operator: BinaryOperator::Mul,
                    location: Location::default(),
                })),
                location: Location::default()
            })
        );
    }
}
