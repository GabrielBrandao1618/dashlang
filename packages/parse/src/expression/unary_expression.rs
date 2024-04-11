use ast::{Location, UnaryExpr, UnaryOperator};
use pest::Parser;

use crate::{
    parser::{DashlangParser, Rule},
    utils::get_pair_location,
};

use super::parse_expression;

pub fn parse_unary_expression(input: &str) -> UnaryExpr {
    let parsed = DashlangParser::parse(Rule::unary_expression, input)
        .expect("Could not parse unary expression")
        .next()
        .expect("Could not get unary expression");
    let (start, end) = get_pair_location(&parsed);
    let mut parsed_inner = parsed.into_inner();
    let operator = parsed_inner
        .next()
        .expect("Could not get unary expression operator");
    let operand = parsed_inner
        .next()
        .expect("Could not get unary expression operand");
    UnaryExpr {
        operator: match operator.as_str() {
            "!" => UnaryOperator::Not,
            any => panic!("Invalid unary operator: {any}"),
        },
        operand: (parse_expression(operand.as_str())),
        location: Location::new(start, end),
    }
}

#[cfg(test)]
mod tests {
    use ast::{BinaryExpr, BinaryOperator, Boolean, Expr, Literal};

    use super::*;

    #[test]
    fn test_not_true() {
        assert_eq!(
            parse_unary_expression("!true"),
            UnaryExpr {
                operator: ast::UnaryOperator::Not,
                operand: Expr::Literal(Literal::Bool(Boolean {
                    value: true,
                    location: Location::new(0, 4)
                })),
                location: Location::new(0, 5),
            }
        );
    }
    #[test]
    fn test_sub() {
        assert_eq!(
            parse_unary_expression("!(true && false)"),
            UnaryExpr {
                operator: UnaryOperator::Not,
                operand: Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Bool(Boolean {
                        value: true,
                        location: Location::new(0, 4)
                    })),
                    right: Expr::Literal(Literal::Bool(Boolean {
                        value: false,
                        location: Location::new(0, 5)
                    })),
                    operator: BinaryOperator::And,
                    location: Location::default(),
                })),
                location: Location::new(0, 16),
            }
        );
    }
}
