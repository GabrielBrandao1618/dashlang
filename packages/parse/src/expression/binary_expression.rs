use super::{
    binary_operator::parse_binary_operator, call_expression::parse_call_expression,
    parse_expression, unary_expression::parse_unary_expression,
};
use crate::{
    expression::parse_sub_expression, literal::parse_literal, utils::get_pair_location,
    DashlangParser, Rule,
};
use ast::{BinaryExpr, Expr, Located, Location, Symbol};
use errors::{DashlangError, DashlangResult, ErrorKind, ParsingErrorKind};
use pest::{
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};

pub fn parse_binary_expression(
    input: &str,
    base_location: usize,
) -> DashlangResult<Located<BinaryExpr>> {
    let pratt = PrattParser::new()
        .op(Op::infix(Rule::and, Assoc::Left)
            | Op::infix(Rule::eq, Assoc::Left)
            | Op::infix(Rule::or, Assoc::Left))
        .op(Op::infix(Rule::lt, Assoc::Left)
            | Op::infix(Rule::le, Assoc::Left)
            | Op::infix(Rule::gt, Assoc::Left)
            | Op::infix(Rule::ge, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::infix(Rule::bitwise_right_shift, Assoc::Left)
            | Op::infix(Rule::bitwise_left_shift, Assoc::Left)
            | Op::infix(Rule::bitwise_and, Assoc::Left)
            | Op::infix(Rule::bitwise_or, Assoc::Left)
            | Op::infix(Rule::bitwise_xor, Assoc::Left));

    let pairs = DashlangParser::parse(Rule::binary_expression, input)
        .unwrap()
        .next()
        .unwrap()
        .into_inner();
    let parsed = pratt
        .map_primary(|primary| {
            let primary_start = primary.as_span().start();
            match primary.as_rule() {
                Rule::literal => Ok(Expr::Literal(parse_literal(
                    primary.as_str(),
                    base_location + primary_start,
                )?)),
                Rule::symbol => Ok({
                    Expr::Symbol(Symbol {
                        value: primary.as_str().to_owned(),
                        location: {
                            let (start, end) = get_pair_location(&primary);
                            (start + base_location, end + base_location).into()
                        },
                    })
                }),
                Rule::sub_expression => Ok(Expr::SubExpr(parse_sub_expression(
                    primary.as_str(),
                    primary_start + base_location,
                )?)),
                Rule::expression => Ok(parse_expression(
                    primary.as_str(),
                    primary_start + base_location,
                )?),
                Rule::unary_expression => Ok(Expr::UnaryExpr(Box::new(parse_unary_expression(
                    primary.as_str(),
                    primary_start + base_location,
                )?))),
                Rule::call_expression => Ok(Expr::Call(parse_call_expression(
                    primary.as_str(),
                    primary_start + base_location,
                )?)),
                _ => unreachable!("{:#?}", primary.as_rule()),
            }
        })
        .map_infix(|lhs, op, rhs| {
            let lhs = lhs?;
            let rhs = rhs?;
            let location = Location::new(lhs.get_location().start, rhs.get_location().end);
            Ok(Expr::BinaryExpr(Box::new(BinaryExpr {
                location,
                left: lhs,
                right: rhs,
                operator: parse_binary_operator(op.as_str())?,
            })))
        })
        .parse(pairs)?;
    if let Expr::BinaryExpr(bin_expr) = parsed {
        Ok(*bin_expr)
    } else {
        Err(DashlangError {
            location: Some(Location::default()),
            message: "Expected binary expression".to_owned(),
            kind: ErrorKind::Parsing(ParsingErrorKind::Default),
        })
    }
}

#[cfg(test)]
mod tests {
    use ast::{BinaryOperator, Boolean, Int, Literal, SubExpr};

    use super::*;

    #[test]
    fn test_parse_binary_op() {
        assert_eq!(
            parse_binary_expression("1 * 2", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                right: Expr::Literal(Literal::Int(Int {
                    value: 2,
                    location: Location::new(4, 5)
                })),
                operator: BinaryOperator::Mul,
                location: Location::new(0, 5),
            })
        );
        assert_eq!(
            parse_binary_expression("1 + 2", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                right: Expr::Literal(Literal::Int(Int {
                    value: 2,
                    location: Location::new(4, 5)
                })),
                operator: BinaryOperator::Add,
                location: Location::new(0, 5),
            })
        );
        assert_eq!(
            parse_binary_expression("1 + 2 * 2", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                right: Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Int(Int {
                        value: 2,
                        location: Location::new(4, 5)
                    })),
                    right: Expr::Literal(Literal::Int(Int {
                        value: 2,
                        location: Location::new(8, 9)
                    })),
                    operator: BinaryOperator::Mul,
                    location: Location::new(4, 9),
                })),
                operator: BinaryOperator::Add,
                location: Location::new(0, 9),
            })
        );
    }
    #[test]
    fn test_parse_binary_expr() {
        assert_eq!(
            parse_binary_expression("1 + 2", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                right: Expr::Literal(Literal::Int(Int {
                    value: 2,
                    location: Location::new(4, 5)
                })),
                operator: BinaryOperator::Add,
                location: Location::new(0, 5),
            })
        );
        assert_eq!(
            parse_binary_expression("2 > 1", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 2,
                    location: Location::new(0, 1)
                })),
                right: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(4, 5)
                })),
                operator: BinaryOperator::Gt,
                location: Location::new(0, 5),
            })
        );
        assert_eq!(
            parse_binary_expression("2 == 2", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 2,
                    location: Location::new(0, 1)
                })),
                right: Expr::Literal(Literal::Int(Int {
                    value: 2,
                    location: Location::new(5, 6)
                })),
                operator: BinaryOperator::Eq,
                location: Location::new(0, 6),
            })
        );
        assert_eq!(
            parse_binary_expression("true || false", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Bool(Boolean {
                    value: true,
                    location: Location::new(0, 4)
                })),
                right: Expr::Literal(Literal::Bool(Boolean {
                    value: false,
                    location: Location::new(8, 13)
                })),
                operator: BinaryOperator::Or,
                location: Location::new(0, 13),
            })
        );
        assert_eq!(
            parse_binary_expression("true && false", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Bool(Boolean {
                    value: true,
                    location: Location::new(0, 4)
                })),
                right: Expr::Literal(Literal::Bool(Boolean {
                    value: false,
                    location: Location::new(8, 13)
                })),
                operator: BinaryOperator::And,
                location: Location::new(0, 13),
            })
        );
    }
    #[test]
    fn test_parse_sub_expressions() {
        assert_eq!(
            parse_binary_expression("1 + (2 + 1)", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                right: Expr::SubExpr(SubExpr {
                    value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                        left: Expr::Literal(Literal::Int(Int {
                            value: 2,
                            location: Location::new(5, 6)
                        })),
                        right: Expr::Literal(Literal::Int(Int {
                            value: 1,
                            location: Location::new(9, 10)
                        })),
                        operator: BinaryOperator::Add,
                        location: Location::new(5, 10),
                    }))),
                    location: Location::new(4, 11)
                }),
                operator: BinaryOperator::Add,
                location: Location::new(0, 11),
            }),
        );
        assert_eq!(
            parse_binary_expression("(1 + 2) + 1", 0),
            Ok(BinaryExpr {
                left: Expr::SubExpr(SubExpr {
                    value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                        left: Expr::Literal(Literal::Int(Int {
                            value: 1,
                            location: Location::new(1, 2)
                        })),
                        right: Expr::Literal(Literal::Int(Int {
                            value: 2,
                            location: Location::new(5, 6)
                        })),
                        operator: BinaryOperator::Add,
                        location: Location::new(1, 6),
                    }))),
                    location: Location::new(0, 7)
                }),
                right: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(10, 11)
                })),
                operator: BinaryOperator::Add,
                location: Location::new(0, 11),
            })
        );
        assert_eq!(
            parse_binary_expression("1 + n", 0),
            Ok(BinaryExpr {
                left: Expr::Literal(Literal::Int(Int {
                    value: 1,
                    location: Location::new(0, 1)
                })),
                operator: BinaryOperator::Add,
                right: Expr::Symbol(Symbol {
                    value: String::from("n"),
                    location: Location::new(4, 5)
                }),
                location: Location::new(0, 5),
            })
        );
    }
}
