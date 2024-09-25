use crate::{literal::parse_literal, utils::get_pair_location, DashlangParser, Rule};

use errors::{DashlangError, DashlangResult, ErrorKind, ParsingErrorKind};

use ast::{Expr, Located, Location, SubExpr, Symbol};
use pest::Parser;

use self::{
    assignment_expression::parse_assignment_expression, binary_expression::parse_binary_expression,
    call_expression::parse_call_expression, compound_assign_expr::parse_compound_assign_expr,
    dash_expression::parse_dash_expression,
    destructuring_assignment::parse_destructuring_assignment,
    unary_expression::parse_unary_expression,
};

mod assignment_expression;
mod binary_expression;
mod binary_operator;
mod call_expression;
mod compound_assign_expr;
mod dash_expression;
mod destructuring_assignment;
mod unary_expression;

pub fn parse_expression(input: &str, base_location: usize) -> DashlangResult<Located<Expr>> {
    let ast = DashlangParser::parse(Rule::expression, input)
        .map_err(|err| DashlangError {
            location: match err.location {
                pest::error::InputLocation::Pos(_) => None,
                pest::error::InputLocation::Span((start, end)) => Some(Location { start, end }),
            },
            message: format!("Could not parse expression: {}", err),
            kind: ErrorKind::Parsing(ParsingErrorKind::Default),
        })?
        .next()
        .expect("Could not parse expression");
    let (start, end) = get_pair_location(&ast);
    let mut inner_ast = ast.into_inner();
    let expression = inner_ast.next().expect("Could not get expression type");
    let mut parsed = match expression.as_rule() {
        Rule::binary_expression => {
            let parsed = parse_binary_expression(expression.as_str(), start + base_location)?;
            Expr::BinaryExpr(Box::new(parsed))
        }
        Rule::assignment_expression => {
            let parsed = parse_assignment_expression(expression.as_str(), start + base_location)?;
            Expr::Assignment(parsed)
        }
        Rule::compound_assignment_expr => Expr::Assignment(parse_compound_assign_expr(
            expression.as_str(),
            start + base_location,
        )?),
        Rule::call_expression => {
            let parsed = parse_call_expression(expression.as_str(), start + base_location)?;
            Expr::Call(parsed)
        }
        Rule::symbol => Expr::Symbol(Symbol {
            value: expression.as_str().to_owned(),
            location: Location::new(start + base_location, end + base_location),
        }),
        Rule::literal => Expr::Literal(parse_literal(expression.as_str(), start + base_location)?),
        Rule::unary_expression => Expr::UnaryExpr(Box::new(parse_unary_expression(
            expression.as_str(),
            start + base_location,
        )?)),
        Rule::destructuring_assignment => {
            let parsed =
                parse_destructuring_assignment(expression.as_str(), start + base_location)?;
            Expr::DestructuringAsignment(parsed)
        }
        Rule::dash_expr => {
            let parsed = parse_dash_expression(expression.as_str(), start + base_location)?;
            Expr::Assignment(parsed)
        }
        any => unreachable!("{:#?}", any),
    };
    for piping in inner_ast {
        let inner_call = piping
            .into_inner()
            .next()
            .expect("Could not get call from piping");
        let (start_call, _end) = get_pair_location(&inner_call);
        let mut parsed_inner_call =
            parse_call_expression(inner_call.as_str(), start_call + base_location)?;
        parsed_inner_call.args.insert(0, parsed);
        parsed = Expr::Call(parsed_inner_call);
    }
    Ok(parsed)
}
pub fn parse_sub_expression(input: &str, base_location: usize) -> DashlangResult<Located<SubExpr>> {
    let ast = DashlangParser::parse(Rule::sub_expression, input)
        .expect("Could not parse sub expression")
        .next()
        .expect("Could not parse sub expression");
    let (start, end) = get_pair_location(&ast);
    let ast_expr = ast
        .into_inner()
        .next()
        .expect("Could not get expression from sub expression");
    let ast_expr_start = ast_expr.as_span().start();
    let parsed = parse_expression(ast_expr.as_str(), ast_expr_start + base_location)?;
    let absolute_location = (start + base_location, end + base_location).into();

    Ok(SubExpr {
        value: Box::new(parsed),
        location: absolute_location,
    })
}
#[cfg(test)]
mod tests {
    use super::*;
    use ast::{
        AssignmentExpr, BinaryExpr, BinaryOperator, Boolean, Call, Expr, Int, Literal, Location,
        UnaryExpr,
    };
    #[test]
    fn test_parse_expression() {
        assert_eq!(
            parse_expression("1 + 2", 0),
            Ok(Expr::BinaryExpr(Box::new(BinaryExpr {
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
            })))
        );
    }
    #[test]
    fn test_assignment_expression() {
        assert_eq!(
            parse_expression("age = 5 + 1", 0),
            Ok(Expr::Assignment(AssignmentExpr {
                symbol: String::from("age"),
                value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Literal(Literal::Int(Int {
                        value: 5,
                        location: Location::new(6, 7)
                    })),
                    right: Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(10, 11)
                    })),
                    operator: BinaryOperator::Add,
                    location: Location::new(6, 11),
                }))),
                location: Location::new(0, 11),
            }))
        );
    }
    #[test]
    fn test_unary_expression() {
        assert_eq!(
            parse_expression("!(true && false)", 0),
            Ok(Expr::UnaryExpr(Box::new(UnaryExpr {
                operator: ast::UnaryOperator::Not,
                operand: Expr::SubExpr(SubExpr {
                    value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                        left: Expr::Literal(Literal::Bool(Boolean {
                            value: true,
                            location: Location::new(2, 6)
                        })),
                        right: Expr::Literal(Literal::Bool(Boolean {
                            value: false,
                            location: Location::new(10, 15)
                        })),
                        operator: BinaryOperator::And,
                        location: Location::new(2, 15),
                    }))),
                    location: (1, 16).into()
                }),
                location: Location::new(0, 16),
            })))
        );
    }
    #[test]
    fn test_compound_assign_expr() {
        assert_eq!(
            parse_expression("n += 1", 0),
            Ok(Expr::Assignment(AssignmentExpr {
                symbol: String::from("n"),
                value: Box::new(Expr::BinaryExpr(Box::new(BinaryExpr {
                    left: Expr::Symbol(Symbol {
                        value: String::from("n"),
                        location: Location::new(0, 1)
                    }),
                    right: Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(5, 6)
                    })),
                    operator: BinaryOperator::Add,
                    location: Location::new(0, 6),
                }))),
                location: Location::new(0, 6),
            }))
        );
    }
    #[test]
    fn test_piping() {
        assert_eq!(
            parse_expression("4 |> add(1)", 0),
            Ok(Expr::Call(Call {
                symbol: String::from("add"),
                args: vec![
                    Expr::Literal(Literal::Int(Int {
                        value: 4,
                        location: Location::new(0, 1)
                    })),
                    Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(9, 10)
                    }))
                ],
                location: Location::new(5, 11),
            }))
        );
    }
}
