use super::parse_expression;
use crate::{value::parse_values, DashlangParser, Rule};
use ast::{BinaryOp, BinaryOpType, Expr, Literal};
use pest::Parser;

#[derive(Debug, Clone, PartialEq)]
enum BinaryExpressionToken {
    Value(Literal),
    Expr(Expr),
    Operator(BinaryOpType),
}
pub fn parse_binary_expression(input: &str) -> BinaryOp {
    let ast = DashlangParser::parse(Rule::binary_expression, input)
        .expect("Could not parse binary expression")
        .next()
        .expect("Could not parse binary expression");
    let mut flat_expression: Vec<BinaryExpressionToken> = vec![];
    for element in ast.into_inner() {
        match element.as_rule() {
            Rule::binary_operator => match element.as_str() {
                "+" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Add)),
                "-" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Sub)),
                "*" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Mul)),
                "/" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Div)),
                ">" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Gt)),
                ">=" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Ge)),
                "<" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Lt)),
                "<=" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Le)),
                "==" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Eq)),
                "&&" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::And)),
                "||" => flat_expression.push(BinaryExpressionToken::Operator(BinaryOpType::Or)),
                _ => unreachable!(),
            },
            Rule::value => {
                flat_expression.push(BinaryExpressionToken::Value(parse_values(element.as_str())));
            }
            Rule::expression => {
                let parsed = parse_expression(element.as_str());
                flat_expression.push(BinaryExpressionToken::Expr(parsed));
            }
            Rule::symbol => {
                let parsed = element.as_str().to_owned();
                flat_expression.push(BinaryExpressionToken::Expr(Expr::Symbol(parsed)));
            }
            _ => unreachable!(),
        }
    }
    flat_binary_expression_to_ast(&mut flat_expression)
}
fn flat_binary_expression_to_ast(flat_expression: &mut Vec<BinaryExpressionToken>) -> BinaryOp {
    while flat_expression.len() > 1 {
        merge_flat_binary_op_tokens_by_operations(
            flat_expression,
            &vec![
                BinaryExpressionToken::Operator(BinaryOpType::Mul),
                BinaryExpressionToken::Operator(BinaryOpType::Div),
            ],
        );
        merge_flat_binary_op_tokens_by_operations(
            flat_expression,
            &vec![
                BinaryExpressionToken::Operator(BinaryOpType::Add),
                BinaryExpressionToken::Operator(BinaryOpType::Sub),
            ],
        );
        merge_flat_binary_op_tokens_by_operations(
            flat_expression,
            &vec![
                BinaryExpressionToken::Operator(BinaryOpType::Gt),
                BinaryExpressionToken::Operator(BinaryOpType::Ge),
                BinaryExpressionToken::Operator(BinaryOpType::Lt),
                BinaryExpressionToken::Operator(BinaryOpType::Le),
                BinaryExpressionToken::Operator(BinaryOpType::Eq),
            ],
        );
        merge_flat_binary_op_tokens_by_operations(
            flat_expression,
            &vec![
                BinaryExpressionToken::Operator(BinaryOpType::Or),
                BinaryExpressionToken::Operator(BinaryOpType::And),
            ],
        );
    }
    match flat_expression
        .into_iter()
        .next()
        .expect("Expected expression to ended with at least 1 element")
    {
        BinaryExpressionToken::Expr(expr) => match expr {
            Expr::BinaryOp(op) => *op.to_owned(),
            _ => panic!("Expected expression to be binary operation"),
        },
        BinaryExpressionToken::Value(_) => {
            panic!("Expected binary operation to not have just a value")
        }
        BinaryExpressionToken::Operator(_) => {
            panic!("Expected binary operation to not have just a operator")
        }
    }
}
fn merge_flat_binary_op_tokens_by_operations(
    flat_expression: &mut Vec<BinaryExpressionToken>,
    allowed: &Vec<BinaryExpressionToken>,
) {
    for (pos, token) in flat_expression.clone().into_iter().enumerate() {
        if allowed.contains(&token) {
            if let BinaryExpressionToken::Operator(op) = token {
                merge_flat_binary_op_tokens(flat_expression, pos, op);
                merge_flat_binary_op_tokens_by_operations(flat_expression, allowed);
                break;
            }
        }
    }
}
fn merge_flat_binary_op_tokens(
    flat_expression: &mut Vec<BinaryExpressionToken>,
    operator_pos: usize,
    op: BinaryOpType,
) {
    let (previous_element, next_element): (BinaryExpressionToken, BinaryExpressionToken) = {
        let next = flat_expression.remove(operator_pos + 1);
        let previous = flat_expression.remove(operator_pos - 1);
        (previous, next)
    };
    let _ = std::mem::replace(
        &mut flat_expression[operator_pos - 1], // Since we removed the previous item, we use position - 1
        BinaryExpressionToken::Expr(Expr::BinaryOp(Box::new(BinaryOp {
            left: match previous_element {
                BinaryExpressionToken::Value(val) => Expr::Literal(val.clone()),
                BinaryExpressionToken::Expr(expr) => expr,
                BinaryExpressionToken::Operator(_) => {
                    panic!("Expected token after operator to be a value or expression")
                }
            },
            right: match next_element {
                BinaryExpressionToken::Value(val) => Expr::Literal(val.clone()),
                BinaryExpressionToken::Expr(expr) => expr,
                BinaryExpressionToken::Operator(_) => {
                    panic!("Expected token after operator to be a value or expression")
                }
            },
            op_type: op,
        }))),
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_binary_op() {
        assert_eq!(
            parse_binary_expression("1 * 2"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::Literal(Literal::Int(2)),
                op_type: BinaryOpType::Mul
            }
        );
        assert_eq!(
            parse_binary_expression("1 + 2"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::Literal(Literal::Int(2)),
                op_type: BinaryOpType::Add
            }
        );
        assert_eq!(
            parse_binary_expression("1 + 2 * 2"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::Literal(Literal::Int(2)),
                    right: Expr::Literal(Literal::Int(2)),
                    op_type: BinaryOpType::Mul
                })),
                op_type: BinaryOpType::Add
            }
        );
        assert_eq!(
            parse_binary_expression("1 + 2 * 2 / 2"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::BinaryOp(Box::new(BinaryOp {
                        left: Expr::Literal(Literal::Int(2)),
                        right: Expr::Literal(Literal::Int(2)),
                        op_type: BinaryOpType::Mul
                    })),
                    right: Expr::Literal(Literal::Int(2)),
                    op_type: BinaryOpType::Div
                })),
                op_type: BinaryOpType::Add
            }
        );
    }
    #[test]
    fn test_parse_binary_expr() {
        assert_eq!(
            parse_binary_expression("1 + 2"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::Literal(Literal::Int(2)),
                op_type: BinaryOpType::Add
            }
        );
        assert_eq!(
            parse_binary_expression("2 > 1"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(2)),
                right: Expr::Literal(Literal::Int(1)),
                op_type: BinaryOpType::Gt
            }
        );
        assert_eq!(
            parse_binary_expression("2 == 2"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(2)),
                right: Expr::Literal(Literal::Int(2)),
                op_type: BinaryOpType::Eq
            }
        );
        assert_eq!(
            parse_binary_expression("true || false"),
            BinaryOp {
                left: Expr::Literal(Literal::Bool(true)),
                right: Expr::Literal(Literal::Bool(false)),
                op_type: BinaryOpType::Or
            }
        );
        assert_eq!(
            parse_binary_expression("true && false"),
            BinaryOp {
                left: Expr::Literal(Literal::Bool(true)),
                right: Expr::Literal(Literal::Bool(false)),
                op_type: BinaryOpType::And
            }
        );
    }
    #[test]
    fn test_parse_sub_expressions() {
        assert_eq!(
            parse_binary_expression("1 + (2 + 1)"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                right: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::Literal(Literal::Int(2)),
                    right: Expr::Literal(Literal::Int(1)),
                    op_type: BinaryOpType::Add
                })),
                op_type: BinaryOpType::Add
            }
        );
        assert_eq!(
            parse_binary_expression("(1 + 2) + 1"),
            BinaryOp {
                left: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::Literal(Literal::Int(1)),
                    right: Expr::Literal(Literal::Int(2)),
                    op_type: BinaryOpType::Add
                })),
                right: Expr::Literal(Literal::Int(1)),
                op_type: BinaryOpType::Add
            }
        );
        assert_eq!(
            parse_binary_expression("(1 + 2 - (1 + 1)) + 1"),
            BinaryOp {
                left: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::BinaryOp(Box::new(BinaryOp {
                        left: Expr::Literal(Literal::Int(1)),
                        right: Expr::Literal(Literal::Int(2)),
                        op_type: BinaryOpType::Add
                    })),
                    right: Expr::BinaryOp(Box::new(BinaryOp {
                        left: Expr::Literal(Literal::Int(1)),
                        right: Expr::Literal(Literal::Int(1)),
                        op_type: BinaryOpType::Add
                    })),
                    op_type: BinaryOpType::Sub
                })),
                right: Expr::Literal(Literal::Int(1)),
                op_type: BinaryOpType::Add
            }
        );
        assert_eq!(
            parse_binary_expression("1 + n"),
            BinaryOp {
                left: Expr::Literal(Literal::Int(1)),
                op_type: BinaryOpType::Add,
                right: Expr::Symbol(String::from("n"))
            }
        );
    }
}
