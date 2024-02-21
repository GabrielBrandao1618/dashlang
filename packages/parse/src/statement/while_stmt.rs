use ast::{Expr, Value, While};
use pest::Parser;

use crate::{
    expression::parse_expression,
    parser::{DashlangParser, Rule},
    scope::parse_scope,
};

pub fn parse_while_stmt(input: &str) -> While {
    let mut final_while = While {
        cond: Expr::Value(Value::Bool(true)),
        body: vec![],
    };
    let ast = DashlangParser::parse(Rule::while_stmt, input)
        .expect("Could not parse while loop")
        .next()
        .unwrap();
    for element in ast.into_inner() {
        match element.as_rule() {
            Rule::expression => {
                final_while.cond = parse_expression(element.as_str());
            }
            Rule::scope => {
                final_while.body = parse_scope(element.as_str());
            }
            _ => unreachable!(),
        }
    }
    final_while
}
#[cfg(test)]
mod tests {
    use ast::{BinaryOp, BinaryOpType, Instruction, Stmt};

    use super::*;
    #[test]
    fn test_parse_while() {
        assert_eq!(
            parse_while_stmt("while count < 10 {}"),
            While {
                cond: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::Symbol(String::from("count")),
                    right: Expr::Value(Value::Int(10)),
                    op_type: BinaryOpType::Lt
                })),
                body: vec![]
            }
        );

        assert_eq!(
            parse_while_stmt("while count < 10 {return 1}"),
            While {
                cond: Expr::BinaryOp(Box::new(BinaryOp {
                    left: Expr::Symbol(String::from("count")),
                    right: Expr::Value(Value::Int(10)),
                    op_type: BinaryOpType::Lt
                })),
                body: vec![Instruction::Stmt(Stmt::Return(Expr::Value(Value::Int(1))))]
            }
        );
    }
}
