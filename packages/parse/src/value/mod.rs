use ast::{Closure, Literal, Program};
use pest::Parser;

use crate::parser::{DashlangParser, Rule};
use crate::scope::parse_scope;

pub fn parse_values(input: &str) -> Literal {
    let parsed = DashlangParser::parse(Rule::value, input)
        .expect("Could not parse value")
        .next()
        .expect("Could not parse value");
    if parsed.as_rule() != Rule::value {
        panic!("Expected rule to be value");
    }
    let inner_value = parsed.into_inner().next().expect("Could not parse value");
    match inner_value.as_rule() {
        Rule::int => {
            let parsed: i64 = inner_value
                .as_str()
                .parse()
                .expect("Could not parse integer value");
            Literal::Int(parsed)
        }
        Rule::float => {
            let parsed: f64 = inner_value
                .as_str()
                .parse()
                .expect("Could not parse float value");
            Literal::Float(parsed)
        }
        Rule::boolean => {
            let val = inner_value.as_str() == "true";
            Literal::Bool(val)
        }
        Rule::string => Literal::String(
            inner_value
                .into_inner()
                .next()
                .expect("Could not parse string")
                .as_str()
                .to_owned(),
        ),
        Rule::closure => {
            let mut params: Vec<String> = vec![];
            let mut body: Program = vec![];
            for component in inner_value.into_inner() {
                match component.as_rule() {
                    Rule::closure_params => {
                        for param in component.into_inner() {
                            params.push(param.as_str().to_owned());
                        }
                    }
                    Rule::scope => {
                        let parsed = parse_scope(component.as_str());
                        for instruction in parsed {
                            body.push(instruction);
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Literal::Closure(Closure { params, body })
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use ast::{Closure, Expr, Instruction, Stmt};

    use super::*;
    #[test]
    fn parse_value() {
        assert_eq!(parse_values("10"), Literal::Int(10));
        assert_eq!(parse_values("-10"), Literal::Int(-10));
        assert_eq!(parse_values("10.5"), Literal::Float(10.5));
        assert_eq!(parse_values("-10.5"), Literal::Float(-10.5));
        assert_eq!(parse_values("true"), Literal::Bool(true));
        assert_eq!(parse_values("false"), Literal::Bool(false));
        assert_eq!(
            parse_values(r#""apple""#),
            Literal::String(String::from("apple"))
        );
        assert_eq!(
            parse_values(r#""green apple""#),
            Literal::String(String::from("green apple"))
        );
        assert_eq!(
            parse_values("(name, age) {return true}"),
            Literal::Closure(Closure {
                params: vec![String::from("name"), String::from("age")],
                body: vec![Instruction::Stmt(Stmt::Return(Expr::Literal(
                    Literal::Bool(true)
                )))]
            })
        );
    }
}
