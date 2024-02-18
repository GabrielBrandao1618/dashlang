use ast::{Closure, Program, Value};
use pest::Parser;

use crate::parser::{DashlangParser, Rule};
use crate::scope::parse_scope;

pub fn parse_values(input: &str) -> Value {
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
            Value::Int(parsed)
        }
        Rule::float => {
            let parsed: f64 = inner_value
                .as_str()
                .parse()
                .expect("Could not parse float value");
            Value::Float(parsed)
        }
        Rule::boolean => {
            let val = inner_value.as_str() == "true";
            Value::Bool(val)
        }
        Rule::string => Value::String(
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
            Value::Closure(Closure { params, body })
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
        assert_eq!(parse_values("10"), Value::Int(10));
        assert_eq!(parse_values("-10"), Value::Int(-10));
        assert_eq!(parse_values("10.5"), Value::Float(10.5));
        assert_eq!(parse_values("-10.5"), Value::Float(-10.5));
        assert_eq!(parse_values("true"), Value::Bool(true));
        assert_eq!(parse_values("false"), Value::Bool(false));
        assert_eq!(
            parse_values(r#""apple""#),
            Value::String(String::from("apple"))
        );
        assert_eq!(
            parse_values(r#""green apple""#),
            Value::String(String::from("green apple"))
        );
        assert_eq!(
            parse_values("(name, age) {return true}"),
            Value::Closure(Closure {
                params: vec![String::from("name"), String::from("age")],
                body: vec![Instruction::Stmt(Stmt::Return(Expr::Value(Value::Bool(
                    true
                ))))]
            })
        );
    }
}