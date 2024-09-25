mod map;

use ast::{
    Atom, Boolean, Closure, Expr, Float, Int, Literal, Located, Location, Str, Tuple, Vector,
};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};
use pest::Parser;

use crate::body::parse_body;
use crate::expression::parse_expression;
use crate::parser::{DashlangParser, Rule};
use crate::utils::get_pair_location;

use self::map::parse_map;

pub fn parse_literal(input: &str, base_location: usize) -> DashlangResult<Located<Literal>> {
    let parsed = DashlangParser::parse(Rule::literal, input)
        .expect("Could not parse value")
        .next()
        .expect("Could not parse value");
    let (start, end) = get_pair_location(&parsed);
    if parsed.as_rule() != Rule::literal {
        return Err(DashlangError::new(
            "Expected rule to be a literal",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location((start, end).into()));
    }
    let inner_value = parsed.into_inner().next().expect("Could not parse literal");
    match inner_value.as_rule() {
        Rule::int => {
            let parsed: i64 = inner_value.as_str().parse().map_err(|_| {
                DashlangError::new(
                    "Could not parse integer literal",
                    ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation),
                )
                .location((start, end).into())
            })?;
            Ok(Literal::Int(Int {
                value: parsed,
                location: Location::new(start + base_location, end + base_location),
            }))
        }
        Rule::float => {
            let parsed: f64 = inner_value.as_str().parse().map_err(|_| {
                DashlangError::new(
                    "Could not parse float literal",
                    ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation),
                )
                .location((start, end).into())
            })?;
            Ok(Literal::Float(Float {
                value: parsed,
                location: Location::new(start + base_location, end + base_location),
            }))
        }
        Rule::boolean => {
            let val = inner_value.as_str() == "true";
            Ok(Literal::Bool(Boolean {
                value: val,
                location: Location::new(start + base_location, end + base_location),
            }))
        }
        Rule::string => Ok(Literal::String(Str {
            value: inner_value
                .into_inner()
                .next()
                .ok_or(DashlangError::new(
                    "Could not parse string",
                    ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation),
                ))?
                .as_str()
                .to_owned(),
            location: Location::new(start + base_location, end + base_location),
        })),
        Rule::closure => {
            let mut inner_ast = inner_value.into_inner();
            let params: Vec<String> = inner_ast
                .next()
                .expect("Could not get closure params")
                .into_inner()
                .map(|component| component.as_str().to_owned())
                .collect();
            let ast_body = inner_ast.next().expect("Could not get closure body");
            let (body_start, _end) = get_pair_location(&ast_body);
            let body = parse_body(ast_body.as_str(), body_start + base_location)?;
            Ok(Literal::Closure(Closure {
                params,
                body,
                location: Location::new(start + base_location, end + base_location),
            }))
        }
        Rule::vector => {
            let inner_ast = inner_value.into_inner();
            let parsed_elements: DashlangResult<Vec<Expr>> = inner_ast
                .map(|element| {
                    let (element_start, _) = get_pair_location(&element);
                    parse_expression(element.as_str(), element_start + base_location)
                })
                .collect();
            Ok(Literal::Vector(Vector {
                value: parsed_elements?,
                location: Location::new(start + base_location, end + base_location),
            }))
        }
        Rule::tuple => {
            let parsed_elements: DashlangResult<Vec<Expr>> = inner_value
                .into_inner()
                .map(|element| {
                    let (element_start, _) = get_pair_location(&element);
                    parse_expression(element.as_str(), element_start + base_location)
                })
                .collect();

            Ok(Literal::Tuple(Tuple {
                value: parsed_elements?,
                location: (start + base_location, end + base_location).into(),
            }))
        }
        Rule::map => Ok(Literal::Map(parse_map(
            inner_value.as_str(),
            start + base_location,
        )?)),
        Rule::atom => {
            let (start, end) = get_pair_location(&inner_value);
            let atom_value = inner_value.into_inner().next().ok_or(DashlangError::new(
                "Could not parse atom",
                ErrorKind::Parsing(errors::ParsingErrorKind::Default),
            ))?;
            let parsed_atom_value = atom_value.as_str().to_owned();
            Ok(Literal::Atom(Atom {
                value: parsed_atom_value,
                location: (start + base_location, end + base_location).into(),
            }))
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use ast::{Atom, Closure, Expr, Return, Stmt, Tuple};

    use super::*;
    #[test]
    fn parse_value() {
        assert_eq!(
            parse_literal("10", 0),
            Ok(Literal::Int(Int {
                value: 10,
                location: Location::new(0, 2)
            }))
        );
        assert_eq!(
            parse_literal("-10", 0),
            Ok(Literal::Int(Int {
                value: -10,
                location: Location::new(0, 3)
            }))
        );
        assert_eq!(
            parse_literal("10.5", 0),
            Ok(Literal::Float(Float {
                value: 10.5,
                location: Location::new(0, 4)
            }))
        );
        assert_eq!(
            parse_literal("-10.5", 0),
            Ok(Literal::Float(Float {
                value: -10.5,
                location: Location::new(0, 5)
            }))
        );
        assert_eq!(
            parse_literal("true", 0),
            Ok(Literal::Bool(Boolean {
                value: true,
                location: Location::new(0, 4)
            }))
        );
        assert_eq!(
            parse_literal("false", 0),
            Ok(Literal::Bool(Boolean {
                value: false,
                location: Location::new(0, 5)
            }))
        );
        assert_eq!(
            parse_literal(r#""apple""#, 0),
            Ok(Literal::String(Str {
                value: String::from("apple"),
                location: Location::new(0, 7)
            }))
        );
        assert_eq!(
            parse_literal(r#""green apple""#, 0),
            Ok(Literal::String(Str {
                value: "green apple".to_owned(),
                location: Location::new(0, 13)
            }))
        );
        assert_eq!(
            parse_literal("(name, age) {return true}", 0),
            Ok(Literal::Closure(Closure {
                params: vec![String::from("name"), String::from("age")],
                body: vec![Stmt::Return(Return {
                    value: Expr::Literal(Literal::Bool(Boolean {
                        value: true,
                        location: Location::new(20, 24)
                    })),
                    location: Location::new(13, 24)
                })],
                location: Location::new(0, 25)
            }))
        );
    }
    #[test]
    fn test_parse_vector() {
        assert_eq!(
            parse_literal("[1, 8, 7]", 0),
            Ok(Literal::Vector(Vector {
                value: vec![
                    Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: Location::new(1, 2)
                    })),
                    Expr::Literal(Literal::Int(Int {
                        value: 8,
                        location: Location::new(4, 5)
                    })),
                    Expr::Literal(Literal::Int(Int {
                        value: 7,
                        location: Location::new(7, 8)
                    })),
                ],
                location: Location::new(0, 9)
            }))
        );
    }
    #[test]
    fn test_parse_tuple() {
        assert_eq!(
            parse_literal("(1, 4)", 0),
            Ok(Literal::Tuple(Tuple {
                value: vec![
                    Expr::Literal(Literal::Int(Int {
                        value: 1,
                        location: (1, 2).into()
                    })),
                    Expr::Literal(Literal::Int(Int {
                        value: 4,
                        location: (4, 5).into()
                    }))
                ],
                location: (0, 6).into()
            }))
        );
    }
    #[test]
    fn test_parse_atom() {
        assert_eq!(
            parse_literal(":ok", 0).unwrap().value,
            Literal::Atom(Located::new(
                Atom {
                    value: "ok".to_string(),
                },
                (0, 3).into()
            )),
        );
    }
}
