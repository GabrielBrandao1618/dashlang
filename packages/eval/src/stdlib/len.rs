use ast::{Call, Int, Literal, Located};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};

use crate::{eval, scope::Scope, Context};

pub fn stdlib_len<T: Scope + Clone>(
    ctx: &Context<T>,
    call: Located<Call>,
) -> DashlangResult<Located<Literal>> {
    let mut iter_args = call.value.args.into_iter();
    let item = eval(
        iter_args.next().ok_or(DashlangError::new(
            "Expected 'item' arg",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        ))?,
        ctx,
    )?;
    match item.value {
        Literal::String(val) => Ok(Located::new(
            Literal::Int(Located::new(
                ast::Int {
                    value: val.value.value.len() as i64,
                },
                val.location,
            )),
            val.location,
        )),
        Literal::Vector(val) => Ok(Located::new(
            Literal::Int(Located::new(
                Int {
                    value: val.value.value.len() as i64,
                },
                val.location,
            )),
            val.location,
        )),
        _ => Err(DashlangError::new(
            "Could not get length: unsuported operation",
            ErrorKind::Runtime(RuntimeErrorKind::Default),
        )
        .location(call.location)),
    }
}
