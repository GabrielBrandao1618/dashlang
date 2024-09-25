use ast::{Call, Expr, Literal, Located};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};

use crate::{eval, scope::Scope, Context};

pub fn stdlib_map_set<T: Scope + Clone>(
    ctx: &Context<T>,
    call: Located<Call>,
) -> DashlangResult<Located<Literal>> {
    let mut iter_args = call.value.args.into_iter();
    let arg_map = iter_args.next().ok_or_else(|| {
        DashlangError::new(
            "Expected 'map' arg, but none was provided",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(call.location)
    })?;
    let arg_key = iter_args.next().ok_or_else(|| {
        DashlangError::new(
            "Expected 'key' arg, but none was provided",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(call.location)
    })?;
    let arg_value = iter_args.next().ok_or_else(|| {
        DashlangError::new(
            "Expected 'value' arg, but none was provided",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(call.location)
    })?;
    let map = eval(arg_map, ctx)?;
    if let Literal::Map(mut lit_map) = map.value {
        if let Expr::Literal(lit) = arg_key.value {
            if let Literal::String(key) = lit.value {
                lit_map.value.value.insert(key.value.value, arg_value);
                return Ok(Located::new(Literal::Map(lit_map), key.location));
            }
        }
        return Err(DashlangError::new(
            "Expected key to be string",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(call.location));
    }
    Err(DashlangError::new(
        "Expected arg to be a map",
        ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
    )
    .location(call.location))
}
