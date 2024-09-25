use ast::{Call, Literal, Located};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};

use crate::{eval, scope::Scope, Context};

pub fn stdlib_map_get<T: Scope + Clone>(
    ctx: &Context<T>,
    call: Located<Call>,
) -> DashlangResult<Located<Literal>> {
    let mut iter_args = call.value.args.into_iter();
    let map_arg = iter_args.next().ok_or(
        DashlangError::new(
            "Expected 'map' arg, but none was provided",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(call.location),
    )?;
    let map_arg_location = map_arg.location;
    let key_arg = iter_args.next().ok_or(
        DashlangError::new(
            "Expected 'key' arg, but none was provided",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(call.location),
    )?;
    let key_arg_location = key_arg.location;

    if let Literal::Map(map) = eval(map_arg, ctx)?.value {
        if let Literal::String(key) = eval(key_arg, ctx)?.value {
            match map.value.value.get(&key.value.value) {
                Some(found) => return eval(found.clone(), ctx),
                None => {
                    return Ok(Located::new(
                        Literal::Null(Located::new((), call.location)),
                        call.location,
                    ))
                }
            }
        }
        return Err(DashlangError::new(
            "Expected argument to be a string",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        )
        .location(key_arg_location));
    }
    Err(DashlangError::new(
        "Expected argument to be a map",
        ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
    )
    .location(map_arg_location))
}
