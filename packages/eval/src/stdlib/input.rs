use std::io;

use ast::{Call, Literal, Located};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};

use crate::{scope::Scope, Context};

pub fn stdlib_input<T: Scope>(
    _ctx: &Context<T>,
    call: Located<Call>,
) -> DashlangResult<Located<Literal>> {
    let mut input = String::new();
    match io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Located::new(
            Literal::String(Located::new(ast::Str { value: input }, call.location)),
            call.location,
        )),
        Err(_) => Err(DashlangError::new(
            "Could not get input",
            ErrorKind::Runtime(RuntimeErrorKind::Default),
        )
        .location(call.location)),
    }
}
