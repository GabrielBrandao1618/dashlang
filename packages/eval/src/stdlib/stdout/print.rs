use ast::{Call, Literal, Located};
use errors::{DashlangError, ErrorKind};

use crate::{eval, scope::Scope, stdlib::stdout::literal_display::stdlib_literal_display, Context};

pub fn stdlib_print<T: Scope + Clone>(
    call: Located<Call>,
    ctx: &Context<T>,
) -> Result<Located<Literal>, DashlangError> {
    let mut iter_args = call.value.args.into_iter();
    let value = eval(
        iter_args.next().ok_or(
            DashlangError::new(
                "Expected 'expr' argument",
                ErrorKind::Runtime(errors::RuntimeErrorKind::WrongArgs),
            )
            .location(call.location),
        )?,
        ctx,
    )?;
    print!("{}", stdlib_literal_display(&value, ctx)?);
    Ok(value)
}
