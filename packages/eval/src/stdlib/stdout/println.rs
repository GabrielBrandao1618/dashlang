use ast::{Call, Literal};
use errors::{DashlangError, ErrorKind};

use crate::{eval, scope::Scope, stdlib::stdout::literal_display::stdlib_literal_display, Context};

pub fn stdlib_println<T: Scope + Clone>(
    call: Call,
    ctx: &Context<T>,
) -> Result<Literal, DashlangError> {
    let mut iter_args = call.args.into_iter();
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
    println!("{}", stdlib_literal_display(&value, ctx)?);
    Ok(value)
}
