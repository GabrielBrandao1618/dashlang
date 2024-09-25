use ast::{Call, Literal, Located};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};

use crate::{eval, scope::Scope, Context};

pub fn stdlib_nth<T: Scope + Clone>(
    ctx: &Context<T>,
    call: Located<Call>,
) -> DashlangResult<Located<Literal>> {
    let mut iter_args = call.value.args.into_iter();
    let value = eval(iter_args.next().unwrap(), ctx)?;
    let index = eval(iter_args.next().unwrap(), ctx)?;
    if let Literal::Int(int_index) = index.value {
        if let Literal::Vector(vec) = value.value {
            if (int_index.value.value as usize) < vec.value.value.len() {
                return eval(vec.value.value[int_index.value.value as usize].clone(), ctx);
            }
            return Err(DashlangError::new(
                "Index out of bound",
                ErrorKind::Runtime(RuntimeErrorKind::Default),
            )
            .location(call.location));
        }
        return Err(DashlangError::new(
            "Index out of bound",
            ErrorKind::Runtime(RuntimeErrorKind::Default),
        )
        .location(call.location));
    }
    Err(DashlangError::new(
        "Index out of bound",
        ErrorKind::Runtime(RuntimeErrorKind::Default),
    )
    .location(call.location))
}
