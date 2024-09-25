use ast::{Call, Expr, Literal, Located};
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};

use crate::{eval, scope::Scope, Context};

pub fn stdlib_push<T: Scope + Clone>(
    ctx: &Context<T>,
    call: Located<Call>,
) -> DashlangResult<Located<Literal>> {
    let mut iter_args = call.value.args.into_iter();
    let base = eval(
        iter_args.next().ok_or(
            DashlangError::new(
                "Expected 'base' argument",
                ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
            )
            .location(call.location),
        )?,
        ctx,
    )?;
    let item = eval(
        iter_args.next().ok_or(DashlangError::new(
            "Expected 'item' argument",
            ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
        ))?,
        ctx,
    )?;
    match base.value {
        Literal::String(mut val) => match item.value {
            Literal::String(str_push) => {
                val.value.value.push_str(&str_push.value.value);
                Ok(Located::new(Literal::String(val.clone()), val.location))
            }
            _ => Err(DashlangError::new(
                "Unsuported operation",
                ErrorKind::Runtime(RuntimeErrorKind::Default),
            )
            .location(call.location)),
        },
        Literal::Vector(mut vector) => {
            vector.value.value.push(Located::new(
                Expr::Literal(Located::new(item.value, item.location)),
                item.location,
            ));
            Ok(Located::new(
                Literal::Vector(vector.clone()),
                vector.location,
            ))
        }
        _ => Err(DashlangError::new(
            "Unsuported operation",
            ErrorKind::Runtime(RuntimeErrorKind::Default),
        )
        .location(call.location)),
    }
}
