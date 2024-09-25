pub mod ctx;
pub mod extension;
pub mod scope;
pub mod stdlib;
#[cfg(test)]
mod tests;

use std::cmp::Ordering;

use ast::{
    BinaryExpr, BinaryOperator, Boolean, Call, DestructuringAsignment, Expr, Float, Int, Literal,
    Located, Location, Program, Stmt, Tuple, UnaryExpr, Void,
};

use ctx::Context;
use errors::{DashlangError, DashlangResult, ErrorKind, RuntimeErrorKind};
use extension::{Extension, Plugin};
use scope::Scope;

macro_rules! define_aritmetic_operation {
    ($operator:tt, $op:expr, $scope:expr) => {
        match ($op.value.left, $op.value.right) {
            (Expr::Literal(left), Expr::Literal(right)) => match (left, right) {
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Int(Int{value: left.value $operator right.value, location: Default::default()})),
                (Literal::Float(left), Literal::Int(right)) => Ok(Literal::Float(Float{value: left.value $operator (right.value as f64), location: Default::default()})),
                (Literal::Int(left), Literal::Float(right)) => Ok(Literal::Float(Float{value: (left.value as f64) $operator right.value, location: Default::default()})),
                (Literal::Float(left), Literal::Float(right)) => Ok(Literal::Float(Float{value: left.value $operator right.value, location: Default::default()})),
                (_, _) => Err(DashlangError::new("Invalid operation", ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation)).location($op.location)),
            },
            (left, right) => eval_binary_expr(
                BinaryExpr::new(
                    Expr::Literal(eval(left, $scope)?),
                    Expr::Literal(eval(right, $scope)?),
                    $op.value.operator,
                ),
                $scope
            ),
        }
    };
}

macro_rules! define_bitwise_operation {
    ($operator:tt, $op:expr, $scope:expr) => {
        match ($op.left, $op.right) {
            (Expr::Literal(left), Expr::Literal(right)) => match (left, right) {
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Int(Int{value: left.value $operator right.value, location: Default::default()})),
                (_, _) => Err(DashlangError::new("Invalid operation", ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation)).location($op.location)),

            }
            (left, right) => eval_binary_expr(
                BinaryExpr::new(
                    Expr::Literal(eval(left, $scope)?),
                    Expr::Literal(eval(right, $scope)?),
                    $op.value.operator,
                ),
                $scope
            ),
        }

    };
}

macro_rules! define_boolean_operation {
    ($operator:tt, $op:expr, $scope:expr) => {
        match ($op.value.left.value, $op.value.right.value) {
            (Expr::Literal(left), Expr::Literal(right)) => match (left.value, right.value) {
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Bool(Boolean{value: left.value.value $operator right.value.value})),
                (Literal::Float(left), Literal::Int(right)) => Ok(Literal::Bool(Boolean{value: left.value.value $operator (right.value.value as f64), location: Default::default()})),
                (Literal::Int(left), Literal::Float(right)) => Ok(Literal::Bool(Boolean{value: (left.value.value as f64) $operator right.value.value, location: Default::default()})),
                (Literal::Float(left), Literal::Float(right)) => Ok(Literal::Bool(Boolean{value: left.value.value $operator right.value.value, location: Default::default()})),
                (_, _) => Err(DashlangError::new("Invalid operation", ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation)).location($op.location)),
            },
            (left, right) => eval_binary_expr(
                Located::new(BinaryExpr::new(
                    Located::new(Expr::Literal(eval(Located::new(left, (0, 0).into()), $scope)?), (0, 0).into()),
                    Located::new(Expr::Literal(eval(Located::new(right, (0, 0).into()), $scope)?), (0, 0).into()),
                    $op.value.operator,
                ), (0, 0).into()),
                $scope
            ),
        }
    };
}

fn is_truthy<T: Scope + Clone>(expr: Located<Expr>, scope: &Context<T>) -> DashlangResult<bool> {
    let expr_location = expr.location;
    match expr.value {
        Expr::Literal(value) => match value.value {
            Literal::Closure(_) => Ok(true),
            Literal::Int(num) => Ok(num.value.value != 0),
            Literal::Float(num) => Ok(num.value.value != 0.0),
            Literal::String(string) => Ok(!string.value.value.is_empty()),
            Literal::Vector(val) => Ok(!val.value.value.is_empty()),
            Literal::Bool(val) => Ok(val.value.value),
            Literal::Null(_) => Ok(false),
            Literal::Void(_) => Ok(false),
            Literal::Tuple(_) => Ok(false),
            Literal::Map(map) => Ok(!map.value.value.is_empty()),
            Literal::Atom(_) => Ok(true),
        },
        expr => is_truthy(
            Located::new(
                Expr::Literal(eval(Located::new(expr, expr_location), scope)?),
                expr_location,
            ),
            scope,
        ),
    }
}

fn eval_binary_expr<T: Scope + Clone>(
    op: Located<BinaryExpr>,
    ctx: &Context<T>,
) -> DashlangResult<Located<Literal>> {
    match op.value.operator {
        BinaryOperator::Add => define_aritmetic_operation!(+, op, ctx),
        BinaryOperator::Sub => define_aritmetic_operation!(-, op, ctx),
        BinaryOperator::Mul => define_aritmetic_operation!(*, op, ctx),
        BinaryOperator::Div => define_aritmetic_operation!(/, op, ctx),
        BinaryOperator::Gt => define_boolean_operation!(>, op, ctx),
        BinaryOperator::Eq => define_boolean_operation!(==, op, ctx),
        BinaryOperator::Ge => define_boolean_operation!(>=, op, ctx),
        BinaryOperator::Lt => define_boolean_operation!(<, op, ctx),
        BinaryOperator::Le => define_boolean_operation!(<=, op, ctx),
        BinaryOperator::And => {
            let left_evaluated = is_truthy(op.value.left, ctx)?;
            Ok(Located::new(
                Literal::Bool(Located::new(
                    Boolean {
                        value: if !left_evaluated {
                            false
                        } else {
                            is_truthy(op.value.right, ctx)?
                        },
                    },
                    op.location,
                )),
                op.location,
            ))
        }
        BinaryOperator::Or => {
            let left_evaluated = is_truthy(op.value.left, ctx)?;
            Ok(Located::new(
                Literal::Bool(Located::new(
                    Boolean {
                        value: if left_evaluated {
                            true
                        } else {
                            is_truthy(op.value.right, ctx)?
                        },
                    },
                    op.location,
                )),
                op.location,
            ))
        }
        BinaryOperator::BitwiseOr => define_bitwise_operation!(|, op, ctx),
        BinaryOperator::BitwiseAnd => define_bitwise_operation!(&, op, ctx),
        BinaryOperator::BitwiseShiftLeft => define_bitwise_operation!(<<, op, ctx),
        BinaryOperator::BitwiseShiftRight => define_bitwise_operation!(>>, op, ctx),
        BinaryOperator::BitwiseXor => define_bitwise_operation!(^, op, ctx),
    }
}
fn eval_unary_op<T: Scope + Clone>(
    op: Located<UnaryExpr>,
    ctx: &Context<T>,
) -> DashlangResult<Located<Literal>> {
    match op.value.operator {
        ast::UnaryOperator::Not => Ok(Located::new(
            Literal::Bool(Located::new(
                Boolean {
                    value: !is_truthy(op.value.operand, ctx)?,
                },
                op.location,
            )),
            op.location,
        )),
        ast::UnaryOperator::BitwiseNot => Ok(Located::new(
            Literal::Int(Located::new(
                Int {
                    value: match eval(op.value.operand, ctx)?.value {
                        Literal::Int(integer) => !integer.value.value,
                        _ => Err(DashlangError::new(
                            "Expected integer",
                            ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation),
                        ))?,
                    },
                },
                op.location,
            )),
            op.location,
        )),
    }
}

pub fn eval_program<T: Scope + Clone>(
    program: Program,
    ctx: &Context<T>,
) -> DashlangResult<Located<Literal>> {
    for stmt in program {
        match stmt {
            Stmt::Return(val) => {
                return eval(Located::new(val.value.value, val.location), ctx);
            }
            Stmt::If(if_stmt) => {
                if is_truthy(Located::new(if_stmt.value.cond, if_stmt.location), ctx)? {
                    let block_result = eval_program(if_stmt.value.body, ctx)?;
                    match block_result.value {
                        Literal::Void(_) => (),
                        val => return Ok(Located::new(val, block_result.location)),
                    }
                } else if let Some(else_block) = if_stmt.value.else_block {
                    let block_result = eval_program(else_block, ctx)?;
                    match block_result.value {
                        Literal::Null(_) => (),
                        val => return Ok(Located::new(val, block_result.location)),
                    }
                }
            }
            Stmt::While(while_stmt) => {
                while is_truthy(
                    Located::new(while_stmt.clone().value.cond, while_stmt.location),
                    ctx,
                )? {
                    let block_result = eval_program(while_stmt.clone().value.body, ctx)?;
                    match block_result.value {
                        Literal::Void(_) => (),
                        val => return Ok(Located::new(val, block_result.location)),
                    }
                }
            }
            Stmt::For(for_stmt) => {
                eval_program(vec![for_stmt.clone().value.init], ctx)?;
                while is_truthy(
                    Located::new(for_stmt.clone().value.cond, for_stmt.location),
                    ctx,
                )? {
                    let block_result = eval_program(for_stmt.clone().value.body, ctx)?;
                    match block_result.value {
                        Literal::Void(_) => (),
                        val => return Ok(Located::new(val, block_result.location)),
                    }
                    eval_program(vec![for_stmt.clone().value.iteration], ctx)?;
                }
            }
            Stmt::Expr(expr) => {
                eval(expr, ctx)?;
            }
        }
    }
    Ok(Located::new(
        Literal::Void(Located::new((), Default::default())),
        Default::default(),
    ))
}

fn eval_call<T: Scope + Clone>(
    call: Located<Call>,
    ctx: &Context<T>,
) -> DashlangResult<Located<Literal>> {
    if let Some(found_extension) = ctx.extensions.get(&call.value.symbol) {
        let local_context = ctx.clone();
        return (found_extension.implementation)(&local_context, call);
    }
    if let Literal::Closure(closure) = ctx.scope.get(&call.value.symbol) {
        match closure.value.params.len().cmp(&call.value.args.len()) {
            Ordering::Less | Ordering::Greater => {
                return Err(DashlangError::new(
                    &format!(
                    "Could not evaluate '{}'. Expected {} argument{s}, but {} {s1} given instead",
                    call.value.symbol,
                    closure.value.params.len(),
                    call.value.args.len(),
                    s = if closure.value.params.len() > 1_usize {"s"} else {""},
                    s1 = if call.value.args.len() > 1 {"were"} else {"was"}
                ),
                    ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
                )
                .location(call.location))
            }
            Ordering::Equal => {
                let local_context = ctx.clone();
                let args: Result<Vec<Located<Literal>>, DashlangError> = call
                    .value
                    .args
                    .into_iter()
                    .map(|expr| eval(expr, &local_context))
                    .collect();
                match args {
                    Ok(ok_args) => {
                        for (symbol, val) in closure.value.params.iter().zip(ok_args) {
                            // Inject all arguments into local scope
                            local_context.scope.set(symbol, val.value);
                        }
                    }
                    Err(args_err) => return Err(args_err),
                }
                return eval_program(closure.value.body, &local_context);
            }
        }
    }
    Err(DashlangError::new(
        &format!("Cannot call '{}': not callable", call.value.symbol),
        ErrorKind::Runtime(RuntimeErrorKind::NonCallable),
    )
    .location(call.location))
}

fn eval_destructuring_assign_expr<T: Scope + Clone>(
    expr: Located<DestructuringAsignment>,
    ctx: &Context<T>,
) -> DashlangResult<Located<Literal>> {
    let expr_location = expr.location;
    let value = eval(Located::new(*expr.value.value, expr_location), ctx)?;
    if let Literal::Tuple(tup) = value.value {
        let mut eval_expressions: Vec<Located<Expr>> = vec![];
        if expr.value.symbols.len() != tup.value.value.len() {
            return Err(DashlangError::new(
                "Number os elements in tuples don't match",
                ErrorKind::Runtime(RuntimeErrorKind::WrongArgs),
            )
            .location(expr.location));
        }
        for (symbol, expr) in expr.value.symbols.into_iter().zip(tup.value.value) {
            let evaluated_expr = eval(expr, ctx)?;
            eval_expressions.push(Located::new(
                Expr::Literal(evaluated_expr.clone()),
                evaluated_expr.location,
            ));
            ctx.scope.set(&symbol.value, evaluated_expr.value);
        }
        Ok(Located::new(
            Literal::Tuple(Located::new(
                Tuple {
                    value: eval_expressions,
                },
                expr.location,
            )),
            expr.location,
        ))
    } else {
        Err(DashlangError::new(
            "Expected value to be a tuple",
            ErrorKind::Runtime(RuntimeErrorKind::InvalidOperation),
        ))
    }
}

pub fn eval<T: Scope + Clone>(
    expr: Located<Expr>,
    ctx: &Context<T>,
) -> DashlangResult<Located<Literal>> {
    let expr_location = expr.location;
    match expr.value {
        Expr::Literal(val) => Ok(val),
        Expr::BinaryExpr(op) => eval_binary_expr(*op, ctx),
        Expr::Assignment(assign) => {
            let evaluated = eval(
                Located::new(Expr::Assignment(assign.clone()), expr_location),
                ctx,
            )?;
            ctx.scope.set(&assign.value.symbol, evaluated.value.clone());
            Ok(evaluated)
        }
        Expr::Call(call) => eval_call(call, ctx),
        Expr::Symbol(symbol) => Ok(Located::new(
            ctx.scope.get(&symbol.value.value),
            expr.location,
        )),
        Expr::UnaryExpr(op) => eval_unary_op(*op, ctx),
        Expr::SubExpr(sub) => eval(
            Located::new(
                Expr::SubExpr(Located::new(sub.value, expr_location)),
                expr_location,
            ),
            ctx,
        ),
        Expr::DestructuringAsignment(dest) => {
            eval_destructuring_assign_expr(Located::new(dest.value, expr_location), ctx)
        }
    }
}
