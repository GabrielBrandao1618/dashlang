pub mod errors;
pub mod scope;
pub mod stdlib;
#[cfg(test)]
mod tests;

use std::{cmp::Ordering, collections::HashMap, rc::Rc};

use ast::{
    BinaryExpr, BinaryOperator, Boolean, Call, Expr, Float, Instruction, Int, Literal, Program,
    Stmt, UnaryExpr, Void,
};

use errors::RuntimeErrorKind;
use scope::Scope;

use crate::errors::RuntimeError;

macro_rules! define_aritmetic_operation {
    ($operator:tt, $op:expr, $scope:expr) => {
        match ($op.left, $op.right) {
            (Expr::Literal(left), Expr::Literal(right)) => match (left, right) {
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Int(Int{value: left.value $operator right.value, location: Default::default()})),
                (Literal::Float(left), Literal::Int(right)) => Ok(Literal::Float(Float{value: left.value $operator (right.value as f64), location: Default::default()})),
                (Literal::Int(left), Literal::Float(right)) => Ok(Literal::Float(Float{value: (left.value as f64) $operator right.value, location: Default::default()})),
                (Literal::Float(left), Literal::Float(right)) => Ok(Literal::Float(Float{value: left.value $operator right.value, location: Default::default()})),
                (_, _) => Err(RuntimeError::new("Invalid operation", RuntimeErrorKind::InvalidOperation).location($op.location)),
            },
            (left, right) => eval_binary_op(
                BinaryExpr::new(
                    Expr::Literal(eval(left, $scope)?),
                    Expr::Literal(eval(right, $scope)?),
                    $op.operator,
                ),
                $scope
            ),
        }
    };
}

macro_rules! define_boolean_operation {
    ($operator:tt, $op:expr, $scope:expr) => {
        match ($op.left, $op.right) {
            (Expr::Literal(left), Expr::Literal(right)) => match (left, right) {
                (Literal::Int(left), Literal::Int(right)) => Ok(Literal::Bool(Boolean{value: left.value $operator right.value, location: Default::default()})),
                (Literal::Float(left), Literal::Int(right)) => Ok(Literal::Bool(Boolean{value: left.value $operator (right.value as f64), location: Default::default()})),
                (Literal::Int(left), Literal::Float(right)) => Ok(Literal::Bool(Boolean{value: (left.value as f64) $operator right.value, location: Default::default()})),
                (Literal::Float(left), Literal::Float(right)) => Ok(Literal::Bool(Boolean{value: left.value $operator right.value, location: Default::default()})),
                (_, _) => Err(RuntimeError::new("Invalid operation", RuntimeErrorKind::InvalidOperation).location($op.location)),
            },
            (left, right) => eval_binary_op(
                BinaryExpr::new(
                    Expr::Literal(eval(left, $scope)?),
                    Expr::Literal(eval(right, $scope)?),
                    $op.operator,
                ),
                $scope
            ),
        }
    };
}

fn is_truthy<T: Scope + Clone>(expr: Expr, scope: &Context<T>) -> Result<bool, RuntimeError> {
    match expr {
        Expr::Literal(value) => match value {
            Literal::Closure(_) => Ok(true),
            Literal::Int(num) => Ok(num.value != 0),
            Literal::Float(num) => Ok(num.value != 0.0),
            Literal::String(string) => Ok(!string.value.is_empty()),
            Literal::Vector(val) => Ok(!val.value.is_empty()),
            Literal::Bool(val) => Ok(val.value),
            Literal::Null(_) => Ok(false),
            Literal::Void(_) => Ok(false),
        },
        expr => is_truthy(Expr::Literal(eval(expr, scope)?), scope),
    }
}

fn eval_binary_op<T: Scope + Clone>(
    op: BinaryExpr,
    ctx: &Context<T>,
) -> Result<Literal, RuntimeError> {
    match op.operator {
        BinaryOperator::Add => define_aritmetic_operation!(+, op, ctx),
        BinaryOperator::Sub => define_aritmetic_operation!(-, op, ctx),
        BinaryOperator::Mul => define_aritmetic_operation!(*, op, ctx),
        BinaryOperator::Div => define_aritmetic_operation!(/, op, ctx),
        BinaryOperator::Gt => define_boolean_operation!(>, op, ctx),
        BinaryOperator::Eq => define_boolean_operation!(==, op, ctx),
        BinaryOperator::Ge => define_boolean_operation!(>=, op, ctx),
        BinaryOperator::Lt => define_boolean_operation!(<, op, ctx),
        BinaryOperator::Le => define_boolean_operation!(<=, op, ctx),
        BinaryOperator::And => Ok(Literal::Bool(Boolean {
            value: is_truthy(op.left, ctx)? && is_truthy(op.right, ctx)?,
            location: op.location,
        })),
        BinaryOperator::Or => Ok(Literal::Bool(Boolean {
            value: is_truthy(op.left, ctx)? || is_truthy(op.right, ctx)?,
            location: op.location,
        })),
    }
}
fn eval_unary_op<T: Scope + Clone>(
    op: UnaryExpr,
    ctx: &Context<T>,
) -> Result<Literal, RuntimeError> {
    match op.operator {
        ast::UnaryOperator::Not => Ok(Literal::Bool(Boolean {
            value: !is_truthy(op.operand, ctx)?,
            location: op.location,
        })),
    }
}

pub fn eval_program<T: Scope + Clone>(
    program: Program,
    ctx: &Context<T>,
) -> Result<Literal, RuntimeError> {
    for instruction in program {
        match instruction {
            Instruction::Stmt(stmt) => match stmt {
                Stmt::Return(val) => {
                    return eval(val.value, ctx);
                }
                Stmt::If(if_stmt) => {
                    if is_truthy(if_stmt.cond, ctx)? {
                        let block_result = eval_program(if_stmt.body, ctx)?;
                        match block_result {
                            Literal::Void(_) => (),
                            val => return Ok(val),
                        }
                    } else if let Some(else_block) = if_stmt.else_block {
                        let block_result = eval_program(else_block, ctx)?;
                        match block_result {
                            Literal::Null(_) => (),
                            val => return Ok(val),
                        }
                    }
                }
                Stmt::While(while_stmt) => {
                    while is_truthy(while_stmt.clone().cond, ctx)? {
                        let block_result = eval_program(while_stmt.clone().body, ctx)?;
                        match block_result {
                            Literal::Void(_) => (),
                            val => return Ok(val),
                        }
                    }
                }
                Stmt::For(for_stmt) => {
                    eval_program(vec![for_stmt.clone().init], ctx)?;
                    while is_truthy(for_stmt.clone().cond, ctx)? {
                        let block_result = eval_program(for_stmt.clone().body, ctx)?;
                        match block_result {
                            Literal::Void(_) => (),
                            val => return Ok(val),
                        }
                        eval_program(vec![for_stmt.clone().iteration], ctx)?;
                    }
                }
            },
            Instruction::Expr(expr) => {
                eval(expr, ctx)?;
            }
        }
    }
    Ok(Literal::Void(Void {
        location: Default::default(),
    }))
}

fn eval_call<T: Scope + Clone>(call: Call, ctx: &Context<T>) -> Result<Literal, RuntimeError> {
    if let Some(found_extension) = ctx.extensions.get(&call.symbol) {
        match found_extension.params.len().cmp(&call.args.len()) {
            Ordering::Less | Ordering::Greater => {
                return Err(RuntimeError::new(
                    &format!(
                        "Could not evaluate '{}'. Expected {} arguments, but {} were given instead",
                        call.symbol,
                        found_extension.params.len(),
                        call.args.len()
                    ),
                    RuntimeErrorKind::WrongArgs,
                )
                .location(call.location))
            }
            Ordering::Equal => {
                let local_context = ctx.clone();
                let args: Result<Vec<Literal>, RuntimeError> = call
                    .clone()
                    .args
                    .into_iter()
                    .map(|expr| eval(expr, &local_context))
                    .collect();
                match args {
                    Ok(ok_args) => {
                        for (symbol, val) in found_extension.params.iter().zip(ok_args) {
                            // Inject all arguments into local scope
                            local_context.scope.set(symbol, val);
                        }
                    }
                    Err(args_err) => return Err(args_err),
                }
                return (found_extension.implementation)(&local_context, call);
            }
        }
    }
    if let Literal::Closure(closure) = ctx.scope.get(&call.symbol) {
        match closure.params.len().cmp(&call.args.len()) {
            Ordering::Less | Ordering::Greater => {
                return Err(RuntimeError::new(
                    &format!(
                    "Could not evaluate '{}'. Expected {} argument{s}, but {} {s1} given instead",
                    call.symbol,
                    closure.params.len(),
                    call.args.len(),
                    s = if closure.params.len() > 1 as usize {"s"} else {""},
                    s1 = if call.args.len() > 1 {"were"} else {"was"}
                ),
                    RuntimeErrorKind::WrongArgs,
                )
                .location(call.location))
            }
            Ordering::Equal => {
                let local_context = ctx.clone();
                let args: Result<Vec<Literal>, RuntimeError> = call
                    .args
                    .into_iter()
                    .map(|expr| eval(expr, &local_context))
                    .collect();
                match args {
                    Ok(ok_args) => {
                        for (symbol, val) in closure.params.iter().zip(ok_args) {
                            // Inject all arguments into local scope
                            local_context.scope.set(symbol, val);
                        }
                    }
                    Err(args_err) => return Err(args_err),
                }
                return eval_program(closure.body, &local_context);
            }
        }
    }
    Err(RuntimeError::new(
        &format!("Cannot call '{}': not callable", call.symbol),
        RuntimeErrorKind::NonCallable,
    )
    .location(call.location))
}

pub fn eval<T: Scope + Clone>(expr: Expr, ctx: &Context<T>) -> Result<Literal, RuntimeError> {
    match expr {
        Expr::Literal(val) => Ok(val),
        Expr::BinaryExpr(op) => eval_binary_op(*op, ctx),
        Expr::Assignment(assign) => {
            let evaluated = eval(*assign.value, ctx)?;
            ctx.scope.set(&assign.symbol, evaluated.clone());
            Ok(evaluated)
        }
        Expr::Call(call) => eval_call(call, ctx),
        Expr::Symbol(symbol) => Ok(ctx.scope.get(&symbol.value)),
        Expr::UnaryExpr(op) => eval_unary_op(*op, ctx),
    }
}
type ExtensionImplementation<S> = dyn Fn(&Context<S>, Call) -> Result<Literal, RuntimeError>;
#[derive(Clone)]
pub struct Extension<S: Scope> {
    pub params: Vec<String>,
    pub implementation: Rc<ExtensionImplementation<S>>,
}
pub trait Plugin<T: Scope> {
    fn get_extensions(&self) -> Vec<(String, Extension<T>)>;
}
pub struct Context<T: Scope> {
    scope: T,
    extensions: HashMap<String, Extension<T>>,
}
impl<T: Scope + Clone> Context<T> {
    pub fn new(s: T) -> Self {
        Self {
            scope: s,
            extensions: HashMap::new(),
        }
    }
    pub fn use_extension(&mut self, extension: Extension<T>, name: String) {
        self.extensions.insert(name, extension);
    }
    pub fn run_program(&self, program: Program) -> Result<Literal, RuntimeError> {
        eval_program(program, self)
    }
    pub fn use_plugin(&mut self, plug: &dyn Plugin<T>) {
        for (name, extension) in plug.get_extensions() {
            self.use_extension(extension, name);
        }
    }
}

impl<T: Scope + Clone> Clone for Context<T> {
    fn clone(&self) -> Self {
        Self {
            scope: self.scope.clone(),
            extensions: self.extensions.clone(),
        }
    }
}
