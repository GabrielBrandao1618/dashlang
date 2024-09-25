use std::rc::Rc;

use ast::{Call, Literal, Located};
use errors::DashlangResult;

use crate::{scope::Scope, Context};

type ExtensionImplementation<S> =
    dyn Fn(&Context<S>, Located<Call>) -> DashlangResult<Located<Literal>>;
#[derive(Clone)]
pub struct Extension<S: Scope> {
    pub implementation: Rc<ExtensionImplementation<S>>,
}
pub trait Plugin<T: Scope> {
    fn get_extensions(&self) -> Vec<(String, Extension<T>)>;
}
