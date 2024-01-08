use std::{cell::RefCell, rc::Rc};

use crate::ir::{Function, Module};

pub trait ModulePass {
    fn run_on_module(&mut self);
}
pub trait FunctionPass {
    fn run_on_function(
        &mut self,
        function: Rc<RefCell<Function>>,
        module: &mut Module,
    ) -> Option<Rc<RefCell<Function>>>;
}
