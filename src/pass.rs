use std::{cell::RefCell, rc::Rc};

use crate::{
    ir::Function,
    region::{Region, RegionInfo},
};

pub trait ModulePass {
    fn run_on_module(&mut self);
}
pub trait FunctionPass {
    fn run_on_function(&mut self, function: Rc<RefCell<Function>>)
        -> Option<Rc<RefCell<Function>>>;
}
pub trait RegionPass {
    fn run_on_region(
        &mut self,
        region: Rc<RefCell<Region>>,
        function: Rc<RefCell<Function>>,
        region_info: &RegionInfo,
    ) -> Option<Rc<RefCell<Function>>>;
}
