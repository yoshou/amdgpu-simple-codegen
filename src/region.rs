use crate::{
    dominators::{BasicBlockRefNode, ControlFlowGraph, DominatorTree},
    ir::{BasicBlockRef, Function},
};
use std::{cell::RefCell, rc::{Rc, Weak}};

#[derive(Clone, Debug)]
pub struct Region {
    pub entry: BasicBlockRef,
    pub exit: Option<BasicBlockRef>,
    pub subregions: Vec<Rc<RefCell<Region>>>,
}

pub struct RegionInfo {
    pub regions: Vec<Rc<RefCell<Region>>>,
}

impl RegionInfo {
    fn build_region_tree_internal(
        &self,
        node: &BasicBlockRefNode,
        dom_tree: &DominatorTree,
        region: Rc<RefCell<Region>>,
    ) {
        let subregion = if let Some(subregion) = self
            .regions
            .iter()
            .find(|region| Weak::ptr_eq(&region.borrow().entry, &node.data))
        {
            region.borrow_mut().subregions.push(subregion.clone());
            subregion
        } else {
            &region
        };

        for child in dom_tree.get_children(node) {
            self.build_region_tree_internal(child, dom_tree, subregion.clone());
        }
    }

    fn build_region_tree(&mut self, entry_node: &BasicBlockRefNode, dom_tree: &DominatorTree) {
        let root_region = self.add_region(entry_node.data.clone(), None);

        self.build_region_tree_internal(entry_node, dom_tree, root_region);
    }

    fn add_region(
        &mut self,
        entry: BasicBlockRef,
        exit: Option<BasicBlockRef>,
    ) -> Rc<RefCell<Region>> {
        let region = Rc::new(RefCell::new(Region {
            entry: entry,
            exit: exit,
            subregions: vec![],
        }));
        self.regions.push(region);
        self.regions.last().unwrap().clone()
    }

    pub fn from_function(function: &Function) -> Self {
        let cfg = ControlFlowGraph::from_function(function);
        let dom_tree = DominatorTree::from_cfg(&cfg);
        let postdom_tree = DominatorTree::from_cfg_post(&cfg);

        let mut region_info = RegionInfo { regions: vec![] };

        for node in dom_tree.get_nodes() {
            let exit_node = postdom_tree.get_post_node(node);

            if let Some(exit_node) = exit_node {
                region_info.add_region(node.data.clone(), Some(exit_node.data.clone()));
            }
        }

        region_info.build_region_tree(&cfg.get_entry_node(), &dom_tree);
        region_info
    }
}
