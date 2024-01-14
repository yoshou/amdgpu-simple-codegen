use crate::{
    dominators::{BasicBlockRefNode, ControlFlowGraph, DominatorTree},
    ir::{BasicBlockRef, Function},
};
use std::{
    cell::RefCell,
    collections::HashSet,
    hash::{Hash, Hasher},
    rc::{Rc, Weak},
};

pub type RegionRef = Rc<RefCell<Region>>;

#[derive(Clone, Debug)]
pub struct RegionNode {
    pub data: RegionRef,
}

impl PartialEq for RegionNode {
    fn eq(&self, other: &RegionNode) -> bool {
        Rc::ptr_eq(&self.data, &other.data)
    }
}

impl Eq for RegionNode {}

impl Hash for RegionNode {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        hasher.write_usize(Rc::as_ptr(&self.data) as usize);
    }
}

#[derive(Clone, Debug)]
pub struct Region {
    pub entry: BasicBlockRef,
    pub exit: Option<BasicBlockRef>,
    pub parent: Option<Weak<RefCell<Region>>>,
    pub subregions: Vec<RegionNode>,
}

pub struct RegionInfo {
    pub root_region: RegionNode,
    pub regions: Vec<RegionNode>,
}

impl RegionInfo {
    fn build_region_tree(
        &self,
        node: &BasicBlockRefNode,
        dom_tree: &DominatorTree,
        region: Weak<RefCell<Region>>,
    ) {
        let mut region = region;
        while let Some(exit) = &region.upgrade().unwrap().borrow().exit {
            if !Weak::ptr_eq(exit, &node.data) {
                break;
            }
            region = region.upgrade().unwrap().borrow().parent.clone().unwrap();
        } 
        let subregion = if let Some(subregion) = self
            .regions
            .iter()
            .find(|region| Weak::ptr_eq(&region.data.borrow().entry, &node.data))
        {
            region.upgrade().unwrap().borrow_mut().subregions.push(subregion.clone());
            subregion.data.borrow_mut().parent = Some(region);
            Rc::downgrade(&subregion.data)
        } else {
            region
        };

        for child in dom_tree.get_children(node) {
            self.build_region_tree(child, dom_tree, subregion.clone());
        }
    }

    pub fn dfs_nodes_post(
        &self,
        node: RegionNode,
        action: &mut impl FnMut(&RegionNode) -> (),
        visited: &mut HashSet<RegionNode>,
    ) {
        if visited.contains(&node) {
            return;
        }
        visited.insert(node.clone());

        for child in &node.data.borrow().subregions {
            self.dfs_nodes_post(child.clone(), action, visited);
        }
        action(&node);
    }

    fn add_region(
        &mut self,
        entry: BasicBlockRef,
        exit: Option<BasicBlockRef>,
    ) -> Rc<RefCell<Region>> {
        let region = Rc::new(RefCell::new(Region {
            entry: entry,
            exit: exit,
            parent: None,
            subregions: vec![],
        }));
        self.regions.push(RegionNode { data: region });
        self.regions.last().unwrap().data.clone()
    }

    pub fn from_function(function: &Function) -> Self {
        let cfg = ControlFlowGraph::from_function(function);
        let dom_tree = DominatorTree::from_cfg(&cfg);
        let postdom_tree = DominatorTree::from_cfg_post(&cfg);

        let root_region = Rc::new(RefCell::new(Region {
            entry: cfg.get_entry_node().data,
            exit: None,
            parent: None,
            subregions: vec![],
        }));

        let mut region_info = RegionInfo {
            regions: vec![],
            root_region: RegionNode { data: root_region.clone() },
        };

        for node in dom_tree.get_nodes() {
            let mut post_node = postdom_tree.get_post_node(node);

            while let Some(exit_node) = post_node {
                region_info.add_region(node.data.clone(), Some(exit_node.data.clone()));

                post_node = postdom_tree.get_post_node(exit_node)
            }
        }

        region_info.build_region_tree(&cfg.get_entry_node(), &dom_tree, Rc::downgrade(&region_info.root_region.data));
        region_info
    }
}
