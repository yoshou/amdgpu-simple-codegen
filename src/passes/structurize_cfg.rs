use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::{Rc, Weak}, vec,
};

use crate::{
    dominators::BasicBlockRefNode,
    ir::{Function, Inst, Module, BasicBlock, Value, BranchInst, ValueName},
    pass::RegionPass,
    region::{Region, RegionInfo, RegionRef},
};

pub struct StructurizeCFG {}

pub struct RegionGraph {
    nodes: Vec<BasicBlockRefNode>,
    preds: Vec<Vec<usize>>,
    succs: Vec<Vec<usize>>,
}

fn get_succ_nodes(node: &BasicBlockRefNode) -> Vec<BasicBlockRefNode> {
    let mut nodes = vec![];

    for inst in &node.data.upgrade().unwrap().borrow().insts {
        match &*inst.upgrade().unwrap().borrow() {
            Inst::BranchInst(inst) => {
                nodes.push(BasicBlockRefNode {
                    data: inst.true_condition.clone(),
                });

                if let Some((bb, _)) = &inst.false_condition {
                    nodes.push(BasicBlockRefNode { data: bb.clone() });
                }
            }
            _ => {}
        }
    }

    nodes
}

pub fn dfs_region_nodes_post(
    node: &BasicBlockRefNode,
    exit: &BasicBlockRefNode,
    action: &mut impl FnMut(&BasicBlockRefNode) -> (),
    visited: &mut HashSet<BasicBlockRefNode>,
) {
    if visited.contains(&node) {
        return;
    }
    visited.insert(node.clone());

    if node == exit {
        return;
    }

    for succ in get_succ_nodes(node) {
        dfs_region_nodes_post(&succ, &exit, action, visited);
    }

    action(&node);
}

impl RegionGraph {
    pub fn from_entry(region: &RegionRef) -> Self {
        let mut nodes = vec![];
        let mut visited = HashSet::new();

        dfs_region_nodes_post(
            &BasicBlockRefNode {
                data: region.borrow().entry.clone(),
            },
            &BasicBlockRefNode {
                data: region.borrow().exit.clone().unwrap(),
            },
            &mut |node| {
                nodes.push(node.clone());
            },
            &mut visited,
        );

        let mut graph = RegionGraph {
            nodes: nodes.iter().map(|x| x.clone()).collect(),
            preds: (0..nodes.len()).map(|_| Vec::new()).collect(),
            succs: (0..nodes.len()).map(|_| Vec::new()).collect(),
        };
        for (i, node) in graph.nodes.iter().enumerate() {
            for succ in get_succ_nodes(node) {
                if let Some(j) = graph.nodes.iter().position(|x| *x == succ) {
                    graph.succs[i].push(j);
                    graph.preds[j].push(i);
                }
            }
        }
        graph
    }

    pub fn get_successor_nodes(&self, node: &BasicBlockRefNode) -> Vec<&BasicBlockRefNode> {
        let index = self.nodes.iter().position(|x| x == node).unwrap();
        self.succs[index]
            .iter()
            .map(|succ| &self.nodes[*succ])
            .collect()
    }

    pub fn dfs_nodes_post(
        &self,
        node: &BasicBlockRefNode,
        action: &mut impl FnMut(&BasicBlockRefNode) -> (),
        visited: &mut HashSet<BasicBlockRefNode>,
    ) {
        if visited.contains(&node) {
            return;
        }
        visited.insert(node.clone());

        for succ in self.get_successor_nodes(node) {
            self.dfs_nodes_post(succ, action, visited);
        }

        action(&node);
    }

    pub fn dfs_nodes_post_from_entry(
        &self,
        action: &mut impl FnMut(&BasicBlockRefNode) -> (),
        visited: &mut HashSet<BasicBlockRefNode>,
    ) {
        self.dfs_nodes_post(&self.nodes.last().unwrap(), action, visited)
    }

    pub fn inverse(&self) -> Self {
        let graph = RegionGraph {
            nodes: self.nodes.clone(),
            preds: self.succs.clone(),
            succs: self.preds.clone(),
        };
        graph
    }
}

struct SCCNode {
    pub data: Vec<BasicBlockRefNode>,
}

struct SCCGraph {
    nodes: Vec<SCCNode>,
    preds: Vec<Vec<usize>>,
    succs: Vec<Vec<usize>>,
}

impl SCCGraph {
    fn from_entry(region: &RegionRef) -> Self {
        let mut labels = HashMap::new();
        let mut visited = HashSet::new();

        let region_graph = RegionGraph::from_entry(region);

        region_graph.dfs_nodes_post_from_entry(
            &mut |node| {
                let label = labels.len();
                labels.insert(node.clone(), label);
            },
            &mut visited,
        );

        let mut vec = labels.iter().collect::<Vec<(_, _)>>();
        vec.sort_by(|(_, a), (_, b)| a.cmp(b));

        let mut scc_nodes = vec![];
        let mut scc_node_map = HashMap::new();

        let inv_region_graph = region_graph.inverse();

        let mut visited = HashSet::new();
        for (node, _) in vec.iter().rev() {
            let mut scc_node = SCCNode { data: vec![] };

            inv_region_graph.dfs_nodes_post(
                node,
                &mut |node| {
                    scc_node.data.push(node.clone());
                },
                &mut visited,
            );

            let scc_id = scc_nodes.len();
            for node in &scc_node.data {
                scc_node_map.insert(node.clone(), scc_id);
            }

            scc_nodes.push(scc_node);
        }

        let scc_graph_succs = (0..scc_nodes.len()).map(|_| vec![]).collect();
        let scc_graph_preds = (0..scc_nodes.len()).map(|_| vec![]).collect();
        let mut scc_graph = SCCGraph {
            nodes: scc_nodes,
            succs: scc_graph_succs,
            preds: scc_graph_preds,
        };
        for (i, scc_node) in scc_graph.nodes.iter().enumerate() {
            for node in &scc_node.data {
                for succ in get_succ_nodes(node) {
                    if let Some(j) = scc_node_map.get(&succ) {
                        if i != *j {
                            scc_graph.succs[i].push(*j);
                            scc_graph.preds[*j].push(i);
                        }
                    }
                }
            }
        }

        scc_graph
    }
}

impl RegionPass for StructurizeCFG {
    fn run_on_region(
        &mut self,
        region: Rc<RefCell<Region>>,
        function: Rc<RefCell<Function>>,
        _: &RegionInfo,
        _: &mut Module,
    ) -> Option<Rc<RefCell<Function>>> {
        if let None = region.borrow().exit {
            return None;
        }

        let scc_graph = SCCGraph::from_entry(&region);

        let mut order = vec![];
        for (i, scc) in scc_graph.nodes.iter().enumerate() {
            if scc_graph.preds[i].len() >= 2 {
                panic!("The CFG is not reducible");
            }
            for node in &scc.data {
                order.push(node.clone());
            }
        }

        let mut entries = HashSet::new();
        let mut loop_backedges = HashMap::new();

        for node in &order {
            entries.insert(node);

            if let Some(exit) = &region.borrow().exit {
                if get_succ_nodes(node).iter().any(|succ| Weak::ptr_eq(&succ.data, exit)) {
                    for succ in get_succ_nodes(node) {
                        if entries.contains(&succ) {
                            loop_backedges.insert(succ, node);
                        }
                    }
                }
            }
        }

        for node in &order {
            let loop_start = node;
            let loop_end = if let Some(value) = loop_backedges.get(loop_start) {
                value
            } else {
                continue;
            };

            let loop_end_index = function.borrow_mut().bbs.iter().position(|x| { return Weak::ptr_eq(x, &loop_end.data); }).unwrap();

            let flow = Rc::new(RefCell::new(BasicBlock { insts: vec![], inst_values: vec![] }));

            function.borrow_mut().bbs.insert(loop_end_index + 1,Rc::downgrade(&flow));

            let flow_value = Rc::new(RefCell::new(Value::BasicBlock(flow.clone())));
            function.borrow_mut().bb_values.insert(loop_end_index + 1, flow_value.clone());

            // Modify terminator
            let terminator = loop_end.data.upgrade().unwrap().borrow().insts.last().unwrap().clone();
            match &mut *terminator.upgrade().unwrap().borrow_mut() {
                Inst::BranchInst(inst) => {
                    if Weak::ptr_eq(&inst.true_condition, &loop_start.data) {
                        let cond = if let Some((_, cond)) = &inst.false_condition {
                            Some(cond)
                        } else {
                            None
                        };
                        if let Some(cond) = cond {
                            inst.false_condition = Some((Rc::downgrade(&flow), cond.clone()));
                        }
                    }
                }
                _ => {}
            }

            let br = Inst::BranchInst(BranchInst {
                true_condition: region.borrow().exit.clone().unwrap(),
                false_condition: None,
                name: ValueName::None,
            });

            flow.borrow_mut().push_instruction(br);
        }

        None
    }
}
