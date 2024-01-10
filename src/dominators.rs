use crate::ir::{BasicBlockRef, Function, Inst};
use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
    rc::Weak,
};

#[derive(Clone, Debug)]
pub struct BasicBlockRefNode {
    pub data: BasicBlockRef,
}

impl PartialEq for BasicBlockRefNode {
    fn eq(&self, other: &BasicBlockRefNode) -> bool {
        Weak::ptr_eq(&self.data, &other.data)
    }
}

impl Eq for BasicBlockRefNode {}

impl Hash for BasicBlockRefNode {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        hasher.write_usize(Weak::as_ptr(&self.data) as usize);
    }
}

pub struct ControlFlowGraph {
    nodes: Vec<BasicBlockRefNode>,
    preds: Vec<Vec<usize>>,
    succs: Vec<Vec<usize>>,
}

impl ControlFlowGraph {
    pub fn from_function(function: &Function) -> ControlFlowGraph {
        let mut graph = ControlFlowGraph {
            nodes: function
                .bbs
                .iter()
                .map(|x| BasicBlockRefNode { data: x.clone() })
                .collect(),
            preds: (0..function.bbs.len()).map(|_| Vec::new()).collect(),
            succs: (0..function.bbs.len()).map(|_| Vec::new()).collect(),
        };
        for (i, node) in graph.nodes.iter().enumerate() {
            for inst in &node.data.upgrade().unwrap().borrow().insts {
                match &*inst.upgrade().unwrap().borrow() {
                    Inst::BranchInst(inst) => {
                        let j = graph
                            .nodes
                            .iter()
                            .position(|x| Weak::ptr_eq(&x.data, &inst.true_condition))
                            .unwrap();
                        graph.succs[i].push(j);
                        graph.preds[j].push(i);

                        if let Some((bb, _)) = &inst.false_condition {
                            let j = graph
                                .nodes
                                .iter()
                                .position(|x| Weak::ptr_eq(&x.data, bb))
                                .unwrap();
                            graph.succs[i].push(j);
                            graph.preds[j].push(i);
                        }
                    }
                    _ => {}
                }
            }
        }
        graph
    }

    pub fn get_entry_node(&self) -> BasicBlockRefNode {
        self.nodes[0].clone()
    }

    pub fn get_exit_nodes(&self) -> Vec<&BasicBlockRefNode> {
        self.succs
            .iter()
            .enumerate()
            .filter_map(|(i, nodes)| {
                if nodes.len() == 0 {
                    Some(&self.nodes[i])
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_successor_nodes(&self, node: &BasicBlockRefNode) -> Vec<&BasicBlockRefNode> {
        let index = self.nodes.iter().position(|x| x == node).unwrap();
        self.succs[index]
            .iter()
            .map(|succ| &self.nodes[*succ])
            .collect()
    }

    pub fn get_predecessor_nodes(&self, node: &BasicBlockRefNode) -> Vec<&BasicBlockRefNode> {
        let index = self.nodes.iter().position(|x| x == node).unwrap();
        self.preds[index]
            .iter()
            .map(|pred| &self.nodes[*pred])
            .collect()
    }

    pub fn dfs_succ_nodes(
        &self,
        node: &BasicBlockRefNode,
        action: &impl Fn(&BasicBlockRefNode) -> (),
        visited: &mut HashSet<BasicBlockRefNode>,
    ) {
        if visited.contains(node) {
            return;
        }
        action(node);
        visited.insert(node.clone());

        let index = self.nodes.iter().position(|x| x == node).unwrap();
        for succ in &self.succs[index] {
            self.dfs_succ_nodes(&self.nodes[*succ], action, visited);
        }
    }

    pub fn dfs_pred_nodes(
        &self,
        node: &BasicBlockRefNode,
        action: &impl Fn(&BasicBlockRefNode) -> (),
        visited: &mut HashSet<BasicBlockRefNode>,
    ) {
        if visited.contains(node) {
            return;
        }
        action(node);
        visited.insert(node.clone());

        let index = self.nodes.iter().position(|x| x == node).unwrap();
        for pred in &self.preds[index] {
            self.dfs_pred_nodes(&self.nodes[*pred], action, visited);
        }
    }

    pub fn dfs_nodes(
        &self,
        node: &BasicBlockRefNode,
        action: &impl Fn(&BasicBlockRefNode) -> (),
        visited: &mut HashSet<BasicBlockRefNode>,
    ) {
        if visited.contains(node) {
            return;
        }
        action(node);
        visited.insert(node.clone());

        let index = self.nodes.iter().position(|x| x == node).unwrap();
        for succ in &self.succs[index] {
            self.dfs_nodes(&self.nodes[*succ], action, visited);
        }
        for pred in &self.preds[index] {
            self.dfs_nodes(&self.nodes[*pred], action, visited);
        }
    }

    pub fn find_reachable_nodes(
        &self,
        s: &BasicBlockRefNode,
        excludes: HashSet<BasicBlockRefNode>,
    ) -> Vec<BasicBlockRefNode> {
        let mut visited = excludes.clone();
        self.dfs_nodes(s, &|_| {}, &mut visited);
        visited.iter().map(|x| x.clone()).collect()
    }
}

pub struct DominatorTree {
    nodes: Vec<BasicBlockRefNode>,
    parent: Vec<Option<usize>>,
    children: Vec<Vec<usize>>,
}

impl DominatorTree {
    pub fn get_nodes(&self) -> &Vec<BasicBlockRefNode> {
        &self.nodes
    }

    pub fn get_post_node(&self, node: &BasicBlockRefNode) -> Option<&BasicBlockRefNode> {
        let index = self
            .nodes
            .iter()
            .position(|x| x == node)
            .unwrap();
        self.nodes.get(self.parent[index]?)
    }

    pub fn get_children(&self, node: &BasicBlockRefNode) -> Vec<&BasicBlockRefNode> {
        let index = self
            .nodes
            .iter()
            .position(|x| x == node)
            .unwrap();
        self.children[index].iter().map(|child| &self.nodes[*child]).collect()
    }

    pub fn from_cfg(graph: &ControlFlowGraph) -> DominatorTree {
        let mut tree = DominatorTree {
            nodes: graph.nodes.clone(),
            parent: (0..graph.nodes.len()).map(|_| None).collect(),
            children: (0..graph.nodes.len()).map(|_| Vec::new()).collect(),
        };
        let mut dominators = (0..graph.nodes.len())
            .map(|_| Vec::new())
            .collect::<Vec<Vec<usize>>>();
        for (i, node) in tree.nodes.iter().enumerate() {
            let mut excludes = HashSet::new();
            excludes.insert(node.clone());

            let rechable_nodes: HashSet<BasicBlockRefNode> = HashSet::from_iter(
                graph
                    .find_reachable_nodes(&graph.get_entry_node(), excludes)
                    .iter()
                    .map(|x| x.clone()),
            );
            let all_nodes = HashSet::from_iter(graph.nodes.iter().map(|x| x.clone()));
            let unrechable_nodes = all_nodes
                .difference(&rechable_nodes)
                .map(|x| x.clone())
                .collect::<Vec<_>>();

            for unreachable_node in unrechable_nodes {
                let j = graph
                    .nodes
                    .iter()
                    .position(|x| Weak::ptr_eq(&x.data, &unreachable_node.data))
                    .unwrap();
                dominators[j].push(i);
            }
        }
        for (i, node) in tree.nodes.iter().enumerate() {
            let successors = graph.get_successor_nodes(node);
            for successor in successors {
                let j = graph.nodes.iter().position(|x| x == successor).unwrap();
                if dominators[j].contains(&i) {
                    tree.children[i].push(j);
                    tree.parent[j] = Some(i);
                }
            }
        }
        tree
    }

    pub fn from_cfg_post(graph: &ControlFlowGraph) -> DominatorTree {
        let mut tree = DominatorTree {
            nodes: graph.nodes.clone(),
            parent: (0..graph.nodes.len() + 1).map(|_| None).collect(),
            children: (0..graph.nodes.len() + 1).map(|_| Vec::new()).collect(),
        };
        let mut postdominators = (0..graph.nodes.len())
            .map(|_| Vec::new())
            .collect::<Vec<Vec<usize>>>();
        for (i, node) in tree.nodes.iter().enumerate() {
            let mut excludes = HashSet::new();
            excludes.insert(node.clone());

            let mut rechable_nodes = HashSet::new();
            for exit_node in graph.get_exit_nodes() {
                rechable_nodes.extend(graph.find_reachable_nodes(exit_node, excludes.clone()));
            }

            let all_nodes = HashSet::from_iter(graph.nodes.iter().map(|x| x.clone()));
            let unrechable_nodes = all_nodes
                .difference(&rechable_nodes)
                .map(|x| x.clone())
                .collect::<Vec<_>>();

            for unreachable_node in unrechable_nodes {
                let j = graph
                    .nodes
                    .iter()
                    .position(|x| Weak::ptr_eq(&x.data, &unreachable_node.data))
                    .unwrap();
                postdominators[j].push(i);
            }
        }
        for (i, node) in tree.nodes.iter().enumerate() {
            let predecessors = graph.get_predecessor_nodes(node);
            for predecessor in predecessors {
                let j = graph.nodes.iter().position(|x| x == predecessor).unwrap();
                if postdominators[j].contains(&i) {
                    tree.children[i].push(j);
                    tree.parent[j] = Some(i);
                }
            }
        }

        let virtual_exit_node = tree.children.len() - 1;
        for node in graph.get_exit_nodes() {
            let i = graph.nodes.iter().position(|x| x == node).unwrap();

            tree.children[virtual_exit_node].push(i);
            tree.parent[i] = Some(virtual_exit_node);
        }
        tree
    }
}
