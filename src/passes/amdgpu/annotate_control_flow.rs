use core::panic;
use std::{cell::RefCell, rc::{Rc, Weak}, collections::HashSet};

use num_bigint::ToBigUint;

use crate::{pass::FunctionPass, ir::{Function, Module, BasicBlockRef, Inst, BranchInst, ValueRef, IntegerType, Type, ValueName, ExtractValueInst, get_index_type, PhiNode, Value, ConstantInt}, dominators::{BasicBlockRefNode, ControlFlowGraph, DominatorTree}};

pub struct AnnotateControlFlow {
    if_func: ValueRef,
    else_func: ValueRef,
    if_break_func: ValueRef,
    loop_func: ValueRef,
    end_cf_func: ValueRef,
    stack: Vec<(BasicBlockRef, ValueRef)>,
}

fn get_bb_successors(bb: &BasicBlockRef) -> Vec<BasicBlockRef> {
    let mut successors = vec![];

    for inst in &bb.upgrade().unwrap().borrow().insts {
        match &*inst.upgrade().unwrap().borrow() {
            Inst::BranchInst(inst) => {
                successors.push(inst.true_condition.clone());

                if let Some((bb, _)) = &inst.false_condition {
                    successors.push(bb.clone());
                }
            }
            _ => {}
        }
    }

    successors
}

pub fn dfs_bb(
    bb: &BasicBlockRef,
    action: &mut impl FnMut(&BasicBlockRef) -> (),
    visited: &mut HashSet<BasicBlockRefNode>,
) {
    let node = BasicBlockRefNode { data: bb.clone() };
    if visited.contains(&node) {
        return;
    }
    visited.insert(node.clone());
    action(bb);

    for succ in get_bb_successors(bb) {
        dfs_bb(&succ, action, visited);
    }
}

impl AnnotateControlFlow {
    pub fn new(module: &mut Module) -> Self {
        let int_mask = Type::IntegerType(IntegerType { num_bits: 64 });
        AnnotateControlFlow {
            if_func: module.create_or_declar_intrinsic("amdgcn_if", &vec![int_mask.clone()]).unwrap(),
            else_func: module.create_or_declar_intrinsic("amdgcn_else", &vec![int_mask.clone(), int_mask.clone()]).unwrap(),
            if_break_func: module.create_or_declar_intrinsic("amdgcn_if_break", &vec![int_mask.clone()]).unwrap(),
            loop_func: module.create_or_declar_intrinsic("amdgcn_loop", &vec![int_mask.clone()]).unwrap(),
            end_cf_func: module.create_or_declar_intrinsic("amdgcn_end_cf", &vec![int_mask.clone()]).unwrap(),
            stack: vec![],
        }
    }
    
    fn handle_loop(&mut self, module: &mut Module, block: BasicBlockRef, cfg: &ControlFlowGraph) {
        let block = block.upgrade().unwrap().clone();
        let term_value = block.borrow().inst_values.last().unwrap().clone();
        let term = block.borrow().insts.last().unwrap().clone();

        let int_mask = Type::IntegerType(IntegerType { num_bits: 64 });
        let (broken_value, broken) = block
            .borrow_mut()
            .insert_instruction(
                0,
                Inst::PhiNode(PhiNode {
                    ty: int_mask.clone(),
                    incoming: vec![],
                    name: ValueName::None,
                }));
                
        let cond_value = match &*term.upgrade().unwrap().borrow_mut() {
            Inst::BranchInst(inst) => {
                match &inst.false_condition {
                    Some((_, cond)) => cond.clone(),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        };

        let arg = match &*cond_value.upgrade().unwrap().borrow() {
            Value::Instruction(_) => {
                block
                    .borrow_mut()
                    .create_call_before(
                        Some(&Rc::downgrade(&term_value)),
                        self.if_break_func.clone(),
                        vec![cond_value, broken_value],
                        ValueName::None,
                    )
                    .unwrap()
            }
            _ => panic!(),
        };

        for pred in cfg.get_predecessor_nodes(&BasicBlockRefNode { data: Rc::downgrade(&block) }) {
            let bb = BasicBlockRefNode { data: Rc::downgrade(&block) };
            let phi_value = if pred == &bb {
                arg.clone()
            } else {
                let int_mask_zero = Rc::new(RefCell::new(Value::ConstantInt(Rc::new(RefCell::new(ConstantInt {
                    ty: int_mask.clone(),
                    value: 0.to_biguint().unwrap(),
                    name: ValueName::None,
                })))));
                module.values.push(int_mask_zero.clone());
                Rc::downgrade(&int_mask_zero)
            };

            match &mut *broken.upgrade().unwrap().borrow_mut() {
                Inst::PhiNode(phi) => {
                    phi.incoming.push((pred.data.clone(), phi_value));
                }
                _ => panic!()
            }
        }
        
        match &mut *term.upgrade().unwrap().borrow_mut() {
            Inst::BranchInst(inst) => {
                let cond_value = block
                    .borrow_mut()
                    .create_call_before(
                        Some(&Rc::downgrade(&term_value)),
                        self.loop_func.clone(),
                        vec![arg.clone()],
                        ValueName::None,
                    )
                    .unwrap();

                inst.false_condition = inst.false_condition.clone().map(|(bb, _)| { (bb, cond_value) });
            }
            _ => panic!(),
        }

        self.stack.push((get_bb_successors(&Rc::downgrade(&block))[1].clone(), arg));
    }

    fn open_if(&mut self, block: BasicBlockRef) {
        let block = block.upgrade().unwrap().clone();
        let term_value = block.borrow().inst_values.last().unwrap().clone();
        let term = block.borrow().insts.last().unwrap().clone();
        let cond = match &*term.upgrade().unwrap().borrow_mut() {
            Inst::BranchInst(inst) => {
                inst.false_condition.clone().unwrap().1
            }
            _ => panic!(),
        };
        
        let ret = block
            .borrow_mut()
            .create_call_before(
                Some(&Rc::downgrade(&term_value)),
                self.if_func.clone(),
                vec![cond],
                ValueName::None,
            )
            .unwrap();
        
        match &mut *term.upgrade().unwrap().borrow_mut() {
            Inst::BranchInst(inst) => {
                let ty = ret.upgrade().unwrap().clone().borrow().ty();
                let position = block.borrow().position(&Rc::downgrade(&term_value)).unwrap();
                let indexes = vec![0];
                let (cond_value, _) = block
                    .borrow_mut()
                    .insert_instruction(
                        position,
                        Inst::ExtractValueInst(ExtractValueInst {
                            aggregate: ret.clone(),
                            ty: get_index_type(&ty, &indexes).unwrap(),
                            indexes: indexes,
                            name: ValueName::None,
                        }));

                inst.false_condition = inst.false_condition.clone().map(|(bb, _)| { (bb, cond_value) });
            }
            _ => panic!(),
        }

        let ty = ret.upgrade().unwrap().clone().borrow().ty();
        let position = block.borrow().position(&Rc::downgrade(&term_value)).unwrap();
        let indexes = vec![1];
        let (exec_value, _) = block
            .borrow_mut()
            .insert_instruction(
                position,
                Inst::ExtractValueInst(ExtractValueInst {
                    aggregate: ret,
                    ty: get_index_type(&ty, &indexes).unwrap(),
                    indexes: indexes,
                    name: ValueName::None,
                }));

        self.stack.push((get_bb_successors(&Rc::downgrade(&block))[1].clone(), exec_value));
    }

    fn close_control_flow(&mut self, block: BasicBlockRef) {
        let block = block.upgrade().unwrap().clone();
        if let Some((_, exec)) = self.stack.pop() {
            block
                .borrow_mut()
                .create_call_before(
                    None,
                    self.end_cf_func.clone(),
                    vec![exec],
                    ValueName::None,
                )
                .unwrap();
        }
    }
}

fn is_unconditional(inst: &Inst) -> bool {
    match inst {
        Inst::BranchInst(inst) => {
            inst.false_condition.is_none()
        }
        Inst::ReturnInst(_) => {
            true
        }
        _ => panic!(),
    }
}

impl FunctionPass for AnnotateControlFlow {
    fn run_on_function(
        &mut self,
        function: Rc<RefCell<Function>>,
        module: &mut Module,
    ) -> Option<Rc<RefCell<Function>>> {
        let cfg = ControlFlowGraph::from_function(&*function.borrow());
        let dom_tree = DominatorTree::from_cfg(&cfg);
        
        let mut bbs = vec![];
        let mut visited = HashSet::new();

        dfs_bb(
            &function.borrow().bbs.first().unwrap(),
            &mut |bb| {
                bbs.push(bb.clone());
            },
            &mut visited,
        );

        let mut visited = HashSet::new();
        for bb in &bbs {
            visited.insert(BasicBlockRefNode { data: bb.clone() });

            let block = bb.upgrade().unwrap().clone();
            let term = block.borrow().insts.last().unwrap().clone();
            if is_unconditional(&*term.upgrade().unwrap().borrow_mut()) {
                if let Some((close_bb, _)) = self.stack.last() {
                    if Weak::ptr_eq(close_bb, bb) {
                        self.close_control_flow(bb.clone());
                    }
                }
                continue;
            }

            let succs = get_bb_successors(bb);
            if succs.len() >= 1 && visited.contains(&BasicBlockRefNode { data: succs[0].clone() }) {
                if let Some((close_bb, _)) = self.stack.last() {
                    if Weak::ptr_eq(close_bb, bb) {
                        self.close_control_flow(bb.clone());
                    }
                }

                if dom_tree.dominates(&BasicBlockRefNode { data: succs[0].clone() }, &BasicBlockRefNode { data: bb.clone() }) {
                    self.handle_loop(module, bb.clone(), &cfg);
                }
                continue;
            }

            if let Some((close_bb, _)) = self.stack.last() {
                if Weak::ptr_eq(close_bb, bb) {
                    self.close_control_flow(bb.clone());
                }
            }
            self.open_if(bb.clone());
        }

        Some(function)
    }
}
