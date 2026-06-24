use std::{
    cell::RefCell,
    collections::HashMap,
    hash::Hash,
    hash::Hasher,
    rc::{Rc, Weak},
};

use crate::{
    codegen::{amdgpu::amdgpu_call_lowering::AMDGPUCallLowering, machine_function::MachineFunction, machine_info::{LowerLevelType, MachineRegisterInfo, Register}},
    ir::{DataLayout, Function, Module, Type, ValueRef},
    pass::FunctionPass,
};

#[derive(Clone, Debug)]
pub struct ValueRefNode {
    pub data: ValueRef,
}

impl PartialEq for ValueRefNode {
    fn eq(&self, other: &ValueRefNode) -> bool {
        Weak::ptr_eq(&self.data, &other.data)
    }
}

impl Eq for ValueRefNode {}

impl Hash for ValueRefNode {
    fn hash<H>(&self, hasher: &mut H)
    where
        H: Hasher,
    {
        hasher.write_usize(Weak::as_ptr(&self.data) as usize);
    }
}

struct VirtualRegisterMap {
    value_to_vregs: HashMap<ValueRefNode, Vec<Register>>,
    type_to_offsets: HashMap<Type, Vec<u64>>,
}

impl VirtualRegisterMap {
    pub fn new() -> Self {
        VirtualRegisterMap {
            value_to_vregs: HashMap::new(),
            type_to_offsets: HashMap::new(),
        }
    }

    fn get_vregs(&self, value: &ValueRef) -> Option<&Vec<Register>> {
        self.value_to_vregs.get(&ValueRefNode {
            data: value.clone(),
        })
    }
    fn get_offsets(&self, value: &ValueRef) -> Option<&Vec<u64>> {
        self.type_to_offsets
            .get(&value.upgrade().unwrap().borrow().ty())
    }
    fn insert_vregs(&mut self, value: &ValueRef, vregs: &Vec<Register>) {
        self.value_to_vregs.insert(
            ValueRefNode {
                data: value.clone(),
            },
            vregs.clone(),
        );
    }
    fn insert_offsets(&mut self, value: &ValueRef, offsets: &Vec<u64>) {
        self.type_to_offsets.insert(
            value.upgrade().unwrap().borrow().ty().clone(),
            offsets.clone(),
        );
    }
}

pub struct IRTranslator {
    module: Rc<RefCell<Module>>,
    vregs_map: VirtualRegisterMap,
    machine_register_info: Rc<RefCell<MachineRegisterInfo>>,
}

fn compute_value_lower_level_type(
    data_layout: &DataLayout,
    ty: &Type,
    offset: u64,
) -> (Vec<LowerLevelType>, Vec<u64>) {
    match ty {
        Type::IntegerType(_) => {
            let size_in_bits = data_layout.get_type_size_in_bits(ty).unwrap();
            let value_tys = vec![LowerLevelType::get_scalar(size_in_bits as u64)];
            let offsets = vec![offset * 8];
            (value_tys, offsets)
        }
        Type::PointerType(ty) => {
            let size_in_bits = data_layout
                .get_pointer_size_in_bits(ty.address_space)
                .unwrap();
            let value_tys = vec![LowerLevelType::get_pointer(
                ty.address_space,
                size_in_bits as u64,
            )];
            let offsets = vec![offset * 8];
            (value_tys, offsets)
        }
        _ => unimplemented!(),
    }
}

impl IRTranslator {
    pub fn new(
        module: &Rc<RefCell<Module>>,
        machine_register_info: Rc<RefCell<MachineRegisterInfo>>,
    ) -> Self {
        IRTranslator {
            module: module.clone(),
            vregs_map: VirtualRegisterMap::new(),
            machine_register_info: machine_register_info,
        }
    }

    fn get_or_create_vreg(&mut self, value: &ValueRef) -> Vec<Register> {
        let vregs = self.vregs_map.get_vregs(value);
        if let Some(vregs) = vregs {
            return vregs.clone();
        }

        let mut vregs = vec![];

        let ty = &value.upgrade().unwrap().borrow().ty();
        let (tys, offsets) =
            compute_value_lower_level_type(&self.module.borrow().data_layout, ty, 0);

        for ty in tys {
            let vreg = self
                .machine_register_info
                .borrow_mut()
                .create_virtual_register(&ty, None);
            vregs.push(vreg);
        }

        self.vregs_map.insert_vregs(value, &vregs);
        self.vregs_map.insert_offsets(value, &offsets);

        vregs
    }
}

impl FunctionPass for IRTranslator {
    fn run_on_function(
        &mut self,
        function: Rc<RefCell<Function>>,
    ) -> Option<Rc<RefCell<Function>>> {
        let mut arg_vregs = vec![];
        for arg_value in &function.borrow().argument_values {
            let vregs = self.get_or_create_vreg(&Rc::downgrade(&arg_value));
            arg_vregs.push(vregs);
        }

        let machine_function = Rc::new(RefCell::new(MachineFunction {}));

        let call_lowering_info = AMDGPUCallLowering {};

        call_lowering_info.lower_formal_arguments(&*function.borrow(), &arg_vregs, &mut *machine_function.borrow_mut());

        Some(function)
    }
}
