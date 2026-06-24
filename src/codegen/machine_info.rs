use std::collections::HashMap;


#[derive(Eq, Hash, PartialEq, Clone, Debug, Copy)]
pub enum Register {
    Virtual(u64)
}

#[derive(Clone, Debug)]
pub struct ElementCount {
    quantity: u32,
    is_scalable: bool,
}

impl ElementCount {
    pub fn get_fixed(min_value: u32) -> Self {
        ElementCount {
            quantity: min_value,
            is_scalable: false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct LowerLevelType {
    is_pointer: bool,
    is_vector: bool,
    is_scalar: bool,
    element_count: ElementCount,
    size_in_bits: u64,
    address_space: u32,
}

impl LowerLevelType {
    pub fn get_scalar(size_in_bits: u64) -> Self {
        LowerLevelType {
            is_pointer: false,
            is_vector: false,
            is_scalar: true,
            element_count: ElementCount::get_fixed(0),
            size_in_bits: size_in_bits,
            address_space: 0,
        }
    }
    
    pub fn get_pointer(address_space: u32, size_in_bits: u64) -> Self {
        LowerLevelType {
            is_pointer: true,
            is_vector: false,
            is_scalar: false,
            element_count: ElementCount::get_fixed(0),
            size_in_bits: size_in_bits,
            address_space: address_space,
        }
    }
}

pub struct MachineRegisterInfo {
    vreg_info: HashMap<u64, ()>,
    vreg_names: HashMap<u64, String>,
    vreg_types: HashMap<u64, LowerLevelType>,
}

impl MachineRegisterInfo {
    pub fn new() -> Self {
        MachineRegisterInfo {
            vreg_info: HashMap::new(),
            vreg_names: HashMap::new(),
            vreg_types: HashMap::new(),
        }
    }

    pub fn create_virtual_register(&mut self, ty: &LowerLevelType, name: Option<String>) -> Register {
        let num = self.vreg_info.len() as u64;
        self.vreg_info.insert(num, ());
        if let Some(name) = name {
            self.vreg_names.insert(num, name);
        }
        self.vreg_types.insert(num, ty.clone());
        let reg = Register::Virtual(num);
        reg
    }
}