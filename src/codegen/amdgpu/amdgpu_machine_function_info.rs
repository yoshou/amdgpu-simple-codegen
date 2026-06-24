use super::amdgpu_register_info::{AMDGPURegisterInfo, TargetRegister, SGPRS, SGPR_128_REG_CLASS};

#[derive(Clone, Copy, Debug)]
pub enum ArgLocation {
    Register(&'static str),
    Stack(u32),
}

pub struct ArgDescriptor {
    pub loc: ArgLocation,
    pub mask: u32,
}

impl ArgDescriptor {
    pub fn create_register<T>(register: &TargetRegister<T>, mask: u32) -> Self {
        ArgDescriptor {
            loc: ArgLocation::Register(register.name),
            mask: mask,
        }
    }
}

pub type ArgDescriptorOpt = Option<ArgDescriptor>;

pub struct AMDGPUFunctionArgInfo {
    pub private_segment_buffer: ArgDescriptorOpt,
    pub dispatch_ptr: ArgDescriptorOpt,
    pub queue_ptr: ArgDescriptorOpt,
    pub kernarg_segment_ptr: ArgDescriptorOpt,
    pub dispatch_id: ArgDescriptorOpt,
    pub flat_scratch_init: ArgDescriptorOpt,
    pub private_segment_size: ArgDescriptorOpt,
    pub lds_kernel_id: ArgDescriptorOpt,
    pub work_group_id_x: ArgDescriptorOpt,
    pub work_group_id_y: ArgDescriptorOpt,
    pub work_group_id_z: ArgDescriptorOpt,
    pub work_group_info: ArgDescriptorOpt,
    pub private_segment_wave_byte_offset: ArgDescriptorOpt,
    pub implicit_arg_ptr: ArgDescriptorOpt,
    pub implicit_buffer_ptr: ArgDescriptorOpt,
    pub work_item_id_x: ArgDescriptorOpt,
    pub work_item_id_y: ArgDescriptorOpt,
    pub work_item_id_z: ArgDescriptorOpt,
}

pub struct AMDGPUMachineFunctionInfo {
    pub arg_info: AMDGPUFunctionArgInfo,
    pub enable_sgpr_private_segment_buffer: bool,
    pub num_user_sgprs: u32,
}

impl AMDGPUMachineFunctionInfo {
    pub fn new() -> Self {
        AMDGPUMachineFunctionInfo {
            arg_info: AMDGPUFunctionArgInfo {
                private_segment_buffer: None,
                dispatch_ptr: None,
                queue_ptr: None,
                kernarg_segment_ptr: None,
                dispatch_id: None,
                flat_scratch_init: None,
                private_segment_size: None,
                lds_kernel_id: None,
                work_group_id_x: None,
                work_group_id_y: None,
                work_group_id_z: None,
                work_group_info: None,
                private_segment_wave_byte_offset: None,
                implicit_arg_ptr: None,
                implicit_buffer_ptr: None,
                work_item_id_x: None,
                work_item_id_y: None,
                work_item_id_z: None,
            },
            enable_sgpr_private_segment_buffer: true,
            num_user_sgprs: 0,
        }
    }

    pub fn add_private_segment_buffer(&mut self, register_info: &AMDGPURegisterInfo) {
        self.arg_info.private_segment_buffer = Some(ArgDescriptor::create_register::<()>(
            register_info.get_matching_super_register(&SGPRS[0], 0, &SGPR_128_REG_CLASS).unwrap(), 0
        ));
        self.num_user_sgprs += 4;
    }
}
