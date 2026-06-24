use crate::{
    codegen::{
        calling_conv_lower::CallingConvState, machine_function::MachineFunction,
        machine_info::Register,
    },
    ir::Function,
};

use super::{amdgpu_machine_function_info::AMDGPUMachineFunctionInfo, amdgpu_register_info::*};

pub struct AMDGPUCallLowering {}

impl AMDGPUCallLowering {
    pub fn new() -> Self {
        AMDGPUCallLowering {}
    }

    pub fn allocate_hsa_user_sgprs(
        calling_conv_info: &mut CallingConvState,
        machine_function: &mut MachineFunction,
        register_info: &AMDGPURegisterInfo,
        machine_function_info: &mut AMDGPUMachineFunctionInfo
    ) {
        if machine_function_info.enable_sgpr_private_segment_buffer {
            machine_function_info.add_private_segment_buffer(register_info);
        }
    }

    pub fn lower_formal_arguments_kernel(
        &self,
        function: &Function,
        arg_vregs: &Vec<Vec<Register>>,
        machine_function: &mut MachineFunction,
    ) {
        let mut calling_conv_info = CallingConvState::new();

        let register_info = AMDGPURegisterInfo {};
        let mut machine_function_info = AMDGPUMachineFunctionInfo::new();

        Self::allocate_hsa_user_sgprs(&mut calling_conv_info, machine_function, &register_info, &mut machine_function_info);
    }

    pub fn lower_formal_arguments(
        &self,
        function: &Function,
        arg_vregs: &Vec<Vec<Register>>,
        machine_function: &mut MachineFunction,
    ) {
        if function.calling_conv == 91 {
            self.lower_formal_arguments_kernel(function, arg_vregs, machine_function);
            return;
        }

        unimplemented!()
    }
}
