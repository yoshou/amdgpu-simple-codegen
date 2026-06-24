pub struct TargetRegister<T> {
    pub asm_name: &'static str,
    pub name: &'static str,
    pub spill_size: u32,
    pub spill_alignment: u32,
    pub aliases: &'static [&'static str],
    pub sub_regs: &'static [&'static str],
    pub data: T,
}

macro_rules! define_sgprs {
    ( $num:literal ) => {
        ::seq_macro::seq!(N in 0..$num {
            pub const SGPR_LO~N: TargetRegister<()> = TargetRegister {
                asm_name: concat!("s", stringify!(N)),
                name: concat!("s", stringify!(N)),
                spill_size: 0,
                spill_alignment: 0,
                aliases: &[],
                sub_regs: &[],
                data: ()
            };
            pub const SGPR_HI~N: TargetRegister<()> = TargetRegister {
                asm_name: concat!("s", stringify!(N)),
                name: concat!("s", stringify!(N)),
                spill_size: 0,
                spill_alignment: 0,
                aliases: &[],
                sub_regs: &[],
                data: ()
            };
            pub const SGPR~N: TargetRegister<()> = TargetRegister {
                asm_name: concat!("s", stringify!(N)),
                name: concat!("s", stringify!(N)),
                spill_size: 0,
                spill_alignment: 0,
                aliases: &[],
                sub_regs: &[concat!("s", stringify!(N), ".l"), concat!("s", stringify!(N), ".h")],
                data: ()
            };
        });
        pub const SGPRS: [&TargetRegister<()>; $num] = ::seq_macro::seq!(N in 0..$num {
            [
                #(
                    &SGPR~N,
                )*
            ]
        });
    };
}

define_sgprs!(106);

macro_rules! define_vgprs {
    ( $num:literal ) => {
        ::seq_macro::seq!(N in 0..$num {
            pub const VGPR~N: TargetRegister<()> = TargetRegister {
                asm_name: "",
                name: "",
                spill_size: 0,
                spill_alignment: 0,
                aliases: &[],
                sub_regs: &[concat!("v", stringify!(N), ".l"), concat!("s", stringify!(N), ".h")],
                data: ()
            };
        });
        pub const VGPRS: [&TargetRegister<()>; $num] = ::seq_macro::seq!(N in 0..$num {
            [
                #(
                    &VGPR~N,
                )*
            ]
        });
    };
}

define_vgprs!(256);

pub struct AMDGPURegisterInfo {}

pub struct RegisterClass<T> {
    pub data: T,
}

pub const SGPR_128_REG_CLASS: RegisterClass<()> = RegisterClass {
    data: (),
};

impl AMDGPURegisterInfo {
    pub fn get_matching_super_register<T, U, V>(
        &self,
        register: &TargetRegister<T>,
        sub_index: u32,
        register_class: &RegisterClass<U>,
    ) -> Option<&TargetRegister<V>> {
        let _registers = SGPRS.iter().chain(VGPRS.iter());
//         for super_reg in

//   for (MCPhysReg Super : superregs(Reg))
//     if (RC->contains(Super) && Reg == getSubReg(Super, SubIdx))
//       return Super;
        None
    }
}
