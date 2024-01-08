use num_bigint::ToBigUint;
use std::{cell::RefCell, rc::Rc};

use crate::{
    ir::{
        Attribute, AttributeKind, ConstantInt, Function, IntegerType, Module, Type, Value,
        ValueName,
    },
    pass::FunctionPass,
};

pub struct LowerKernelArguments {}

impl FunctionPass for LowerKernelArguments {
    fn run_on_function(
        &mut self,
        function: Rc<RefCell<Function>>,
        module: &mut Module,
    ) -> Option<Rc<RefCell<Function>>> {
        if function.borrow().calling_conv != 91 {
            return None;
        }

        let entry_block = function.borrow().bbs.first().unwrap().clone();

        let kernarg_segment = entry_block
            .upgrade()
            .unwrap()
            .borrow_mut()
            .create_call(
                None,
                module
                    .create_or_declar_intrinsic("amdgcn_kernarg_segment_ptr", &vec![])
                    .unwrap(),
                vec![],
                ValueName::Postfix(
                    Value::Function(function.clone()),
                    ".kernarg.segment".to_string(),
                ),
            )
            .unwrap();

        let arguments = function.borrow().argument_values.clone();
        let attributes = function.borrow().attributes.attributes.clone();

        let mut explicit_arg_offset = 0;
        for (arg, attribute) in arguments.iter().zip(attributes.iter()) {
            let is_by_ref = attribute.contains(&Attribute::Enum(AttributeKind::ByRef));

            if is_by_ref {
                unimplemented!();
            }

            let alignment = module
                .data_layout
                .get_abi_alignment_by_type(&arg.borrow().ty())
                .unwrap();

            let size = module
                .data_layout
                .get_type_size_in_bits(&arg.borrow().ty())
                .unwrap();
            let alloc_size = module
                .data_layout
                .get_type_alloc_size(&arg.borrow().ty())
                .unwrap();

            let offset = ((explicit_arg_offset + alignment - 1) / alignment) * alignment;
            explicit_arg_offset = offset + alloc_size;

            let (arg_ptr, adjusted_ty) = if size < 32 && !arg.borrow().ty().is_aggregate() {
                unimplemented!();
            } else {
                let offset = Rc::new(RefCell::new(Value::ConstantInt(Rc::new(RefCell::new(
                    ConstantInt {
                        ty: Type::IntegerType(IntegerType { num_bits: 64 }),
                        value: offset.to_biguint().unwrap(),
                        name: ValueName::None,
                    },
                )))));
                module.values.push(offset.clone());

                let arg_ptr = entry_block
                    .upgrade()
                    .unwrap()
                    .borrow_mut()
                    .create_get_element_ptr_inbounds_index1(
                        Some(&kernarg_segment),
                        Type::IntegerType(IntegerType { num_bits: 8 }),
                        kernarg_segment.clone(),
                        Rc::downgrade(&offset),
                        ValueName::Postfix(arg.borrow().clone(), ".kernarg.offset".to_string()),
                    )
                    .unwrap();
                (arg_ptr, arg.borrow().ty().clone())
            };

            let load = entry_block
                .upgrade()
                .unwrap()
                .borrow_mut()
                .create_aligned_load(
                    Some(&arg_ptr),
                    adjusted_ty,
                    arg_ptr.clone(),
                    alignment,
                    false,
                    ValueName::None,
                )
                .unwrap();

            arg.replace(load.upgrade().unwrap().borrow().clone());
        }

        Some(function)
    }
}
