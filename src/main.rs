use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fs;
use std::io::Read;
use std::path;
use std::rc::Rc;

use amdgpu_simple_codegen::bitcode::*;
use amdgpu_simple_codegen::codegen::machine_info::MachineRegisterInfo;
use amdgpu_simple_codegen::ir::*;
use amdgpu_simple_codegen::pass::*;
use amdgpu_simple_codegen::passes::amdgpu::annotate_control_flow::AnnotateControlFlow;
use amdgpu_simple_codegen::passes::amdgpu::lower_kernel_arguments::LowerKernelArguments;
use amdgpu_simple_codegen::passes::ir_translator::IRTranslator;
use amdgpu_simple_codegen::passes::structurize_cfg::StructurizeCFG;
use amdgpu_simple_codegen::region::RegionInfo;

fn print_function(function: std::rc::Rc<std::cell::RefCell<Function>>) {
    let mut labels = HashMap::new();
    for (bb, bb_value) in function
        .borrow()
        .bbs
        .iter()
        .zip(function.borrow().bb_values.iter())
    {
        if !labels.contains_key(&bb_value.as_ptr()) {
            labels.insert(bb_value.as_ptr(), labels.len());
        }

        for inst_value in &bb.upgrade().unwrap().borrow().inst_values {
            if !labels.contains_key(&inst_value.as_ptr()) {
                labels.insert(inst_value.as_ptr(), labels.len());
            }
        }
    }
    for (bb, bb_value) in function
        .borrow()
        .bbs
        .iter()
        .zip(function.borrow().bb_values.iter())
    {
        let label = labels.get(&bb_value.as_ptr()).unwrap();
        println!("{}:", label);

        for (inst, inst_value) in bb
            .upgrade()
            .unwrap()
            .borrow()
            .insts
            .iter()
            .zip(bb.upgrade().unwrap().borrow().inst_values.iter())
        {
            let label = labels.get(&inst_value.as_ptr()).unwrap();
            print!("  %{} = ", label);

            match inst.upgrade().unwrap().borrow().clone() {
                Inst::LoadInst(rhs) => {
                    print!("load");
                }
                Inst::BranchInst(rhs) => {
                    if let Some((false_bb, cond)) = rhs.false_condition {
                        let true_bb_idx = function
                            .borrow()
                            .bbs
                            .iter()
                            .position(|x| std::rc::Weak::ptr_eq(x, &rhs.true_condition))
                            .unwrap();
                        let true_bb_value = function.borrow().bb_values[true_bb_idx].clone();
                        let false_bb_idx = function
                            .borrow()
                            .bbs
                            .iter()
                            .position(|x| std::rc::Weak::ptr_eq(x, &false_bb))
                            .unwrap();
                        let false_bb_value = function.borrow().bb_values[false_bb_idx].clone();
                        print!(
                            "br i1 %{}, label %{}, label %{}",
                            labels.get(&cond.upgrade().unwrap().as_ptr()).unwrap(),
                            labels.get(&true_bb_value.as_ptr()).unwrap(),
                            labels.get(&false_bb_value.as_ptr()).unwrap()
                        );
                    } else {
                        let true_bb_idx = function
                            .borrow()
                            .bbs
                            .iter()
                            .position(|x| std::rc::Weak::ptr_eq(x, &rhs.true_condition))
                            .unwrap();
                        let true_bb_value = function.borrow().bb_values[true_bb_idx].clone();
                        print!("br label %{}", labels.get(&true_bb_value.as_ptr()).unwrap());
                    }
                }
                Inst::PhiNode(rhs) => {
                    print!("phi");
                }
                Inst::CallInst(rhs) => {
                    print!("call");
                }
                Inst::ExtractValueInst(rhs) => {
                    print!("extractvalue");
                }
                _ => {}
            }
            // println!("{:?}", inst);
            println!();
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Missing input");
        return;
    }

    let path = path::Path::new(&args[1]);
    let mut file = match fs::File::open(&path) {
        Ok(f) => f,
        Err(err) => panic!("File error: {}", err),
    };

    let mut data = Vec::<u8>::new();
    file.read_to_end(&mut data).ok();

    if !is_bitcode(&data) {
        panic!("Invalid format")
    }

    let bitcode = Bitcode::new(&data);
    let module = match bitcode.decode() {
        Ok(value) => Rc::new(RefCell::new(value)),
        Err(err) => panic!("Decode bitcode error: {}", err.message),
    };

    let machine_register_info = Rc::new(RefCell::new(MachineRegisterInfo::new()));

    let mut lower_kernel_argument_pass = Box::new(LowerKernelArguments::new(&module));
    let mut structurize_cfg_pass = Box::new(StructurizeCFG::new());
    let mut annotate_control_flow_pass = Box::new(AnnotateControlFlow::new(&module));
    let mut ir_translator_pass =
        Box::new(IRTranslator::new(&module, machine_register_info.clone()));

    let values = module.borrow().values.clone();
    for value in values {
        match &*value.borrow() {
            Value::Function(function) => {
                if function.borrow().bbs.len() >= 1 {
                    lower_kernel_argument_pass.run_on_function(function.clone());

                    let region_info = RegionInfo::from_function(&*function.borrow());

                    let mut regions = vec![];
                    let mut visited = HashSet::new();

                    region_info.dfs_nodes_post(
                        region_info.root_region.clone(),
                        &mut |region| {
                            regions.push(region.data.clone());
                        },
                        &mut visited,
                    );

                    for region in &regions {
                        structurize_cfg_pass.run_on_region(
                            region.clone(),
                            function.clone(),
                            &region_info,
                        );
                    }

                    annotate_control_flow_pass.run_on_function(function.clone());
                    print_function(function.clone());

                    ir_translator_pass.run_on_function(function.clone());
                    println!();
                    print_function(function.clone());

                    println!();
                }
            }
            _ => {}
        }
    }
}
