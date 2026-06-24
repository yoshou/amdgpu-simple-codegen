# amdgpu-simple-codegen

A learning and experimentation project that reimplements, in a simplified form, the code generation path that turns LLVM bitcode into AMD GPU code. The goal is to understand how LLVM's AMDGPU backend works (in particular its GlobalISel pipeline) by reconstructing its main passes in a minimal Rust implementation.

## Goals

- Decode LLVM bitcode (`.bc`) into an in-house IR representation
- Run the target-independent and target-specific passes, such as converting to the structured control flow that AMD GPUs require
- Lower the IR into a machine-IR representation (using virtual registers) that is close to machine code
- Ultimately, emit an AMD GPU kernel object as the final output

This is not a full, production backend — it is a minimal implementation meant for following how each step works.

## Pipeline

The pipeline is not yet complete; the final goal is to carry it all the way through to emitting an AMD GPU kernel object. The stages implemented so far run as follows — after decoding the bitcode, `main` applies these passes to each function in order ([src/main.rs](src/main.rs)):

1. **Bitcode decode** — parse a `.bc` file into an IR module ([src/bitcode.rs](src/bitcode.rs))
2. **LowerKernelArguments** — lower kernel arguments ([src/passes/amdgpu/lower_kernel_arguments.rs](src/passes/amdgpu/lower_kernel_arguments.rs))
3. **RegionInfo / StructurizeCFG** — region analysis and CFG structurization ([src/region.rs](src/region.rs), [src/passes/structurize_cfg.rs](src/passes/structurize_cfg.rs))
4. **AnnotateControlFlow** — annotate the control flow ([src/passes/amdgpu/annotate_control_flow.rs](src/passes/amdgpu/annotate_control_flow.rs))
5. **IRTranslator** — translate the IR into machine IR using virtual registers ([src/passes/ir_translator.rs](src/passes/ir_translator.rs), [src/codegen/](src/codegen/))

Supporting infrastructure includes dominator / post-dominator trees ([src/dominators.rs](src/dominators.rs)) and data-layout analysis ([src/ir.rs](src/ir.rs)).

## Build and run

```bash
cargo build
cargo run -- <input.bc>
```

The input is an LLVM bitcode file.

## Status

This project is a work in progress. Bitcode decoding, region analysis, CFG structurization, and control-flow annotation are implemented; the code generation layer (IR-to-machine-IR translation and AMDGPU calling-convention lowering) is currently being built. The remaining stages needed to reach the goal — instruction selection, register allocation, and emitting the final AMD GPU kernel object — are not yet implemented.
