// Simple test kernel: element-wise vector addition.
// Compiled to AMDGPU bitcode for exercising the codegen pipeline.
//
// Build (inside the ROCm dev container, where clang supports gfx1200 and the
// ROCm device libraries resolve get_global_id):
//   ./build.sh
// or directly:
//   /opt/rocm/llvm/bin/clang -x cl -cl-std=CL2.0 \
//       --target=amdgcn-amd-amdhsa -mcpu=gfx1200 \
//       -emit-llvm -c vector_add.cl -o vector_add_gfx1200.bc

__kernel void vector_add(__global const float *a,
                         __global const float *b,
                         __global float *c) {
    size_t i = get_global_id(0);
    c[i] = a[i] + b[i];
}
