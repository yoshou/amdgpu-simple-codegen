// Minimal test kernel: no get_global_id, no GEP — each thread reads a[0]/b[0]
// and writes their sum to c[0]. Produces just load/fadd/store, which keeps the
// generated bitcode small enough to exercise the decoder and passes end-to-end.
//
// Build (inside the ROCm dev container): ./build.sh

__kernel void load_store(__global const float *a,
                         __global const float *b,
                         __global float *c) {
    *c = *a + *b;
}
