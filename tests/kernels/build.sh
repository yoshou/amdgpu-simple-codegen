#!/usr/bin/env bash
# Compile the test kernels to AMDGPU (gfx1200) LLVM bitcode.
# Run inside the ROCm dev container: the ROCm clang supports gfx1200 and the
# ROCm device libraries resolve OpenCL builtins such as get_global_id.
set -euo pipefail

MCPU="${1:-gfx1200}"
CLANG="${CLANG:-/opt/rocm/llvm/bin/clang}"
cd "$(dirname "$0")"

for cl in *.cl; do
    out="${cl%.cl}_${MCPU}.bc"
    echo "[build] $cl -> $out (mcpu=$MCPU)"
    "$CLANG" -x cl -cl-std=CL2.0 \
        --target=amdgcn-amd-amdhsa -mcpu="$MCPU" \
        -emit-llvm -c "$cl" -o "$out"
done

echo "[build] done."
