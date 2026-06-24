#!/usr/bin/env bash
# Build a debug LLVM (clang + AMDGPU backend) to study how the LLVM-based GPU
# kernel compiler works and step through it with gdb. The source is pinned to
# the exact revision shipped by ROCm 7.2.4 (tag rocm-7.2.4), so the code you
# debug is the same compiler that produces the gfx1201 kernels in this repo.
#
# Run inside the ROCm dev container. Output lives under llvm-project/ (git
# ignored). Re-running resumes the incremental build.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
SRC="$REPO_ROOT/llvm-project"
BUILD="$SRC/build"
TAG="${LLVM_TAG:-rocm-7.2.4}"

# Self-host with the ROCm clang/lld already in the container (fast).
CC=/opt/rocm/llvm/bin/clang
CXX=/opt/rocm/llvm/bin/clang++

if [ ! -d "$SRC/.git" ]; then
    echo "[llvm] Shallow-cloning ROCm/llvm-project @ $TAG ..."
    git clone --depth 1 --branch "$TAG" \
        https://github.com/ROCm/llvm-project.git "$SRC"
fi

echo "[llvm] Configuring (Debug, AMDGPU;X86, clang;lld) ..."
cmake -G Ninja -S "$SRC/llvm" -B "$BUILD" \
    -DCMAKE_BUILD_TYPE=Debug \
    -DLLVM_TARGETS_TO_BUILD="AMDGPU;X86" \
    -DLLVM_ENABLE_PROJECTS="clang;lld" \
    -DBUILD_SHARED_LIBS=ON \
    -DLLVM_USE_SPLIT_DWARF=ON \
    -DLLVM_OPTIMIZED_TABLEGEN=ON \
    -DLLVM_ENABLE_ASSERTIONS=ON \
    -DLLVM_CCACHE_BUILD=ON \
    -DLLVM_USE_LINKER=lld \
    -DLLVM_PARALLEL_LINK_JOBS=4 \
    -DCMAKE_C_COMPILER="$CC" \
    -DCMAKE_CXX_COMPILER="$CXX"

echo "[llvm] Building (this takes a while) ..."
ninja -C "$BUILD" llc opt clang lld llvm-dis

echo "[llvm] Done. Binaries in $BUILD/bin"
