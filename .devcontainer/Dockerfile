# ROCm dev image for Ubuntu 24.04. 7.2.x is the first line that supports
# RDNA4 / gfx1200 (Radeon RX 9070 XT) on WSL via the librocdxg bridge.
# It ships the ROCm runtime, HIP, rocminfo, and an LLVM/clang (under
# /opt/rocm/llvm) new enough to target gfx1200.
FROM rocm/dev-ubuntu-24.04:7.2.4

ARG DEBIAN_FRONTEND=noninteractive

# Build tooling for librocdxg (CMake >= 3.15, GCC >= 11.4) and general dev.
RUN apt-get update && apt-get install -y --no-install-recommends \
        cmake \
        build-essential \
        git \
        curl \
        ca-certificates \
        pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Rust toolchain for building amdgpu-simple-codegen. Put the ROCm LLVM/clang
# and ROCm binaries on PATH so `clang`/`amdclang` and `rocminfo` are available.
ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:/opt/rocm/llvm/bin:/opt/rocm/bin:$PATH
RUN curl -fsSL https://sh.rustup.rs | sh -s -- -y --no-modify-path --default-toolchain stable \
    && rustup component add clippy rustfmt

# On WSL the GPU is reached through the Windows driver's libdxcore.so, which is
# bind-mounted from the host at /usr/lib/wsl/lib (see devcontainer.json). Make
# that directory part of the dynamic linker search path.
RUN echo "/usr/lib/wsl/lib" > /etc/ld.so.conf.d/wsl.conf

# ROCm releases prior to 7.13 require this so the HSA runtime loads librocdxg.
ENV HSA_ENABLE_DXG_DETECTION=1
