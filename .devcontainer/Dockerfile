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
        sudo \
    && rm -rf /var/lib/apt/lists/*

# Create a 'vscode' user sharing the host uid/gid (1000) so that bind-mounted
# files (the workspace and ~/.claude) keep the host owner instead of being
# polluted to root, and grant passwordless sudo for dev convenience. Ubuntu
# 24.04 base ships a default 'ubuntu' user at uid 1000, so remove it first.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1000
RUN if id ubuntu >/dev/null 2>&1; then userdel -r ubuntu 2>/dev/null || true; fi \
    && groupadd --gid $USER_GID $USERNAME 2>/dev/null || true \
    && useradd --uid $USER_UID --gid $USER_GID -m -s /bin/bash $USERNAME \
    && echo "$USERNAME ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

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
