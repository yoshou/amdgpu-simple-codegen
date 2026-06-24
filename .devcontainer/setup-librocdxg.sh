#!/usr/bin/env bash
# Post-create setup: register the WSL driver libraries and build/install the
# librocdxg bridge so the in-container ROCm runtime can reach the GPU through
# the Windows driver. Runs as the 'vscode' user; privileged steps use sudo.
set -euo pipefail

echo "[setup] Registering WSL driver libraries with ldconfig..."
if [ -d /usr/lib/wsl/lib ]; then
    echo "/usr/lib/wsl/lib" | sudo tee /etc/ld.so.conf.d/wsl.conf >/dev/null
    sudo ldconfig
else
    echo "[setup] WARNING: /usr/lib/wsl/lib not found — the GPU passthrough mount is missing."
fi

# librocdxg is the user-mode bridge between the Linux ROCm runtime and the
# Windows GPU driver. Build it against the mounted Windows SDK headers.
if [ -f /opt/rocm/lib/librocdxg.so ]; then
    echo "[setup] librocdxg already installed."
elif [ -d /opt/winsdk/shared ]; then
    echo "[setup] Building librocdxg against Windows SDK at /opt/winsdk..."
    tmp="$(mktemp -d)"
    git clone --depth 1 https://github.com/ROCm/librocdxg.git "$tmp/librocdxg"
    cmake -S "$tmp/librocdxg" -B "$tmp/librocdxg/build" -DWIN_SDK=/opt/winsdk/shared
    cmake --build "$tmp/librocdxg/build" -j"$(nproc)"
    sudo cmake --install "$tmp/librocdxg/build"
    sudo ldconfig
    rm -rf "$tmp"
else
    echo "[setup] WARNING: /opt/winsdk/shared not found — cannot build librocdxg."
    echo "[setup]          GPU execution will be unavailable until librocdxg is installed."
fi

echo "[setup] Verifying GPU visibility (rocminfo)..."
if rocminfo 2>/dev/null | grep -E "Marketing Name:|gfx12"; then
    echo "[setup] GPU detected."
else
    echo "[setup] rocminfo did not list a GPU agent."
    echo "[setup] Check: (1) Windows AMD driver is recent enough for ROCm-on-WSL,"
    echo "[setup]        (2) /dev/dxg was passed in, (3) librocdxg built successfully."
fi
