#!/usr/bin/env bash
# Makes the esp-rs toolchain from the espressif/idf-rust container usable in
# GitHub Actions steps, which run as root (for apt-get) while the toolchain is
# installed for the container's own user.
set -euo pipefail

home=$(getent passwd esp | cut -d: -f6 || true)
for dir in "$home" "$HOME" /home/esp; do
    if [ -n "$dir" ] && [ -f "$dir/export-esp.sh" ]; then
        # Persist the toolchain environment for the following steps.
        {
            echo "RUSTUP_HOME=$dir/.rustup"
            echo "CARGO_HOME=$dir/.cargo"
        } >> "$GITHUB_ENV"
        echo "$dir/.cargo/bin" >> "$GITHUB_PATH"
        # export-esp.sh only sets PATH and LIBCLANG_PATH.
        # shellcheck disable=SC1091
        . "$dir/export-esp.sh"
        echo "$PATH" | tr ':' '\n' | grep -v "^$dir/.cargo/bin$" >> "$GITHUB_PATH"
        echo "LIBCLANG_PATH=${LIBCLANG_PATH:-}" >> "$GITHUB_ENV"
        exit 0
    fi
done

echo "export-esp.sh not found; is this the espressif/idf-rust container?" >&2
exit 1
