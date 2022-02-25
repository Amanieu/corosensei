#!/usr/bin/env sh

set -ex

CARGO=cargo
if [ "${CROSS}" = "1" ]; then
    export CARGO_NET_RETRY=5
    export CARGO_NET_TIMEOUT=10

    cargo install cross
    CARGO=cross
fi

# Cross doesn't have an image for thumbv7neon-unknown-linux-gnueabihf
export CARGO_TARGET_THUMBV7NEON_UNKNOWN_LINUX_GNUEABIHF_LINKER=arm-linux-gnueabihf-gcc
export CARGO_TARGET_THUMBV7NEON_UNKNOWN_LINUX_GNUEABIHF_RUNNER="/linux-runner armv7"

# If a test crashes, we want to know which one it was.
export RUST_TEST_THREADS=1

# No default features (can't run tests without default-stack)
"${CARGO}" build --target "${TARGET}" --no-default-features
"${CARGO}" build --target "${TARGET}" --no-default-features --release

# No unwind
"${CARGO}" test --target "${TARGET}" --all-targets --no-default-features --features default-stack
"${CARGO}" test --target "${TARGET}" --all-targets --no-default-features --features default-stack --release

# unwind
"${CARGO}" test --target "${TARGET}" --all-targets
"${CARGO}" test --target "${TARGET}" --all-targets --release

# asm-unwind
if [ "${CHANNEL}" = "nightly" ]; then
    "${CARGO}" test --target "${TARGET}" --all-targets --features asm-unwind
    "${CARGO}" test --target "${TARGET}" --all-targets --features asm-unwind --release
fi
