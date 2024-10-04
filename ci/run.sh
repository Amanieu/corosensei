#!/usr/bin/env sh

set -ex

CARGO=cargo
if [ "${CROSS}" = "1" ]; then
    export CARGO_NET_RETRY=5
    export CARGO_NET_TIMEOUT=10

    cargo install cross
    CARGO=cross
fi

CARGO_TEST_FLAGS=
if [ "${NO_RUN}" = "1" ]; then
    CARGO_TEST_FLAGS=--no-run
fi

# If a test crashes, we want to know which one it was.
export RUST_TEST_THREADS=1

# No default features (can't run tests without default-stack)
"${CARGO}" build --target "${TARGET}" --no-default-features
"${CARGO}" build --target "${TARGET}" --no-default-features --release

# No unwind
"${CARGO}" test $CARGO_TEST_FLAGS --target "${TARGET}" --all-targets --no-default-features --features default-stack
"${CARGO}" test $CARGO_TEST_FLAGS --target "${TARGET}" --all-targets --no-default-features --features default-stack --release

# unwind
"${CARGO}" test $CARGO_TEST_FLAGS --target "${TARGET}" --all-targets
"${CARGO}" test $CARGO_TEST_FLAGS --target "${TARGET}" --all-targets --release

# asm-unwind
# Currently disabled because of LLVM issues.
#if [ "${CHANNEL}" = "nightly" ]; then
#    "${CARGO}" test $CARGO_TEST_FLAGS --target "${TARGET}" --all-targets --features asm-unwind
#    "${CARGO}" test $CARGO_TEST_FLAGS --target "${TARGET}" --all-targets --features asm-unwind --release
#fi
