#!/usr/bin/env bash
# Builds the Xtensa test runner and runs it under Espressif's QEMU fork.
#
# Usage: ci/xtensa/run-qemu.sh <esp32|esp32s3> [release|dev] [extra cargo args...]
#
# Requirements:
# - the esp-rs Rust toolchain (`espup install`, then `. ~/export-esp.sh`);
# - espflash (`cargo install espflash`) to create the flash image;
# - qemu-system-xtensa from https://github.com/espressif/qemu/releases, either
#   on PATH or pointed to by $QEMU.
#
# Exits with status 0 if the runner printed "ALL TESTS PASSED", non-zero if
# it panicked, hit an exception or timed out.
#
# Environment variables:
# - QEMU: path to qemu-system-xtensa.
# - TIMEOUT: timeout in seconds (default 900).
# - EXTRA_RUSTFLAGS: additional rustflags, e.g. "-C force-frame-pointers=yes".
#   Use together with CARGO_TARGET_DIR to avoid rebuilding everything when
#   switching between configurations.

set -euo pipefail

chip=${1:?usage: run-qemu.sh <esp32|esp32s3> [release|dev] [cargo args...]}
profile=${2:-release}
shift $(( $# >= 2 ? 2 : 1 ))

case "$chip" in
    esp32|esp32s3) ;;
    esp32s2) echo "Espressif QEMU has no ESP32-S2 machine; build only with cargo." >&2; exit 2 ;;
    *) echo "unknown chip: $chip" >&2; exit 2 ;;
esac

cd "$(dirname "$0")"
target="xtensa-$chip-none-elf"
qemu=${QEMU:-qemu-system-xtensa}
timeout_secs=${TIMEOUT:-900}

if [ -n "${EXTRA_RUSTFLAGS:-}" ]; then
    # RUSTFLAGS replaces the rustflags from .cargo/config.toml, so repeat them.
    export RUSTFLAGS="-C link-arg=-nostartfiles -C link-arg=-Tlinkall.x -C link-arg=-Wl,--no-warn-rwx-segments $EXTRA_RUSTFLAGS"
fi

cargo +esp build --profile "$profile" --target "$target" --features "$chip" "$@"

target_dir=${CARGO_TARGET_DIR:-target}
out_dir="$target_dir/$target/$([ "$profile" = dev ] && echo debug || echo "$profile")"
image="$out_dir/flash-$chip.bin"
log="$out_dir/qemu-$chip.log"
espflash save-image --skip-update-check --chip "$chip" --merge --flash-size 4mb \
    "$out_dir/corosensei-xtensa-tests" "$image"

: > "$log"
"$qemu" -nographic -machine "$chip" \
    -drive "file=$image,if=mtd,format=raw" \
    -serial "file:$log" -monitor none &
qemu_pid=$!
trap 'kill $qemu_pid 2>/dev/null || true' EXIT

status=1
for _ in $(seq "$timeout_secs"); do
    if grep -q "ALL TESTS PASSED" "$log"; then
        status=0
        break
    fi
    if grep -qE "panicked|PANIC|Exception occurred|Guru Meditation" "$log"; then
        sleep 2 # let the backtrace finish printing
        break
    fi
    if ! kill -0 $qemu_pid 2>/dev/null; then
        echo "QEMU exited unexpectedly" >&2
        break
    fi
    sleep 1
done

cat "$log"
if [ $status -ne 0 ]; then
    echo "FAILED: $chip $profile" >&2
fi
exit $status
