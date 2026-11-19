# Xtensa test runner

Upstream Rust doesn't support Xtensa, so corosensei's test suite can't run
there with `cargo test`. This crate is a `no_std` firmware for the ESP32,
ESP32-S2 and ESP32-S3 which runs:

- a port of corosensei's tests (`src/ported.rs`, from `src/tests/`);
- the trap handler tests, using the Xtensa exception handler instead of
  signals (`src/trap.rs`);
- Xtensa-specific stress tests for the windowed register ABI
  (`src/stress.rs`):
  - call chains deeper than the 16 register windows before suspending;
  - suspending at every level of deep call chains;
  - switching at varying depths on both sides;
  - many interleaved coroutines and nested coroutines;
  - FPU values, SAR and zero-overhead loops in both contexts across
    switches;
  - `on_stack`/`on_parent_stack` from deep inside a coroutine;
  - the window state after a switch.
- the stress tests again, with a high-frequency timer interrupt
  (`src/irq.rs`) whose handler makes deep windowed calls. The jittered
  period makes interrupts, and the window exceptions they cause, land on
  every instruction of the switching code. The runner counts how many
  interrupts arrived inside corosensei's assembly routines and fails if
  there were too few.

Results are printed on UART0. The run ends with `ALL TESTS PASSED`. A failure
ends with a panic message and backtrace.

## Requirements

- The esp-rs toolchain: `cargo install espup && espup install`, then
  `. ~/export-esp.sh`.
- [`espflash`](https://github.com/esp-rs/espflash) to create flash images or
  flash a board.
- For emulation, `qemu-system-xtensa` from Espressif's QEMU fork:
  <https://github.com/espressif/qemu/releases>. It provides `esp32` and
  `esp32s3` machines. There is no ESP32-S2 machine, so the ESP32-S2 is
  only built.

## Running under QEMU

```sh
ci/xtensa/run-qemu.sh esp32 release
ci/xtensa/run-qemu.sh esp32s3 dev
```

The script builds the runner, creates a merged flash image with `espflash`,
boots it in QEMU and waits for the result. Set `QEMU` if `qemu-system-xtensa`
isn't on `PATH`.

Extra configurations used for testing:

```sh
# a7 is LLVM's frame pointer under the windowed ABI and must be preserved
# across switches.
CARGO_TARGET_DIR=target/ffp EXTRA_RUSTFLAGS="-C force-frame-pointers=yes" \
    ci/xtensa/run-qemu.sh esp32 release

# Same register options as the ESP32-S2 (no FPU, loop, boolean or MAC16
# options), run on the ESP32 machine since QEMU has no ESP32-S2.
CARGO_TARGET_DIR=target/nofp EXTRA_RUSTFLAGS="-C target-feature=-fp,-loop,-bool,-mac16" \
    ci/xtensa/run-qemu.sh esp32 release
```

## Running on hardware

```sh
cd ci/xtensa
cargo +esp run --release --target xtensa-esp32s3-none-elf --features esp32s3 \
    --config 'target.xtensa-esp32s3-none-elf.runner="espflash flash --monitor"'
```

## Notes

- Release builds use the default codegen units (local ThinLTO). This matters
  because the esp-rs compiler parses module-level assembly without the target
  features in that configuration (see `src/arch/xtensa.rs`).
- The runner uses esp-hal 1.0. Later versions wait for the ESP32-S3 BBPLL
  calibration to complete during initialization, which Espressif's QEMU
  doesn't emulate.
- esp-hal saves the FPU registers on every exception and interrupt, which
  faults if the FPU coprocessor is disabled. It is disabled at boot under
  QEMU, so the runner enables it explicitly.
- Stacks are allocated from the heap and have no guard pages. Instead they're
  filled with a pattern, and dropping a stack panics if its lowest 256 bytes
  were overwritten. That way an overflow is reported as an overflow, not as
  a corrupted frame. The stack overflow tests from the main test suite, which
  rely on guard pages, are not ported. Unoptimized frames are much larger,
  so the stress tests size their stacks per profile.

## CI

`.github/workflows/xtensa.yml` is an optional workflow which builds the
library for all Xtensa targets and runs this test runner under QEMU, in the
`espressif/idf-rust` container. It only runs when triggered manually
(`workflow_dispatch`); uncomment the `push`/`pull_request` triggers to run it
automatically. `gha-env.sh` makes the container's toolchain usable from steps
running as root.
