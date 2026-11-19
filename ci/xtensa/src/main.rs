//! On-target test runner for corosensei on Xtensa (ESP32, ESP32-S2, ESP32-S3).
//!
//! This runs a `no_std` port of corosensei's test suite plus Xtensa-specific
//! stress tests, on real hardware or under Espressif's QEMU fork. Results are
//! printed on UART0. The run ends with `ALL TESTS PASSED`, while any failure
//! ends in a panic message and backtrace from `esp-backtrace`.
//!
//! See `ci/xtensa/README.md` for how to build and run it.

#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

extern crate alloc;

mod irq;
mod ported;
mod stack;
mod stress;
mod trap;

use esp_println::println;

esp_bootloader_esp_idf::esp_app_desc!();

/// Heap used for coroutine stacks and test data.
#[cfg(feature = "esp32s3")]
const HEAP_SIZE: usize = 192 * 1024;
#[cfg(not(feature = "esp32s3"))]
const HEAP_SIZE: usize = 112 * 1024;

pub struct Test {
    pub name: &'static str,
    pub run: fn(),
}

macro_rules! tests {
    ($($path:path),* $(,)?) => {
        &[$(Test { name: stringify!($path), run: $path }),*]
    };
}

/// Tests which are run a second time with a high-frequency timer interrupt
/// whose handler performs deep windowed calls, so that interrupts and window
/// exceptions hit the stack switching code at arbitrary points.
const INTERRUPT_STRESS_TESTS: &[Test] = tests![
    stress::switch_storm,
    stress::deep_call_chain_before_suspend,
    stress::suspend_at_every_depth,
    stress::varying_parent_depth,
    stress::many_coroutines_interleaved,
    stress::nested_coroutines,
    stress::fpu_live_across_switch,
    stress::sar_live_across_switch,
    stress::on_stack_and_parent_stack_deep,
];

#[esp_hal::main]
fn main() -> ! {
    // esp-hal saves the FPU registers on every exception and interrupt
    // (`float-save-restore`), which itself faults if the FPU coprocessor is
    // disabled. Nothing in esp-hal 1.0 enables it, and it is disabled at boot
    // under QEMU, so enable it explicitly. This depends on the chip, not on
    // whether the code is compiled to use the FPU.
    #[cfg(any(feature = "esp32", feature = "esp32s3"))]
    unsafe {
        core::arch::asm!("movi {0}, 1", "wsr.cpenable {0}", "rsync", out(reg) _);
    }

    let _peripherals = esp_hal::init(esp_hal::Config::default());
    esp_alloc::heap_allocator!(size: HEAP_SIZE);

    println!(
        "corosensei xtensa test runner: chip={} profile={} fp={} loop={}",
        CHIP,
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        },
        cfg!(target_feature = "fp"),
        cfg!(target_feature = "loop"),
    );

    let tests: &[Test] = tests![
        ported::smoke,
        ported::suspend_and_resume,
        ported::suspend_and_resume_values,
        ported::stateful,
        ported::force_unwind,
        ported::force_reset,
        ported::stack_growth,
        ported::scoped,
        ported::on_stack_smoke,
        ported::on_stack_stack_growth,
        ported::on_parent_stack,
        ported::backtrace_traces_to_host,
        ported::on_stack_backtrace_traces_to_host,
        trap::trap_handler,
        trap::trap_handler_deep,
        stress::deep_call_chain_before_suspend,
        stress::suspend_at_every_depth,
        stress::varying_parent_depth,
        stress::switch_storm,
        stress::many_coroutines_interleaved,
        stress::nested_coroutines,
        stress::fpu_live_across_switch,
        stress::sar_live_across_switch,
        stress::hardware_loops,
        stress::on_stack_and_parent_stack_deep,
        stress::window_state_after_switch,
    ];

    // Only run tests whose name contains this string, if set at build time.
    let filter = option_env!("TEST_FILTER").unwrap_or("");
    let selected = |test: &&Test| test.name.contains(filter);

    let mut passed = 0;
    for test in tests.iter().filter(selected) {
        println!("test {} ...", test.name);
        (test.run)();
        println!("test {} ... ok", test.name);
        passed += 1;
    }

    irq::start();
    for test in INTERRUPT_STRESS_TESTS.iter().filter(selected) {
        println!("test {} (with interrupts) ...", test.name);
        let before = irq::count();
        (test.run)();
        let fired = irq::count() - before;
        println!(
            "test {} (with interrupts) ... ok ({} interrupts)",
            test.name, fired
        );
        passed += 1;
    }
    irq::stop();
    let total_interrupts = irq::count();
    let in_switch_code = irq::in_switch_code_count();
    println!(
        "{} timer interrupts, {} of them inside the stack switching code",
        total_interrupts, in_switch_code
    );
    assert!(
        !filter.is_empty() || total_interrupts >= 1000,
        "only {} timer interrupts fired during the interrupt stress tests",
        total_interrupts
    );
    assert!(
        !filter.is_empty() || in_switch_code >= 10,
        "only {} timer interrupts hit the stack switching code",
        in_switch_code
    );

    println!("test result: ok. {} passed", passed);
    println!("ALL TESTS PASSED");
    loop {
        core::hint::spin_loop();
    }
}

#[cfg(feature = "esp32")]
const CHIP: &str = "esp32";
#[cfg(feature = "esp32s2")]
const CHIP: &str = "esp32s2";
#[cfg(feature = "esp32s3")]
const CHIP: &str = "esp32s3";
