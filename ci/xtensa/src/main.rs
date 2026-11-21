//! corosensei tests for Xtensa, run under QEMU with a jittered timer interrupt so
//! that interrupts and window exceptions hit every instruction of the switch code.
#![no_std]
#![no_main]
#![feature(asm_experimental_arch)]

extern crate alloc;
use alloc::{boxed::Box, vec};
use core::{arch::asm, hint::black_box, sync::atomic::AtomicU32, sync::atomic::Ordering::Relaxed};
use corosensei::stack::{Stack, StackPointer};
use corosensei::{on_stack, Coroutine, CoroutineResult::*, Yielder};
use esp_backtrace as _;

esp_bootloader_esp_idf::esp_app_desc!();

struct HeapStack(Box<[u8]>);
fn stack() -> HeapStack {
    HeapStack(vec![0; 16 * 1024].into_boxed_slice())
}
unsafe impl Stack for HeapStack {
    fn base(&self) -> StackPointer {
        StackPointer::new(self.0.as_ptr_range().end as usize & !15).unwrap()
    }
    fn limit(&self) -> StackPointer {
        StackPointer::new((self.0.as_ptr() as usize + 15) & !15).unwrap()
    }
}

/// Recurses `depth` calls deep (more than 16 overflows the register windows),
/// calling `f` at every level with integer and FP values live.
#[inline(never)]
fn nest(depth: u32, f: &mut dyn FnMut(u32) -> u32) -> u32 {
    let (d, fp, x) = (black_box(depth), black_box(depth as f32), f(depth));
    let r = x + if depth == 0 { 0 } else { nest(depth - 1, f) };
    assert_eq!((d, fp), (depth, depth as f32));
    r
}

fn run_tests() {
    // Suspend at every level of a deep recursion, and resume from varying depths.
    for depth in [0, 1, 3, 13, 30] {
        let mut co = Coroutine::with_stack(stack(), move |y, _| nest(depth, &mut |d| y.suspend(d)));
        let sum = nest(depth + 1, &mut |_| {
            let (Yield(x) | Return(x)) = co.resume(1);
            x
        });
        assert_eq!(
            (sum, co.done()),
            (depth * (depth + 1) / 2 + depth + 1, true)
        );
    }
    let mut outer = Coroutine::with_stack(stack(), |y: &Yielder<(), u32>, ()| {
        let mut inner = Coroutine::with_stack(stack(), |y: &Yielder<(), u32>, ()| {
            (0..3).for_each(|i| y.suspend(i));
            y.on_parent_stack(|| nest(30, &mut |d| d))
        });
        (0..3).for_each(|_| y.suspend(inner.resume(()).as_yield().unwrap() + 10));
        assert_eq!(inner.resume(()), Return(465));
        on_stack(stack(), || nest(30, &mut |d| d + 1))
    });
    (10..13).for_each(|i| assert_eq!(outer.resume(()), Yield(i)));
    assert_eq!(outer.resume(()), Return(496));
}

static IRQS: AtomicU32 = AtomicU32::new(0);

/// Schedules the next CCOMPARE0 interrupt, retrying if CCOUNT passes it first.
fn rearm(period: u32) {
    unsafe {
        asm!("1: rsr.ccount {t}", "add {t}, {t}, {p}", "wsr.ccompare0 {t}", "rsync", "rsr.ccount {n}",
             "sub {n}, {t}, {n}", "slli {p}, {p}, 1", "blti {n}, 1, 1b", p = inout(reg) period => _, t = out(reg) _, n = out(reg) _)
    };
}

#[unsafe(no_mangle)]
#[unsafe(link_section = ".rwtext")]
extern "C" fn Timer0(_: &mut xtensa_lx_rt::exception::Context) {
    let n = IRQS.load(Relaxed) + 1;
    IRQS.store(n, Relaxed); // No atomic RMW on the ESP32-S2.
    nest(24, &mut |d| d); // Spill the interrupted code's windows.
    rearm(600 + (n.wrapping_mul(2_654_435_761) >> 23) % 511); // Last, so the tests progress.
}

#[esp_hal::main]
fn main() -> ! {
    #[cfg(not(feature = "esp32s2"))] // esp-hal saves the FPU on interrupts.
    unsafe {
        asm!("movi {0}, 1", "wsr.cpenable {0}", "rsync", out(reg) _)
    };
    esp_hal::init(esp_hal::Config::default());
    esp_alloc::heap_allocator!(size: 96 * 1024);
    rearm(600);
    unsafe { asm!("wsr.intenable {0}", "rsync", in(reg) 1 << 6) }; // CCOMPARE0
    (0..500).for_each(|_| run_tests());
    esp_println::println!("{} timer interrupts", IRQS.load(Relaxed));
    exit(if IRQS.load(Relaxed) >= 1000 { 0 } else { 1 })
}

/// Called by esp-backtrace after a panic.
#[unsafe(no_mangle)]
fn custom_halt() -> ! {
    exit(1)
}

/// Exits QEMU, which must run with `-semihosting`.
fn exit(code: u32) -> ! {
    unsafe { asm!("simcall", in("a2") 1, in("a3") code) }; // SYS_exit
    loop {}
}
