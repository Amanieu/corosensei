//! High-frequency timer interrupt used to stress the stack switching code.
//!
//! This uses the Xtensa core's own CCOMPARE0 timer (CPU interrupt 6, level 1)
//! rather than a peripheral timer, so it doesn't depend on the interrupt
//! matrix and behaves the same on every chip, under QEMU and on hardware.
//! esp-hal dispatches it to the `Timer0` symbol defined here.
//!
//! The handler performs deep windowed calls. Window overflow exceptions in
//! interrupt context therefore spill whatever frames the interrupted code had
//! live, including the frames of `stack_switch` and of the `call12` chain
//! that spills register windows. When the handler returns, those frames are
//! reloaded through window underflows.

use core::arch::asm;
use core::cell::Cell;
use core::hint::black_box;

use critical_section::Mutex;
use xtensa_lx_rt::exception::Context;

static COUNT: Mutex<Cell<u32>> = Mutex::new(Cell::new(0));
/// Number of interrupts which arrived while executing corosensei's Xtensa
/// assembly routines (stack_switch, the window spill chain, trampolines).
static IN_SWITCH_CODE: Mutex<Cell<u32>> = Mutex::new(Cell::new(0));

/// Base interrupt period in CPU cycles. A pseudo-random jitter is added so
/// that interrupts land on many different instructions.
const PERIOD_CYCLES: u32 = 600;
const JITTER_CYCLES: u32 = 511;

extern "C" {
    // First and last of corosensei's Xtensa routines, which are emitted
    // consecutively in their own section.
    fn stack_switch_link();
    fn stack_trap_trampoline();
}

fn in_switch_code(pc: u32) -> bool {
    let start = stack_switch_link as *const () as u32;
    // stack_trap_trampoline is the last routine and is shorter than this.
    let end = stack_trap_trampoline as *const () as u32 + 0x30;
    (start..end).contains(&pc)
}

/// CPU interrupt number of the CCOMPARE0 timer.
const TIMER0_INTERRUPT: u32 = 6;

#[unsafe(no_mangle)]
#[unsafe(link_section = ".rwtext")]
extern "C" fn Timer0(save_frame: &mut Context) {
    let n = critical_section::with(|cs| {
        let count = COUNT.borrow(cs);
        count.set(count.get() + 1);
        if in_switch_code(save_frame.PC) {
            let hits = IN_SWITCH_CODE.borrow(cs);
            hits.set(hits.get() + 1);
        }
        count.get()
    });
    // Writing CCOMPARE0 also acknowledges the interrupt.
    rearm(PERIOD_CYCLES + (n.wrapping_mul(2_654_435_761) >> 23) % JITTER_CYCLES);

    // More than 16 windows deep, so that every live window of the interrupted
    // code is spilled even if the interrupt entry code didn't do it already.
    black_box(burn(black_box(24), 1));
}

#[inline(never)]
fn burn(depth: u32, acc: u32) -> u32 {
    let x = black_box(acc.wrapping_mul(31).wrapping_add(depth));
    if depth == 0 {
        return x;
    }
    burn(depth - 1, x) ^ x
}

#[inline(always)]
fn ccount() -> u32 {
    let ccount: u32;
    unsafe { asm!("rsr.ccount {0}", out(reg) ccount, options(nomem, nostack)) };
    ccount
}

/// Schedules the next interrupt `period` cycles from now. Under emulation
/// CCOUNT may advance past the new compare value before it is written, in
/// which case the next match would only happen after CCOUNT wraps around, so
/// retry with a longer period until the compare value is in the future.
fn rearm(mut period: u32) {
    loop {
        let target = ccount().wrapping_add(period);
        unsafe { asm!("wsr.ccompare0 {0}", "rsync", in(reg) target, options(nostack)) };
        if (target.wrapping_sub(ccount()) as i32) > 0 {
            break;
        }
        period = period.saturating_mul(2);
    }
}

pub fn start() {
    critical_section::with(|_| {
        rearm(PERIOD_CYCLES);
        unsafe {
            asm!(
                "rsr.intenable {t}",
                "or {t}, {t}, {mask}",
                "wsr.intenable {t}",
                "rsync",
                t = out(reg) _,
                mask = in(reg) 1u32 << TIMER0_INTERRUPT,
            );
        }
    });
}

pub fn stop() {
    critical_section::with(|_| unsafe {
        asm!(
            "rsr.intenable {t}",
            "and {t}, {t}, {mask}",
            "wsr.intenable {t}",
            "rsync",
            t = out(reg) _,
            mask = in(reg) !(1u32 << TIMER0_INTERRUPT),
        );
    });
}

pub fn count() -> u32 {
    critical_section::with(|cs| COUNT.borrow(cs).get())
}

pub fn in_switch_code_count() -> u32 {
    critical_section::with(|cs| IN_SWITCH_CODE.borrow(cs).get())
}
