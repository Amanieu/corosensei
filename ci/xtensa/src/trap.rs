//! Trap handler tests, the Xtensa equivalent of corosensei's signal-based
//! `trap_handler` tests.
//!
//! The coroutine executes an illegal instruction. The exception is caught by
//! overriding xtensa-lx-rt's `__exception` hook, which updates the saved
//! exception context with the registers returned by `setup_trap_handler`, so
//! that returning from the exception continues in the trap handler function on
//! the coroutine stack.

use core::cell::RefCell;
use core::hint::black_box;

use corosensei::trap::{CoroutineTrapHandler, TrapHandlerRegs};
use corosensei::{Coroutine, CoroutineResult};
use critical_section::Mutex;
use xtensa_lx_rt::exception::{Context, ExceptionCause};

use crate::stack::HeapStack;

type Setup = fn(&CoroutineTrapHandler<u32>) -> TrapHandlerRegs;

static HANDLER: Mutex<RefCell<Option<(CoroutineTrapHandler<u32>, Setup)>>> =
    Mutex::new(RefCell::new(None));

extern "C" {
    // The default handler from esp-hal, which panics.
    fn __user_exception(cause: ExceptionCause, context: &mut Context);
}

// xtensa-lx-rt calls this with a direct call4 from IRAM, so it must also be in
// IRAM to be in range.
#[unsafe(no_mangle)]
#[unsafe(link_section = ".rwtext")]
unsafe extern "C" fn __exception(cause: ExceptionCause, context: &mut Context) {
    if let ExceptionCause::Illegal = cause {
        let handler = critical_section::with(|cs| HANDLER.borrow_ref_mut(cs).take());
        if let Some((handler, setup)) = handler {
            if handler.stack_ptr_in_bounds(context.A1 as usize) {
                let TrapHandlerRegs {
                    pc,
                    a0,
                    a1,
                    a2,
                    a3,
                    a4,
                } = setup(&handler);
                context.PC = pc as u32;
                context.A0 = a0 as u32;
                context.A1 = a1 as u32;
                context.A2 = a2 as u32;
                context.A3 = a3 as u32;
                context.A4 = a4 as u32;
                return;
            }
        }
    }
    unsafe { __user_exception(cause, context) }
}

fn install(handler: CoroutineTrapHandler<u32>, setup: Setup) {
    critical_section::with(|cs| HANDLER.replace(cs, Some((handler, setup))));
}

#[inline(never)]
fn illegal_instruction() {
    unsafe { core::arch::asm!("ill") };
}

/// Recurses with live values in every frame, then traps at the bottom. None of
/// this ever returns.
#[inline(never)]
fn trap_at_depth(depth: u32, value: u32) -> u32 {
    let local = black_box(value.wrapping_mul(3).wrapping_add(depth));
    if depth == 0 {
        illegal_instruction();
        unreachable!("returned from a trap");
    }
    let result = trap_at_depth(depth - 1, local);
    result ^ local
}

pub fn trap_handler() {
    let mut coroutine = Coroutine::with_stack(HeapStack::default(), |y, ()| {
        illegal_instruction();
        y.suspend(1);
        2
    });
    install(coroutine.trap_handler(), |h| unsafe {
        h.setup_trap_handler(|| 42)
    });
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(42));

    // Reuse the stack and do it again, with a different return value.
    let stack = coroutine.into_stack();
    let mut coroutine = Coroutine::with_stack(stack, |y, ()| {
        illegal_instruction();
        y.suspend(1);
        2
    });
    install(coroutine.trap_handler(), |h| unsafe {
        h.setup_trap_handler(|| black_box(1234))
    });
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(1234));

    // And check that a normal coroutine still works on the reused stack.
    let stack = coroutine.into_stack();
    let mut coroutine = Coroutine::with_stack(stack, |y, x: u32| y.suspend(x + 1) + 1);
    assert_eq!(coroutine.resume(1), CoroutineResult::Yield(2));
    assert_eq!(coroutine.resume(10), CoroutineResult::Return(11));
}

/// Traps with many live register windows (more than the 16 physical windows)
/// belonging to the trapped code, which must be discarded rather than restored
/// or spilled onto the reused stack.
pub fn trap_handler_deep() {
    for depth in [5, 17, 40] {
        let mut coroutine = Coroutine::with_stack(HeapStack::default(), move |y, x: u32| {
            let x = y.suspend(x);
            trap_at_depth(depth, x)
        });
        install(coroutine.trap_handler(), |h| unsafe {
            h.setup_trap_handler(|| {
                // Make deep calls in the handler, which would trigger window
                // overflows for any stale windows of the trapped frames.
                trap_free_recursion(30, 7)
            })
        });
        assert_eq!(coroutine.resume(3), CoroutineResult::Yield(3));
        assert_eq!(
            coroutine.resume(5),
            CoroutineResult::Return(trap_free_recursion(30, 7))
        );
    }
}

#[inline(never)]
fn trap_free_recursion(depth: u32, value: u32) -> u32 {
    let local = black_box(value.wrapping_mul(31).wrapping_add(depth));
    if depth == 0 {
        return local;
    }
    trap_free_recursion(depth - 1, local) ^ local
}
