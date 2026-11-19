//! Port of corosensei's own test suite (`src/tests/`) to `no_std`.
//!
//! Differences from the original tests:
//! - Stacks are `HeapStack` instead of `DefaultStack` (which needs `std`).
//! - Tests which need `std` (threads, signal handlers, catching panics) are
//!   omitted: Xtensa targets are `panic = "abort"`. The trap handler tests are
//!   reimplemented in `trap.rs` using the Xtensa exception handler.
//! - Backtrace tests use `esp-backtrace`, which walks the windowed ABI base
//!   save areas, instead of the `backtrace` crate.
//! - Stack sizes are scaled down to fit in the ESP32's internal RAM.

use alloc::rc::Rc;
use alloc::string::{String, ToString};
use core::cell::Cell;
use core::hint::black_box;
use core::ptr;

use corosensei::{on_stack, Coroutine, CoroutineResult, ScopedCoroutine};
use esp_println::println;

use crate::stack::HeapStack;

pub fn smoke() {
    let hit = Rc::new(Cell::new(false));
    let hit2 = hit.clone();
    let mut coroutine =
        Coroutine::<(), (), (), _>::with_stack(HeapStack::default(), move |_, _| {
            hit2.set(true);
        });
    assert!(!hit.get());
    assert!(!coroutine.started());
    assert!(!coroutine.done());
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(()));
    assert!(hit.get());
    assert!(coroutine.started());
    assert!(coroutine.done());
}

pub fn suspend_and_resume() {
    let hit = Rc::new(Cell::new(false));
    let hit2 = hit.clone();
    let mut coroutine =
        Coroutine::<(), (), (), _>::with_stack(HeapStack::default(), move |y, _| {
            y.suspend(());
            hit2.set(true);
            y.suspend(());
        });
    assert!(!hit.get());
    assert!(!coroutine.started());
    assert!(!coroutine.done());
    assert_eq!(coroutine.resume(()), CoroutineResult::Yield(()));
    assert!(!hit.get());
    assert!(coroutine.started());
    assert!(!coroutine.done());
    assert_eq!(coroutine.resume(()), CoroutineResult::Yield(()));
    assert!(hit.get());
    assert!(coroutine.started());
    assert!(!coroutine.done());
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(()));
    assert!(hit.get());
    assert!(coroutine.started());
    assert!(coroutine.done());
}

pub fn suspend_and_resume_values() {
    let mut coroutine = Coroutine::with_stack(HeapStack::default(), move |y, first| {
        assert_eq!(first, 2.0);
        assert_eq!(y.suspend(4), 3.0);
        "hello".to_string()
    });
    assert_eq!(coroutine.resume(2.0), CoroutineResult::Yield(4));
    assert_eq!(
        coroutine.resume(3.0),
        CoroutineResult::Return("hello".to_string())
    );
}

pub fn stateful() {
    #[allow(dead_code)]
    #[repr(align(128))]
    struct Aligned(u8);
    let state = [41, 42, 43, 44, 45];
    let aligned = Aligned(100);
    let mut coroutine = Coroutine::with_stack(HeapStack::default(), move |y, _| {
        assert_eq!(&aligned as *const _ as usize % 128, 0);
        for i in state {
            y.suspend(i);
        }
    });
    for i in state {
        assert_eq!(coroutine.resume(()), CoroutineResult::Yield(i));
    }
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(()));
}

// Rc<Cell> instead of the original Arc<AtomicBool>: the ESP32-S2 has no atomic
// compare-and-swap, so `alloc::sync` isn't available there.
struct SetOnDrop(Rc<Cell<bool>>);
impl Drop for SetOnDrop {
    fn drop(&mut self) {
        self.0.set(true);
    }
}

pub fn force_unwind() {
    // A coroutine which hasn't started yet only needs its initial closure
    // dropped, which works without unwinding.
    let a = Rc::new(Cell::new(false));
    let b = Rc::new(Cell::new(false));
    let a_drop = SetOnDrop(a.clone());
    let b_drop = SetOnDrop(b.clone());
    let mut coroutine =
        Coroutine::<(), (), (), _>::with_stack(HeapStack::default(), move |y, ()| {
            drop(a_drop);
            y.suspend(());
            drop(b_drop);
        });
    assert!(!coroutine.started());
    assert!(!coroutine.done());
    coroutine.force_unwind();
    assert!(coroutine.started());
    assert!(coroutine.done());
    drop(coroutine);
    assert!(a.get());
    assert!(b.get());

    // Unwinding a suspended coroutine requires the `unwind` feature and
    // `panic = "unwind"`, neither of which is available on Xtensa. Those parts
    // of the original test are omitted.

    // A completed coroutine.
    let a = Rc::new(Cell::new(false));
    let b = Rc::new(Cell::new(false));
    let a_drop = SetOnDrop(a.clone());
    let b_drop = SetOnDrop(b.clone());
    let mut coroutine =
        Coroutine::<(), (), (), _>::with_stack(HeapStack::default(), move |y, ()| {
            drop(a_drop);
            y.suspend(());
            drop(b_drop);
        });
    coroutine.resume(());
    coroutine.resume(());
    assert!(coroutine.started());
    assert!(coroutine.done());
    coroutine.force_unwind();
    assert!(coroutine.started());
    assert!(coroutine.done());
    drop(coroutine);
    assert!(a.get());
    assert!(b.get());
}

/// Without unwinding support, a suspended coroutine can be abandoned with
/// `force_reset` as long as nothing on its stack needs to be dropped. Its
/// stack can then be reused.
pub fn force_reset() {
    let mut coroutine = Coroutine::<u32, u32, (), _>::with_stack(HeapStack::default(), |y, x| {
        let mut x = x;
        loop {
            x = y.suspend(x + 1);
        }
    });
    assert_eq!(coroutine.resume(1), CoroutineResult::Yield(2));
    assert_eq!(coroutine.resume(5), CoroutineResult::Yield(6));
    unsafe { coroutine.force_reset() };
    assert!(coroutine.done());
    let stack = coroutine.into_stack();

    let mut coroutine = Coroutine::with_stack(stack, |y, x: u32| y.suspend(x * 2) + 1);
    assert_eq!(coroutine.resume(21), CoroutineResult::Yield(42));
    assert_eq!(coroutine.resume(9), CoroutineResult::Return(10));
}

fn recurse(i: u32, p: &mut [u8; 2000]) {
    unsafe {
        // Ensure the stack allocation isn't optimized away.
        ptr::read_volatile(&p);
    }
    if i > 0 {
        recurse(i - 1, &mut [0; 2000]);
    }
}

pub fn stack_growth() {
    let mut coroutine =
        Coroutine::<(), (), (), _>::with_stack(HeapStack::new(48 * 1024), |_, ()| {
            // Use ~40KB of stack.
            recurse(20, &mut [0; 2000]);
        });
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(()));
}

pub fn scoped() {
    let mut local = 10;
    let coroutine = ScopedCoroutine::with_stack(HeapStack::default(), |y, x: i32| {
        local += x;
        local += y.suspend(local);
        local
    });
    let result = coroutine.scope(|mut coroutine| {
        assert_eq!(coroutine.resume(5), CoroutineResult::Yield(15));
        coroutine.resume(7)
    });
    assert_eq!(result, CoroutineResult::Return(22));
    assert_eq!(local, 22);
}

pub fn on_stack_smoke() {
    let hit = Rc::new(Cell::new(false));
    let hit2 = hit.clone();
    let result = on_stack(HeapStack::default(), move || {
        hit2.set(true);
        "hello".to_string()
    });
    assert!(hit.get());
    assert_eq!(result, String::from("hello"));
}

pub fn on_stack_stack_growth() {
    on_stack(HeapStack::new(48 * 1024), || {
        recurse(20, &mut [0; 2000]);
    });
}

pub fn on_parent_stack() {
    let mut coroutine = Coroutine::with_stack(HeapStack::new(4096), |y, x: u32| {
        // The parent stack is much larger than the 4KB coroutine stack.
        let a = y.on_parent_stack(|| {
            let mut buf = [0u8; 8000];
            buf[7999] = 3;
            black_box(&mut buf)[7999] as u32 + x
        });
        let b = y.suspend(a);
        y.on_parent_stack(move || b * 2)
    });
    assert_eq!(coroutine.resume(1), CoroutineResult::Yield(4));
    assert_eq!(coroutine.resume(21), CoroutineResult::Return(42));
}

/// Checks that a backtrace captured with esp-backtrace contains a frame of
/// `look_for_me`, i.e. that the walk continues from the coroutine (or
/// `on_stack`) stack into the stack of its parent.
fn assert_contains_host(look_for_me: usize) {
    let trace = esp_backtrace::Backtrace::capture();
    let found = trace
        .frames()
        .iter()
        .any(|f| (look_for_me..look_for_me + 64).contains(&f.program_counter()));
    if !found {
        println!("look_for_me = {:#x}", look_for_me);
        for frame in trace.frames() {
            println!("  frame pc = {:#x}", frame.program_counter());
        }
        panic!("backtrace doesn't contain look_for_me");
    }
}

pub fn backtrace_traces_to_host() {
    #[inline(never)] // try to get this to show up in backtraces
    fn look_for_me() {
        run_test();
        black_box(());
    }

    fn run_test() {
        let host = look_for_me as *const () as usize;
        assert_contains_host(host);
        let mut coroutine =
            Coroutine::<(), (), (), _>::with_stack(HeapStack::default(), move |y, ()| {
                assert_contains_host(host);
                y.suspend(());
                y.on_parent_stack(|| assert_contains_host(host));
                y.suspend(());
                assert_contains_host(host);
            });
        assert_eq!(coroutine.resume(()), CoroutineResult::Yield(()));
        assert_eq!(coroutine.resume(()), CoroutineResult::Yield(()));
        assert_eq!(coroutine.resume(()), CoroutineResult::Return(()));
    }

    look_for_me();
}

pub fn on_stack_backtrace_traces_to_host() {
    #[inline(never)] // try to get this to show up in backtraces
    fn look_for_me() {
        run_test();
        black_box(());
    }

    fn run_test() {
        let host = look_for_me as *const () as usize;
        assert_contains_host(host);
        on_stack(HeapStack::default(), || {
            assert_contains_host(host);
        });
    }

    look_for_me();
}
