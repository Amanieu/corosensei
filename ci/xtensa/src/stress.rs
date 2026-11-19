//! Xtensa-specific stress tests for the windowed register ABI.
//!
//! Most tests recurse through many windowed calls, so that more frames are
//! live than fit in the 16 physical register windows, and keep values live in
//! registers across each call. A frame whose registers are spilled to or
//! reloaded from the wrong place is detected when it checks its values on the
//! way back up.

use alloc::vec::Vec;
use core::hint::black_box;

use corosensei::{on_stack, Coroutine, CoroutineResult, Yielder};

use crate::stack::HeapStack;

/// Recurses `depth` levels with several values live across each recursive
/// call. Calls `at_bottom` at the bottom and returns a checksum which depends
/// on every frame. Variable shifts and rotations also keep SAR busy.
#[inline(never)]
fn deep(depth: u32, seed: u32, at_bottom: &mut dyn FnMut(u32)) -> u32 {
    let a = black_box(seed.wrapping_mul(0x9e37_79b9).wrapping_add(depth));
    let b = black_box(a ^ depth.wrapping_mul(0x85eb_ca6b));
    let c = black_box(b.rotate_left(depth % 32));
    let d = black_box(c.wrapping_add(0x1234_5678) >> (depth % 7));
    let r = if depth == 0 {
        at_bottom(d);
        d
    } else {
        deep(depth - 1, d, at_bottom)
    };
    assert_eq!(
        b,
        a ^ depth.wrapping_mul(0x85eb_ca6b),
        "corrupted frame at depth {}",
        depth
    );
    assert_eq!(
        c,
        b.rotate_left(depth % 32),
        "corrupted frame at depth {}",
        depth
    );
    assert_eq!(
        d,
        c.wrapping_add(0x1234_5678) >> (depth % 7),
        "corrupted frame at depth {}",
        depth
    );
    r.wrapping_mul(3) ^ a ^ c
}

/// Approximate stack frame size of `deep`, which is much larger in unoptimized
/// builds.
const DEEP_FRAME_SIZE: usize = if cfg!(debug_assertions) { 320 } else { 80 };

/// Room for everything other than `deep` frames: the closures and helpers in
/// the tests, and the timer interrupt handler (exception frame plus a deep
/// call chain), which runs on whatever stack it interrupts.
const STACK_HEADROOM: usize = if cfg!(debug_assertions) {
    8 * 1024
} else {
    4 * 1024
};

/// A stack big enough for a chain of `depth` calls to `deep`.
fn stack_for_depth(depth: u32) -> HeapStack {
    HeapStack::new((depth as usize + 1) * DEEP_FRAME_SIZE + STACK_HEADROOM)
}

/// Returns the value passed to `at_bottom` and the result of `deep`.
fn deep_reference(depth: u32, seed: u32) -> (u32, u32) {
    let mut bottom = 0;
    let result = deep(depth, seed, &mut |b| bottom = b);
    (bottom, result)
}

/// Calls `f` from `depth` levels of recursion, with a value live in each frame.
#[inline(never)]
fn at_depth<R>(depth: u32, f: &mut dyn FnMut() -> R) -> R {
    let guard = black_box(depth ^ 0x5a5a_5a5a);
    let r = if depth == 0 {
        f()
    } else {
        at_depth(depth - 1, f)
    };
    assert_eq!(
        guard,
        depth ^ 0x5a5a_5a5a,
        "corrupted frame at depth {}",
        depth
    );
    r
}

/// Deep call chains (more than 16 windows) inside the coroutine before
/// suspending, with the coroutine suspended several times at the bottom.
pub fn deep_call_chain_before_suspend() {
    for depth in [3, 20, 48, 90] {
        let mut coroutine = Coroutine::with_stack(
            stack_for_depth(depth),
            move |y: &Yielder<u32, u32>, seed: u32| {
                deep(depth, seed, &mut |bottom| {
                    for i in 0..4 {
                        let input = y.suspend(bottom.wrapping_add(i));
                        assert_eq!(input, i * 7 + 1);
                    }
                })
            },
        );
        let (bottom, result) = deep_reference(depth, 1234);
        assert_eq!(coroutine.resume(1234), CoroutineResult::Yield(bottom));
        for i in 0..3 {
            assert_eq!(
                coroutine.resume(i * 7 + 1),
                CoroutineResult::Yield(bottom.wrapping_add(i + 1))
            );
        }
        assert_eq!(coroutine.resume(3 * 7 + 1), CoroutineResult::Return(result));
    }
}

fn nested(y: &Yielder<u32, u32>, depth: u32, max: u32) -> u32 {
    let local = black_box(depth.wrapping_mul(1000).wrapping_add(7));
    let input = y.suspend(depth);
    assert_eq!(input, depth + 100);
    let below = if depth < max {
        nested(y, depth + 1, max)
    } else {
        0
    };
    let input = y.suspend(1000 + depth);
    assert_eq!(input, depth + 200);
    assert_eq!(local, depth.wrapping_mul(1000).wrapping_add(7));
    below.wrapping_add(local)
}

/// Suspends at every level of a deep chain of windowed calls, both on the way
/// down and on the way back up.
pub fn suspend_at_every_depth() {
    let max = 40;
    let mut coroutine = Coroutine::with_stack(HeapStack::new(16 * 1024), move |y, _: u32| {
        nested(y, 0, max)
    });
    let expected: u32 = (0..=max).map(|d| d * 1000 + 7).sum();
    let mut input = 0;
    let mut down = 0;
    let mut up = 0;
    loop {
        match coroutine.resume(input) {
            CoroutineResult::Yield(v) if v < 1000 => {
                assert_eq!(v, down);
                down += 1;
                input = v + 100;
            }
            CoroutineResult::Yield(v) => {
                let depth = v - 1000;
                assert_eq!(depth, max - up);
                up += 1;
                input = depth + 200;
            }
            CoroutineResult::Return(r) => {
                assert_eq!(r, expected);
                break;
            }
        }
    }
    assert_eq!(down, max + 1);
    assert_eq!(up, max + 1);
}

/// Switches between contexts at many different call depths on both sides, so
/// that the switch happens with many different WindowBase/WindowStart states
/// and with the parent link pointing to different parent frames each time.
pub fn varying_parent_depth() {
    const ROUNDS: u32 = 200;
    let mut coroutine = Coroutine::with_stack(
        HeapStack::new(16 * 1024),
        |y: &Yielder<u32, u32>, mut x: u32| {
            for i in 0..ROUNDS {
                let depth = (i * 7) % 19;
                let input = at_depth(depth, &mut || y.suspend(x.wrapping_mul(3)));
                assert_eq!(input, x + 1);
                x = input;
            }
            x
        },
    );
    let mut x = 5;
    for i in 0..ROUNDS {
        let depth = (i * 5) % 23;
        let result = at_depth(depth, &mut || coroutine.resume(x));
        assert_eq!(result, CoroutineResult::Yield(x.wrapping_mul(3)));
        x += 1;
    }
    assert_eq!(coroutine.resume(x), CoroutineResult::Return(x));
}

/// A large number of switches at varying call depths on both sides. Mostly
/// useful with the timer interrupt enabled, so that interrupts and the window
/// exceptions they cause hit every instruction of the switch code.
pub fn switch_storm() {
    const ROUNDS: u32 = 20_000;
    let mut coroutine = Coroutine::with_stack(
        HeapStack::new(8 * 1024),
        |y: &Yielder<u32, u32>, mut x: u32| {
            for i in 0..ROUNDS {
                let depth = i % 11;
                x = at_depth(depth, &mut || y.suspend(x ^ i));
            }
            x
        },
    );
    let mut x = 1u32;
    for i in 0..ROUNDS {
        let depth = (i * 3) % 13;
        let result = at_depth(depth, &mut || coroutine.resume(x));
        assert_eq!(result, CoroutineResult::Yield(x ^ i));
        x = x.wrapping_mul(5).wrapping_add(1);
    }
    assert_eq!(coroutine.resume(x), CoroutineResult::Return(x));
}

/// Many coroutines, each suspended deep inside its own call chain, resumed in
/// a pseudo-random order.
pub fn many_coroutines_interleaved() {
    const N: u32 = 8;
    const STEPS: u32 = 1000;

    fn depth_for(k: u32, input: u32) -> u32 {
        k % 7 + 3 + input % 5
    }

    let mut coroutines: Vec<_> = (0..N)
        .map(|k| {
            Coroutine::with_stack(
                stack_for_depth(6 + 3 + 4),
                move |y: &Yielder<u32, u32>, mut input: u32| {
                    let mut count = 0;
                    while input != u32::MAX {
                        let mut next = 0;
                        deep(depth_for(k, input), input ^ k, &mut |bottom| {
                            next = y.suspend(bottom);
                        });
                        count += 1;
                        input = next;
                    }
                    count
                },
            )
        })
        .collect();

    let mut resumes = [0u32; N as usize];
    let mut rng = 0x1234_5678u32;
    for step in 0..STEPS {
        rng = rng.wrapping_mul(1_103_515_245).wrapping_add(12345);
        let k = (rng >> 16) % N;
        let input = step.wrapping_mul(2_654_435_761) >> 1;
        let (bottom, _) = deep_reference(depth_for(k, input), input ^ k);
        assert_eq!(
            coroutines[k as usize].resume(input),
            CoroutineResult::Yield(bottom)
        );
        resumes[k as usize] += 1;
    }
    for (k, coroutine) in coroutines.iter_mut().enumerate() {
        let expected = resumes[k];
        if expected == 0 {
            assert_eq!(coroutine.resume(u32::MAX), CoroutineResult::Return(0));
        } else {
            assert_eq!(
                coroutine.resume(u32::MAX),
                CoroutineResult::Return(expected)
            );
        }
    }
}

/// A coroutine which resumes another coroutine from deep inside its own call
/// chain, while the inner coroutine also suspends from deep inside its own.
pub fn nested_coroutines() {
    const ROUNDS: u32 = 20;
    let mut outer = Coroutine::with_stack(
        HeapStack::new(16 * 1024),
        |y: &Yielder<u32, u32>, mut x: u32| {
            let mut inner = Coroutine::with_stack(
                HeapStack::new(8 * 1024),
                |yi: &Yielder<u32, u32>, mut x: u32| {
                    for _ in 0..ROUNDS {
                        x = at_depth(9, &mut || yi.suspend(x.wrapping_mul(2)));
                    }
                    x
                },
            );
            loop {
                match at_depth(12, &mut || inner.resume(x)) {
                    CoroutineResult::Yield(v) => x = at_depth(5, &mut || y.suspend(v)),
                    CoroutineResult::Return(v) => return v,
                }
            }
        },
    );
    let mut x = 1;
    for _ in 0..ROUNDS {
        assert_eq!(outer.resume(x), CoroutineResult::Yield(x * 2));
        x += 3;
    }
    assert_eq!(outer.resume(x), CoroutineResult::Return(x));
}

/// Clobbers all FPU registers and FCR-independent FPU state with garbage.
#[cfg(target_feature = "fp")]
#[inline(never)]
fn clobber_fpu_registers(garbage: u32) {
    unsafe {
        core::arch::asm!(
            "wfr f0, {g}", "wfr f1, {g}", "wfr f2, {g}", "wfr f3, {g}",
            "wfr f4, {g}", "wfr f5, {g}", "wfr f6, {g}", "wfr f7, {g}",
            "wfr f8, {g}", "wfr f9, {g}", "wfr f10, {g}", "wfr f11, {g}",
            "wfr f12, {g}", "wfr f13, {g}", "wfr f14, {g}", "wfr f15, {g}",
            g = in(reg) garbage,
            out("f0") _, out("f1") _, out("f2") _, out("f3") _,
            out("f4") _, out("f5") _, out("f6") _, out("f7") _,
            out("f8") _, out("f9") _, out("f10") _, out("f11") _,
            out("f12") _, out("f13") _, out("f14") _, out("f15") _,
        );
    }
}

#[cfg(not(target_feature = "fp"))]
fn clobber_fpu_registers(_garbage: u32) {}

macro_rules! fp_values {
    ($x:expr; $($name:ident = $k:expr),*) => {
        $(let $name = black_box($x * $k);)*
    };
}

macro_rules! check_fp_values {
    ($x:expr; $($name:ident = $k:expr),*) => {
        $(assert_eq!($name.to_bits(), ($x * $k).to_bits(), concat!("corrupted ", stringify!($name)));)*
    };
}

/// Floating-point values live across switches in both directions, while the
/// other context uses and clobbers all FPU registers. On cores with the FPU
/// (ESP32, ESP32-S3) these live in f0-f15, which are caller-saved and declared
/// as clobbered by the switch, so the compiler must save them.
pub fn fpu_live_across_switch() {
    let mut coroutine = Coroutine::with_stack(
        HeapStack::new(8 * 1024),
        |y: &Yielder<f32, f32>, mut x: f32| {
            for round in 0..20u32 {
                fp_values!(x; c0 = 1.5, c1 = 2.25, c2 = -3.0, c3 = 4.125, c4 = 0.5, c5 = 6.0,
                    c6 = 7.75, c7 = -8.5, c8 = 9.0, c9 = 10.25, c10 = 11.5, c11 = 12.0,
                    c12 = -13.25, c13 = 14.5, c14 = 15.0, c15 = 16.75, c16 = 17.0, c17 = 18.5);
                let sum = c0
                    + c1
                    + c2
                    + c3
                    + c4
                    + c5
                    + c6
                    + c7
                    + c8
                    + c9
                    + c10
                    + c11
                    + c12
                    + c13
                    + c14
                    + c15
                    + c16
                    + c17;
                clobber_fpu_registers(0x7fc0_0000 + round);
                let next = y.suspend(sum);
                check_fp_values!(x; c0 = 1.5, c1 = 2.25, c2 = -3.0, c3 = 4.125, c4 = 0.5, c5 = 6.0,
                    c6 = 7.75, c7 = -8.5, c8 = 9.0, c9 = 10.25, c10 = 11.5, c11 = 12.0,
                    c12 = -13.25, c13 = 14.5, c14 = 15.0, c15 = 16.75, c16 = 17.0, c17 = 18.5);
                assert_eq!(next, x + 1.0);
                x = next;
            }
            x * 2.0
        },
    );

    let mut x = 0.375f32;
    for round in 0..20u32 {
        if round > 0 {
            x += 1.0;
        }
        fp_values!(x; p0 = 0.25, p1 = -1.75, p2 = 3.5, p3 = 5.125, p4 = -6.0, p5 = 7.0,
            p6 = 8.25, p7 = 9.5, p8 = -10.75, p9 = 11.0, p10 = 12.5, p11 = 13.0,
            p12 = 14.25, p13 = -15.5, p14 = 16.0, p15 = 17.25);
        let expected_sum = {
            let k = [
                1.5f32, 2.25, -3.0, 4.125, 0.5, 6.0, 7.75, -8.5, 9.0, 10.25, 11.5, 12.0, -13.25,
                14.5, 15.0, 16.75, 17.0, 18.5,
            ];
            let mut s = 0.0f32;
            for (i, k) in k.iter().enumerate() {
                if i == 0 {
                    s = black_box(x * k);
                } else {
                    s += black_box(x * k);
                }
            }
            s
        };
        clobber_fpu_registers(0xffc0_0000 - round);
        match coroutine.resume(x) {
            CoroutineResult::Yield(sum) => assert_eq!(sum.to_bits(), expected_sum.to_bits()),
            CoroutineResult::Return(_) => panic!("unexpected return"),
        }
        check_fp_values!(x; p0 = 0.25, p1 = -1.75, p2 = 3.5, p3 = 5.125, p4 = -6.0, p5 = 7.0,
            p6 = 8.25, p7 = 9.5, p8 = -10.75, p9 = 11.0, p10 = 12.5, p11 = 13.0,
            p12 = 14.25, p13 = -15.5, p14 = 16.0, p15 = 17.25);
    }
    assert_eq!(
        coroutine.resume(x + 1.0),
        CoroutineResult::Return((x + 1.0) * 2.0)
    );
}

/// Sets SAR (the shift amount register) to a garbage value.
#[inline(never)]
fn clobber_sar(garbage: u32) {
    unsafe {
        core::arch::asm!("wsr.sar {0}", in(reg) garbage, out("sar") _);
    }
}

/// Variable shift amounts live across switches. The compiler must not assume
/// that SAR survives a switch, since it's caller-saved and both contexts use
/// it.
pub fn sar_live_across_switch() {
    let mut coroutine = Coroutine::with_stack(
        HeapStack::new(8 * 1024),
        |y: &Yielder<u32, u32>, mut x: u32| {
            for i in 0..64u32 {
                let amount = black_box(i % 31 + 1);
                let value = black_box(x | 1);
                let shifted = value << amount;
                clobber_sar(31 - (i % 31));
                x = y.suspend(shifted);
                // Shift by the same amount after the switch.
                assert_eq!(value << amount, shifted);
                assert_eq!(
                    (shifted >> amount) & (u32::MAX >> amount),
                    value & (u32::MAX >> amount)
                );
            }
            x
        },
    );
    let mut x = 0x0123_4567u32;
    for i in 0..64u32 {
        let amount = black_box((i * 3) % 29 + 2);
        let mine = black_box(x.rotate_right(amount));
        clobber_sar(i % 31);
        let expected = (x | 1) << (i % 31 + 1);
        assert_eq!(coroutine.resume(x), CoroutineResult::Yield(expected));
        assert_eq!(mine.rotate_left(amount), x);
        x = x.wrapping_mul(0x0101_0101).wrapping_add(i);
    }
    assert_eq!(coroutine.resume(x), CoroutineResult::Return(x));
}

/// Sums `n` copies of 3 with a zero-overhead loop.
#[cfg(target_feature = "loop")]
#[inline(never)]
fn zero_overhead_loop_sum(n: u32) -> u32 {
    let acc: u32;
    unsafe {
        core::arch::asm!(
            "movi {acc}, 0",
            // Align the loop body (LBEG) to 4 bytes: 3-byte nop + 2-byte nop.n
            // + 3-byte loopnez starting at a 4-byte boundary.
            ".balign 4",
            "nop",
            "nop.n",
            "loopnez {n}, 1f",
            "addi {acc}, {acc}, 3",
            "1:",
            n = in(reg) n,
            acc = out(reg) acc,
            out("lbeg") _, out("lend") _, out("lcount") _,
        );
    }
    acc
}

/// Starts a zero-overhead loop of `n` iterations but branches out of it after
/// `stop` iterations, leaving LCOUNT non-zero and LBEG/LEND pointing at this
/// function.
#[cfg(target_feature = "loop")]
#[inline(never)]
fn abandoned_zero_overhead_loop(n: u32, stop: u32) -> u32 {
    let acc: u32;
    unsafe {
        core::arch::asm!(
            "movi {acc}, 0",
            ".balign 4",
            "nop",
            "nop.n",
            "loopnez {n}, 1f",
            "addi {acc}, {acc}, 1",
            "beq {acc}, {stop}, 2f",
            "1:",
            "2:",
            n = in(reg) n,
            stop = in(reg) stop,
            acc = out(reg) acc,
            out("lbeg") _, out("lend") _, out("lcount") _,
        );
    }
    acc
}

/// Zero-overhead loops in both contexts between switches. LLVM doesn't keep
/// hardware loops live across the switch (the loop registers are declared as
/// clobbered), so this checks that loop state left behind by one context
/// doesn't affect the other.
#[cfg(target_feature = "loop")]
pub fn hardware_loops() {
    let mut coroutine = Coroutine::with_stack(
        HeapStack::new(8 * 1024),
        |y: &Yielder<u32, u32>, mut n: u32| {
            for _ in 0..32 {
                let a = zero_overhead_loop_sum(n);
                assert_eq!(a, 3 * n);
                let b = abandoned_zero_overhead_loop(n + 10, 2);
                assert_eq!(b, 2);
                n = y.suspend(a);
                assert_eq!(zero_overhead_loop_sum(n), 3 * n);
            }
            n
        },
    );
    let mut n = 1;
    for _ in 0..32 {
        assert_eq!(abandoned_zero_overhead_loop(100, 7), 7);
        assert_eq!(coroutine.resume(n), CoroutineResult::Yield(3 * n));
        assert_eq!(zero_overhead_loop_sum(n + 5), 3 * (n + 5));
        n += 1;
    }
    assert_eq!(coroutine.resume(n), CoroutineResult::Return(n));
}

#[cfg(not(target_feature = "loop"))]
pub fn hardware_loops() {
    esp_println::println!("  (skipped: target has no loop option)");
}

/// `on_stack` and `on_parent_stack` from deep inside a coroutine, including
/// suspending the coroutine while it runs on a stack given to `on_stack`.
pub fn on_stack_and_parent_stack_deep() {
    let mut coroutine = Coroutine::with_stack(
        HeapStack::new(16 * 1024),
        |y: &Yielder<u32, u32>, x: u32| {
            at_depth(15, &mut || {
                on_stack(stack_for_depth(25), || {
                    at_depth(10, &mut || {
                        let a = y.on_parent_stack(move || deep_reference(30, x).1);
                        let b = y.suspend(a);
                        let c = at_depth(4, &mut || y.on_parent_stack(move || b + 1));
                        c + deep_reference(25, c).1
                    })
                })
            })
        },
    );
    let a = deep_reference(30, 77).1;
    assert_eq!(coroutine.resume(77), CoroutineResult::Yield(a));
    let c = 101;
    assert_eq!(
        coroutine.resume(100),
        CoroutineResult::Return(c + deep_reference(25, c).1)
    );
}

#[inline(always)]
fn window_start() -> u32 {
    let ws: u32;
    unsafe { core::arch::asm!("rsr.windowstart {0}", out(reg) ws, options(nomem, nostack)) };
    ws
}

/// After a switch, only the current frame may be live in the register file:
/// everything else is reloaded from the stack of the resumed context on
/// demand. Checks this by reading WindowStart immediately after the switch
/// returns, on both sides.
pub fn window_state_after_switch() {
    let mut coroutine =
        Coroutine::with_stack(HeapStack::new(8 * 1024), |y: &Yielder<(), u32>, ()| {
            let mut max_bits = 0;
            for _ in 0..8 {
                let ws = black_box(at_depth(20, &mut || {
                    y.suspend(0);
                    window_start()
                }));
                max_bits = max_bits.max(ws.count_ones());
            }
            max_bits
        });
    // Make the parent have lots of live windows when it switches.
    let mut parent_max_bits = 0;
    for _ in 0..8 {
        at_depth(20, &mut || {
            let _ = coroutine.resume(());
            parent_max_bits = parent_max_bits.max(window_start().count_ones());
        });
    }
    // Reading WindowStart in the closure, after the inlined switch returned
    // and after Yielder::suspend/Coroutine::resume returned: at most the
    // closure frame and its direct callee frames that are still live.
    match coroutine.resume(()) {
        CoroutineResult::Return(bits) => {
            assert!(bits <= 2, "coroutine WindowStart had {} bits", bits)
        }
        CoroutineResult::Yield(_) => panic!("unexpected yield"),
    }
    assert!(
        parent_max_bits <= 2,
        "parent WindowStart had {} bits",
        parent_max_bits
    );
}
