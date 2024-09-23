use corosensei::stack::DefaultStack;
use corosensei::{Coroutine, CoroutineResult};

fn main() {
    let mut coroutine = Coroutine::<(), (), ()>::with_stack(
        DefaultStack::new(128 * 1024).expect("failed to allocate stack"),
        |_, ()| {
            fn recurse(i: u32, p: &mut [u8; 10000]) {
                Coroutine::<(), (), ()>::maybe_grow(32 * 1024, 128 * 1024, || {
                    // Ensure the stack allocation isn't optimized away.
                    unsafe { std::ptr::read_volatile(&p) };
                    if i > 0 {
                        recurse(i - 1, &mut [0; 10000]);
                    }
                })
                .expect("failed to grow stack");
            }

            // Use ~500KB of stack.
            recurse(50, &mut [0; 10000]);
        },
    );
    coroutine.set_growable(true);
    assert_eq!(coroutine.resume(()), CoroutineResult::Return(()));
}

#[test]
fn maybe_grow_stack() {
    main()
}
