use corosensei::stack::DefaultStack;
use corosensei::{Coroutine, CoroutineResult, ScopedCoroutine};
use std::cell::RefCell;
use std::collections::VecDeque;
use std::ffi::c_void;

fn main() {
    thread_local! {
        static CURRENT: RefCell<VecDeque<*const c_void>> = const { RefCell::new(VecDeque::new()) };
    }
    let mut coroutine = Coroutine::new(|yielder, ()| {
        for _ in 0..5 {
            if let Some(co) = CURRENT.with(|s| {
                s.borrow().front().map(|ptr| unsafe {
                    &*(*ptr).cast::<ScopedCoroutine<(), (), (), DefaultStack>>()
                })
            }) {
                // safe
                assert!(unsafe { co.remaining_stack() } > 0);
            }
            yielder.suspend(());
        }
    });
    // init current
    CURRENT.with(|s| {
        s.borrow_mut()
            .push_front(core::ptr::from_ref(&coroutine).cast::<c_void>());
    });
    // safe
    assert!(unsafe { coroutine.remaining_stack() } > 0);
    loop {
        if let CoroutineResult::Return(()) = coroutine.resume(()) {
            break;
        }
        // safe
        assert!(unsafe { coroutine.remaining_stack() } > 0);
    }
    // clean current
    CURRENT.with(|s| _ = s.borrow_mut().pop_front());
    // unsafe
    // assert!(unsafe { coroutine.remaining_stack() } > 0);
}

#[test]
fn remaining_stack() {
    main()
}
