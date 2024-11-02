extern crate alloc;
extern crate std;

use std::cell::Cell;
use std::convert::identity;
use std::rc::Rc;
use std::string::ToString;
use std::{println, ptr};

use crate::Fiber;

#[test]
fn smoke() {
    let hit = Rc::new(Cell::new(false));
    let hit2 = hit.clone();
    let fiber = Fiber::new(move |parent| {
        hit2.set(true);
        (parent, drop)
    });
    assert!(!hit.get());
    fiber.switch(identity);
    assert!(hit.get());
}

#[test]
fn suspend_and_resume() {
    let hit = Rc::new(Cell::new(false));
    let hit2 = hit.clone();
    let fiber = Fiber::new(move |parent: Fiber<Fiber<Fiber<Fiber<Fiber<()>>>>>| {
        let parent = parent.switch(identity);
        hit2.set(true);
        let parent = parent.switch(identity);
        (parent, drop)
    });
    assert!(!hit.get());
    let fiber = fiber.switch(identity);
    assert!(!hit.get());
    let fiber = fiber.switch(identity);
    assert!(hit.get());
    fiber.switch(identity);
    assert!(hit.get());
}

// Linked backtraces are not supported on x86 Windows.
#[cfg_attr(all(windows, target_arch = "x86"), ignore)]
#[test]
fn backtrace_traces_to_host() {
    #[inline(never)] // try to get this to show up in backtraces
    fn look_for_me() {
        run_test();
    }
    fn assert_contains_host() {
        let trace = backtrace::Backtrace::new();
        println!("{:?}", trace);
        assert!(trace
            .frames()
            .iter()
            .flat_map(|f| f.symbols())
            .filter_map(|s| Some(s.name()?.to_string()))
            .any(|s| s.contains("look_for_me")));
    }

    fn run_test() {
        assert_contains_host();
        let fiber = Fiber::new(move |parent: Fiber<Fiber<Fiber<Fiber<Fiber<()>>>>>| {
            assert_contains_host();
            let parent = parent.switch(identity);
            let parent = parent.switch(|f| {
                assert_contains_host();
                f.switch(identity)
            });
            let parent = parent.switch(identity);
            assert_contains_host();
            (parent, drop)
        });
        let fiber = fiber.switch(identity);
        let fiber = fiber.switch(identity);
        fiber.switch(identity);
    }

    look_for_me();
}

#[test]
fn suspend_and_resume_values() {
    let fiber = Fiber::new(move |(parent, first): (Fiber<_>, _)| {
        assert_eq!(first, 2.0);
        let (parent, second) = parent.switch(|f| (f, 4));
        assert_eq!(second, 3.0);
        (parent, |_| "hello".to_string())
    });
    let (fiber, second) = fiber.switch(|f| (f, 2.0));
    assert_eq!(second, 4);
    let third = fiber.switch(|f| (f, 3.0));
    assert_eq!(third, "hello");
}

#[test]
fn stateful() {
    enum Yield {
        Continue(Fiber<Fiber<Yield>>, i32),
        Stop,
    }

    #[allow(dead_code)]
    #[repr(align(128))]
    #[allow(dead_code)]
    struct Aligned(u8);
    let state = [41, 42, 43, 44, 45];
    let aligned = Aligned(100);
    let mut fiber = Fiber::new(move |mut parent: Fiber<Yield>| {
        assert_eq!(&aligned as *const _ as usize % 128, 0);
        for i in state {
            parent = parent.switch(move |f| Yield::Continue(f, i));
        }
        (parent, |_| Yield::Stop)
    });
    for i in state {
        match fiber.switch(identity) {
            Yield::Continue(f, j) => {
                fiber = f;
                assert_eq!(j, i);
            }
            Yield::Stop => panic!("stopped too early"),
        }
    }
    assert!(matches!(fiber.switch(identity), Yield::Stop));
}

// The Windows stack starts out small with only one page comitted. Check that it
// gets properly grown by the kernel as needed.
#[test]
fn stack_growth() {
    let fiber = Fiber::new(|parent| {
        fn recurse(i: u32, p: &mut [u8; 10000]) {
            unsafe {
                // Ensure the stack allocation isn't optimized away.
                ptr::read_volatile(&p);
            }
            if i > 0 {
                recurse(i - 1, &mut [0; 10000]);
            }
        }

        // Use ~500KB of stack.
        recurse(50, &mut [0; 10000]);
        (parent, drop)
    });
    fiber.switch(identity);
}
