use core::{convert::Infallible, ffi, marker::PhantomData, mem};

use crate::{
    arch,
    stack::{self, DefaultStack, StackPointer},
    unwind::fiber_switch_func_abi,
    util::{decode_val, encode_val, EncodedValue},
};

struct SwitchPayload<F> {
    function: F,
}

/// A suspended execution
///
/// This type represents a lightweight thread of execution or a [fiber] in its suspended state.
/// Every fiber logically owns a [program stack] it uses during the execution.
/// However dropping a `Fiber` object simply leaks.
/// The only way to free its resources such as its program stack is for call of the closure it was initialized with to return.
/// As such any running execution context can be converted into a `Fiber` (see [`Fiber::switch`]), even if its the [main function].
///
/// These restrictions might seem limiting due to this design trying to be minimalistic so that any reasonable functionallity can be added after using safe Rust.
///
/// [fiber]: https://en.wikipedia.org/wiki/Fiber_(computer_science)
/// [program stack]: https://en.wikipedia.org/wiki/Call_stack
/// [main function]: https://en.wikipedia.org/wiki/Entry_point
pub struct Fiber<Yield> {
    sp: StackPointer,
    _arg: PhantomData<fn(fn(Self) -> Yield)>,
    _thread_unsafe: PhantomData<*mut ()>,
    _unwind_unsafe: PhantomData<&'static mut ()>,
}

impl<Arg> Fiber<Arg> {
    /// Create a new `Fiber`.
    ///
    /// Create a fiber by using a default-initialized [`DefaultStack`] instance.
    ///
    /// For more details see [`Fiber::with_stack`].
    #[cfg(feature = "default-stack")]
    pub fn new<F, Q, Return>(f: F) -> Self
    where
        F: FnOnce(Arg) -> (Fiber<Return>, Q) + 'static,
        Q: FnOnce(DefaultStack) -> Return + 'static,
        Return: 'static,
        Arg: 'static,
    {
        Fiber::with_stack(f, DefaultStack::default())
    }

    /// Create a new `Fiber` with some specified `stack`
    ///
    /// Initializes the stack to be ready for the initial switch, but doesn't call the `f` closure argument yet.
    /// Closure `f` is expected to return a pair of two objects: an `Fiber` to switch to after `f` returns, and a closure of type `Q` that is run on that `Fiber`'s stack.
    /// The `Q` closure converts previously used stack into some payload of type `Return`.
    /// It has similar purpose as the input closure of [`Fiber::switch`], see its documentation for more details.
    ///
    /// # Unwinding
    ///
    /// If call to `f` ever panics or unwinds then the entire process is aborted.
    pub fn with_stack<F, Q, Return, Stack>(f: F, stack: Stack) -> Self
    where
        F: FnOnce(Arg) -> (Fiber<Return>, Q) + 'static,
        Q: FnOnce(Stack) -> Return + 'static,
        Return: 'static,
        Arg: 'static,
        Stack: stack::Stack + 'static,
    {
        let sp = unsafe { arch::fiber_init_stack(&stack) };
        let exec = Fiber::<Infallible> {
            sp,
            _arg: PhantomData,
            _thread_unsafe: PhantomData,
            _unwind_unsafe: PhantomData,
        };
        // Never return from this as this is the "main" function of any `Fiber`
        exec.switch(|execution| {
            let _guard = scopeguard::guard((), |()| {
                panic!("execution panicked, aborting...");
            });

            let input = execution.switch(core::convert::identity);
            let (execution, exit) = f(input);
            let () = execution.switch(|_| exit(stack));
            // Stack is no longer used, but we do not want to return
            unreachable!("switched back to the finished execution")
        })
    }

    /// Context switch
    ///
    /// Switch or jump to a different `Fiber`. Suspends the current execution, then resumes `self` fiber and calls the `intermediate` closure, while passing previous execution as another `Fiber`.
    /// The `YieldBack` generic type argument
    ///
    /// The `intermediate` closure in intended to convert previous fiber into the custom `Arg` type, allowing the use of caller's generic types during it.
    /// Call to the `intermediate` closure happens on the `self` fiber instead of within the current execution, just because closure's `Fiber` argument must already represent this execution in the suspended state.
    ///
    /// # Unwind
    ///
    /// If call to `f` ever panics or unwinds by other means, then the process is aborted.
    pub fn switch<YieldBack, F>(self, intermediate: F) -> YieldBack
    where
        F: FnOnce(Fiber<YieldBack>) -> Arg + 'static,
        Arg: 'static,
    {
        fiber_switch_func_abi! {
            unsafe fn switcheroo<Arg, YieldBack, F>(
                sp: StackPointer,
                arg: EncodedValue,
                ret: *mut ffi::c_void,
            ) where
                F: FnOnce(Fiber<YieldBack>) -> Arg + 'static,
            {
                // TODO: unwind support?
                let guard = scopeguard::guard((), |()| {
                    panic!("execution panicked, aborting...");
                });
                let SwitchPayload { function } = decode_val::<SwitchPayload<F>>(arg);
                let execution = Fiber {
                    sp,
                    _arg: PhantomData,
                    _thread_unsafe: PhantomData,
                    _unwind_unsafe: PhantomData,
                };
                ret.cast::<Arg>().write(function(execution));
                mem::forget(guard);
            }
        }

        let mut output = mem::MaybeUninit::<YieldBack>::uninit();
        let Fiber { sp, .. } = self;
        let mut payload = mem::ManuallyDrop::new(SwitchPayload {
            function: intermediate,
        });
        unsafe {
            let arg = encode_val(&mut payload);
            arch::fiber_switch(
                sp,
                arg,
                output.as_mut_ptr().cast::<ffi::c_void>(),
                switcheroo::<Arg, YieldBack, F>,
            );
            output.assume_init()
        }
    }
}
