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

pub struct Fiber<Yield> {
    sp: StackPointer,
    _arg: PhantomData<fn(fn(Self) -> Yield)>,
    _thread_unsafe: PhantomData<*mut ()>,
    _unwind_unsafe: PhantomData<&'static mut ()>,
}

impl<Arg> Fiber<Arg> {
    /// Create a new `Fiber`.
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

    /// Create a new `Fiber` with some specified `stack`.
    ///
    /// Takes `stack` and initializes it to be ready for the initial switch, but doesn't call the `f` closure argument yet.
    /// Closure `f` is expected to return a pair of two objects: an `Fiber` to switch to after `f` returns, and a closure of type `Q` that is run on that `Fiber`'s stack.
    /// The `Q` closure converts previously used stack into some payload of type `Return`.
    /// It has similar purpose as the input closure of [`Fiber::switch`], see its documentation for more details.
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

    pub fn switch<YieldBack, F>(self, f: F) -> YieldBack
    where
        F: FnOnce(Fiber<YieldBack>) -> Arg + 'static,
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
        let mut payload = mem::ManuallyDrop::new(SwitchPayload { function: f });
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
