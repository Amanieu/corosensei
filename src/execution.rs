use core::{ffi, marker::PhantomData, mem};

use crate::{
    arch,
    stack::{self, DefaultStack, StackPointer},
    unwind::fiber_init_func_abi,
    util::{decode_val, encode_val, EncodedValue},
};

struct SwitchPayload<F> {
    function: F,
}

pub struct Execution<Yield> {
    sp: StackPointer,
    _arg: PhantomData<fn(fn(Self) -> Yield)>,
}

impl<Arg> Execution<Arg> {
    #[cfg(feature = "default-stack")]
    pub fn new<F, Q, Return>(f: F) -> Self
    where
        F: FnOnce(Arg) -> (Execution<Return>, Q) + 'static,
        Q: FnOnce(DefaultStack) -> Return + 'static,
        Return: 'static,
        Arg: 'static,
    {
        Execution::with_stack(f, DefaultStack::default())
    }

    pub fn with_stack<F, Q, Return, Stack>(f: F, stack: Stack) -> Self
    where
        F: FnOnce(Arg) -> (Execution<Return>, Q) + 'static,
        Q: FnOnce(Stack) -> Return + 'static,
        Return: 'static,
        Arg: 'static,
        Stack: stack::Stack + 'static,
    {
        let sp = unsafe { arch::fiber_init_stack(&stack) };
        let exec = Execution {
            sp,
            _arg: PhantomData,
        };
        // Never return from this as this is the "main" function of any `Execution`
        exec.switch(|execution| {
            let _guard = scopeguard::guard((), |()| {
                panic!("execution panicked, aborting...");
            });

            let execution = execution.switch(core::convert::identity);
            let (execution, exit) = f(execution);
            let () = execution.switch(|_| exit(stack));
            // Stack is no longer used, but we do not want to return
            unreachable!("switched back to the finished execution")
        })
    }

    pub fn switch<YieldBack, F>(self, f: F) -> YieldBack
    where
        F: FnOnce(Execution<YieldBack>) -> Arg + 'static,
    {
        // fiber_func_abi! {
        // TODO: unwind support
        unsafe extern "sysv64" fn switcheroo<F, Yield, NewYield>(
            sp: StackPointer,
            arg: EncodedValue,
            ret: *mut ffi::c_void,
        ) where
            F: FnOnce(Execution<NewYield>) -> Yield + 'static,
        {
            let guard = scopeguard::guard((), |()| {
                panic!("execution panicked, aborting...");
            });
            let SwitchPayload { function } = decode_val::<SwitchPayload<F>>(arg);
            let execution = Execution {
                sp,
                _arg: PhantomData,
            };
            ret.cast::<Yield>().write(function(execution));
            mem::forget(guard);
        }
        // }

        let mut output = mem::MaybeUninit::<YieldBack>::uninit();
        let Execution { sp, .. } = self;
        let mut payload = mem::ManuallyDrop::new(SwitchPayload { function: f });
        unsafe {
            let arg = encode_val(&mut payload);
            arch::fiber_switch(
                sp,
                arg,
                output.as_mut_ptr().cast::<ffi::c_void>(),
                switcheroo::<F, Arg, YieldBack>,
            );
            output.assume_init()
        }
    }
}
