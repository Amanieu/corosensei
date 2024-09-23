use core::{ffi, marker::PhantomData, mem};

use crate::{
    arch,
    stack::{self, DefaultStack, StackPointer},
    unwind::fiber_func_abi,
    util::{decode_val, encode_val, EncodedValue},
};

struct InitialPayload<F, Stack> {
    function: F,
    stack: Stack,
}

struct SwitchPayload<F> {
    function: F,
}

pub struct Execution<Yield> {
    sp: StackPointer,
    _arg: PhantomData<fn(fn(Self) -> Yield)>,
}

impl<Yield> Execution<Yield> {
    #[cfg(feature = "default-stack")]
    pub fn new<F, Q, YieldBack, Return>(f: F) -> Self
    where
        F: FnOnce(Execution<YieldBack>) -> (Execution<Return>, Q) + 'static,
        Q: FnOnce(DefaultStack) -> Return + 'static,
        Return: 'static,
        Yield: 'static,
        YieldBack: 'static,
    {
        Execution::with_stack(f, DefaultStack::default())
    }

    pub fn with_stack<F, Q, Return, YieldBack, Stack>(f: F, stack: Stack) -> Self
    where
        F: FnOnce(Execution<YieldBack>) -> (Execution<Return>, Q) + 'static,
        Q: FnOnce(Stack) -> Return + 'static,
        Return: 'static,
        Yield: 'static,
        YieldBack: 'static,
        Stack: stack::Stack + 'static,
    {
        fiber_func_abi! {
            unsafe fn entry<F, Q, Return, Stack, YieldBack>(sp: StackPointer, arg: EncodedValue) -> !
            where
                F: FnOnce(Execution<YieldBack>) -> (Execution<Return>, Q) + 'static,
                Q: FnOnce(Stack) -> Return + 'static,
                Return: 'static,
                YieldBack: 'static,
                Stack: stack::Stack + 'static,
            {
                let _guard = scopeguard::guard((), |()| {
                    panic!("execution panicked, aborting...");
                });

                let InitialPayload { function, stack } = decode_val::<InitialPayload<F, Stack>>(arg);
                let execution = Execution::<()> {
                    sp,
                    _arg: PhantomData,
                };

                let execution = execution.switch(|_| ());
                let (execution, exit) = function(execution);
                let () = execution.switch(|_| exit(stack));
                // Stack is no longer used
                unreachable!("switched back to the finished execution")
            }
        }
        let sp = stack.base();
        let mut payload = mem::ManuallyDrop::new(InitialPayload { function: f, stack });
        let arg = unsafe { encode_val(&mut payload) };
        let arch::Yield { sp, val: _ } =
            unsafe { arch::fiber_init(sp, arg, entry::<F, Q, Return, Stack, YieldBack>) };
        Execution {
            sp,
            _arg: PhantomData,
        }
    }

    pub fn switch<YieldBack, F>(self, f: F) -> YieldBack
    where
        F: FnOnce(Execution<YieldBack>) -> Yield + 'static,
    {
        fiber_func_abi! {
            unsafe fn switcheroo<F, Yield, NewYield>(sp: StackPointer, arg: EncodedValue, ret: *mut ffi::c_void)
            where
                F: FnOnce(Execution<NewYield>) -> Yield + 'static,
            {
                let _guard = scopeguard::guard((), |()| {
                    panic!("execution panicked, aborting...");
                });
                let SwitchPayload { function } = decode_val::<SwitchPayload<F>>(arg);
                let execution = Execution {
                    sp,
                    _arg: PhantomData,
                };
                ret.cast::<Yield>().write(function(execution));
            }
        }

        let mut output = mem::MaybeUninit::<YieldBack>::uninit();
        let Execution { sp, .. } = self;
        let mut payload = mem::ManuallyDrop::new(SwitchPayload { function: f });
        unsafe {
            let arg = encode_val(&mut payload);
            arch::fiber_switch(
                sp,
                arg,
                switcheroo::<F, Yield, YieldBack>,
                output.as_mut_ptr().cast::<ffi::c_void>(),
            );
            output.assume_init()
        }
    }
}
