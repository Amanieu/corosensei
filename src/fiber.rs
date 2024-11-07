use core::{convert::Infallible, ffi, marker::PhantomData, mem, ptr};

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

// TODO: update docs
/// Create a new `Fiber`.
///
/// Create a fiber by using a default-initialized [`DefaultStack`] instance.
///
/// For more details see [`Fiber::with_stack`].
pub fn fiber<Return>() -> Fiber<(Fiber<Return>, Return)>
where
    Return: 'static,
{
    fiber_with_stack(
        |arg, stack| {
            drop(stack);
            arg
        },
        DefaultStack::default(),
    )
}

// TODO: update docs
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
pub fn fiber_with_stack<Arg, Return, F, Stack>(
    after_exit: F,
    stack: Stack,
) -> Fiber<(Fiber<Return>, Arg)>
where
    F: FnOnce(Arg, Stack) -> Return + 'static,
    Arg: 'static,
    Return: 'static,
    Stack: stack::Stack + 'static,
{
    // SAFETY: TODO
    let exec = unsafe { fiber_unchecked(stack.base()) };
    // Never return from this as this is the "main" function of any `Fiber`
    exec.switch(|execution| {
        // Double panic is good enough to prevent UB from happening.  It would either abort
        // in case unwinding is enabled, halt, or stuck in a loop forever, which is fine
        // because `Fiber`s are `!Send`.
        let _guard = scopeguard::guard((), |()| {
            panic!("fiber panicked, aborting...");
        });

        let (execution, arg): (Fiber<_>, _) = execution.switch(core::convert::identity);
        execution.switch(|_| after_exit(arg, stack))
    })
}

impl<Arg> Fiber<Arg> {
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
                let SwitchPayload { function } = decode_val::<SwitchPayload<F>>(arg);
                let execution = Fiber {
                    sp,
                    _arg: PhantomData,
                    _thread_unsafe: PhantomData,
                    _unwind_unsafe: PhantomData,
                };
                ret.cast::<Arg>().write(function(execution));
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

    /// In-place context switch
    ///
    /// See [`Fiber::switch`] for more information.
    ///
    /// Exploits exclusivity of mutable reference to context switch to a pointed-to fiber.
    /// Such detail basically disallows a fiber to be simply destroyed,
    /// as such [`Fiber::switch`] could be a better alternative in most cases.
    ///
    /// # Unwind
    ///
    /// In case a call of the `convert` closure unwinds, abort is triggered.
    pub fn switch_in_place<YieldBack, Output, F, G>(
        &mut self,
        intermediate: F,
        convert: G,
    ) -> Output
    where
        F: FnOnce(Fiber<YieldBack>) -> Arg + 'static,
        G: FnOnce(YieldBack) -> (Fiber<Arg>, Output),
        Arg: 'static,
    {
        let this = ptr::addr_of_mut!(*self);
        // We ensure the `self` fiber won't be invalid outside of this method,
        // even in case the `convert` closure unwinds.
        let guard = scopeguard::guard((), |()| {
            panic!("convert closure panicked, aborting...");
        });
        let fiber = unsafe { ptr::read(this) };

        let yield_ = fiber.switch(intermediate);
        let (fiber, output) = convert(yield_);

        unsafe { ptr::write(this, fiber) };
        mem::forget(guard);
        output
    }
}
