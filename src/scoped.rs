#[cfg(feature = "default-stack")]
use crate::stack::DefaultStack;
use crate::Yielder;
use crate::{stack, trap::CoroutineTrapHandler};
use crate::{Coroutine, CoroutineResult};
use core::pin::Pin;

/// Variant of [`Coroutine`] which allows the use of non-`'static` lifetimes.
///
/// Specifically this allows:
/// - A borrowed stack.
/// - Lifetimes in `Input`, `Yield` and `Return`,
/// - Borrowed values in the coroutine body closure.
///
/// However this requires that the coroutine itself be pinned to ensure that it
/// is properly dropped before any of these lifetimes end. The easiest way to
/// do so it to use the [`pin!`] macro when constructing the coroutine
///
/// [`pin!`]: core::pin::pin
#[cfg(not(feature = "default-stack"))]
pub struct ScopedCoroutine<Input, Yield, Return, Stack: stack::Stack> {
    inner: Coroutine<Input, Yield, Return, Stack>,
}

/// Variant of [`Coroutine`] which allows the use of non-`'static` lifetimes.
///
/// Specifically this allows:
/// - A borrowed stack.
/// - Lifetimes in `Input`, `Yield` and `Return`,
/// - Borrowed values in the coroutine body closure.
///
/// However this requires that the coroutine itself be pinned to ensure that it
/// is properly dropped before any of these lifetimes end. The easiest way to
/// do so it to use the [`pin!`] macro when constructing the coroutine
///
/// [`pin!`]: core::pin::pin
#[cfg(feature = "default-stack")]
pub struct ScopedCoroutine<Input, Yield, Return, Stack: stack::Stack = DefaultStack> {
    inner: Coroutine<Input, Yield, Return, Stack>,
}

#[cfg(feature = "default-stack")]
impl<Input, Yield, Return> ScopedCoroutine<Input, Yield, Return, DefaultStack> {
    /// Creates a new coroutine which will execute `func` on a new stack.
    ///
    /// This function returns a `ScopedCoroutine` which, when resumed, will
    /// execute `func` to completion. When desired the `func` can suspend
    /// itself via `Yielder::suspend`.
    pub fn new<F>(f: F) -> Self
    where
        F: FnOnce(&Yielder<Input, Yield>, Input) -> Return,
    {
        Self::with_stack(Default::default(), f)
    }
}

impl<Input, Yield, Return, Stack: stack::Stack> ScopedCoroutine<Input, Yield, Return, Stack> {
    /// Creates a new coroutine which will execute `func` on the given stack.
    ///
    /// This function returns a coroutine which, when resumed, will execute
    /// `func` to completion. When desired the `func` can suspend itself via
    /// [`Yielder::suspend`].
    pub fn with_stack<F>(stack: Stack, f: F) -> Self
    where
        F: FnOnce(&Yielder<Input, Yield>, Input) -> Return,
    {
        unsafe {
            Self {
                inner: Coroutine::with_stack_unchecked(stack, f),
            }
        }
    }

    /// Resumes execution of this coroutine.
    ///
    /// This function will transfer execution to the coroutine and resume from
    /// where it last left off.
    ///
    /// If the coroutine calls [`Yielder::suspend`] then this function returns
    /// [`CoroutineResult::Yield`] with the value passed to `suspend`.
    ///
    /// If the coroutine returns then this function returns
    /// [`CoroutineResult::Return`] with the return value of the coroutine.
    ///
    /// # Panics
    ///
    /// Panics if the coroutine has already finished executing.
    ///
    /// If the coroutine itself panics during execution then the panic will be
    /// propagated to this caller.
    pub fn resume(self: Pin<&mut Self>, val: Input) -> CoroutineResult<Yield, Return> {
        unsafe { self.get_unchecked_mut().inner.resume(val) }
    }

    /// Returns whether this coroutine has been resumed at least once.
    pub fn started(&self) -> bool {
        self.inner.started()
    }

    /// Returns whether this coroutine has finished executing.
    ///
    /// A coroutine that has returned from its initial function can no longer
    /// be resumed.
    pub fn done(&self) -> bool {
        self.inner.done()
    }

    /// Forcibly marks the coroutine as having completed, even if it is
    /// currently suspended in the middle of a function.
    ///
    /// # Safety
    ///
    /// This is equivalent to a `longjmp` all the way back to the initial
    /// function of the coroutine, so the same rules apply.
    ///
    /// This can only be done safely if there are no objects currently on the
    /// coroutine's stack that need to execute `Drop` code.
    pub unsafe fn force_reset(self: Pin<&mut Self>) {
        unsafe { self.get_unchecked_mut().inner.force_reset() }
    }

    /// Unwinds the coroutine stack, dropping any live objects that are
    /// currently on the stack. This is automatically called when the coroutine
    /// is dropped.
    ///
    /// If the coroutine has already completed then this function is a no-op.
    ///
    /// If the coroutine is currently suspended on a `Yielder::suspend` call
    /// then unwinding it requires the `unwind` feature to be enabled and
    /// for the crate to be compiled with `-C panic=unwind`.
    ///
    /// # Panics
    ///
    /// This function panics if the coroutine could not be fully unwound. This
    /// can happen for one of two reasons:
    /// - The `ForcedUnwind` panic that is used internally was caught and not
    ///   rethrown.
    /// - This crate was compiled without the `unwind` feature and the
    ///   coroutine is currently suspended in the yielder (`started && !done`).
    pub fn force_unwind(self: Pin<&mut Self>) {
        unsafe { self.get_unchecked_mut().inner.force_unwind() }
    }

    /// Extracts the stack from a coroutine that has finished executing.
    ///
    /// This allows the stack to be re-used for another coroutine.
    pub fn into_stack(self) -> Stack {
        self.inner.into_stack()
    }

    /// Returns a [`CoroutineTrapHandler`] which can be used to handle traps that
    /// occur inside the coroutine. Examples of traps that can be handled are
    /// invalid memory accesses and stack overflows.
    ///
    /// The returned [`CoroutineTrapHandler`] can be used in a trap handler to
    /// force the trapping coroutine to return with a specific value, after
    /// which is it considered to have completed and can no longer be resumed.
    ///
    /// Needless to say, this is extremely unsafe and must be used with extreme
    /// care. See [`CoroutineTrapHandler::setup_trap_handler`] for the exact
    /// safety requirements.
    pub fn trap_handler(&self) -> CoroutineTrapHandler<Return> {
        self.inner.trap_handler()
    }
}
