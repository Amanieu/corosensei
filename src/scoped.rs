use crate::stack;
#[cfg(feature = "default-stack")]
use crate::stack::DefaultStack;
use crate::trap::CoroutineTrapHandler;
use crate::{Coroutine, CoroutineResult, Yielder};
use core::marker::PhantomPinned;
use core::pin::{pin, Pin};

/// Variant of [`Coroutine`] which allows the use of non-`'static` lifetimes.
///
/// Specifically this allows:
/// - A borrowed stack.
/// - Lifetimes in `Input`, `Yield` and `Return`,
/// - Borrowed values in the coroutine body closure.
///
/// To ensure safety, this requires that the coroutine only be accessed within
/// a scope, at the end of which it is dropped. Within the scope, a pinned
/// reference to a `ScopedCoroutine` is provided, which mirrors the API of
/// `Coroutine`.
///
/// # Example
///
/// ```
/// use corosensei::{ScopedCoroutine, Yielder};
///
/// let mut counter = 0;
/// let body = |yielder: &Yielder<i32, ()>, input| {
///     if input == 0 {
///         return;
///     }
///     counter += input;
///     loop {
///         let input = yielder.suspend(());
///         if input == 0 {
///             break;
///         }
///         counter += input;
///     }
/// };
/// ScopedCoroutine::new(
///     body,
///     |mut coroutine| {
///         coroutine.as_mut().resume(1);
///         coroutine.as_mut().resume(2);
///         coroutine.as_mut().resume(3);
///         coroutine.as_mut().resume(4);
///         coroutine.as_mut().resume(5);
///     }
/// );
/// assert_eq!(counter, 15);
/// ```
#[cfg(not(feature = "default-stack"))]
pub struct ScopedCoroutine<Input, Yield, Return, Stack: stack::Stack> {
    inner: Coroutine<Input, Yield, Return, Stack>,
    _marker: PhantomPinned,
}

/// Variant of [`Coroutine`] which allows the use of non-`'static` lifetimes.
///
/// Specifically this allows:
/// - A borrowed stack.
/// - Lifetimes in `Input`, `Yield` and `Return`,
/// - Borrowed values in the coroutine body closure.
///
/// To ensure safety, this requires that the coroutine only be accessed within
/// a scope, at the end of which it is dropped. Within the scope, a pinned
/// reference to a `ScopedCoroutine` is provided, which mirrors the API of
/// `Coroutine`.
///
/// # Example
///
/// ```
/// use corosensei::{ScopedCoroutine, Yielder};
///
/// let mut counter = 0;
/// let body = |yielder: &Yielder<i32, ()>, input| {
///     if input == 0 {
///         return;
///     }
///     counter += input;
///     loop {
///         let input = yielder.suspend(());
///         if input == 0 {
///             break;
///         }
///         counter += input;
///     }
/// };
/// ScopedCoroutine::new(
///     body,
///     |mut coroutine| {
///         coroutine.as_mut().resume(1);
///         coroutine.as_mut().resume(2);
///         coroutine.as_mut().resume(3);
///         coroutine.as_mut().resume(4);
///         coroutine.as_mut().resume(5);
///     }
/// );
/// assert_eq!(counter, 15);
/// ```
#[cfg(feature = "default-stack")]
pub struct ScopedCoroutine<Input, Yield, Return, Stack: stack::Stack = DefaultStack> {
    inner: Coroutine<Input, Yield, Return, Stack>,
    _marker: PhantomPinned,
}

#[cfg(feature = "default-stack")]
impl<Input, Yield, Return> ScopedCoroutine<Input, Yield, Return, DefaultStack> {
    /// Creates a new coroutine which will execute `func` on the given stack.
    ///
    /// This function creates a coroutine which, when resumed, will execute
    /// `body` to completion. When desired the `func` can suspend itself via
    /// [`Yielder::suspend`].
    ///
    /// The coroutine is passed as a `Pin<&mut ScopedCoroutine>` to `scope` and
    /// dropped at the end of the scope.
    pub fn new<F, S, T>(body: F, scope: S) -> T
    where
        F: FnOnce(&Yielder<Input, Yield>, Input) -> Return,
        S: FnOnce(Pin<&mut Self>) -> T,
    {
        Self::with_stack(Default::default(), body, scope)
    }
}

impl<Input, Yield, Return, Stack: stack::Stack> ScopedCoroutine<Input, Yield, Return, Stack> {
    /// Creates a new coroutine which will execute `func` on the given stack.
    ///
    /// This function creates a coroutine which, when resumed, will execute
    /// `body` to completion. When desired the `func` can suspend itself via
    /// [`Yielder::suspend`].
    ///
    /// The coroutine is passed as a `Pin<&mut ScopedCoroutine>` to `scope` and
    /// dropped at the end of the scope.
    pub fn with_stack<F, S, T>(stack: Stack, body: F, scope: S) -> T
    where
        F: FnOnce(&Yielder<Input, Yield>, Input) -> Return,
        S: FnOnce(Pin<&mut Self>) -> T,
    {
        let coroutine = pin!(Self {
            inner: unsafe { Coroutine::with_stack_unchecked(stack, body) },
            _marker: PhantomPinned,
        });
        scope(coroutine)
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
