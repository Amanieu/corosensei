use core::{marker::PhantomData, mem::ManuallyDrop};

use crate::{arch::{fiber_exit, fiber_init, fiber_switch}, stack::{DefaultStack, Stack, StackPointer}, unwind::initial_func_abi, util::encode_val};

trait Session {
    type Argument;
    type Successor;
}

pub struct Chain<A, S> {
    
}

struct InitialPayload<F, Stack> {
    function: F,
    stack: Stack,
}

struct Execution<Arg, Stack> {
    sp: StackPointer,
    _arg: PhantomData<fn(Arg) -> Arg>,
    _stack: PhantomData<Stack>,
}

impl<Arg, Stack> Execution<Arg, Stack> {
    pub fn new<F>(f: F, arg: Arg) -> Self
    where
        F: FnOnce(Execution<Arg>) -> Execution<Arg> + 'static,
        T: Default + 'static,
    {
        let stack = DefaultStack::default();
        initial_func_abi!(unsafe fn entry() {});
        unsafe { fiber_init(stack.base(), , entry) }
    }

    pub fn switch(self, arg: T) -> (Option<Execution<T>>, T) {
        fiber_switch(, )
    }

    fn exit(self, arg: T) -> ! {
        let mut arg = ManuallyDrop::new(arg);
        let arg = unsafe { encode_val(&mut arg) };
        unsafe { fiber_exit(self.sp, arg) }
    }
}
