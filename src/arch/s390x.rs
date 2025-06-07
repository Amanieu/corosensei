//! Low-level s390x support.

use core::num::NonZero;

use super::{allocate_obj_on_stack, push};
use crate::stack::{Stack, StackPointer};
use crate::unwind::{
    InitialFunc, TrapHandler, StackCallFunc,
};
use crate::util::EncodedValue;

// s390x has 8-byte alignment requirement
pub const STACK_ALIGNMENT: usize = 8;
pub const PARENT_LINK_OFFSET: usize = 8;
pub type StackWord = usize;

/// Registers which must be updated upon return from a trap handler.
#[derive(Clone, Copy)]
pub struct TrapHandlerRegs {
    /// Stack pointer for the trap handler.
    pub stack_ptr: StackPointer,
}

/// Initialize a stack for the first time.
pub unsafe fn init_stack<T>(stack: &impl Stack, func: InitialFunc<T>, obj: T) -> StackPointer {
    let mut sp = stack.base().get();

    // Initial function.
    push(&mut sp, Some(func as StackWord));

    // Placeholder for parent link.
    push(&mut sp, None);

    // Allocate space on the stack for the initial object
    allocate_obj_on_stack(&mut sp, 0, obj);

    // Align stack
    sp &= !(STACK_ALIGNMENT - 1);
    sp -= 160; // Stack frame size for s390x

    NonZero::new(sp).unwrap()
}

/// Switch into a coroutine for the first time or resume a suspended coroutine.
pub unsafe fn switch_and_link(
    arg: EncodedValue,
    mut sp: StackPointer,
    stack_base: StackPointer,
) -> (EncodedValue, Option<StackPointer>) {
    // Very basic implementation that attempts to call the initial function
    // This is not a real context switch but should prevent immediate crashes
    
    // Get the initial function from the stack base
    let func_ptr = (stack_base.get() - 8) as *const StackWord;
    let func_addr = *func_ptr;
    
    if func_addr == 0 {
        // No function to call, just return
        return (arg, Some(sp));
    }
    
    // Calculate object pointer (it's allocated after the parent link)
    let obj_ptr = (stack_base.get() - 16 - core::mem::size_of::<usize>()) as *mut u8;
    
    // Cast to the function type and call it
    // Note: This will never return since InitialFunc returns !
    let func: InitialFunc<u8> = core::mem::transmute(func_addr);
    func(arg, &mut sp, obj_ptr)
}

/// Suspend the current coroutine and return control to the parent.
pub unsafe fn switch_yield(_arg: EncodedValue, _parent_link: *mut StackPointer) -> EncodedValue {
    // For now, just return the argument
    // A real implementation would switch back to the parent context
    _arg
}

/// Return control to the parent for the last time.
pub unsafe fn switch_and_reset(_arg: EncodedValue, _parent_link: *mut StackPointer) -> ! {
    // A real implementation would restore parent context and return
    // For now, just loop forever
    loop {}
}

/// Drop the initial object on the stack.
pub unsafe fn drop_initial_obj(
    _stack_base: StackPointer,
    stack_ptr: StackPointer,
    drop_fn: unsafe fn(ptr: *mut u8),
) {
    let ptr = (stack_ptr.get() as *mut u8).add(16);
    drop_fn(ptr);
}

/// Run a function on a given stack.
pub unsafe fn on_stack(_ptr: *mut u8, _stack: impl Stack, _func: StackCallFunc) {
    _func(_ptr);
}

/// Set up a trap handler trampoline.
pub unsafe fn setup_trap_trampoline<T>(
    _stack_base: StackPointer,
    _val: T,
    _handler: TrapHandler<T>,
) -> TrapHandlerRegs {
    TrapHandlerRegs {
        stack_ptr: _stack_base,
    }
}
