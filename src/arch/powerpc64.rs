//! Low-level PowerPC support.
//!
//! This file is heavily based on the x86_64 implementation.
//! Relevant differences are highlighted in comments, but otherwise most
//! comments have been removed to avoid duplication. Refer to x86_64.rs for
//! detailed comments about what is happening in this file.
//!
//! ## Stack layout
//!
//! Here is what the layout of the stack looks like when a coroutine is
//! suspended.
//!
//! Note: "link area" refers to the minimal 32-byte stack frame header that is
//! required by the PowerPC ABI.
//!
//! ```text
//! +--------------+  <- Stack base
//! | Padding      |  <-
//! +--------------+   |
//! | Initial func |  <-
//! +--------------+   | This mirrors the 32-byte link area layout.
//! | Padding      |  <-
//! +--------------+   |
//! | Parent link  |  <-
//! +--------------+ <- The 4 fields above form a fake link area which
//! |              |    backchains back to the parent stack.
//! ~     ...      ~
//! |              |
//! +--------------+
//! |              |
//! | Link area    |  <- 32-byte link area of the last frame before the suspend.
//! |              |     The LR field holds the PC to resume execution at.
//! +--------------+  <- Saved stack pointer points here.
//! | Saved R31    |
//! +--------------+
//! | Saved R30    |
//! +--------------+
//! | Saved R29    |
//! +--------------+
//! | Saved R2     |
//! +--------------+
//! ```
//!
//! And this is the layout of the parent stack when a coroutine is running:
//!
//! ```text
//! |           |
//! ~    ...    ~
//! |           |
//! +-----------+
//! |           |
//! | Link area |  <- 32-byte link area of the last frame before the suspend.
//! |           |     The LR field holds the PC to resume execution at.
//! +-----------+  <- Parent link points here.
//! | Saved R31 |
//! +-----------+
//! | Saved R30 |
//! +-----------+
//! | Saved R29 |
//! +-----------+
//! | Saved R2  |
//! +-----------+
//! ```
//!
//! And finally, this is the stack layout of a coroutine that has just been
//! initialized:
//!
//! ```text
//! +--------------+  <- Stack base
//! | Padding      |
//! +--------------+
//! | Initial func |
//! +--------------+
//! | Padding      |
//! +--------------+
//! | Parent link  |
//! +--------------+
//! |              |
//! ~ Initial obj  ~
//! |              |
//! +--------------+
//! | Padding      |  <-
//! +--------------+   |
//! | Initial PC   |  <-
//! +--------------+   | This mirrors the 32-byte link area layout.
//! | Padding      |  <-
//! +--------------+   |
//! | Padding      |  <-
//! +--------------+  <- Initial SP points here
//! | Saved R31    |
//! +--------------+
//! | Saved R30    |
//! +--------------+
//! | Saved R29    |
//! +--------------+
//! | Saved R2     |
//! +--------------+  <- stack top
//! ```

use core::arch::{asm, global_asm};

use super::{allocate_obj_on_stack, push};
use crate::coroutine::adjusted_stack_base;
use crate::stack::{Stack, StackPointer};
use crate::unwind::{
    asm_may_unwind_root, asm_may_unwind_yield, cfi_reset_args_size_root, cfi_reset_args_size_yield,
    InitialFunc, StackCallFunc, TrapHandler,
};
use crate::util::EncodedValue;

pub const STACK_ALIGNMENT: usize = 16;
pub const PARENT_STACK_OFFSET: usize = 0;
pub const PARENT_LINK_OFFSET: usize = 32;
pub type StackWord = u64;

// This is a pretty special function that has no real signature. Its use is to
// be the "base" function of all coroutines. This entrypoint is used in
// init_stack() to bootstrap the execution of a new coroutine.
//
// We also use this function as a persistent frame on the stack to emit dwarf
// information to unwind into the caller. This allows us to unwind from the
// coroutines's stack back to the main stack that the coroutine was called from.
// We use special dwarf directives here to do so since this is a pretty
// nonstandard function.
global_asm!(
    ".balign 4",
    asm_function_begin!("stack_init_trampoline"),
    ".cfi_startproc",
    //cfi_signal_frame!(),
    // At this point our register state contains the following:
    // - SP points to the top of the parent stack.
    // - LR contains the return address in the parent context.
    // - R29, R30, and R31 contain their values from the parent context.
    // - R5 points to the top of the initial coroutine stack.
    // - R4 points to the base of the initial coroutine stack.
    // - R3 contains the argument passed from switch_and_link.
    //
    // Save the R2, R29, R30, and R31 of the parent context to the parent stack.
    // When combined with the return address this forms a valid frame record
    // (R2, R29, R30, R31 & LR) in the frame pointer chain.
    "mflr 0",
    // Allocate a frame of 56 bytes (link area + space for R29, R30, and R31).
    "stdu 1, -56(1)",
    "std 31, 48(1)",
    "std 30, 40(1)",
    "std 29, 32(1)",
    // Save R2 and LR in the link area of the parent frame.
    "std 2, 80(1)",
    "std 0, 72(1)",
    // Adjust R4 to point to the parent link and Write the parent stack pointer
    // to the parent link (back chain slot) of the coroutine stack.
    "addi 4, 4, -32",
    "std 1, 0(4)",
    // Switch to the coroutine stack.
    "mr 1, 5",
    // Set up the frame pointer to point at the parent link. This is needed for
    // the unwinding code below.
    "mr 31, 4",
    // The actual meanings of the magic bytes are:
    // 0x0f: DW_CFA_def_cfa_expression
    // 5: byte length of the following DWARF expression
    // 0x8f 0x00: DW_OP_breg31 (31 + 0)
    // 0x06: DW_OP_deref
    // 0x23, 0x38: DW_OP_plus_uconst 56
    ".cfi_escape 0x0f, 5, 0x8f, 0x00, 0x06, 0x23, 0x38",
    // Now we can tell the unwinder how to restore the 3 registers that were
    // pushed on the parent stack. These are described as offsets from the CFA
    // that we just calculated.
    ".cfi_offset r31, -8",
    ".cfi_offset r30, -16",
    ".cfi_offset r29, -24",
    ".cfi_offset r2, 24",
    ".cfi_offset lr, 16",
    // Set up the 3rd argument to the initial function to point to the object
    // that init_stack() set up on the stack.
    "addi 5, 1, 32",
    // As in the original x86_64 code, hand-write the call operation so that it
    // doesn't push an entry into the CPU's return prediction stack.

    // Set the return address in LR.
    // FIXME: Workaround for P9 and earlier that do not have pc-rel instructions.
    "bl 0f",
    "0:",
    "mflr 6",
    "addi 0, 6, 24",
    "mtlr 0",
    // load the initial function to R12 for function linkage.
    "ld 12, 16(4)",
    "mtctr 12",
    "bctr",
    asm_function_alt_entry!("stack_init_trampoline_return"),
    // This 'trap' instruction is necessary because of our use of .cfi_signal_frame earlier.
    "trap",
    ".cfi_endproc",
    asm_function_end!("stack_init_trampoline"),
);

// This function calls a function pointer on a new stack and restores the
// original stack upon returning. It is used by on_stack() and is much simpler
// than the full coroutine logic, but also more limited since yielding is not
// possible.
global_asm!(
    // See stack_init_trampoline for an explanation of the assembler directives
    // used here.
    ".balign 4",
    asm_function_begin!("stack_call_trampoline"),
    ".cfi_startproc",
    cfi_signal_frame!(),
    // At this point our register state contains the following:
    // - SP points to the top of the parent stack.
    // - R2, R29, R30, and R31 hold their value from the parent context.
    // - R5 is the function that should be called.
    // - R4 points to the top of our stack.
    // - R3 contains the argument to be passed to the function.
    //
    // Create a stack frame and point the frame pointer at it.
    "mflr 0",
    "stdu 1, -56(1)",
    "std 31, 48(1)",
    "std 30, 40(1)",
    "std 29, 32(1)",
    // Save R2 and LR in the link area of the parent frame.
    "std 2, 80(1)",
    "std 0, 72(1)",
    ".cfi_def_cfa 1, 0",
    ".cfi_offset r31, 48",
    ".cfi_offset r30, 40",
    ".cfi_offset r29, 32",
    ".cfi_offset r2, 80",
    ".cfi_offset lr, 72",
    // Switch to the new stack.
    "mr 31, 1",
    "mr 1, 4",
    // FIXME: Workaround for P9 and earlier that do not have pc-rel instructions.
    "bl 0f",
    "0:",
    "mflr 6",
    "addi 0, 6, 24",
    "mtlr 0",
    // Call the function pointer. The argument is already in the correct
    // register for the function.
    "mr 12, 5",
    "mtctr 12",
    "bctr",
    // Switch back to the original stack by restoring from the frame pointer,
    // then return.
    "addi 1, 31, 56",
    "ld 31, -8(1)",
    "ld 30, -16(1)",
    "ld 29, -24(1)",
    "ld 2, 24(1)",
    "ld 12, 16(1)",
    "mtlr 12",
    "blr",
    ".cfi_endproc",
    asm_function_end!("stack_call_trampoline"),
);

// These trampolines use a custom calling convention and should only be called
// with inline assembly.
extern "C" {
    fn stack_init_trampoline(arg: EncodedValue, stack_base: StackPointer, stack_ptr: StackPointer);
    static stack_init_trampoline_return: [u8; 0];
    #[allow(dead_code)]
    fn stack_call_trampoline(arg: *mut u8, sp: StackPointer, f: StackCallFunc);
}

/// Sets up the initial state on a stack so that the given function is
/// executed on the first switch to this stack.
///
/// The given object is written to the stack and its address on the stack is
/// passed as the 3rd argument to the initial function.
#[inline]
pub unsafe fn init_stack<T>(stack: &impl Stack, func: InitialFunc<T>, obj: T) -> StackPointer {
    let mut sp = adjusted_stack_base(stack).get();

    // The following 4 slots form a fake 32-byte link area layout.
    push(&mut sp, None);

    // Initial function.
    push(&mut sp, Some(func as StackWord));

    push(&mut sp, None);

    // Placeholder for parent link.
    push(&mut sp, None);

    // Allocate space on the stack for the initial object, rounding to
    // STACK_ALIGNMENT.
    allocate_obj_on_stack(&mut sp, 16, obj);

    // This mirrors the 32-byte link area layout.
    push(&mut sp, None);

    // Set the LR slot with the entry point called by switch_and_link().
    push(&mut sp, Some(stack_init_trampoline as StackWord));

    push(&mut sp, None);
    push(&mut sp, None);

    StackPointer::new_unchecked(sp)
}

/// This function is used to transfer control to a coroutine along with an
/// argument. A pointer back to our context is stored at a fixed offset from
/// the base of the target stack.
///
/// When another context switches back to us, we receive the argument they sent
/// as well as the stack pointer of the originating context. This can be `None`
/// if the caller used `switch_and_reset` and can't be returned to.
#[inline]
pub unsafe fn switch_and_link(
    arg: EncodedValue,
    sp: StackPointer,
    stack_base: StackPointer,
) -> (EncodedValue, Option<StackPointer>) {
    let (ret_val, ret_sp);

    asm_may_unwind_root!(
        // DW_CFA_GNU_args_size 0
        //
        // Indicate to the unwinder that this "call" does not take any arguments
        // and no stack space needs to be popped before executing a landing pad.
        // This is mainly here to undo the effect of any previous
        // DW_CFA_GNU_args_size that may have been set in the current function.
        cfi_reset_args_size_root!(),

        // Read the saved PC from the link area of the coroutine stack and call it.
        "ld 12, 16(5)",
        "mtctr 12",
        "bctrl",
        "nop",

        // Upon returning from switch_yield or switch_and_reset, our register
        // state contains the following:
        // - R5: parant stack pointer.
        // - R4: The top of the coroutine stack, or 0 if coming from
        //       switch_and_reset.
        // - R3: The return value from the coroutine.

        // Switch back to parent stack and free the saved registers.
        "addi 1, 5, 56",

        // Pass the argument in R3.
        inlateout("3") arg => ret_val,

        // We get the coroutine stack pointer back in R4.
        lateout("4") ret_sp,

        // We pass the stack base in R4.
        in("4") stack_base.get() as u64,

        // We pass the target stack pointer in R5.
        in("5") sp.get() as u64,

        // Mark all registers as clobbered.
        lateout("14") _, lateout("15") _, lateout("16") _, lateout("17") _,
        lateout("18") _, lateout("19") _, lateout("20") _, lateout("21") _,
        lateout("22") _, lateout("23") _, lateout("24") _, lateout("25") _,
        lateout("26") _, lateout("27") _, lateout("28") _,
        clobber_abi("C"),
    );

    (ret_val, StackPointer::new(ret_sp))
}

/// This function performs the inverse of `switch_and_link` by returning
/// control to the parent context.
///
/// This function does not return a stack pointer value for the parent context
/// when it switches back to us. Instead, the stack pointer value for the parent
/// context is available in the parent link on the stack.
// This function must always be inlined because it is very sensitive to the
// CPU's return address predictor. See stack_init_trampoline for more details.
//
// FIXME: Inlining this function fails tests.
//#[inline(always)]
pub unsafe fn switch_yield(arg: EncodedValue, parent_link: *mut StackPointer) -> EncodedValue {
    let ret_val;

    asm_may_unwind_yield!(
        // Save R29, R30, and R31. Ideally this would be done by specifying them as
        // clobbers but that is not possible since they are LLVM reserved
        // registers. Also save our R2 and LR.
        "std 31, -8(1)", // Save Back chain
        "std 30, -16(1)",
        "std 29, -24(1)",
        "std 2, 24(1)",
        "stdu 1, -64(1)", // The 32-byte link area and the space for R29, R30, R31.

        // Get the return address.
        // FIXME: Workaroud for P9 and earlier that do not have pc-rel instructions.
        "bl 1f",
        "1:",
        "mflr 6",
        "addi 0, 6, 48",
        // Save return address in the parent frame.
        "std 0, 16(1)",

        // Get the parent stack pointer from the parent link.
        "ld 5, 0(4)",

        // Save our stack pointer to R4.
        "mr 4, 1",

        // Restore R2, R29, R30, R31, and LR from the parent stack.
        "ld 29, 32(5)",
        "ld 30, 40(5)",
        "ld 31, 48(5)",
        "ld 2, 80(5)",
        "ld 12, 72(5)", // Get the LR.
        "mtlr 12",

        // DW_CFA_GNU_args_size 0
        //
        // Indicate to the unwinder that this "call" does not take any arguments
        // and no stack space needs to be popped before executing a landing pad.
        // This is mainly here to undo the effect of any previous
        // DW_CFA_GNU_args_size that may have been set in the current function.
        //
        // This is needed here even though we don't call anything because
        // switch_and_throw may inject a call which returns to this point.
        cfi_reset_args_size_yield!(),

        // Return into the parent context
        "blr",
        "nop",

        // This gets called by switch_and_link(). At this point our register
        // state contains the following:
        // - SP points to the top of the parent stack.
        // - LR contains the return address in the parent context.
        // - R2, R29, R30, and R31 contain their value from the parent context.
        // - R5 points to the top of the coroutine stack.
        // - R4 points to the base of our stack.
        // - R3 contains the argument passed from switch_and_link.
        "0:",

        // Push the R2, R29, R30, R31, and LR values of the parent context onto
        // the parent stack.
        "mflr 0",
        "addi 1, 1, -56",
        "std 31, 48(1)",
        "std 30, 40(1)",
        "std 29, 32(1)",
        "std 2, 80(1)",
        "std 0, 72(1)",

        // Write the parent stack pointer to the parent link.
        "std 1, -32(4)",

        // Switch to the coroutine stack while popping the saved registers.
        "addi 1, 5, 64",

        // Load our R2, R29, R30, and R31 values from the coroutine stack.
        "ld 31, -8(1)",
        "ld 30, -16(1)",
        "ld 29, -24(1)",
        "ld 2, 24(1)",

        // Pass the argument in R3.
        inlateout("3") arg => ret_val,

        // The parent link can be in any register, R5 is arbitrarily chosen
        // here.
        in("5") parent_link as u64,

        // See switch_and_link() for an explanation of the clobbers.
        lateout("14") _, lateout("15") _, lateout("16") _, lateout("17") _,
        lateout("18") _, lateout("19") _, lateout("20") _, lateout("21") _,
        lateout("22") _, lateout("23") _, lateout("24") _, lateout("25") _,
        lateout("26") _, lateout("27") _, lateout("28") _,
        clobber_abi("C"),
    );

    ret_val
}

/// Variant of `switch_yield` used when returning from the initial function in a
/// context.
///
/// This works by returning a stack pointer value of 0 which prevents the
/// current context from being resumed. There must not be any object left on the
/// stack with pending destructors when this is called.
///
/// Since the stack is still available at this point, `arg` can safely point to
/// memory on the stack until the parent context frees or reuses the stack.
// This function must always be inlined because it is very sensitive to the
// CPU's return address predictor. See stack_init_trampoline for more details.
#[inline(always)]
pub unsafe fn switch_and_reset(arg: EncodedValue, parent_link: *mut StackPointer) -> ! {
    // Most of this code is identical to switch_yield(), refer to the
    // comments there. Only the differences are commented.
    asm!(
        // Load the parent context's stack pointer.
        "ld 5, 0({parent_link})",

        // Restore R2, R29, R30, R31, and LR from the parent stack.
        "ld 29, 32(5)",
        "ld 30, 40(5)",
        "ld 31, 48(5)",
        "ld 2, 80(5)",
        "ld 12, 72(5)",
        "mtlr 12",

        // Return into the parent context
        "blr",

        parent_link = in(reg) parent_link as u64,

        in("3") arg,

        // Hard-code the returned stack pointer value to 0 to indicate that this
        // coroutine is done.
        in("4") 0,

        options(noreturn),
    )
}

/// Variant of `switch_and_link` which runs a function on the coroutine stack
/// instead of resuming the coroutine. This function will throw an exception
/// which will unwind the coroutine stack to its root.
#[inline]
#[cfg(feature = "asm-unwind")]
pub unsafe fn switch_and_throw(
    forced_unwind: crate::unwind::ForcedUnwind,
    sp: StackPointer,
    stack_base: StackPointer,
) -> (EncodedValue, Option<StackPointer>) {
    extern "C-unwind" fn throw(forced_unwind: crate::unwind::ForcedUnwind) -> ! {
        extern crate std;
        use std::boxed::Box;
        std::panic::resume_unwind(Box::new(forced_unwind));
    }

    let (ret_val, ret_sp);

    asm_may_unwind_root!(
        // Set up a return address.
        // FIXME: Workaroud for P9 and earlier that do not have pc-rel instructions.
        "bl 0f",
        "0:",
        "mflr 6",
        "addi 0, 6, 72",

        // Save the parent context onto the parent stack.
        "mflr 0",
        "stdu 1, -56(1)",
        "std 31, 48(1)",
        "std 30, 40(1)",
        "std 29, 32(1)",
        "std 2, 80(1)",
        "std 0, 72(1)",

        // Write the parent stack pointer to the parent link.
        "std 1, -32(4)",

        // Switch to the coroutine stack while popping the saved registers.
        "addi 1, 5, 64",

        // Load the coroutine registers, with the saved LR value into LR.
        "ld 31, -8(1)",
        "ld 30, -16(1)",
        "ld 29, -24(1)",
        "ld 2, 24(1)",
        "ld 0, 16(1)",
        "mtlr 0",

        // DW_CFA_GNU_args_size 0
        //
        // Indicate to the unwinder that this "call" does not take any arguments
        // and no stack space needs to be popped before executing a landing pad.
        // This is mainly here to undo the effect of any previous
        // DW_CFA_GNU_args_size that may have been set in the current function.
        cfi_reset_args_size_root!(),

        // Simulate a call with an artificial return address so that the throw
        // function will unwind straight into the switch_and_yield() call with
        // the register state expected outside the asm! block.
        "b {throw}",

        // Upon returning, our register state is just like a normal return into
        // switch_and_link().
        "0:",

        // Switch back to our stack and free the saved registers.
        "addi 1, 5, 56",

        // Helper function to trigger stack unwinding.
        throw = sym throw,

        // Argument to pass to the throw function.
        in("3") forced_unwind.0.get(),

        // Same output registers as switch_and_link().
        lateout("3") ret_val,
        lateout("4") ret_sp,

        // Stack pointer and stack base inputs for stack switching.
        in("4") stack_base.get() as u64,
        in("5") sp.get() as u64,

        // See switch_and_link() for an explanation of the clobbers.
        lateout("14") _, lateout("15") _, lateout("16") _, lateout("17") _,
        lateout("18") _, lateout("19") _, lateout("20") _, lateout("21") _,
        lateout("22") _, lateout("23") _, lateout("24") _, lateout("25") _,
        lateout("26") _, lateout("27") _, lateout("28") _,
        clobber_abi("C"),
    );

    (ret_val, StackPointer::new(ret_sp))
}

#[inline]
pub unsafe fn drop_initial_obj(
    _stack_base: StackPointer,
    stack_ptr: StackPointer,
    drop_fn: unsafe fn(ptr: *mut u8),
) {
    let ptr = (stack_ptr.get() as *mut u8).add(32);
    drop_fn(ptr);
}

/// Registers which must be updated upon return from a trap handler.
///
/// The exact set of registers that need to be updated varies depending on the
/// target. Note that *all* registers must be updated to the specified values,
/// otherwise behavior is undefined.
///
/// To catch any issues at compilation time, it is recommended to use Rust's
/// pattern matching syntax to extract the individual registers from this
/// struct.
///
/// ```
/// # use corosensei::trap::TrapHandlerRegs;
/// # let regs = TrapHandlerRegs { pc: 0, sp: 0, r3: 0, r4: 0, r31: 0, lr: 0 };
/// let TrapHandlerRegs { pc, sp, r3, r4, r31, lr } = regs;
/// ```
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub struct TrapHandlerRegs {
    pub pc: u64,
    pub sp: u64,
    pub r3: u64,
    pub r4: u64,
    pub r31: u64,
    pub lr: u64,
}

pub unsafe fn setup_trap_trampoline<T>(
    stack_base: StackPointer,
    val: T,
    handler: TrapHandler<T>,
) -> TrapHandlerRegs {
    // Preserve the top 16 bytes of the stack since they contain the parent
    // link.
    let parent_link = stack_base.get() - PARENT_LINK_OFFSET;

    // Everything below this can be overwritten. Write the object to the stack.
    let mut sp = parent_link;
    allocate_obj_on_stack(&mut sp, 16, val);

    // Space for 32 bytes link area.
    push(&mut sp, None);
    push(&mut sp, None);
    push(&mut sp, None);
    push(&mut sp, None);

    let val_ptr = sp;

    // Set up registers for entry into the function.
    TrapHandlerRegs {
        pc: handler as u64,
        sp: sp as u64,
        r3: val_ptr as u64,
        r4: parent_link as u64,
        r31: parent_link as u64,
        lr: stack_init_trampoline_return.as_ptr() as u64,
    }
}

/// This function executes a function on the given stack. The argument is passed
/// through to the called function.
#[inline]
pub unsafe fn on_stack(arg: *mut u8, stack: impl Stack, f: StackCallFunc) {
    // This is a bit subtle: because we use .cfi_signal_frame in the trampoline,
    // the unwinder will look for unwinding information at the instruction
    // after the return address. Normal compiler code generation does not
    // expect this and may generate incorrect entries in the exception handling
    // table. We work around this by adding a NOP instruction after the call.
    asm_may_unwind_root!(
        // DW_CFA_GNU_args_size 0
        cfi_reset_args_size_root!(),
        concat!("bl ", asm_mangle!("stack_call_trampoline")),
        "nop",
        in("3") arg,
        in("4") adjusted_stack_base(&stack).get(),
        in("5") f,
        clobber_abi("C"),
    );
}
