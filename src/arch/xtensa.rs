//! Low-level Xtensa support (windowed ABI).
//!
//! This targets the Xtensa cores used in Espressif's ESP32, ESP32-S2 and
//! ESP32-S3 chips. All Rust Xtensa targets use the *windowed* register ABI
//! (`call4`/`call8`/`call12`, `entry`, `retw`), which makes context switching
//! quite different from the other architectures. Refer to x86_64.rs for an
//! overview of the operations implemented in this file and of the linked stack
//! concept.
//!
//! ## Register windows
//!
//! The processor has 64 physical address registers (AR), of which 16 (`a0`-
//! `a15`) are visible at any time. A `callN` rotates the visible window by `N`
//! registers when the callee executes `entry`, and `retw` rotates it back. The
//! `WindowStart` special register has one bit per group of 4 physical
//! registers marking where a live frame starts, and `WindowBase` points to the
//! current frame.
//!
//! When a call would reuse physical registers still owned by an older frame,
//! the hardware raises a *window overflow* exception and the handler saves
//! that frame to its stack. When `retw` returns to a frame whose registers are
//! no longer in the register file, a *window underflow* exception reloads them.
//! The handlers use two save areas:
//!
//! - **Base save area:** the 16 bytes immediately below the stack pointer of a
//!   frame hold the `a0`-`a3` of that frame's *caller*.
//! - **Extra save area:** a frame which made a `call8`/`call12` keeps its
//!   `a4`-`a7`/`a4`-`a11` at the top of its own stack frame, just below the base
//!   save area of its caller's caller.
//!
//! A frame's caller may therefore be partially or entirely held in registers
//! rather than on the stack. Before switching to another stack every live
//! window must be spilled, otherwise a later window underflow would reload
//! stale register contents.
//!
//! ## Switching stacks
//!
//! All stack switches go through `stack_switch`, which is called with `call4`
//! from the inline assembly in `switch_and_link`, `switch_yield` and
//! `switch_and_reset`. It:
//!
//! 1. Saves its own return address (`a0`) and its `a3` (the caller's `a7`, see
//!    below) at the bottom of its own stack frame.
//! 2. Spills every other live register window with `stack_window_spill`.
//!    Afterwards `WindowStart` only has the bit for `stack_switch` itself,
//!    and every caller frame's registers are in their save areas on the
//!    current stack.
//! 3. Loads the target stack pointer into `a1` and reloads `a0`/`a3` from the
//!    `stack_switch` frame saved on that stack.
//! 4. Executes `retw`. Since the caller window is not live, this triggers a
//!    window underflow which reloads the caller frame's `a0`-`a3` from the base
//!    save area of the *target* stack. Execution continues in the target
//!    context exactly as if its own call to `stack_switch` had returned.
//!
//! The target context therefore restarts with a clean window state: only the
//! current frame is live, and everything else comes back from its stack on
//! demand through the normal underflow mechanism.
//!
//! ### Spilling register windows
//!
//! `stack_window_spill` uses the classic `call12` chain: 4 nested `call12`
//! calls rotate the window by 12 registers each, and the innermost function
//! then references `a15`. Together these touch every one of the 15 window
//! positions after the frame of `stack_switch`, so any older live frame
//! triggers a window overflow and is saved to its stack by the standard
//! window overflow handlers. The chain frames themselves are never spilled
//! since they don't wrap around to `stack_switch`, so they return without any
//! underflow.
//!
//! This approach was chosen over a `WindowStart`/`WindowBase`-driven `rotw`
//! loop because every intermediate state is an ordinary, valid call chain:
//!
//! - It doesn't need to mask interrupts. An interrupt or window exception
//!   arriving during the spill sees normal frames and is handled normally.
//! - It doesn't need privileged instructions (`rotw`, writing
//!   `WindowStart`).
//! - It needs no knowledge of the window exception handlers beyond the
//!   standard save-area layout.
//!
//! It assumes 64 physical address registers, which is the configuration of all
//! ESP32, ESP32-S2 and ESP32-S3 cores.
//!
//! ## Register save set
//!
//! The windowed ABI has no callee-saved registers in the usual sense: a
//! `call4` preserves the caller's `a0`-`a3` through the window mechanism and
//! clobbers `a4`-`a15`. The inline assembly therefore:
//!
//! - relies on the window underflow to restore `a0`-`a3` of the resumed frame;
//! - declares `a4`-`a15` as clobbered, except `a7`. `a7` is the frame pointer
//!   under LLVM's windowed ABI, and LLVM silently ignores clobbers of the frame
//!   pointer, so `stack_switch` saves and restores it (its own `a3`)
//!   explicitly;
//! - declares as clobbered every caller-saved special register that the
//!   compiler knows about: `SAR`, the zero-overhead loop registers
//!   `LBEG`/`LEND`/`LCOUNT`, the boolean registers `b0`-`b15`, the MAC16
//!   registers, `SCOMPARE1` and the floating-point registers `f0`-`f15`. All
//!   of them are caller-saved in the Xtensa ABI. Clobbering them lets the
//!   compiler save only what is actually live across a switch. Each group is
//!   gated on the corresponding target feature (`fp`, `loop`, `bool`, `mac16`,
//!   `s32c1i`), so `cfg(target_feature)` is how FPU support is detected. The
//!   ESP32 and ESP32-S3 have the single-precision FPU and the ESP32-S2 does
//!   not.
//!
//! `clobber_abi("C")` is deliberately not used. The esp-rs compiler implements
//! it with the call0 ABI clobber set, which includes `a7` but not `a12`-`a15`.
//! It also lists registers which don't exist on targets without the FPU,
//! loop, boolean, MAC16 or S32C1I options, and fails to compile there (ESP32-S2).
//!
//! Global state which is not part of the ABI's register save set is not
//! switched: the `FCR`/`FSR` floating-point control and status registers, as
//! with the FP control registers on other architectures, `THREADPTR`, and
//! the ESP32-S3 vector/TIE extension state (`q0`-`q7`, `ACCX`, ...), which the
//! compiler never allocates.
//!
//! Zero-overhead loops can't be live across a switch: the loop registers are
//! declared as clobbered, so LLVM can't keep a hardware loop active across the
//! inline assembly. In practice the esp-rs LLVM doesn't generate zero-overhead
//! loops at all. A switch also doesn't need to clear a stale `LCOUNT`: a loop
//! only branches back when execution reaches `LEND` sequentially, which can only
//! happen inside the loop body that set it up.
//!
//! ## Toolchain requirements
//!
//! Xtensa requires the esp-rs compiler, version 1.93 or later (for `cfg` on
//! inline assembly operands).
//!
//! The assembly routines (`stack_switch`, `stack_window_spill` and the
//! trampolines) are emitted from inline assembly in a dedicated function rather
//! than with `global_asm!`. Rust <= 1.98 (LLVM <= 22) parses module-level
//! assembly without the target's features in optimized builds with multiple
//! codegen units, as part of local ThinLTO. On Xtensa this rejects even the
//! windowed ABI instructions themselves, not only FPU or other optional
//! instructions.
//!
//! ## Interrupts and exceptions
//!
//! Switching never masks interrupts and is safe against interrupts and window
//! exceptions at any point: every intermediate state is either a valid call
//! chain or the result of a completed spill. The 16 bytes below the stack
//! pointer always hold the base save area of the current frame's caller, as
//! the ABI requires.
//!
//! However, a context switch must not be performed from an interrupt or
//! exception handler, or more generally with `PS.EXCM` set or `PS.WOE`
//! cleared. In that state window overflow exceptions are not taken, so the
//! window spill would not work.
//!
//! ## Backtraces and unwinding
//!
//! Xtensa targets are `panic = "abort"` and Xtensa debuggers and backtrace
//! code walk the stack using the windowed ABI's base save areas rather than
//! DWARF CFI. Therefore the `asm-unwind` feature is not supported, and the
//! `unwind` feature can only be enabled with `std` and has no effect on the
//! behavior of panics.
//!
//! To still allow backtraces to continue from a coroutine into its parent,
//! the base save area chain is linked from the coroutine stack to the parent
//! stack. A walker (e.g. `esp-backtrace` or ESP-IDF) computes the next frame
//! as `pc = *(sp - 16)` and `sp = *(sp - 12)`. Each time a coroutine is
//! resumed, `stack_switch_link` stores the parent's suspended stack pointer
//! (the parent link) and its return address just below the stack base. A
//! pseudo-frame record below the initial object then points the walk at
//! these two words:
//!
//! ```text
//! coroutine frames -> stack_init_trampoline -> (link pseudo-frame)
//!     -> stack_switch (parent) -> parent frames ...
//! ```
//!
//! `on_stack` similarly copies its caller's `a0`/`a1` below the new stack's
//! base so walks continue back onto the original stack.
//!
//! ## Stack layout
//!
//! Here is what the layout of the stack looks like when a coroutine is
//! suspended. `stack_switch` frames are 64 bytes: the saved `a0` and `a3` at
//! the bottom, the `call12` extra save area and the base save area of the
//! caller's caller at the top.
//!
//! ```text
//! +-----------------+  <- Stack base
//! | Padding         |
//! +-----------------+
//! | Padding         |
//! +-----------------+
//! | Parent link     |  <- Points to the stack_switch frame on the parent
//! +-----------------+     stack while the coroutine is running.
//! | Parent PC       |  <- Return address of the parent's stack_switch call
//! +-----------------+
//! |                 |
//! ~      ...        ~  <- Frames of the suspended coroutine, all spilled.
//! |                 |
//! +-----------------+  <- Stack pointer of the frame which called stack_switch
//! | a0-a3 of the    |  <- Spilled by the window overflow handler.
//! | caller's caller |
//! +-----------------+
//! | Extra save area |  <- 32 bytes, only used if stack_switch itself is spilled
//! +-----------------+     while it calls stack_window_spill.
//! | Padding         |
//! +-----------------+
//! | Saved a3 (a7)   |
//! +-----------------+
//! | Saved a0 (PC)   |
//! +-----------------+  <- Saved stack pointer (stack_switch frame)
//! | a0-a3 of the    |  <- Reloaded by the window underflow on resume.
//! | caller          |
//! +-----------------+
//! ```
//!
//! And this is the layout of the parent stack when a coroutine is running.
//! It is identical to a suspended coroutine; the parent link points to the
//! saved stack pointer.
//!
//! And finally, this is the stack layout of a coroutine that has just been
//! initialized. The first `stack_switch` into it "returns" into
//! `stack_init_trampoline`, whose initial `a0`-`a3` are loaded from the base
//! save area by the window underflow.
//!
//! ```text
//! +-----------------+  <- Stack base
//! | Padding         |
//! +-----------------+
//! | Padding         |
//! +-----------------+
//! | Parent link     |
//! +-----------------+
//! | Parent PC       |
//! +-----------------+
//! |                 |
//! ~ Initial obj     ~
//! |                 |
//! +-----------------+  <- Trampoline stack pointer (a1)
//! | Padding         |
//! +-----------------+
//! | Padding         |
//! +-----------------+
//! | Link SP         |  <- Stack base. Used by backtraces to find the parent.
//! +-----------------+
//! | Link PC         |  <- Address of stack_switch_suspended.
//! +-----------------+
//! |                 |
//! ~ Padding         ~
//! |                 |
//! +-----------------+
//! | Parent link ptr |  <- Restored as a3 by stack_switch, becomes a7 of the
//! +-----------------+     trampoline.
//! | Initial PC      |  <- stack_init_trampoline, with the call4 window
//! |                 |     increment in the top bits.
//! +-----------------+  <- Initial stack pointer
//! | Trampoline a3   |
//! +-----------------+
//! | Initial func    |  <- Trampoline a2
//! +-----------------+
//! | Trampoline SP   |  <- Trampoline a1
//! +-----------------+
//! | Trampoline PC   |  <- Trampoline a0, only used by backtraces.
//! +-----------------+
//! ```

use core::arch::asm;

use super::allocate_obj_on_stack;
use crate::coroutine::adjusted_stack_base;
use crate::stack::{Stack, StackPointer};
use crate::unwind::{InitialFunc, StackCallFunc, TrapHandler};
use crate::util::EncodedValue;

#[cfg(feature = "asm-unwind")]
compile_error!("the `asm-unwind` feature is not supported on Xtensa targets");

pub const STACK_ALIGNMENT: usize = 16;
// The 16 bytes below the parent's saved stack pointer hold the a0-a3 of the
// frame which called stack_switch, which are needed to resume the parent.
pub const PARENT_STACK_OFFSET: usize = 16;
pub const PARENT_LINK_OFFSET: usize = 12;
pub type StackWord = u32;

// Size of the stack frame of stack_switch and stack_call_trampoline:
// - 16 bytes for local variables (saved a0 and a3, and padding);
// - 32 bytes extra save area for the call12 to stack_window_spill;
// - 16 bytes base save area of the caller's caller.
macro_rules! switch_frame_size {
    () => {
        "64"
    };
}
const SWITCH_FRAME_SIZE: usize = 64;

// Offset from the stack base of the word holding the return address into the
// parent's call to stack_switch_link, just below the parent link.
const PARENT_PC_OFFSET: usize = PARENT_LINK_OFFSET + 4;

// All of the assembly routines below are emitted from a single inline `asm!`
// block into their own text section, rather than with `global_asm!`.
//
// Rust <= 1.98 (LLVM <= 22) parses module-level assembly (`global_asm!` and
// naked functions) without the target's features in optimized builds with
// multiple codegen units, as part of local ThinLTO. On Xtensa this rejects not
// only FPU and other optional instructions but also the windowed ABI
// instructions themselves (`entry`, `retw`, `callN`, ...). Inline assembly is
// always assembled with the target features of the enclosing function.
//
// This function is never called. It only needs to be emitted once, which is
// guaranteed by it being non-generic, never inlined and kept alive by the
// `#[used]` static below. This is also why named labels are acceptable here.
#[inline(never)]
#[allow(named_asm_labels)]
unsafe extern "C" fn asm_routines() {
    asm!(
        ".pushsection .text.corosensei_xtensa,\"ax\",@progbits",
        // Switches to another stack. Called with call4 from inline assembly.
        //
        // Inputs:
        // - a2: Value passed through to the target context.
        // - a3: The caller's a7, which is preserved for the current context
        //       and restored for the target context.
        // - a4: Target stack pointer, as returned by a previous switch or by
        //       init_stack().
        // - a5 (stack_switch): Value returned to the target context in a4.
        // - a5 (stack_switch_link): Address of the parent PC slot just below
        //       the parent link. The return address is stored in the parent
        //       PC slot and the current stack pointer in the parent link.
        //
        // Outputs (in the target context):
        // - a2: Value passed from the context which switched to us.
        // - a4: Value of a5 in the context which switched to us.
        ".balign 4",
        asm_function_begin!("stack_switch_link"),
        concat!("entry a1, ", switch_frame_size!()),
        "s32i a0, a5, 0",
        "s32i a1, a5, 4",
        "j 0f",
        asm_function_end!("stack_switch_link"),
        ".balign 4",
        asm_function_begin!("stack_switch"),
        concat!("entry a1, ", switch_frame_size!()),
        "0:",
        // Save the return address and the caller's a7.
        "s32i a0, a1, 0",
        "s32i a3, a1, 4",
        // Spill all live register windows other than ours to the stack. This
        // preserves a0-a11.
        concat!("call12 ", asm_mangle!("stack_window_spill")),
        // All suspended contexts are stopped here, which is used as the PC of
        // the parent frame in coroutine backtraces.
        asm_function_alt_entry!("stack_switch_suspended"),
        // Switch to the target stack and load its saved registers.
        "mov a1, a4",
        "l32i a0, a1, 0",
        "l32i a3, a1, 4",
        "mov a4, a5",
        // Return into the target context. Our caller's window is not live (it
        // was either spilled above or belongs to another context), so this
        // triggers a window underflow which restores the caller's a0-a3 from
        // the 16 bytes below our new stack pointer.
        "retw",
        asm_function_end!("stack_switch"),
        // Spills all live register windows except the caller's to the stack.
        // Must be called with call12, and preserves the caller's a0-a11.
        //
        // Each call12 + entry rotates the window by 3 positions (12 registers)
        // and checks that the 3 positions after the caller's window are free,
        // causing a window overflow for any live frame there. After 4 levels
        // the innermost function references a15, which checks the 3 positions
        // after its own window. This covers the 15 positions after the caller's
        // window. Each level reserves the standard base save area plus a
        // call12 extra save area.
        ".balign 4",
        asm_function_begin!("stack_window_spill"),
        "entry a1, 48",
        "call12 1f",
        "retw",
        ".balign 4",
        "1:",
        "entry a1, 48",
        "call12 2f",
        "retw",
        ".balign 4",
        "2:",
        "entry a1, 48",
        "call12 3f",
        "retw",
        ".balign 4",
        "3:",
        "entry a1, 16",
        "mov a15, a15",
        "retw",
        asm_function_end!("stack_window_spill"),
        // This is entered by the retw in stack_switch the first time a
        // coroutine is resumed, due to the initial state set up by
        // init_stack(). It is not a real function since it doesn't have an
        // entry instruction: it continues the frame whose a0-a3 were reloaded
        // from the base save area by the window underflow.
        //
        // At this point our register state contains the following:
        // - a1 points to the initial object, below the parent link.
        // - a2 contains the initial function.
        // - a6 contains the argument passed from switch_and_link.
        // - a7 points to the parent link.
        ".balign 4",
        asm_function_begin!("stack_init_trampoline"),
        // Call the initial function with the argument, parent link and initial
        // object. call4 maps our a6-a8 to the callee's a2-a4. Using call4
        // instead of call8 means that this frame doesn't need an extra save
        // area.
        "mov a8, a1",
        "callx4 a2",
        // The initial function never returns.
        asm_function_alt_entry!("stack_init_trampoline_return"),
        "ill",
        asm_function_end!("stack_init_trampoline"),
        // This function calls a function pointer on a new stack and restores
        // the original stack upon returning. It is used by on_stack() and is
        // much simpler than the full coroutine logic, but also more limited
        // since yielding is not possible.
        //
        // Called with call4 from inline assembly:
        // - a2: Argument to pass to the function.
        // - a3: The caller's a7, which is preserved.
        // - a4: Base of the new stack.
        // - a5: Function to call.
        ".balign 4",
        asm_function_begin!("stack_call_trampoline"),
        concat!("entry a1, ", switch_frame_size!()),
        // We are about to change a1, so the caller's registers must be spilled
        // to the base save area below our current stack pointer first.
        // Otherwise a window overflow would later store them relative to the
        // new stack pointer and the underflow upon returning would read them
        // from the old one.
        concat!("call12 ", asm_mangle!("stack_window_spill")),
        // Copy the caller's a0 and a1 from our base save area to the base save
        // area below the new stack. Backtraces taken on the new stack then
        // continue with our caller on the original stack.
        "addi a6, a1, -16",
        "addi a7, a4, -16",
        "l32i a8, a6, 0",
        "s32i a8, a7, 0",
        "l32i a8, a6, 4",
        "s32i a8, a7, 4",
        // Keep the original stack pointer in a2, which is preserved by the
        // call4.
        "mov a6, a2",
        "mov a2, a1",
        // Switch to the new stack and call the function. Like
        // stack_init_trampoline, this uses call4 so that this frame doesn't
        // need an extra save area on the new stack.
        "mov a1, a4",
        "callx4 a5",
        // Switch back to the original stack. Our caller is still spilled, so
        // the retw reloads it from the base save area below the original
        // stack pointer.
        "mov a1, a2",
        "retw",
        asm_function_end!("stack_call_trampoline"),
        // This is entered from a trap handler by returning from the exception
        // (see setup_trap_trampoline):
        // - a0: Backtrace link (see setup_trap_trampoline).
        // - a1: Stack pointer on the coroutine stack.
        // - a2: Pointer to the object passed to the handler function.
        // - a3: Pointer to the parent link.
        // - a4: Handler function.
        ".balign 4",
        asm_function_begin!("stack_trap_trampoline"),
        // The register windows of all other frames belong to the trapped code
        // on the coroutine stack, whose memory is now being reused. Discard
        // them by setting WindowStart to only contain the current window,
        // rather than spilling them, which would corrupt the reused stack.
        "rsr.windowbase a5",
        "movi a6, 1",
        "ssl a5",
        "sll a6, a6",
        "wsr.windowstart a6",
        "rsync",
        // Call the handler function with call4 (a6-a7 become a2-a3).
        "mov a6, a2",
        "mov a7, a3",
        "callx4 a4",
        // The handler function never returns.
        "ill",
        asm_function_end!("stack_trap_trampoline"),
        ".popsection",
    );
}

#[used]
static ASM_ROUTINES: unsafe extern "C" fn() = asm_routines;

// These functions use a custom calling convention and should only be called
// with inline assembly or used as addresses.
extern "C" {
    fn stack_switch();
    fn stack_switch_link();
    static stack_switch_suspended: [u8; 0];
    fn stack_init_trampoline();
    fn stack_call_trampoline();
    fn stack_trap_trampoline();
}

/// Encodes a code address as a return address for a call4, with the window
/// increment in the top 2 bits.
///
/// `retw` keeps the top 2 bits of the PC, so the target must be in the same 1GB
/// region as `stack_switch`, which is always the case for code on ESP32 chips.
#[inline]
fn call4_return_address(addr: usize) -> usize {
    (addr & 0x3fff_ffff) | (1 << 30)
}

/// Writes a pseudo-frame record in the 16 bytes below `sp` which points stack
/// walkers to the parent context (see "Backtraces and unwinding" above).
///
/// `sp` must be the stack pointer of a frame without a caller, so that this
/// base save area is never used by the hardware.
#[inline]
unsafe fn write_parent_link_record(sp: usize, stack_base: usize) {
    let record = (sp - 16) as *mut usize;
    // PC of the next frame: the parent is suspended in stack_switch.
    *record = call4_return_address(stack_switch_suspended.as_ptr() as usize);
    // SP of the next frame: points the walker at the parent link and parent
    // PC slots at the stack base.
    *record.add(1) = stack_base;
}

/// Common clobbers for inline assembly blocks which call into `stack_switch`
/// or `stack_call_trampoline` with call4.
///
/// a0-a3 are preserved by the window mechanism, a7 is preserved explicitly, and
/// a6, a8, a9 and a12 are used as operands by the callers of this macro.
macro_rules! asm_switch {
    ($($asm:tt)*) => {
        asm!(
            $($asm)*
            lateout("a4") _, lateout("a5") _, lateout("a10") _, lateout("a11") _,
            lateout("a13") _, lateout("a14") _, lateout("a15") _,
            lateout("sar") _,
            #[cfg(target_feature = "loop")] lateout("lbeg") _,
            #[cfg(target_feature = "loop")] lateout("lend") _,
            #[cfg(target_feature = "loop")] lateout("lcount") _,
            #[cfg(target_feature = "s32c1i")] lateout("scompare1") _,
            #[cfg(target_feature = "mac16")] lateout("acclo") _,
            #[cfg(target_feature = "mac16")] lateout("acchi") _,
            #[cfg(target_feature = "mac16")] lateout("m0") _,
            #[cfg(target_feature = "mac16")] lateout("m1") _,
            #[cfg(target_feature = "mac16")] lateout("m2") _,
            #[cfg(target_feature = "mac16")] lateout("m3") _,
            #[cfg(target_feature = "bool")] lateout("b0") _,
            #[cfg(target_feature = "bool")] lateout("b1") _,
            #[cfg(target_feature = "bool")] lateout("b2") _,
            #[cfg(target_feature = "bool")] lateout("b3") _,
            #[cfg(target_feature = "bool")] lateout("b4") _,
            #[cfg(target_feature = "bool")] lateout("b5") _,
            #[cfg(target_feature = "bool")] lateout("b6") _,
            #[cfg(target_feature = "bool")] lateout("b7") _,
            #[cfg(target_feature = "bool")] lateout("b8") _,
            #[cfg(target_feature = "bool")] lateout("b9") _,
            #[cfg(target_feature = "bool")] lateout("b10") _,
            #[cfg(target_feature = "bool")] lateout("b11") _,
            #[cfg(target_feature = "bool")] lateout("b12") _,
            #[cfg(target_feature = "bool")] lateout("b13") _,
            #[cfg(target_feature = "bool")] lateout("b14") _,
            #[cfg(target_feature = "bool")] lateout("b15") _,
            #[cfg(target_feature = "fp")] lateout("f0") _,
            #[cfg(target_feature = "fp")] lateout("f1") _,
            #[cfg(target_feature = "fp")] lateout("f2") _,
            #[cfg(target_feature = "fp")] lateout("f3") _,
            #[cfg(target_feature = "fp")] lateout("f4") _,
            #[cfg(target_feature = "fp")] lateout("f5") _,
            #[cfg(target_feature = "fp")] lateout("f6") _,
            #[cfg(target_feature = "fp")] lateout("f7") _,
            #[cfg(target_feature = "fp")] lateout("f8") _,
            #[cfg(target_feature = "fp")] lateout("f9") _,
            #[cfg(target_feature = "fp")] lateout("f10") _,
            #[cfg(target_feature = "fp")] lateout("f11") _,
            #[cfg(target_feature = "fp")] lateout("f12") _,
            #[cfg(target_feature = "fp")] lateout("f13") _,
            #[cfg(target_feature = "fp")] lateout("f14") _,
            #[cfg(target_feature = "fp")] lateout("f15") _,
        )
    };
}

#[inline]
pub unsafe fn init_stack<T>(stack: &impl Stack, func: InitialFunc<T>, obj: T) -> StackPointer {
    let stack_base = adjusted_stack_base(stack).get();

    // Reserve space for the parent link and parent PC slots, plus padding.
    let mut sp = stack_base - 16;

    // Allocate space on the stack for the initial object, rounding to
    // STACK_ALIGNMENT. This also becomes the stack pointer of the trampoline
    // frame which calls the initial function.
    allocate_obj_on_stack(&mut sp, 0, obj);
    let trampoline_sp = sp;

    // The base save area below the trampoline frame is never used by the
    // hardware since that frame has no caller. Use it to link backtraces to
    // the parent context.
    write_parent_link_record(trampoline_sp, stack_base);

    // Create a stack_switch frame as if this context had been suspended by a
    // call4 to stack_switch from stack_init_trampoline.
    let sp = trampoline_sp - SWITCH_FRAME_SIZE;
    let frame = sp as *mut StackWord;
    // Return address of the stack_switch frame.
    *frame = call4_return_address(stack_init_trampoline as *const () as usize) as StackWord;
    // Saved a3 of the stack_switch frame, which becomes a7 of the trampoline:
    // the address of the parent link, second argument of the initial
    // function.
    *frame.add(1) = (stack_base - PARENT_LINK_OFFSET) as StackWord;

    // Base save area of the stack_switch frame, holding the a0-a3 of the
    // trampoline frame. These are loaded by the window underflow on the first
    // switch into this context.
    let base_save_area = (sp - 16) as *mut usize;
    // a0: return address of the trampoline frame. It never returns, so this
    // is only used by backtraces, which will continue with the link record.
    *base_save_area = call4_return_address(stack_init_trampoline as *const () as usize);
    // a1: stack pointer of the trampoline frame.
    *base_save_area.add(1) = trampoline_sp;
    // a2: initial function.
    *base_save_area.add(2) = func as usize;
    // a3: unused.
    *base_save_area.add(3) = 0;

    StackPointer::new_unchecked(sp)
}

#[inline]
pub unsafe fn switch_and_link(
    arg: EncodedValue,
    sp: StackPointer,
    stack_base: StackPointer,
) -> (EncodedValue, Option<StackPointer>) {
    let (ret_val, ret_sp);

    asm_switch!(
        // Call stack_switch_link, which saves our state, records our stack
        // pointer in the parent link and switches to the coroutine stack.
        //
        // Upon returning, our register state contains the following:
        // - a6: The argument passed from the coroutine.
        // - a8: The stack pointer of the suspended coroutine, or 0 if coming
        //       from switch_and_reset.
        "callx4 a12",

        // Pass the argument in a6 (a2 in stack_switch).
        inlateout("a6") arg => ret_val,

        // Pass the target stack pointer in a8 (a4 in stack_switch) and get
        // the coroutine stack pointer back in it.
        inlateout("a8") sp.get() => ret_sp,

        // Pass the address of the parent PC slot in a9 (a5 in stack_switch).
        inlateout("a9") stack_base.get() - PARENT_PC_OFFSET => _,

        // Use a register for the call target to avoid any range limits on the
        // call instruction.
        inlateout("a12") stack_switch_link as *const () as usize => _,
    );

    (ret_val, StackPointer::new(ret_sp))
}

#[inline(always)]
pub unsafe fn switch_yield(arg: EncodedValue, parent_link: *mut StackPointer) -> EncodedValue {
    let ret_val;

    asm_switch!(
        // Pass the stack pointer of our stack_switch frame back to the parent
        // in a9 (a5 in stack_switch).
        concat!("addi a9, a1, -", switch_frame_size!()),

        // Switch to the parent context.
        "callx4 a12",

        // Pass the argument in a6 (a2 in stack_switch).
        inlateout("a6") arg => ret_val,

        // Pass the parent stack pointer in a8 (a4 in stack_switch).
        inlateout("a8") (*parent_link).get() => _,

        out("a9") _,
        inlateout("a12") stack_switch as *const () as usize => _,
    );

    ret_val
}

#[inline(always)]
pub unsafe fn switch_and_reset(arg: EncodedValue, parent_link: *mut StackPointer) -> ! {
    // Most of this code is identical to switch_yield(), refer to the
    // comments there. Only the differences are commented.
    //
    // We still go through stack_switch, which spills our register windows to
    // the coroutine stack before switching away. Otherwise live windows of
    // this context could later be spilled into memory which no longer belongs
    // to this coroutine.
    asm!(
        "callx4 a12",
        in("a6") arg,
        in("a8") (*parent_link).get(),

        // Hard-code the returned stack pointer value to 0 to indicate that this
        // coroutine is done.
        in("a9") 0,

        in("a12") stack_switch as *const () as usize,
        options(noreturn),
    );
}

#[inline]
pub unsafe fn drop_initial_obj(
    _stack_base: StackPointer,
    stack_ptr: StackPointer,
    drop_fn: unsafe fn(ptr: *mut u8),
) {
    let ptr = (stack_ptr.get() + SWITCH_FRAME_SIZE) as *mut u8;
    drop_fn(ptr);
}

/// Registers which must be updated upon return from a trap handler.
///
/// The exact set of registers that need to be updated varies depending on the
/// target. Note that *all* registers must be updated to the specified values,
/// otherwise behavior is undefined.
///
/// On Xtensa the trapped context must be resumed with `PS.WOE` set and
/// `PS.EXCM` cleared, as is the case when returning from an exception to
/// normal code. `WindowBase` must be left unchanged; the trampoline discards
/// the other register windows itself.
///
/// To catch any issues at compilation time, it is recommended to use Rust's
/// pattern matching syntax to extract the individual registers from this
/// struct.
///
/// ```
/// # use corosensei::trap::TrapHandlerRegs;
/// # let regs = TrapHandlerRegs { pc: 0, a0: 0, a1: 0, a2: 0, a3: 0, a4: 0 };
/// let TrapHandlerRegs { pc, a0, a1, a2, a3, a4 } = regs;
/// ```
#[allow(missing_docs)]
#[derive(Clone, Copy, Debug)]
pub struct TrapHandlerRegs {
    pub pc: usize,
    pub a0: usize,
    pub a1: usize,
    pub a2: usize,
    pub a3: usize,
    pub a4: usize,
}

pub unsafe fn setup_trap_trampoline<T>(
    stack_base: StackPointer,
    val: T,
    handler: TrapHandler<T>,
) -> TrapHandlerRegs {
    let stack_base = stack_base.get();

    // Preserve the top 16 bytes of the stack since they contain the parent
    // link and parent PC.
    let parent_link = stack_base - PARENT_LINK_OFFSET;
    let mut sp = stack_base - 16;

    // Everything below this can be overwritten. Write the object to the stack.
    allocate_obj_on_stack(&mut sp, 0, val);
    let val_ptr = sp;

    debug_assert_eq!(sp % STACK_ALIGNMENT, 0);

    // Link backtraces from the handler to the parent context, like in
    // init_stack().
    write_parent_link_record(sp, stack_base);

    // Set up registers for entry into the trampoline.
    TrapHandlerRegs {
        pc: stack_trap_trampoline as *const () as usize,
        a0: call4_return_address(stack_trap_trampoline as *const () as usize),
        a1: sp,
        a2: val_ptr,
        a3: parent_link,
        a4: handler as usize,
    }
}

/// This function executes a function on the given stack. The argument is passed
/// through to the called function.
#[inline]
pub unsafe fn on_stack(arg: *mut u8, stack: impl Stack, f: StackCallFunc) {
    asm_switch!(
        "callx4 a12",
        inlateout("a6") arg => _,
        inlateout("a8") adjusted_stack_base(&stack).get() => _,
        inlateout("a9") f => _,
        inlateout("a12") stack_call_trampoline as *const () as usize => _,
    );
}
