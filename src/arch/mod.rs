use core::mem;

// Helper macros to deal with platform-specific differences in assembly code
// between ELF, Mach-O and COFF file formats.
cfg_if::cfg_if! {
    if #[cfg(any(
            target_vendor = "apple",
            all(windows, target_arch = "x86"),
        ))] {
        macro_rules! asm_mangle {
            ($name:literal) => { concat!("_", $name) };
        }
    } else {
        macro_rules! asm_mangle {
            ($name:literal) => { $name };
        }
    }
}
cfg_if::cfg_if! {
    if #[cfg(any(windows, target_os = "uefi"))] {
        // COFF
        macro_rules! asm_function_begin {
            ($name:literal) => {
                concat!(
                    ".globl ", asm_mangle!($name), "\n",
                    ".def ", asm_mangle!($name), "\n",
                    ".scl 2\n",
                    ".type 32\n",
                    ".endef ", asm_mangle!($name), "\n",
                    asm_mangle!($name), ":\n",
                )
            };
        }
        macro_rules! asm_function_alt_entry {
            ($name:literal) => {
                asm_function_begin!($name)
            };
        }
        macro_rules! asm_function_end {
            ($name:literal) => {
                ""
            };
        }
        // This macro is not used on Windows targets (which use
        // x86_64_windows.rs) but is used on UEFI targets via x86_64.rs.
        #[allow(unused_macros)]
        macro_rules! cfi_signal_frame {
            () => { ".cfi_signal_frame" }
        }
    } else if #[cfg(target_vendor = "apple")] {
        // Mach-O
        macro_rules! asm_function_begin {
            ($name:literal) => {
                concat!(
                    ".globl ", asm_mangle!($name), "\n",
                    ".private_extern ", asm_mangle!($name), "\n",
                    asm_mangle!($name), ":\n",
                )
            };
        }
        // Mach-O normally splits each symbol into its own section unless we
        // explicitly mark it as an alternate entry point.
        macro_rules! asm_function_alt_entry {
            ($name:literal) => {
                concat!(
                    ".alt_entry ", asm_mangle!($name), "\n",
                    asm_function_begin!($name),
                )
            };
        }
        macro_rules! asm_function_end {
            ($name:literal) => {
                ""
            };
        }
        // Darwin refuses to unwind through signal frames, but we don't need it
        // there anyways since those platforms don't use GDB (which was the
        // whole point of using .cfi_signal_frame in the first place).
        macro_rules! cfi_signal_frame {
            () => { "" }
        }
    } else {
        // Everything else uses ELF
        #[cfg(not(target_arch = "arm"))]
        macro_rules! asm_function_type {
            () => { "@function" }
        }
        #[cfg(target_arch = "arm")]
        macro_rules! asm_function_type {
            () => { "%function" }
        }
        macro_rules! asm_function_begin {
            ($name:literal) => {
                concat!(
                    ".globl ", asm_mangle!($name), "\n",
                    ".type ", asm_mangle!($name), ", ", asm_function_type!(), "\n",
                    asm_mangle!($name), ":\n",
                )
            };
        }
        macro_rules! asm_function_alt_entry {
            ($name:literal) => {
                asm_function_begin!($name)
            };
        }
        macro_rules! asm_function_end {
            ($name:literal) => {
                concat!(".size ", asm_mangle!($name), ", . - ", asm_mangle!($name), "\n")
            };
        }
        macro_rules! cfi_signal_frame {
            () => { ".cfi_signal_frame" }
        }
    }
}

/// Helper function to allocate an object on the stack with proper alignment.
///
/// This function is written such that the stack pointer alignment can be
/// constant-folded away when the object doesn't need an alignment greater than
/// `STACK_ALIGNMENT`.
#[inline]
unsafe fn allocate_obj_on_stack<T>(sp: &mut usize, sp_offset: usize, obj: T) {
    // Sanity check to avoid stack overflows.
    assert!(mem::size_of::<T>() <= 1024, "type is too big to transfer");

    if mem::align_of::<T>() > STACK_ALIGNMENT {
        *sp -= mem::size_of::<T>();
        *sp &= !(mem::align_of::<T>() - 1);
    } else {
        // We know that sp + sp_offset is aligned to STACK_ALIGNMENT. Calculate
        // how much padding we need to add so that sp_offset + padding +
        // sizeof(T) is aligned to STACK_ALIGNMENT.
        let total_size = sp_offset + mem::size_of::<T>();
        let align_offset = total_size % STACK_ALIGNMENT;
        if align_offset != 0 {
            *sp -= STACK_ALIGNMENT - align_offset;
        }
        *sp -= mem::size_of::<T>();
    }
    (*sp as *mut T).write(obj);

    // The stack is aligned to STACK_ALIGNMENT at this point.
    debug_assert_eq!(*sp % STACK_ALIGNMENT, 0);
}

cfg_if::cfg_if! {
    if #[cfg(all(target_arch = "x86_64", not(windows)))] {
        mod x86_64;
        pub use self::x86_64::*;
    } else if #[cfg(all(target_arch = "x86_64", windows))] {
        mod x86_64_windows;
        pub use self::x86_64_windows::*;
    } else if #[cfg(all(target_arch = "x86", not(windows)))] {
        mod x86;
        pub use self::x86::*;
    } else if #[cfg(all(target_arch = "x86", windows))] {
        mod x86_windows;
        pub use self::x86_windows::*;
    } else if #[cfg(all(target_arch = "aarch64", not(windows)))] {
        mod aarch64;
        pub use self::aarch64::*;
    } else if #[cfg(all(target_arch = "arm", not(any(windows, target_vendor = "apple"))))] {
        // Apple on ARM uses SJLJ unwinding which we don't support.
        mod arm;
        pub use self::arm::*;
    } else if #[cfg(all(any(target_arch = "riscv64", target_arch = "riscv32"), not(windows)))] {
        mod riscv;
        pub use self::riscv::*;
    } else if #[cfg(all(target_arch = "loongarch64", not(windows)))] {
        mod loongarch64;
        pub use self::loongarch64::*;
    } else if #[cfg(all(target_arch = "powerpc64", target_abi = "elfv2", not(windows)))] {
        mod powerpc64;
        pub use self::powerpc64::*;
    } else {
        compile_error!("Unsupported target");
    }
}

/// Helper function to push a value onto a stack.
#[inline]
unsafe fn push(sp: &mut usize, val: Option<StackWord>) {
    *sp -= mem::size_of::<StackWord>();
    if let Some(val) = val {
        *(*sp as *mut StackWord) = val;
    }
}

/// Callee-saved x86_64 floating-point control state (MXCSR + x87 FCW), packed
/// into a `u64` as `(fcw << 32) | mxcsr`.
///
/// Both the System V AMD64 ABI and the Windows x64 ABI classify the MXCSR
/// control bits (bits 6..=15) and the x87 control word as callee-saved, so
/// `clobber_abi` does not cause the compiler to preserve them around inline
/// assembly.
///
/// On soft-float x86_64 targets (e.g. `x86_64-unknown-none`, `x86_64-unknown-uefi`
/// without SSE), this is a no-op so we never emit SSE/x87 instructions.
#[cfg(target_arch = "x86_64")]
#[derive(Clone, Copy)]
struct FpControlState(u64);

#[cfg(all(target_arch = "x86_64", target_feature = "sse"))]
impl FpControlState {
    /// Mask of callee-saved bits in the packed `(fcw << 32) | mxcsr` layout:
    /// MXCSR bits 0..=5 are volatile exception status flags, while MXCSR
    /// control bits (bits 6..=15) and all 16 bits of the x87 control word are
    /// callee-saved.
    const CONTROL_MASK: u64 = 0x0000_ffff_ffff_ffc0;

    #[inline(always)]
    unsafe fn read() -> Self {
        // Store and load MXCSR (32-bit) and x87 FCW (16-bit) at their exact
        // widths to avoid store-to-load forwarding stalls.
        let mut mxcsr = mem::MaybeUninit::<u32>::uninit();
        let mut fcw = mem::MaybeUninit::<u16>::uninit();
        core::arch::asm!(
            "stmxcsr [{mxcsr}]",
            "fnstcw [{fcw}]",
            mxcsr = in(reg) mxcsr.as_mut_ptr(),
            fcw = in(reg) fcw.as_mut_ptr(),
            options(nostack, preserves_flags),
        );
        Self((mxcsr.assume_init() as u64) | ((fcw.assume_init() as u64) << 32))
    }

    #[inline(always)]
    unsafe fn restore_if_changed(self, current: Self) {
        if ((current.0 ^ self.0) & Self::CONTROL_MASK) != 0 {
            self.restore();
        }
    }

    #[cold]
    #[inline(never)]
    unsafe fn restore(self) {
        let mxcsr = self.0 as u32;
        let fcw = (self.0 >> 32) as u16;
        core::arch::asm!(
            "ldmxcsr [{mxcsr}]",
            "fldcw [{fcw}]",
            mxcsr = in(reg) &mxcsr,
            fcw = in(reg) &fcw,
            options(nostack, preserves_flags, readonly),
        );
    }
}

#[cfg(all(target_arch = "x86_64", not(target_feature = "sse")))]
impl FpControlState {
    #[inline(always)]
    unsafe fn read() -> Self {
        Self(0)
    }

    #[inline(always)]
    unsafe fn restore_if_changed(self, _current: Self) {}

    #[cfg(feature = "asm-unwind")]
    #[inline(always)]
    unsafe fn restore(self) {}
}

/// Restores saved FP control state if an `asm-unwind` exception unwinds
/// directly out of the switch assembly without executing the normal return
/// path.
#[cfg(all(target_arch = "x86_64", feature = "asm-unwind"))]
struct FpUnwindGuard(FpControlState);

#[cfg(all(target_arch = "x86_64", feature = "asm-unwind"))]
impl Drop for FpUnwindGuard {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            self.0.restore();
        }
    }
}
