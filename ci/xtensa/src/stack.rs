//! Heap-allocated coroutine stacks.

use alloc::alloc::{alloc, dealloc, Layout};
use core::ptr::NonNull;

use corosensei::stack::{Stack, StackPointer, STACK_ALIGNMENT};

/// A stack allocated from the heap.
///
/// Bare-metal ESP32 targets have no MMU guard pages, so unlike `DefaultStack`
/// this can't catch a stack overflow when it happens. Instead the stack is
/// filled with a pattern, and dropping it panics if the lowest bytes were
/// overwritten, so that an overflow isn't mistaken for a context switching
/// bug.
pub struct HeapStack {
    ptr: NonNull<u8>,
    size: usize,
}

impl HeapStack {
    pub fn new(size: usize) -> Self {
        assert!(size >= corosensei::stack::MIN_STACK_SIZE);
        let layout = Layout::from_size_align(size, STACK_ALIGNMENT).unwrap();
        let ptr = NonNull::new(unsafe { alloc(layout) }).expect("out of memory for stack");
        // Fill the stack with a pattern so that reads of uninitialized stack
        // memory (e.g. an incorrectly restored register window) show up as
        // recognizable garbage rather than as stale but plausible values.
        unsafe { ptr.as_ptr().write_bytes(0xa5, size) };
        Self { ptr, size }
    }
}

impl Default for HeapStack {
    fn default() -> Self {
        Self::new(16 * 1024)
    }
}

unsafe impl Stack for HeapStack {
    fn base(&self) -> StackPointer {
        StackPointer::new(self.ptr.as_ptr() as usize + self.size).unwrap()
    }

    fn limit(&self) -> StackPointer {
        StackPointer::new(self.ptr.as_ptr() as usize).unwrap()
    }
}

/// Number of bytes at the limit of the stack which must remain untouched.
const CANARY_SIZE: usize = 256;

impl Drop for HeapStack {
    fn drop(&mut self) {
        let canary = unsafe { core::slice::from_raw_parts(self.ptr.as_ptr(), CANARY_SIZE) };
        if canary.iter().any(|&b| b != 0xa5) {
            panic!(
                "stack overflow: the {} byte stack at {:p} overflowed its limit",
                self.size,
                self.ptr.as_ptr()
            );
        }
        let layout = Layout::from_size_align(self.size, STACK_ALIGNMENT).unwrap();
        unsafe { dealloc(self.ptr.as_ptr(), layout) };
    }
}
