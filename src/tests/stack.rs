extern crate std;

/// Returns whether the mapping containing `addr` has the `nh` ("no huge pages")
/// flag in `/proc/self/smaps`.
#[cfg(target_os = "linux")]
fn has_no_huge_pages(addr: usize) -> bool {
    let smaps = std::fs::read_to_string("/proc/self/smaps").unwrap_or_default();
    let hex = |s| usize::from_str_radix(s, 16).ok();
    let mut in_mapping = false;
    for line in smaps.lines() {
        // Mapping headers start with "<start>-<end> " in hex.
        if let Some((lo, hi)) = line.split_once(' ').and_then(|(r, _)| r.split_once('-')) {
            if let (Some(lo), Some(hi)) = (hex(lo), hex(hi)) {
                in_mapping = lo <= addr && addr < hi;
                continue;
            }
        }
        if let Some(flags) = line.strip_prefix("VmFlags:").filter(|_| in_mapping) {
            return flags.split_whitespace().any(|flag| flag == "nh");
        }
    }
    false
}

/// `DefaultStack` must be mapped with `MAP_STACK`, which keeps transparent huge
/// pages off it on Linux 6.8+. Whether the kernel turns `MAP_STACK` into the
/// `nh` flag depends on its version and configuration, so we compare the stack
/// with a reference mapping made with `MAP_STACK`.
#[cfg(target_os = "linux")]
#[test]
fn default_stack_uses_map_stack_on_linux() {
    use crate::stack::{DefaultStack, Stack};

    // Any length works: the kernel rounds it up to whole pages.
    const LEN: usize = 4096;

    let stack = DefaultStack::default();
    let reference = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            LEN,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_PRIVATE | libc::MAP_ANONYMOUS | libc::MAP_STACK,
            -1,
            0,
        )
    };
    assert_ne!(reference, libc::MAP_FAILED);

    // The usable part of the stack ends just below its base.
    let stack_has_no_huge_pages = has_no_huge_pages(stack.base().get() - 1);
    let reference_has_no_huge_pages = has_no_huge_pages(reference as usize);
    unsafe { libc::munmap(reference, LEN) };

    assert_eq!(
        stack_has_no_huge_pages, reference_has_no_huge_pages,
        "the stack and a MAP_STACK mapping disagree on the `nh` VM flag"
    );
}
