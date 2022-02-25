//! Special build-time detection for ARM targets.
//!
//! We need to determine some properties of the target for inline assembly that
//! are not otherwise easily available. Specifically:
//! - Does the target use the ARM or Thumb instruction set by default?
//! - Does the target use R7 or R11 for its frame pointer?
//! - Does the target treat R9 as a reserved register?

use std::env;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    // We only care about ARM.
    let arch = env::var("CARGO_CFG_TARGET_ARCH").unwrap();
    if arch != "arm" {
        return;
    }

    // Probe some registers using asm! to see if they are reserved.
    let cfg = autocfg::new();
    let r7_available = cfg.probe_expression("unsafe { core::arch::asm!(\"\", out(\"r7\") _) }");
    let r9_available = cfg.probe_expression("unsafe { core::arch::asm!(\"\", out(\"r9\") _) }");
    let r11_available = cfg.probe_expression("unsafe { core::arch::asm!(\"\", out(\"r11\") _) }");

    if !r9_available {
        autocfg::emit("r9_reserved");
    }

    match (r7_available, r11_available) {
        (true, false) => {}
        (false, true) => autocfg::emit("fp_is_r7"),
        _ => panic!("could not determine whether frame pointer is r7 or r11"),
    }

    // The most reliable way is to check for the thumb-mode feature in
    // CARGO_CFG_TARGET_FEATURE but this is only available on nightly. As a
    // fallback we just check if the target name starts with "thumb".
    let is_thumb = if let Ok(target_features) = env::var("CARGO_CFG_TARGET_FEATURE") {
        target_features.split(',').any(|s| s == "thumb-mode")
    } else {
        env::var("TARGET").unwrap().starts_with("thumb")
    };
    if is_thumb {
        autocfg::emit("is_thumb");
    }
}
