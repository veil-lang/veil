//! Build script for veil-cli
//!
//! Increases the default stack size on Windows to mitigate stack overflows
//! observed in deeply nested compilation pipelines (parsing / lowering / passes).
//!
//! Strategy:
//! - For MSVC targets use the `/STACK:<bytes>` linker argument.
//! - For GNU (MinGW) targets use the `-Wl,--stack,<bytes>` linker argument.
//!
//! The chosen size: 16 MiB.
//! Adjust if future pipeline changes demand more / less.
//!
//! This affects only binaries produced by this crate (veil-cli).

fn main() {
    // Re-run if this script changes
    println!("cargo:rerun-if-changed=build.rs");

    let target = std::env::var("TARGET").unwrap_or_default();
    // 16 MiB
    const STACK_SIZE: u64 = 16 * 1024 * 1024;
    if target.contains("windows") {
        if target.contains("msvc") {
            // MSVC linker
            println!("cargo:rustc-link-arg=/STACK:{STACK_SIZE}");
        } else {
            // GNU / MinGW
            println!("cargo:rustc-link-arg=-Wl,--stack,{STACK_SIZE}");
        }
    }
}
