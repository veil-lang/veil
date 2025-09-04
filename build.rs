/*!
 Root build script for the workspace (binary: `veil`).

 Purpose:
 - Increase the Windows stack size for the final `veil` executable to mitigate
   stack overflows observed in deep compilation pipelines (parse → lower →
   resolve → typeck → monomorphize → IR → codegen).
 - Allow user override via `VEIL_STACK_MB` environment variable.

 Behavior:
 - On Windows/MSVC: emits `/STACK:<bytes>`
 - On Windows/MinGW: emits `-Wl,--stack,<bytes>`
 - On non-Windows targets: no-op.

 Default stack size: 16 MiB (can be overridden by setting VEIL_STACK_MB >= 4).
*/

use std::env;

fn main() {
    // Re-run if this script changes
    println!("cargo:rerun-if-changed=build.rs");

    let target = env::var("TARGET").unwrap_or_default();

    // Parse override or fall back to default 16 MiB
    let stack_mb: u64 = env::var("VEIL_STACK_MB")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
        .filter(|&mb| mb >= 4)
        .unwrap_or(16);

    let bytes = stack_mb * 1024 * 1024;

    if target.contains("windows") {
        if target.contains("msvc") {
            // MSVC toolchain
            println!("cargo:rustc-link-arg=/STACK:{bytes}");
        } else {
            // GNU / MinGW
            println!("cargo:rustc-link-arg=-Wl,--stack,{bytes}");
        }
        // Expose chosen stack size for potential runtime diagnostics (optional)
        println!("cargo:rustc-env=VEIL_EFFECTIVE_STACK_BYTES={bytes}");
    }
}
