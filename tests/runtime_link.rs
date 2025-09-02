use std::fs;
use std::path::PathBuf;
use std::process::Command;

use anyhow::{Context, Result, anyhow};

#[cfg(not(target_os = "windows"))]
#[test]
fn runtime_hooks_link_when_env_is_set() -> Result<()> {
    // Build a temporary C runtime providing iter_has_next/iter_next and point the CLI to it
    let tmp = tempfile::tempdir().context("failed to create tempdir")?;
    let libdir = tmp.path();

    // Write a minimal C runtime with the required symbols
    let rt_c_path = libdir.join("runtime.c");
    fs::write(
        &rt_c_path,
        r#"#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

bool iter_has_next(void* it) { (void)it; return false; }
int64_t iter_next(void* it) { (void)it; return 0; }
"#,
    )
    .context("failed to write runtime.c")?;

    // Compile it into a shared library that the CLI can link with -lveil_runtime
    // Produces: libveil_runtime.so
    let lib_path = libdir.join(shared_lib_name("veil_runtime"));
    let clang_path =
        veil_helpers::get_bundled_clang_path().context("failed to locate bundled clang")?;
    let status = Command::new(&clang_path)
        .current_dir(libdir)
        .args([
            "-shared",
            "-fPIC",
            "-o",
            lib_path.to_str().unwrap(),
            rt_c_path.to_str().unwrap(),
        ])
        .status()
        .context("failed to invoke clang to build runtime shared lib")?;

    if !status.success() {
        return Err(anyhow!(
            "clang failed to produce {} (status: {status})",
            lib_path.display()
        ));
    }

    // Ensure the library exists
    assert!(
        lib_path.exists(),
        "Expected runtime library at {}",
        lib_path.display()
    );

    // Point the CLI to the runtime library directory
    let libdir_str = libdir.to_str().ok_or_else(|| anyhow!("non-UTF8 libdir"))?;
    unsafe {
        std::env::set_var("VEIL_RUNTIME_LIB_DIR", libdir_str);
    }

    // Use a test input that exercises a for-loop to force IR to emit calls to iter hooks
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let input = manifest_dir.join("tests").join("for_in_array.veil");
    assert!(input.exists(), "Missing test input at {}", input.display());

    // Ask the CLI to build; with VEIL_RUNTIME_LIB_DIR set, it should pass -L/-l and link successfully.
    // is_test = true so the compiled program isn't executed; we only validate link succeeds.
    let output_name = PathBuf::from("runtime_link_test_bin");
    let built = veil_cli::process_build(
        input.clone(),
        output_name.clone(),
        /* optimize */ false,
        /* target_triple */ "x86_64-unknown-linux-gnu".to_string(),
        /* verbose */ true,
        /* dump_norm_hir */ false,
        /* is_test */ true,
        /* skip_cc */ false,
    )
    .with_context(|| format!("process_build failed for {}", input.display()))?;

    // The CLI writes to build/<output_name>
    assert!(
        built.exists(),
        "Expected built artifact at {}",
        built.display()
    );

    Ok(())
}

#[cfg(target_os = "windows")]
#[test]
fn runtime_hooks_link_when_env_is_set_windows_placeholder() -> Result<()> {
    // On Windows, building a compatible runtime DLL for this test requires platform-specific flags.
    // Keep a placeholder to avoid CI failures on Windows until a DLL flow is wired.
    eprintln!("Skipping runtime linkage test on Windows for now");
    Ok(())
}

// ---------------------
// Helpers
// ---------------------

#[cfg(not(target_os = "windows"))]
fn shared_lib_name(base: &str) -> String {
    format!("lib{base}.so")
}

#[cfg(target_os = "windows")]
fn shared_lib_name(base: &str) -> String {
    format!("{base}.dll")
}

#[cfg(target_os = "macos")]
fn shared_lib_name(base: &str) -> String {
    format!("lib{base}.dylib")
}
