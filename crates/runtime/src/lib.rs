#![deny(unsafe_op_in_unsafe_fn)]
#![deny(rust_2018_idioms)]
#![deny(unused_must_use)]

//! Veil runtime shims (C ABI) for iterator hooks used by IR lowerings.
//!
//! Exposes:
//! - `iter_has_next(void*) -> bool`
//! - `iter_next(void*) -> int64_t`
//!
//! Notes:
//! - These are minimal placeholder implementations designed to satisfy linking
//!   and provide a predictable fallback behavior.
//! - Real iterator semantics should be provided by a proper runtime that knows
//!   the concrete iterator representation used by the compiler. When such a
//!   runtime is linked on the C side, the codegen can define
//!   `VEIL_RUNTIME_PROVIDES_ITER` to suppress any C-side stubs emitted by the
//!   backend and rely on these symbols instead.
//! - The ABI must remain stable: `extern "C"` with `#[no_mangle]` and the exact
//!   parameter/return types as below.

use core::ffi::c_void;

/// Returns whether the iterator has more elements.
///
/// Safety:
/// - `it` is an opaque pointer provided by compiled Veil code or the host.
/// - This default implementation does not dereference the pointer and simply
///   returns `false`. It is safe under all inputs but not functional.
/// - A real runtime should replace this with logic matching the iterator layout.
#[unsafe(no_mangle)]
pub extern "C" fn iter_has_next(_it: *mut c_void) -> bool {
    // Default shim: no elements.
    false
}

/// Returns the next element from the iterator as a 64-bit integer.
///
/// Safety:
/// - `it` is an opaque pointer provided by compiled Veil code or the host.
/// - This default implementation does not dereference the pointer and returns 0.
/// - A real runtime should replace this with logic matching the iterator layout
///   and the expected element type. If elements are not integers, the codegen
///   and runtime contract should be updated to support the required types.
#[unsafe(no_mangle)]
pub extern "C" fn iter_next(_it: *mut c_void) -> i64 {
    // Default shim: zero value.
    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_iter_has_next_is_false() {
        assert!(!iter_has_next(std::ptr::null_mut()));
    }

    #[test]
    fn default_iter_next_is_zero() {
        assert_eq!(iter_next(std::ptr::null_mut()), 0);
    }
}
