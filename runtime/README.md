# Veil Runtime

This directory provides a small, portable C runtime that implements the iterator hooks expected by the Veil compiler’s IR → C backend when lowering `for` loops.

The runtime is intentionally minimal and is meant to be swapped or extended by your application. It demonstrates a clean ABI and provides a simple “array iterator” example to prove end-to-end linkage.

- Runtime sources: `veil/runtime/c/*`
- Public header: `veil/runtime/c/veil_runtime.h`
- Implementation: `veil/runtime/c/veil_runtime.c`
- Makefile: `veil/runtime/c/Makefile`

The Veil CLI can link this runtime automatically via an environment variable; see “Use with the Veil CLI” below.

---

## What this runtime provides

The IR backend lowers `for` constructs to two hook calls:

- `bool iter_has_next(void* it);`
- `int64_t iter_next(void* it);`

The pointer is an opaque iterator handle. The shape and meaning of this handle is purely a runtime concern. The included implementation provides:

- Real implementations of the hooks for a trivial “array of int64_t” iterator.
- Optional helpers:
  - `veil_iter_t veil_array_iter_new(const int64_t* data, size_t len);`
  - `void veil_iter_free(veil_iter_t it);`

These helpers are provided as examples to bootstrap your own iterator design. You can keep the ABI stable while changing the underlying handle layout.

Header highlights from `veil/runtime/c/veil_runtime.h`:

```c
#ifndef VEIL_RUNTIME_H
#define VEIL_RUNTIME_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void* veil_iter_t;

#define VEIL_RUNTIME_VERSION_MAJOR 0
#define VEIL_RUNTIME_VERSION_MINOR 1
#define VEIL_RUNTIME_VERSION_PATCH 0
#define VEIL_RUNTIME_VERSION ((VEIL_RUNTIME_VERSION_MAJOR*10000)+ \
                              (VEIL_RUNTIME_VERSION_MINOR*100)+ \
                              (VEIL_RUNTIME_VERSION_PATCH))

bool    iter_has_next(veil_iter_t it);
int64_t iter_next(veil_iter_t it);

/* Example helpers (not required by the minimal ABI) */
veil_iter_t veil_array_iter_new(const int64_t* data, size_t len);
void        veil_iter_free(veil_iter_t it);

#ifdef __cplusplus
}
#endif
#endif /* VEIL_RUNTIME_H */
```

---

## Build

Requirements
- A C toolchain (e.g. clang or gcc).
- make (for the provided Makefile).

Build (POSIX/macOS/Linux; MSYS2/MinGW works similarly on Windows):

```bash
cd veil/runtime/c
make          # builds static and shared libs into build/
# or
make static   # builds libveil_runtime.a
make shared   # builds libveil_runtime.{so|dylib|dll}
```

Outputs (by platform)
- Linux: `build/libveil_runtime.a`, `build/libveil_runtime.so`
- macOS: `build/libveil_runtime.a`, `build/libveil_runtime.dylib`
- Windows (MinGW): `build/libveil_runtime.a`, `build/veil_runtime.dll`

Optional install:

```bash
sudo make install PREFIX=/usr/local
# installs:
#   /usr/local/include/veil_runtime.h
#   /usr/local/lib/libveil_runtime.a
#   /usr/local/lib/libveil_runtime.{so|dylib|dll}
```

Uninstall:

```bash
sudo make uninstall PREFIX=/usr/local
```

Customize build:
- `BUILD_DIR` to change the output directory (default: `build`).
- `PREFIX`, `INCLUDEDIR`, `LIBDIR` for install paths.
- `CC`, `CFLAGS`, `LDFLAGS` for toolchain knobs.

---

## Use with the Veil CLI

The Veil CLI can link a runtime automatically if you point it at the runtime library directory:

- Set `VEIL_RUNTIME_LIB_DIR` to the folder containing `libveil_runtime.{a,so,dylib}` (or `veil_runtime.dll` on Windows).
- The CLI defines `-DVEIL_RUNTIME_PROVIDES_ITER` when invoking the C compiler so the codegen’s internal iterator stubs are suppressed in favor of your runtime.

Example (Linux/macOS):

```bash
# Build the runtime once
cd veil/runtime/c
make

# Point the CLI to the runtime lib dir
export VEIL_RUNTIME_LIB_DIR="$(pwd)/build"

# Build your program with Veil (the CLI will pass -L and -lveil_runtime)
ve build path/to/your_program.veil --verbose
```

What to expect:
- The CLI’s C compiler invocation will include:
  - `-L$VEIL_RUNTIME_LIB_DIR`
  - `-lveil_runtime`
  - `-DVEIL_RUNTIME_PROVIDES_ITER`
- The emitted C translation unit will not include fallback iterator stubs.

Windows notes:
- Use an MSYS2/MinGW environment or a clang toolchain compatible with the CLI.
- The Makefile can emit `veil_runtime.dll` and `libveil_runtime.a` (import library).
- Ensure the DLL is discoverable at runtime (e.g. copy next to the final `exe`, or add its directory to `PATH`).
- `VEIL_RUNTIME_LIB_DIR` should point to the directory that contains the import library and/or static lib.

---

## API notes & contracts

- `veil_iter_t` is an opaque handle. Your application defines and manages its actual layout and lifetime.
- The minimal ABI returns `int64_t` from `iter_next`. If your iterators yield other types:
  - Add additional typed hooks (e.g. `iter_next_f64`, `iter_next_str`).
  - Or use a tagged representation (e.g. return IDs and materialize via another API).
  - Coordinate with the IR lowering to ensure the compiler emits calls that match your ABI.

Stability:
- We aim to keep these hook names and signatures stable for the 0.x series.
- Header includes version macros you can probe (`VEIL_RUNTIME_VERSION_*`).

---

## Example usage (C host)

```c
#include <stdio.h>
#include <stdint.h>
#include "veil_runtime.h"

int main(void) {
  static const int64_t xs[] = {1, 2, 3, 5, 8, 13};
  veil_iter_t it = veil_array_iter_new(xs, sizeof(xs)/sizeof(xs[0]));
  if (!it) return 1;

  while (iter_has_next(it)) {
    printf("%lld\n", (long long)iter_next(it));
  }
  veil_iter_free(it);
  return 0;
}
```

Compile:

```bash
cc -I/usr/local/include -L/usr/local/lib -lveil_runtime myprog.c -o myprog
```

---

## Troubleshooting

Linker errors: undefined reference to `iter_has_next` or `iter_next`
- Ensure `VEIL_RUNTIME_LIB_DIR` points to the directory that actually contains the library files.
- Verify the CLI logs (`--verbose`) show `-L...` and `-lveil_runtime`.
- Check architecture match (e.g. 64-bit toolchain vs 64-bit libs).

Program fails to find a shared library at runtime
- Linux: set `LD_LIBRARY_PATH=/path/to/lib`.
- macOS: set `DYLD_LIBRARY_PATH=/path/to/lib` (or use `install_name_tool` for rpaths).
- Windows: add the directory containing `veil_runtime.dll` to `PATH`, or place the DLL next to the executable.

Stubs vs runtime
- If you forget to provide a runtime, the IR→C backend emits guarded stubs (that do nothing) so your program still links. These are suppressed when the CLI defines `VEIL_RUNTIME_PROVIDES_ITER`.
- If you provide your own runtime, prefer using the CLI’s `VEIL_RUNTIME_LIB_DIR` flow so it sets the macro and linking flags for you.

---

## Extending the runtime

- Add new functions and a header for them (either extend `veil_runtime.h` or create a new header).
- Keep the opaque handle model for flexibility.
- Update your application and/or the Veil IR lowering to emit calls to your new hooks where appropriate.
- If extending the ABI beyond integers, consider adding conversion/utilities or a lightweight boxed representation.

---

## Related tests

- `tests/runtime_link.rs`: A test that builds a tiny C runtime on the fly, sets `VEIL_RUNTIME_LIB_DIR`, and verifies the CLI can link a program that uses `for` loops (exercise iterator hooks).

---

## License

The runtime sources in this directory are provided under the project’s dual license (MIT or Apache-2.0). See the repository’s LICENSE files for details.
