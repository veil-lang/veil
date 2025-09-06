/*
 * Veil Runtime - Portable C Runtime Implementation
 *
 * This file provides a minimal, portable implementation of the iterator hook
 * ABI consumed by the Veil IR → C backend, along with a trivial array iterator
 * example to demonstrate end-to-end usage.
 *
 * Contents:
 *  - Real hook implementations:
 *      bool   iter_has_next(veil_iter_t it);
 *      int64_t iter_next(veil_iter_t it);
 *  - Example array iterator helpers (not part of the minimal core ABI):
 *      veil_iter_t veil_array_iter_new(const int64_t* data, size_t len);
 *      void        veil_iter_free(veil_iter_t it);
 *
 * Notes:
 *  - The hook implementations below expect the opaque iterator handle to point
 *    to the internal `VeilArrayIter` struct (as created by
 * veil_array_iter_new).
 *  - In your own runtime, you can replace `VeilArrayIter` with your preferred
 *    iterator layout while keeping the same hook signatures.
 *  - Define VEIL_RUNTIME_BUILD (and optionally VEIL_RUNTIME_DLL on Windows)
 * when building this runtime as a shared library to export symbols properly.
 *  - When compiling the generated C from Veil, define
 * VEIL_RUNTIME_PROVIDES_ITER so that the codegen's internal stubs are
 * suppressed in favor of these hooks.
 */

#include "veil_runtime.h"

#include <stdlib.h> /* malloc, free */

/* -----------------------------------------------------------------------------------------------
 * Internal Iterator Representation (Example)
 * -----------------------------------------------------------------------------------------------
 *
 * A trivial array iterator for int64_t elements. This is only an example to
 * provide a concrete end-to-end story. Production runtimes may have richer or
 * different iterator shapes and can still expose the same `iter_has_next` /
 * `iter_next` hooks.
 */

typedef struct VeilArrayIter {
  const int64_t *data; /* Pointer to array elements (not owned) */
  size_t len;          /* Total number of elements */
  size_t idx;          /* Current position (next element index) */
} VeilArrayIter;

/* -----------------------------------------------------------------------------------------------
 * Hook Implementations
 * -----------------------------------------------------------------------------------------------
 *
 * These are the functions invoked by code emitted from the Veil IR→C backend
 * for `for` loops. Contract (as declared in veil_runtime.h):
 *   - iter_has_next(veil_iter_t) -> bool
 *   - iter_next(veil_iter_t)     -> int64_t
 *
 * Behavior:
 *   - If `it` is NULL, iter_has_next returns false and iter_next returns 0.
 *   - If `it` points to a `VeilArrayIter`, elements are produced until
 * exhaustion.
 *   - If your runtime uses a different layout, adjust these to interpret `it`
 * accordingly.
 */

VEIL_RUNTIME_API bool iter_has_next(veil_iter_t it) {
  if (!it) {
    return false;
  }
  const VeilArrayIter *a = (const VeilArrayIter *)it;
  return a->idx < a->len;
}

VEIL_RUNTIME_API int64_t iter_next(veil_iter_t it) {
  if (!it) {
    return 0; /* Defensive default */
  }
  VeilArrayIter *a = (VeilArrayIter *)it;
  if (a->idx >= a->len) {
    return 0; /* Out of elements; defensive default */
  }
  int64_t v = a->data[a->idx];
  a->idx += 1;
  return v;
}

/* -----------------------------------------------------------------------------------------------
 * Example Helpers (Optional)
 * -----------------------------------------------------------------------------------------------
 *
 * A small set of helpers to construct and destroy the trivial array iterator
 * used by this runtime. These helpers are exported to make it easy for host
 * code or FFI layers to create iterator handles that Veil-generated code can
 * consume.
 *
 * IMPORTANT:
 *   These helpers are provided as an example; they are not part of the strict
 * minimal ABI. If you ship this runtime as a reusable library, consider adding
 * their declarations to a separate public header, or extend `veil_runtime.h`
 * with guarded declarations as appropriate.
 */

/* Create an iterator over a borrowed array of int64_t. Ownership of `data`
 * remains with the caller; the returned veil_iter_t only borrows it for the
 * lifetime of the iterator handle. Returns NULL on allocation failure or if len
 * > 0 but data is NULL.
 */
VEIL_RUNTIME_API veil_iter_t veil_array_iter_new(const int64_t *data,
                                                 size_t len) {
  if (len > 0 && data == NULL) {
    return NULL;
  }
  VeilArrayIter *a = (VeilArrayIter *)malloc(sizeof(VeilArrayIter));
  if (!a) {
    return NULL;
  }
  a->data = data;
  a->len = len;
  a->idx = 0;
  return (veil_iter_t)a;
}

/* Free an iterator created by veil_array_iter_new. */
VEIL_RUNTIME_API void veil_iter_free(veil_iter_t it) {
  if (!it) {
    return;
  }
  VeilArrayIter *a = (VeilArrayIter *)it;
  free(a);
}

/* -----------------------------------------------------------------------------------------------
 * Optional: Convenience Factory For Static Arrays (Macro)
 * -----------------------------------------------------------------------------------------------
 *
 * A small helper macro that can be used by C callers to construct an iterator
 * from a static array:
 *
 *   static const int64_t xs[] = {1,2,3};
 *   veil_iter_t it = veil_array_iter_new(xs, VEIL_ARRAY_LEN(xs));
 *   while (iter_has_next(it)) { printf("%lld\n", (long long)iter_next(it)); }
 *   veil_iter_free(it);
 */
#ifndef VEIL_ARRAY_LEN
#define VEIL_ARRAY_LEN(arr) (sizeof(arr) / sizeof((arr)[0]))
#endif
