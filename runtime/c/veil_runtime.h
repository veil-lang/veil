/*
 * Veil Runtime - Portable C Header for Iterator Hooks
 *
 * This header defines the minimal iterator hook ABI used by the Veil compiler's
 * IR → C backend when lowering `for` loops. A host application or runtime
 * library should provide implementations for these hooks and link them with the
 * generated C translation units.
 *
 * Default behavior:
 * - The Veil IR→C backend emits guarded C stubs for these hooks unless the
 *   compiler defines VEIL_RUNTIME_PROVIDES_ITER. When you link a real runtime,
 *   define VEIL_RUNTIME_PROVIDES_ITER in your build (or ensure the Veil CLI
 *   does it for you) to suppress codegen stubs and use your implementations.
 *
 * ABI Stability:
 * - These declarations are kept intentionally simple and portable (C99).
 * - The iterator handle is opaque (void*). Its layout is runtime-defined.
 * - The next element is returned as a 64-bit integer. If your iterators
 *   produce other types, adapt your runtime and coordinate with the compiler
 *   lowering to support richer element types as needed.
 */

#ifndef VEIL_RUNTIME_H
#define VEIL_RUNTIME_H

/* -------------------------------------------------------------------------------------------------
 * Configuration & Export Macros
 * -------------------------------------------------------------------------------------------------
 */

/* VEIL_RUNTIME_API controls symbol import/export for shared libraries.
 * Define VEIL_RUNTIME_DLL when building or using a DLL on Windows.
 * Define VEIL_RUNTIME_BUILD when building the runtime itself (to export
 * symbols).
 */
#if defined(_WIN32) && defined(VEIL_RUNTIME_DLL)
#if defined(VEIL_RUNTIME_BUILD)
#define VEIL_RUNTIME_API __declspec(dllexport)
#else
#define VEIL_RUNTIME_API __declspec(dllimport)
#endif
#else
#define VEIL_RUNTIME_API
#endif

#ifdef __cplusplus
#define VEIL_BEGIN_DECLS extern "C" {
#define VEIL_END_DECLS }
#else
#define VEIL_BEGIN_DECLS
#define VEIL_END_DECLS
#endif

/* -------------------------------------------------------------------------------------------------
 * Includes & Types
 * -------------------------------------------------------------------------------------------------
 */

#include <stdbool.h> /* bool */
#include <stddef.h>  /* size_t */
#include <stdint.h>  /* int64_t, uintptr_t */

VEIL_BEGIN_DECLS

/* Opaque iterator handle used by the runtime and codegen.
 * The Veil compiler treats this as an opaque pointer and does not inspect it.
 */
typedef void *veil_iter_t;

/* Optional version macros for consumers that want to check header
 * compatibility. */
#define VEIL_RUNTIME_VERSION_MAJOR 0
#define VEIL_RUNTIME_VERSION_MINOR 1
#define VEIL_RUNTIME_VERSION_PATCH 0
#define VEIL_RUNTIME_VERSION                                                   \
  ((VEIL_RUNTIME_VERSION_MAJOR * 10000) + (VEIL_RUNTIME_VERSION_MINOR * 100) + \
   (VEIL_RUNTIME_VERSION_PATCH))

/* -------------------------------------------------------------------------------------------------
 * Iterator Hooks
 * -------------------------------------------------------------------------------------------------
 */

/* Returns whether the iterator has more elements to produce.
 * Contract:
 * - it: Opaque iterator handle created by your runtime or host.
 * - Return value: true if a subsequent call to iter_next(it) would produce a
 * value.
 * - Ownership: The runtime owns the handle's memory unless documented
 * otherwise.
 */
VEIL_RUNTIME_API bool iter_has_next(veil_iter_t it);

/* Returns the next element as a 64-bit integer.
 * Contract:
 * - it: Opaque iterator handle created by your runtime or host.
 * - Behavior is undefined if called when iter_has_next(it) is false.
 * - Element type: For the initial runtime, values are represented as int64_t.
 *   If your iterator yields a different type, coordinate with your Veil
 * toolchain to extend the ABI (e.g., add typed variants or boxing).
 */
VEIL_RUNTIME_API int64_t iter_next(veil_iter_t it);

/* -------------------------------------------------------------------------------------------------
 * Notes & Recommendations
 * -------------------------------------------------------------------------------------------------
 *
 * 1) Providing a Runtime:
 *    - Implement the two hooks above in a static/shared library.
 *    - Link that library with the generated C code.
 *    - Define VEIL_RUNTIME_PROVIDES_ITER in the compilation of the generated C,
 *      so the codegen’s internal stubs are suppressed.
 *
 * 2) Iterator Handle Layout:
 *    - veil_iter_t is an opaque pointer. Your implementation can point it to a
 *      heap-allocated state struct, an array cursor, or any other
 * representation.
 *    - Consider providing factory and destroy functions in your own API to
 * create and free iterator handles used by Veil programs.
 *
 * 3) Extending the ABI:
 *    - If you need richer element types (e.g., strings, floats, structs), you
 * can: a) Provide additional hooks (e.g., iter_next_f64, iter_next_str). b) Use
 * a tagged union representation, returning int64_t IDs and a separate API to
 * materialize values by ID. c) Coordinate with the Veil compiler team to extend
 * the IR→C lowering.
 */

VEIL_END_DECLS

#endif /* VEIL_RUNTIME_H */
