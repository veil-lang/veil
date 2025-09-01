# Veil v2.0 Migration Plan

A concrete, subsystem-oriented plan to migrate the current compiler and toolchain to the v2.0 language specification under `refs/`. This document focuses on Gap Analysis and an actionable Subsystem Checklist. Complementary documents:

- Architecture Refactor Plan (separate doc)
- Versioned Roadmap (separate doc)

## 1) Scope and Goals

- Align the language implementation to `refs/grammer.md` and `refs/language-spec.md` (v2.0).
- Modernize internals to scale (preparing for traits, async, richer types).
- Preserve incremental compilation and the C backend while refactoring toward cleaner pass boundaries.
- Provide clear, staged, low-risk steps that keep CI green.

Non-goals (in this plan):

- Full async runtime and I/O system design details (tracked separately).
- Full stdlib expansion (tracked on the roadmap).

Target artifacts touched most:

- `src/lexer.rs`, `src/parser/**` — grammar alignment.
- `src/typeck/**` — type rules and features.
- `src/codegen/**` — lifts monomorphization and emits new features.
- `src/compiler/**` — module graph + resolution + caches.
- `src/cli/**` — developer workflows.
- `lib/std/src/**` — minimal prelude and conformance helpers.

---

## 2) Gap Analysis (Current vs. Spec v2.0)

### 2.1 Lexical Structure & Keywords

- Present: `let`, `var`, `if/else`, `while/for/loop`, `match`, `return`, `break/continue`, `new`, `constructor`, `impl`, `struct`, `enum`, `foreign`, `import/export`, `as`, `rawptr`, literals, ranges, `&&/||`, `/`, `**`, array `[]`, template strings, `None`/`none`.
- Missing vs Spec:
  - Declarations: `const` (bindings), visibility (`pub`, `pub(crate)`, `pub(super)`, `pub(in path)`).
  - Async/Concurrency: `async`, `await`, `spawn`.
  - Traits: `trait`, `where` constraints, associated types/consts.
  - Types: `str` (vs `string`), `ch`, `isize/usize`, `i128/u128`, `&T` borrowed, `weak T`, `dyn Trait`, union (`T | U`) and intersection (`T & U`) types.
  - Operators: pipeline `|>`, postfix `?` (error propagation), prefix/postfix `++/--`, logical `&` and `|` (single-char semantics per spec) vs current `&&`/`||`.
  - Modules: `::` as a path separator (alongside `/`).
  - Pattern syntax and advanced guards (mostly present; needs completeness).
  - Attributes breadth (`derive`, `repr`, `inline`, `deprecated`, `must_use`) beyond current `#` handling.
  - `unsafe` blocks and union read rules.

### 2.2 Parser & Precedence

- Present: good coverage of expressions/statements; ranges; FFI; import/export; enums/structs/impl; calls/fields/arrays/template strings.
- Gaps:
  - Precedence table differs: must implement spec precedence (logical vs bitwise ordering, pipeline, power associativity).
  - Module paths only accept `/` (must also accept `::`).
  - Declarations: no `const`, visibility modifiers not parsed, type aliases, unions, traits.
  - Types: optional (`T?`) handled, but no union/intersection, `&T`, `weak T`, `dyn`.
  - Expressions: missing `|>`, `++/--`, postfix `?`, `await`, `spawn`, closures (sync/async), `unsafe` block, `try` expression.

### 2.3 Resolver/Module System

- Present: `compiler::resolve_imports_only` resolves std/local/external with envs; no auto-prelude insertion.
- Gaps:
  - No symbol resolver pass (re-exports, visibility scoping).
  - No auto `import std/prelude` insertion (spec mandates).
  - No `pub(in path)` scoping semantics.

### 2.4 Type Checker

- Present: expression/statement typing; enums/structs/impls; simple generics; FFI variable injection.
- Gaps:
  - Union/intersection type lattice; exhaustive unions.
  - Logical vs bitwise semantics; `/` vs `//` enforcement.
  - Optional flows and postfix `?`.
  - Traits/impl-trait obligations; associated types; `dyn` trait objects (basic).
  - Async correctness (`async fn`, `await`, `spawn`); `unsafe` gating.
  - Visibility enforcement during name resolution.

### 2.5 Codegen (C Backend)

- Present: monomorphization embedded in backend; optional/array helpers; tests harness.
- Gaps:
  - Traits: static dispatch (where bounds) initial; `dyn` via vtables later.
  - Move monomorphization into dedicated pass pre-codegen.
  - Async: state machines and runtime shim emission (later milestone).
  - Error propagation (`?`) to early returns; union/intersection mapping.
  - Visibility-driven name export/mangling policy.

### 2.6 Compiler/Incremental

- Present: incremental builds; single merged C output; caching.
- Gaps:
  - Persisted pass-level caching (parse/HIR/typeck/mono/codegen).
  - Symbol graph and re-export tracking for fast invalidation.
  - Auto-prelude injection at entry.

### 2.7 CLI/Tooling

- Present: build/run/test/benchmark/init/upgrade; `dump_tokens` tool.
- Gaps:
  - `ve fmt`/`ve lint` stubs for later.
  - Test listing is present; doc-tests later.

### 2.8 Stdlib

- Present: minimal `lib/std/src`.
- Gaps:
  - `std/prelude` and core types (Option/Result) alignment with spec; async runtime API later.

### 2.9 Tests/Examples

- Present: good base coverage for ranges, literals, hello world, iterators, tests runner.
- Gaps:
  - New operators precedence suite; optional `?` flows; const/var with destructuring; visibility; traits; async smoke tests; `::` module path; import lists; union/intersection.

---

## 3) Subsystem Migration Checklist

Conventions:

- [ ] = To do
- [x] = Done

- (dep) = depends on previous step
- Paths refer to existing or planned files.

### 3.1 Lexer (`src/lexer.rs`)

- [ ] Add declaration/visibility tokens: `KwConst`, `KwPub`, `KwWhere`, `KwTrait`, `KwType`, `KwUnion`, `KwUnsafe`.
- [ ] Add async/concurrency tokens: `KwAsync`, `KwAwait`, `KwSpawn`, `KwMove`, `KwWeak`.
- [ ] Add operators: `PipePipeline` (`|>`), `PlusPlus` (`++`), `MinusMinus` (`--`), `Question` (postfix `?`), `SlashSlash` (`//`), `Tilde` (`~`), `Caret` (bitwise xor), `Is`, `IsNot`, `NotIn`, (reuse `In`).
- [ ] Add `ColonColon` token for `::`.
- [ ] Add primitive/type tokens: `TyStr`, `TyCh`, `TyIsize`, `TyUsize`, `TyI128`, `TyU128`, and allow `dyn`.
- [ ] Keep legacy: `KwLet`, `TyString` (emit deprecation notes via parser/typeck).
- [ ] Ensure tokenization precedence: match `**` before `*`; `//` before `/`; `::` before `:`.

Acceptance:

- [ ] Golden token streams for sample sources (operators, types, module paths) checked via `dump_tokens`.

### 3.2 Parser — Program/Imports/Visibility

Files: `src/parser/mod.rs`, `src/parser/import_export.rs`, `src/parser/grammar/veil.pest` (Pest grammar), `src/parser/pest_adapter.rs`

- [ ] Auto-insert `import std/prelude;` for non-prelude modules.
- [ ] Module paths: accept both `/` and `::` separators (normalize internally).
- [ ] Parse visibility: optional `pub`, `pub(crate)`, `pub(super)`, `pub(in path)` on items and fields.
- [ ] Export blocks: extend to `enum`, `type` (aliases), `union` in addition to `fn`, `struct`.

### 3.3 Parser — Declarations

Files: `src/parser/decl/*.rs` (extend or add new)

- [ ] Replace/augment `let` parsing:
  - [ ] `const` and `var` declarations (with destructuring).
  - [ ] Keep `let` as alias (deprecation warning).
- [ ] Add `type` alias parsing: `type Identifier<...>? = type;`.
- [ ] Add `union` declarations (fields with types).
- [ ] Add `trait` declarations:
  - [ ] Method signatures (default body optional), associated `type`, `const`, generic params and `where`.
- [ ] `impl` blocks:
  - [ ] `impl <T>? Type { ... }` (methods),
  - [ ] `impl <T>? Trait for Type where ... { ... }`.
- [ ] Attributes: parse and attach metadata to items and fields (re-using existing `#[]` path; extend parsing to lists and key-value).

### 3.4 Parser — Types

Files: `src/parser/types.rs`

- [ ] Map primitives per spec: `str`, `ch`, `isize/usize`, `i128/u128`. Keep `string` as alias to `str` with deprecation.
- [ ] Optional `T?` (present).
- [ ] Unions/intersections: `T | U`, `T & U` (type-level).
- [ ] Arrays: `[T]`, `[T; N]` (present).
- [ ] Pointers/Refs: `*T`, `&T`, `weak T`.
- [ ] Trait objects: `dyn Trait`.
- [ ] Generics: `<T, ...>`; `where` predicate lists.
- [ ] Enum/struct/tuple types remain supported.

### 3.5 Parser — Expressions/Statements/Precedence

Files: `src/parser/expr/**`, `src/parser/stmt/**`, `src/parser/precedence.rs`, `src/parser/grammar/veil.pest`, `src/parser/pest_adapter.rs`

- [ ] Precedence/associativity per spec:
  - [ ] Power `**` right-associative,
  - [ ] Ranges vs casts vs postfix order,
  - [ ] Bitwise vs logical ordering,
  - [ ] Pipeline `|>` between logical and assignment,
  - [ ] Assignment right-associative.
- [ ] Expressions:
  - [ ] Unary: `++x`, `--x`, `await x`, `+`, `-`, `~`, `!`, `&x`, `*x`, `move`.
  - [ ] Postfix: call `()`, index `[]`, field `.`, `x++`, `x--`, `x?`.
  - [ ] Infix: `|>`, `is`, `is not`, `in`, `not in`, `//` vs `/`, bitwise ops.
  - [ ] Closures: `async? (move)? |params| (-> type)? (expr|block)`.
  - [ ] Blocks: `unsafe { ... }`, `try { ... }`.
  - [ ] Async: `spawn { ... }` nursery block (parse-only initially).
- [ ] Statements:
  - [ ] `if let` patterns and guards completeness (already partial).
  - [ ] `const/var` declarations with destructuring.

### 3.6 Resolver Pass (new)

Files: `src/resolve/mod.rs` (new), integrate with `src/compiler/**`

- [ ] Build symbol tables: items, re-exports, aliases, visibility scopes.
- [ ] Enforce `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`.
- [ ] Normalize module paths and record source file to module mapping.
- [ ] Interface with incremental cache for fast invalidation.

Acceptance:

- [ ] Cross-module tests for visibility and re-exports pass.

### 3.7 Type Checker

Files: `src/typeck/**`

- Core typing:
  - [ ] Union/intersection type rules, subtyping/compatibility, match exhaustiveness on unions.
  - [ ] Logical (`&`, `|`) vs bitwise (`&`, `|`, `^`) with correct operand typing rules per spec.
  - [ ] Division rule: `/` floats only; `//` integers; emit actionable diagnostics with fix-its.
  - [ ] Optionals and postfix `?` (propagate early return semantics).
- Traits:
  - [ ] Trait definition and method obligations.
  - [ ] Associated types/consts constraints.
  - [ ] `impl Trait for Type` checking; constrained generics; `where` clause enforcement.
  - [ ] Basic `dyn Trait` object typing (vtable planned in codegen).
- Async/Safety:
  - [ ] `async fn` typing to Future<T>-like type (internal representation).
  - [ ] Validate `await` only in async context.
  - [ ] `spawn` scoping (semantic checks limited in first pass).
  - [ ] `unsafe`-only operations gated (raw pointer deref, union reads).
- Visibility:
  - [ ] Consume resolver’s symbol tables for access checks.

### 3.8 IR/HIR/Monomorphization (new passes)

Files: `src/hir/**` (new), `src/ir/**` (new), `src/mono/**` (new)

- [ ] Lower AST to HIR:
  - [ ] Desugar `|>` to nested calls, `++/--` to `+= 1`/`-= 1`, if-let to if/match forms, postfix `?` to early returns.
- [ ] Monomorphization:
  - [ ] Collect instantiations across program; materialize mono functions/types.
  - [ ] Remove generics from codegen input.
- [ ] IR for codegen: structured representation friendly to C emission.

### 3.9 Codegen (C Backend)

Files: `src/codegen/c/**`

- [ ] Consume IR instead of AST for simpler backend.
- [ ] Error propagation `?`: generate early-exit pattern with proper return conversions.
- [ ] Traits (phase 1): static dispatch under `where` monomorphization.
- [ ] Traits (phase 2): `dyn Trait` vtables, method tables, and trampolines.
- [ ] Unions/intersections:
  - [ ] Lower simple enums/union types; intersection mostly compile-time constraint (no runtime form).
- [ ] Async (phase 1): syntax compiles, state machine stubs; runtime to follow in later milestone.
- [ ] Visibility: symbol name mangling and export policy aligned with `export`.

### 3.10 Compiler/Incremental

Files: `src/compiler/**`

- [ ] Auto-prelude injection hook at root.
- [ ] Track pass-level artifacts in cache (parsed AST, HIR, type facts, mono, IR).
- [ ] Dependency graph enhanced with symbol-level edges (re-exports).
- [ ] Build merged C after IR lowering, not after raw AST.

### 3.11 CLI/Tooling

Files: `src/cli/**`

- [ ] Keep current subcommands stable.
- [ ] Add stubs: `ve fmt`, `ve lint` (no-ops or basic implementation).
- [ ] Add verbose flags to dump HIR/IR to `build/` for debugging (like existing `parsed_ast.txt`).

### 3.12 Stdlib

Files: `lib/std/src/**`

- [ ] Create `std/prelude.veil` aligned to spec (Option/Result and common imports).
- [ ] Minimal `std/core`, `std/string` alignment (aliases and conversions for `str`).
- [ ] Async runtime APIs introduced later (tracked in roadmap).

### 3.13 Tests

Files: `tests/**`

- [ ] Operators/precedence conformance: `/, //, **, |>, &, |, ^, ++/--`.
- [ ] Declarations: `const/var` and destructuring; let deprecation.
- [ ] Visibility: pub and scoped visibility across files.
- [ ] Types: `str`, `ch`, arrays, pointers/refs; union/intersection typing smoke tests.
- [ ] Optionals and postfix `?`.
- [ ] Traits/impl (phase 1): static dispatch and associated types.
- [ ] Async syntax smoke tests (compile-run).
- [ ] Import paths with `::` and `/`, import lists and aliases.

---

## 4) Cross-Cutting Acceptance Criteria

- Parsing:
- - [ ] New grammar features parse without panics and produce expected AST/HIR; Pest grammar + adapter produce AST/HIR matching spec-based golden files; artifacts under `build/`.
- Typing:
  - [ ] Division rules enforced with actionable errors and suggested fixes.
  - [ ] Optionals and `?` propagate correctly; tests cover nested `?`.
  - [ ] Union/intersection and trait obligations have clear diagnostics.
- Codegen:
  - [ ] All prior tests remain passing.
  - [ ] New features that are syntax-only (first phase) still generate compilable C.
- Incremental:
  - [ ] No regressions in incremental rebuild time; cache hit rate visible in verbose mode.
- Compatibility:
  - [ ] Deprecations (`let`, `string`, `&&/||`) emit warnings with auto-fix guidance (when `fmt` lands).
- Documentation:
  - [ ] Companion docs (architecture refactor, roadmap) reviewed and kept in `refs/`.

---

## 5) Risk Mitigation

- Logical operators migration: temporarily support both `&&/||` and single-char `&/|` for logical until formatter is available; emit warnings on legacy usage.
- `::` tokenization: ensure `::` is recognized ahead of `:`; adjust parser accordingly.
- Monomorphization move: isolate into a milestone; keep extensive regression tests; feature-gate behind a flag during transition.
- Async staging: introduce syntax and basic lowering first; runtime integration in later milestone to avoid destabilizing backend.
- Pest/PEG grammar ambiguity or performance regressions; keep grammar LL-friendly, add atomic rules to constrain backtracking, benchmark large sources; plan migration to the self-hosted parser before 1.0.

---

## 6) Immediate “First Two Sprints” Cut

Sprint A (Hardening & groundwork)

- [ ] Auto-prelude injection (`src/parser/mod.rs`).
- [ ] Division diagnostics and fix-its (`src/typeck/expr.rs`).
- [ ] Token additions: `const`, `pub`, `str`, `ch`, `::` (`src/lexer.rs`).
- [ ] Parser: `const/var` alongside `let` alias; visibility on `fn/struct/enum` and fields.
- [ ] Minimal tests for the above.
- [ ] Scaffold Pest grammar at `src/parser/grammar/veil.pest` and adapter `src/parser/pest_adapter.rs`; make it the default parser; add AST/HIR golden tests derived from the spec.

Sprint B (Precedence & operators phase 1)

- [ ] Precedence table overhaul (`src/parser/precedence.rs`).
- [ ] Add `//`, `|>`, single-char logical ops; keep legacy with warnings.
- [ ] Tests for precedence and new operators.
- [ ] Port precedence and new operators to `veil.pest`; validate AST/HIR against golden expectations and spec examples.
- [ ] Begin resolver scaffolding (no enforcement yet), plus import `::` support.

This document should be used alongside:

- Architecture Refactor Plan (HIR/IR/Mono/resolver design)
- Versioned Roadmap (milestones from pre-0.3 to 1.0)
