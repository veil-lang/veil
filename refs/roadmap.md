# Veil Roadmap (pre-0.3 → 1.0)

This roadmap organizes the migration to the v2.0 language spec into concrete, verifiable milestones. Each milestone includes goals, scope, checklists, acceptance criteria, and deprecation notes. It pairs with:
Status notation:

- [ ] To do
- [x] Done

- (dep) Depends on previous step

Legend:

- Parser = syntax + precedence only (no semantics)
- Resolver = name binding + visibility + re-exports
- Typeck = type checker
- HIR/IR/Mono = intermediate representations & passes
- Backend = C codegen

## Pre-0.3 (Hardening & Groundwork)

Goals

- Stabilize current pipeline and lay foundations for grammar changes without breaking users.
- Improve diagnostics for division and prepare for prelude auto-import.

Scope

- Parser: auto-insert `import std/prelude;` (exclude when parsing the prelude itself).
- Typeck: enforce `/` (float) vs `//` (integer) with fix-its.
- Lexer/Parser: add tokens for `const`, `pub`, `str`, `ch`, `::` (no behavior change yet).
- Tests: minimal cases for prelude insertion and division rules.

Checklist

- [ ] Auto-prelude injection for non-prelude modules.
- [ ] Division rule diagnostics with fix-its (/→// or cast).
- [ ] Tokens: const/pub/str/ch/:: wired in lexer and parseable (no codegen change).
- [ ] Minimal tests: prelude, division, tokens smoke tests.
- [ ] Pest grammar and adapter at src/parser/grammar/veil.pest and src/parser/pest_adapter.rs; make Pest the default parser; add smoke tests and AST golden fixtures.
- [ ] Parser: Add increment/decrement operators (++, --) with postfix support.

Acceptance Criteria

- [ ] Existing tests pass unchanged.
- [ ] New tests cover division and prelude.
- [ ] No performance regressions.

Deprecations

- [ ] Introduce warnings framework (to use later for `let`, `string`, `&&/||`).

---

## 0.3 (Grammar Alignment I — Declarations & Visibility)

Goals

- Introduce const/var, str, minimal visibility, and `::` paths.

Scope

- Parser: `const` and `var` declarations (keep `let` as alias w/ warning).
- Parser: visibility modifiers on items and fields: `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`.
- Parser: module paths accept both `/` and `::` (normalize internally).
- Typeck: basic visibility checks within same module unit; full resolver in 0.5.

Checklist

- [ ] Declarations: `const/var` with destructuring support (identifier_list), `let` warns.
- [ ] Visibility parsed and attached to AST nodes.
- [ ] Module paths `::` parsing (and `/`).
- [ ] Parser: Add labeled loops support (`identifier ':' loop`).
- [ ] Parser: Add `let pattern =` support in while loops.
- [ ] Parser: Add label support for break/continue statements.
- [ ] Tests: const/var with destructuring, visibility presence, `::` imports, labeled loops.

Acceptance Criteria

- [ ] Syntax accepted; no regressions in codegen.
- [ ] Warnings appear for `let`, `string` use (informational).

Deprecations

- [ ] `let` and `string` warn; no hard error yet.

---

## 0.4 (Grammar Alignment II — Operators & Precedence)

Goals

- Align operator set and precedence with spec.

Scope

- Lexer/Parser: add `|>`, `//`, `++/--`, `is`, `is not`, `in`, `not in`.
- Precedence overhaul (power `**` right-assoc; bitwise vs logical order; pipeline level; assignment right-assoc).
- Transitional support: keep `&&`/`||` as logical ops with warnings (prefer `&`/`|` logical per spec).

Checklist

- [ ] Precedence table updated to spec.
- [ ] New tokens parsed with correct associativity.
- [ ] Transitional logical ops (`&&`/`||`) warn.
- [ ] Parser: Add missing operators (`is`, `is not`, `in`, `not in`).
- [ ] Tests: precedence conformance suite.
- [ ] Port new operators and precedence to veil.pest; validate AST against golden expectations in CI.

Acceptance Criteria

- [ ] Precedence tests pass (golden expected trees).
- [ ] Legacy code continues to build; warnings present.

Deprecations

- [ ] `&&`/`||` warn; plan to remove after formatter auto-fix lands (post-0.6).

---

## 0.5 (Modules/Resolver/Visibility Completion)

Goals

- Implement a resolver pass; enforce visibility and re-exports.

Scope

- Resolver: symbol tables, re-exports, aliasing, `pub(in path)` scoping.
- Parser: import lists `{a as b} from x` extended; export blocks include `enum`, `type`, `union`.
- Compiler: integrate resolver in build pipeline; incremental graph enriched.

Checklist

- [ ] Resolver pass scaffolding and integration point.
- [ ] Visibility enforcement across modules.
- [ ] Re-exports resolved, symbol graph persisted.
- [ ] Tests: cross-module visibility, aliases, re-exports.

Acceptance Criteria

- [ ] Name resolution errors show precise locations and “why not visible”.
- [ ] Incremental rebuilds respond to re-export changes correctly.

Deprecations

- [ ] Enforce visibility where previously everything was effectively public.

---

## 0.6 (Types & Patterns I)

Goals

- Add type syntax breadth and optional/error propagation.

Scope

- Parser: union types `T | U`, intersection `T & U`, `&T` borrowed, `weak T`, `dyn Trait`, type aliases; postfix `?`; `where` clauses.
- Typeck: optional `T?` flows and `expr?` early return typing; groundwork for union/intersection (basic checks).

Checklist

- [ ] Types: parse unions/intersections, refs/pointers/weak, dyn.
- [ ] Postfix `?` parsed and typed.
- [ ] Parser: Advanced pattern matching - rest patterns (...), @ bindings, reference patterns (&pattern), range patterns.
- [ ] Parser: Try expressions (`try { ... }`).
- [ ] Tests: unions/intersections smoke; optional flows; type aliases; advanced patterns.

Acceptance Criteria

- [ ] Optional and `?` propagation works; diagnostics clear.
- [ ] Union/intersection type compatibility checks pass basic cases.

Deprecations

- [ ] N/A

---

## 0.7 (Traits & Impl for Traits — Phase 1)

Goals

- Introduce traits, associated types/consts, and impls (static dispatch initially).

Scope

- Parser: trait decl (methods w/ default bodies), associated types/consts, impl blocks `impl Trait for Type`, `where` clauses.
- Typeck: obligations checking, associated type binding, method resolution (no dyn dispatch yet).
- Backend: static dispatch via monomorphization of where-bounded generics.

Checklist

- [ ] Trait syntax + impl syntax + where clauses.
- [ ] Obligation solver (basic) and errors with sources.
- [ ] Tests: trait method calls, associated types/consts.

Acceptance Criteria

- [ ] Static dispatch trait usage compiles and runs.
- [ ] Clear errors when impls missing or ambiguous.

Deprecations

- [ ] N/A

---

## 0.8 (IR/Monomorphization Refactor)

Goals

- Decouple monomorphization from backend, introduce HIR/IR pipeline.

Scope

- HIR: AST→HIR lowering (no behavior change).
- Lowering: desugar `|>`, `++/--`, postfix `?`, try-blocks.
- Mono pass: collect/instantiate generics.
- IR: backend-friendly representation; C backend consumes IR.

Checklist

- [ ] HIR introduced with dumps in `build/`.
- [ ] Lowering pass creates normalized HIR.
- [ ] Mono pass produces mono-HIR; backend updated to IR.
- [ ] Tests: parity with previous outputs; golden IR checks.

Acceptance Criteria

- [ ] No functional regressions; backend simplified.
- [ ] Incremental caching viable at pass level.

Deprecations

- [ ] Remove old backend-embedded mono paths.

---

## 0.9 (Async Syntax + Minimal Runtime Hooks)

Goals

- Add async syntax and minimal executor to run smoke tests.

Scope

- Parser: `async fn`, `async {}`, `await`, `spawn {}` nursery blocks; closures `async|...|`.
- Typeck: async typing (Future-like), validate `await` in async scopes, basic spawn scoping.
- Runtime: minimal single-thread executor and stub futures; no I/O integration yet.
- Tests: async smoke tests (simple awaits, spawn join semantics).

Checklist

- [ ] Async grammar and typing.
- [ ] Parser: Async blocks (`async { ... }`), async closures (`async |args| { ... }`).
- [ ] Parser: Basic closure expressions (`|args| expr`, `|args| { ... }`).
- [ ] Minimal runtime shims in C header.
- [ ] Tests for structured concurrency basics, closures, async blocks.

Acceptance Criteria

- [ ] Async examples compile and run deterministically.
- [ ] No leaks; spawn blocks join all tasks.

Deprecations

- [ ] N/A

---

## 0.10 (Unsafe/FFI & Memory Model Guards)

Goals

- Add `unsafe` blocks semantics and unify FFI edge cases.

Scope

- Parser: `unsafe {}` blocks; union decl finalized.
- Typeck: gate raw pointer deref, union reads, and other unsafe operations.
- Backend: clearly separated unsafe codepaths; comments/docstrings for safety invariants.
- Tests: unsafe-required operations + FFI metadata conformance.

Checklist

- [ ] Parser: Unsafe blocks (`unsafe { ... }`) with proper scoping.
- [ ] Parser: Comptime blocks (`comptime { ... }`) for metaprogramming.
- [ ] Unsafe gating and diagnostics.
- [ ] Union read rules enforced.
- [ ] FFI polish where needed.

Acceptance Criteria

- [ ] Safe code cannot perform unsafe operations without `unsafe`.
- [ ] FFI examples compile and run across targets.

Deprecations

- [ ] N/A

---

## 0.11 (Error Handling Polish & Postfix `?`)

Goals

- Tighten Result/Option idioms and diagnostics.

Scope

- Typeck: improve conversions and context on common error flows.
- Backend: early return paths for `?` optimized; reduce branches when possible.
- Tests: nested `?`, combined with loops/match/if-let.

Checklist

- [ ] Richer diagnostics for `?` misuse.
- [ ] Early-return lowering is compact and readable in generated C.

Acceptance Criteria

- [ ] Complex error-handling examples compile cleanly.
- [ ] Diagnostics suggest actionable fixes.

Deprecations

- [ ] N/A

---

## 0.12 (Attribute System & Derives)

Goals

- Implement attribute breadth and basic enforcement.

Scope

- Parser: extend attribute parsing (`derive`, `repr`, `inline`, `deprecated`, `must_use`, kv-pairs).
- Typeck: enforce `must_use`; warnings on `deprecated`.
- Backend: honor `repr(C)`, `repr(packed)`, inlining hints.
- Tests: attribute conformance, deprecation warnings.

Checklist

- [ ] Attribute model + validation hooks.
- [ ] Enforcement where applicable.

Acceptance Criteria

- [ ] Attributes observed in typeck/codegen.
- [ ] Deprecation warnings reference version deadlines.

Deprecations

- [ ] Schedule hard deprecations for `let`, `string`, `&&/||` with dates.

---

## 0.13 (Stdlib Expansion I + fmt/lint stubs)

Goals

- Provide usable prelude and core types; introduce tooling stubs.

Scope

- Stdlib: `std/prelude` (Option/Result, common traits), basic `std/string`, `std/core`.
- CLI: `ve fmt`, `ve lint` stubs (no-ops or minimal).
- Tests: examples using prelude imports only.

Checklist

- [ ] Prelude inserted automatically; user code compiles with fewer imports.
- [ ] fmt/lint commands exist and return success.

Acceptance Criteria

- [ ] Examples in spec run (subset that doesn’t require async I/O).
- [ ] CI includes fmt/lint invocation.

Deprecations

- [ ] Ready to flip default: error on `string` and `let` by 0.16 (with auto-fix in fmt later).

---

## 0.14 (Structured Concurrency Runtime MVP)

Goals

- Provide a simple multi-threaded work-stealing executor and `spawn` nursery semantics.

Scope

- Runtime: work-stealing pool; cooperative yields; join-on-scope exit semantics.
- Typeck: basic cancellation semantics consistent with spawn blocks (best-effort).
- Tests: structured concurrency correctness micro-tests.

Checklist

- [ ] Thread pool; task queues; finalizers at scope exit.
- [ ] Cancellation signals for siblings on failure.

Acceptance Criteria

- [ ] Spawn blocks never leak tasks; cancellation works in tests.
- [ ] Performance is acceptable for microbenchmarks.

Deprecations

- [ ] N/A

---

## 0.15 (Module/Visibility Polish + Docs)

Goals

- Finalize module resolution UX and developer docs.

Scope

- Resolver: corner cases for `pub(in path)`, shadowing rules, and diagnostics improvements.
- Docs: user-facing module and visibility guide.
- CLI: `--list-symbols` or enhanced `dump_tokens` alternative for debugging.

Checklist

- [ ] Resolver covers edge cases with tests.
- [ ] Docs in tree and examples.

Acceptance Criteria

- [ ] External users can navigate modules with clear errors and fix suggestions.

Deprecations

- [ ] Enforce any remaining soft deprecations (enable errors or lints by default).

---

## 0.16 (Performance & Incremental Improvements)

Goals

- Enhance pass-level caching and reduce rebuild times.

Scope

- Compiler: pass manager; cache keys per pass; re-export graph invalidation tuned.
- Parallelization: parse/HIR/typeck where dependencies permit.
- Metrics: verbose mode prints cache hits/misses.

Checklist

- [ ] Cache persists AST/HIR/Resolved/Typed/Mono/IR.
- [ ] Parallel compile on independent subgraphs.

Acceptance Criteria

- [ ] Measurable reductions in rebuild time on sample projects.
- [ ] Correct cache invalidation on symbol changes.

Deprecations

- [ ] Error on legacy tokens (`string`, `let`, `&&/||`) unless `--allow-legacy-syntax`.

---

## 0.17–0.19 (Spec Parity Hardening)

Goals

- Close all remaining gaps to spec and harden diagnostics and tests.

Scope

- Pattern matching: performance and guard edge cases.
- Union/intersection corner cases; trait coherence refinements.
- try expressions; decision tree optimizations; error messages.

Checklist

- [ ] Exhaustive conformance tests green.
- [ ] Benchmark harness expanded for parser/typeck/codegen/runtime.

Acceptance Criteria

- [ ] No known spec deviations in implemented feature set.
- [ ] Documentation complete and aligned.

Deprecations

- [ ] Remove `--allow-legacy-syntax` flag by 1.0 (unless strong user demand).

---

## 1.0 (Language v2.0 Stable)

Goals

- Deliver a feature-complete, documented, and stable toolchain aligned with refs.

Scope

- All major features shipped per milestones.
- Docs and migration guides finalized.
- Self-hosting plan initiated (post-1.0 track) with priorities on lexer/parser/typeck.

Release Readiness Checklist

- [ ] Parser aligns with grammar; precedence verified.
- [ ] Resolver enforces visibility; module UX smooth.
- [ ] Typeck covers unions/intersections, traits (static and dyn phase 2 if included), async typing, unsafe gating.
- [ ] HIR/IR/Mono pipeline is default; backend consumes IR.
- [ ] Runtime MVP for structured concurrency passes tests.
- [ ] Stdlib prelude and core modules usable; examples compile.
- [ ] CLI stable; fmt/lint usable (even if minimal).
- [ ] Incremental build performance acceptable; CI stable.
- [ ] Documentation complete; migration guides (legacy → v2.0).

---

## Cross-Cutting Quality Gates

- Diagnostics
  - [ ] Error codes, notes, fix-its (division rule, visibility, trait obligations, optional `?`).
  - [ ] Spans preserved through lowering; tests verify rendering.

- Artifacts & Debugging
  - [ ] Verbose builds write AST/HIR/Resolved/Typed/Mono/IR dumps.
  - [ ] CI uploads dumps for failing tests.

- Performance
  - [ ] Baseline benchmarks for parse/typeck/codegen/runtime.
  - [ ] Track regressions; set budgets per milestone.

- Compatibility & Deprecations
  - [ ] Clear warnings → lints → errors path for `let`, `string`, `&&/||`.
  - [ ] Provide formatter auto-fixes before enforcing errors.

---

## Risk Management

- Precedence overhaul regressions
  - Mitigation: golden parser tests; operator suite.

- Moving mono out of backend
  - Mitigation: dual-path behind a feature flag during transition; compare outputs in CI.

- Resolver complexity and incremental invalidation
  - Mitigation: strong unit tests; export graph fingerprints; deterministic behavior.

- Async runtime complexity
  - Mitigation: staged approach (syntax/type first; runtime MVP later); limit I/O scope pre-1.0.

---

## Milestone-to-Feature Traceability

- Grammar (0.3, 0.4, 0.6, 0.9, 0.10) - Core syntax (0.3-0.4), advanced patterns (0.6), closures/async blocks (0.9), unsafe/comptime blocks (0.10)
- Modules/Visibility (0.5, 0.15)
- Traits (0.7)
- HIR/IR/Mono (0.8)
- Async (0.9, 0.14)
- Unsafe/FFI (0.10)
- Error handling polish (0.11)
- Attributes (0.12)
- Stdlib + tools (0.13)
- Performance/Incremental (0.16)
- Hardening (0.17–0.19)
- Stable (1.0)

---

## Dependencies & Ordering (Summary)

1. Pre-0.3 → 0.3 → 0.4 (syntax + precedence)
2. 0.5 (resolver) → 0.6 (types breadth) → 0.7 (traits)
3. 0.8 (HIR/IR/Mono refactor) → 0.9 (async syntax) → 0.10 (unsafe/FFI)
4. 0.11–0.13 (polish + attributes + std/tools)
5. 0.14 (runtime MVP) → 0.15–0.16 (polish/perf) → 0.17–0.19 (hardening)
6. 1.0 (stable)

---

## Success Criteria for 1.0

- Feature completeness vs. refs (documented any deliberate deferrals).
- Parser covers 95%+ of language specification including:
  - Advanced patterns (rest, @-bindings, references, ranges)
  - Closures and first-class functions
  - Async blocks and comprehensive async support
  - Unsafe and comptime blocks
  - Destructuring in variable declarations
  - Labeled loops and enhanced control flow
- Stable CLI and compilation model; fast incremental builds.
- Clear diagnostics with actionable fixes.
- Solid tests across parser/typeck/codegen/runtime; examples match spec.
- Documented migration for early adopters (legacy → v2.0).
