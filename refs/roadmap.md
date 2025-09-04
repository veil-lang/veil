# Veil Roadmap (pre-0.3 → 1.0)

This roadmap is aligned with the current codebase (parser/AST/HIR/resolve/typeck/normalize/mono/IR present) and prioritizes migrating the backend from C to LLVM (Inkwell) before 0.3. Each milestone includes goals, scope, checklists, acceptance criteria, and deprecation plans.

Status notation:

- [ ] To do
- [x] Done
- (dep) Depends on previous step

Legend:

- Parser = syntax + precedence only (no semantics)
- Resolver = name binding + visibility + re-exports
- Typeck = type checker
- HIR/IR/Mono = intermediate representations & passes
- Backend = LLVM (Inkwell) primary, C backend as fallback

---

## Pre-0.3 (IR→LLVM Backend Migration via Inkwell)

Goals

- Add a first-class LLVM backend (IR → LLVM IR) using Inkwell and integrate it into the CLI.
- Keep the existing C backend as a fallback; default to LLVM when available.
- Preserve the current pass pipeline; no frontend behavior changes in this milestone.

Scope

- New crate: `codegen-llvm` using Inkwell (feature-gated for a specific LLVM major, e.g., 16 or 18).
- IR→LLVM mapping: functions/blocks/params/locals; ints/floats/bool/pointers/string-as-i8\*; binops, compares, select, casts; load/store; call; branch/jump/return.
- External hooks: declare `iter_has_next(void*) -> i1` and `iter_next(void*) -> i64` to match the runtime ABI used by codegen-c/runtime.
- CLI:
  - Add `--backend {llvm,c}` (default: `llvm`) to `vec build`.
  - If `--verbose`, write `build/lowered_llvm.ll` (and optionally `.bc`) for inspection.
  - Keep `--no-cc` behavior; for LLVM backend, optionally emit `.o` and link via clang/lld.
- CI:
  - Install LLVM dev toolchain matching Inkwell feature and set the right env prefix (`LLVM
_SYS_xx0_PREFIX`).
  - Keep current clang/lld bundling for final link.
- Tests:
  - Golden parity runs: sample programs compiled with both backends produce identical observable outputs.
  - Emit `.c` and `.ll` in verbose CI jobs for debugging.

Checklist

- [ ] Create `crates/codegen-llvm` and wire Inkwell; expose a `Backend`-like API comparable to `codegen-c`.
- [ ] Implement IR→LLVM for: constants (int/float/str-as-i8\*), unary/binary ops, compares, `select`, `cast`, `load/store`, `call`, and terminators.
- [ ] Declare external iterator hooks; ensure correct linkage with `veil-runtime` when provided.
- [ ] CLI: `--backend` flag; default to LLVM; `.ll`/`.bc` dumped when `--verbose`.
- [ ] CI: add LLVM dev installation + env; build/test matrix on Linux/Windows.
- [ ] Parity tests: ensure same outputs across `--backend llvm` and `--backend c`.

Acceptance Criteria

- [ ] LLVM backend compiles samples end-to-end; outputs match C backend.
- [ ] `vec build` defaults to the LLVM backend; `--backend c` still functional.
- [ ] CI green on Linux/Windows with LLVM dev installed; `.ll` persisted in verbose runs.

Deprecations

- [ ] None yet. Announce intent to deprecate the C backend after 0.6 once LLVM path stabilizes.

---

## 0.3 (Grammar Alignment I — Declarations & Visibility)

Goals

- Introduce `const/var`, `str`, minimal visibility, and `::` paths as per spec (no backend changes here).

Scope

- Parser: `const` and `var` (keep `let` as alias w/ warnings).
- Parser: visibility modifiers: `pub`, `pub(crate)`, `pub(super)`, `pub(in path)`.
- Parser: module paths accept both `/` and `::` (normalize internally).
- Typeck: basic visibility checks within the same module; full resolver enforcement in 0.5.

Checklist

- [ ] Declarations: `const/var` with destructuring (identifier_list); `let` warns.
- [ ] Visibility attached to AST/HIR nodes.
- [ ] `::` and `/` path parsing normalized.
- [ ] Labeled loops and break/continue labels; while-let pattern parsing.
- [ ] Tests for declarations, visibility, `::` imports, labeled control flow.

Acceptance Criteria

- [ ] Syntax accepted; no regressions in LLVM or C backends.
- [ ] Warnings shown for `let`, legacy `string` if present.

Deprecations

- [ ] `let`/`string` warn only (no hard errors yet).

---

## 0.4 (Grammar Alignment II — Operators & Precedence)

Goals

- Align operator set and precedence with the v2.0 spec.

Scope

- Lexer/Parser: add `|>`, `//`, `++/--`, `is`, `is not`, `in`, `not in`.
- Precedence overhaul: power `**` right-assoc; bitwise vs logical order; pipeline level; assignment right-assoc.
- Transitional: parse `&&/||` as logical ops with warnings (prefer `&/|` per spec).

Checklist

- [ ] Precedence table updated and golden-tested.
- [ ] New tokens implemented; associativity correct.
- [ ] Transitional `&&/||` warn.
- [ ] Pest grammar synced; AST golden fixtures in CI.

Acceptance Criteria

- [ ] Precedence tests pass; no regressions in codegen-llvm/codegen-c.

Deprecations

- [ ] Plan removal of `&&/||` post-0.6 (after formatter auto-fix lands).

---

## 0.5 (Modules/Resolver/Visibility Completion)

Goals

- Implement/finish resolver; enforce visibility and re-exports across modules.

Scope

- Resolver: symbol tables, re-exports, aliasing, `pub(in path)`.
- Parser: import lists `{a as b} from x`; export blocks
  include `enum`, `type`, `union`.
- Integr
  ate resolver into the build pipeline with solid diagnostics.

Checklist

- [ ] Resolver pass integration point finalized.
- [ ] Cross-module visibility checks.
- [ ] Re-exports resolved and persisted in the symbol graph.
- [ ] Tests: cross-module visibility, aliases, re-exports.

Acceptance Criteria

- [ ] Clear resolution errors with “why not visible”.
- [ ] Incremental rebuilds react to re-export changes correctly.

Deprecations

- [ ] Enforce visibility strictly (remove accidental public exposure).

---

## 0.6 (Types & Patterns I)

Goals

- Add type syntax breadth and optional/error propagation.

Scope

- Types: `T | U` unions, `T & U` intersections, `&T` borrowed, `weak T`, `dyn Trait`, type aliases; postfix `?`; `where` clauses.
- Patterns: rest `...`, `@` bindings, reference patterns, range patterns.
- Typeck: optional `T?` with `expr?` early return typing; baseline union/intersection checks.

Checklist

- [ ] Parse complex type forms and advanced patterns.
- [ ] Typecheck `?` propagation and early returns.
- [ ] Tests for unions/intersections, aliases, optional flows, advanced patterns.

Acceptance Criteria

- [ ] Optional and `?` propagation type correctly; clear diagnostics.

Deprecations

- [ ] N/A

---

## 0.7 (Traits & Impl for Traits — Phase 1)

Goals

- Traits with associated types/consts and impls (static dispatch initially).

Scope

- Parser: trait declarations with default methods; associated types/consts; `impl Trait for Type` + `where`.
- Typeck: obligation solving (basic), method resolution; associated type binding.
- Backends: static dispatch via monomorphization.

Checklist

- [ ] Trait/impl syntax complete; obligation solver MVP.
- [ ] Tests: trait method calls; associated items.

Acceptance Criteria

- [ ] Static dispatch trait usage compiles and runs with helpful errors for missing/ambiguous impls.

Deprecations

- [ ] N/A

---

##

0.7.5 (Trait Objects — dyn & VTables)

Goals

- Enable dynamic dispatch via trait objects (`dyn Trait`) with vtables and object-safety rules.

Scope

- Typeck: object-safety checks; upcasting to `dyn Trait`; trait object bounds; optional downcasting story if included.
- Backend (LLVM): vtable layout; fat-pointer representation (`data` + `vtable`); method calls through vtable; codegen for object-safe methods.
- HIR/IR: represent trait object types and dynamic call sites if needed.
- Tests: dynamic dispatch examples; collections of `dyn Trait`; object-safety violation diagnostics.

Checklist

- [ ] Object-safety checker; `dyn Trait` type handling.
- [ ] Vtable generation/lookup; dynamic call lowering to LLVM.
- [ ] Tests for dyn dispatch and error cases.

Acceptance Criteria

- [ ] Working dynamic dispatch with stable vtable ABI; clear object-safety diagnostics.

Deprecations

- [ ] N/A

---

## 0.8 (IR/Monomorphization Refactor)

Goals

- Decouple monomorphization from backend; stabilize a HIR→Mono pass (move off AST-level mono).

Scope

- HIR: ensure normalization covers sugar (`|>`, postfix `?`, `++/--`, try-blocks).
- Monomorphization pass over HIR; IR lowering consumes mono-HIR.
- Both backends continue to consume IR.

Checklist

- [ ] Mono over HIR; remove legacy AST-level mono paths.
- [ ] Golden IR dumps for parity.

Acceptance Criteria

- [ ] No functional regressions; clearer pass boundaries; easier backend maintenance.

Deprecations

- [ ] Remove backend-embedded mono logic.

---

## 0.9 (Async Syntax + Minimal Runtime Hooks)

Goals

- Async syntax and minimal single-thread executor for smoke tests.

Scope

- Parser: `async fn`, `async {}`, `await`, `spawn {}`; async closures.
- Typeck: future-like typing; `await` in async scopes; basic nursery scoping.
- Runtime: minimal executor and futures stubs; no I/O yet.

Checklist

- [ ] Async grammar/typing; closures.
- [ ] Runtime shim; structured concurrency basics.

Acceptance Criteria

- [ ] Async examples run deterministically; spawn blocks join all tasks.

Deprecations

- [ ] N/A

---

## 0.10 (Unsafe/FFI & Memory Model Guards)

Goals

- `unsafe` blocks; unify FFI behavior and safety gates.

Scope

- Parser: unsafe blocks; finalize union decl.
- Typeck: gate raw pointer deref, union reads, and other unsafe ops.
- Backends: soft boundaries for unsafe paths; ABI docstrings.

Checklist

- [ ] Unsafe gating and diagnostics.
- [ ] Union read rules enforced.

Acceptance Criteria

- [ ] Safe code cannot perform unsafe operations sans `unsafe`.

Deprecations

- [ ] N/A

---

## 0.10A (Comptime Engine MVP)

Goals

- Implement `comptime { ... }` with a sandboxed interpreter suitable for constant folding, static assertions, and codegen hooks per spec.

Scope

- Parser: comptime blocks.
- Engine: sandboxed executor with step/time limits; no I/O; access to type info via introspection APIs; deterministic behavior.
- Integration: hooks for constant folding and simple table generation; error surfaces for exceeded limits.
- Tests: static assertions, small table-generation, compile-time validations.

Checklist

- [ ] Comptime parser and engine; limits enforcement.
- [ ] Integration points (const fold; simple gen).
- [ ] Tests for allowed vs disallowed behaviors.

Acceptance Criteria

- [ ] Comptime works per spec constraints; cannot hang or perform I/O; produces helpful diagnostics.

Deprecations

- [ ] N/A

---

## 0.11 (Error Handling Polish & Postfix `?`)

Goals

- Improve Result/Option ergonomics and diagnostics.

Scope

- Typeck: better context and conversions for common flows.
- Backends: early return paths for `?` optimized; minimize branches.

Checklist

- [ ] Diagnostics for `?` misuse; nested flows; loops/match/if-let combos.

Acceptance Criteria

- [ ] Complex error scenarios compile cleanly with actionable diagnostics.

---

## 0.11.5 (Panics & Hooks)

Goals

- Provide panic and assertion facilities per spec with hooks and unwind support.

Scope

- Macros/APIs: `panic!`, `assert!`, `debug_assert!`, `unreachable!`.
- Hooks: `set_panic_hook`, `PanicInfo` (message/location/backtrace where available).
- Unwind: `catch_unwind` (documented scope and caveats).
- Backend integration: emit necessary metadata/sections for backtraces where supported.
- Tests: panic hook invocation, assertions in debug/release, unwind boundary behavior.

Checklist

- [ ] Panic/Assert APIs; panic hook; unwind support and docs.
- [ ] Backtrace interoperability (best-effort across platforms).
- [ ] Tests covering hooks and behavior toggles.

Acceptance Criteria

- [ ] Panic semantics and hooks match spec; ergonomics acceptable with clear diagnostics.

---

## 0.12 (Attribute System & Derives)

Goals

- Implement attribute breadth and enforcement.

Scope

- Parser: `derive`, `repr`, `inline`, `deprecated`, `must_use`, kv pairs.
- Typeck: enforce `must_use`; warn on `deprecated`.
- Backends: honor `repr(C)`, `repr(packed)`, inlining hints.

Checklist

- [ ] Attribute model and validations; conformance tests.

Acceptance Criteria

- [ ] Attributes are effective in typeck/codegen; deprecation warnings include target versions.

Deprecations

- [ ] Schedule hard deprecations for `let`, `string`, `&&/||` with dates.

---

## 0.12.5 (Test & Bench Harness)

Goals

- Provide a basic test and benchmark runner integrated with attributes.

Scope

- Attributes: `#[test]`, `#[bench]`, grouping.
- CLI: `ve test`, `ve bench` with minimal output formatting and filtering.
- Harness: simple discovery and execution; bench iteration loops and basic timing.

Checklist

- [ ] Attribute discovery; runner commands in CLI.
- [ ] Output formatting; exit codes; filtering.
- [ ] Example tests and benches in CI.

Acceptance Criteria

- [ ] Tests/benches can be written, run, and reported consistently across platforms.

---

## 0.13 (Stdlib Expansion I + fmt/lint stubs)

Goals

- Usable prelude and core types; CLI stubs for fmt/lint.

Scope

- Stdlib: `std/prelude` (Option/Result, traits), basic `std/string`, `std/core`.
- CLI: `ve fmt`, `ve lint` minimal stubs.

Checklist

- [ ] Auto-prelude; examples compile with fewer imports.
- [ ] fmt/lint commands present; return success.

Acceptance Criteria

- [ ] Spec examples (non-async I/O) compile and run; CI invokes fmt/lint.

Deprecations

- [ ] Prepare to flip defaults: error on `string` and `let` by 0.16 (with formatter auto-fix).

---

## 0.14 (Structured Concurrency Runtime MVP)

Goals

- Multi-threaded work-stealing executor and nursery semantics.

Scope

- Runtime: thread pool; cooperative yields; join-on-scope exit semantics.
- Typeck: basic cancellation semantics aligned with `spawn`.

Checklist

- [ ] Task queues; cancellation signals for sibling tasks on failure.

Acceptance Criteria

- [ ] No task leaks; cancellation works in tests; basic perf acceptable.

---

## 0.14.5 (Async Runtime I/O, Channels, Timers, Introspection + Sync Primitives)

Goals

- Expand async runtime with performant I/O, channels, timers, introspection, and synchronization primitives.

Scope

- I/O: integrate epoll/kqueue/io_uring (platform-aware); non-blocking file and network primitives (TCP/UDP).
- Channels: unbounded/bounded MPSC channels with async send/recv; close semantics.
- Timers: `sleep`, `timeout`, `interval` with precise scheduling.
- Introspection: `runtime::stats()`; task-local storage basics.
- Synchronization: `std::sync::Mutex<T>`, `RwLock<T>` integrated with async runtime (awaitable locks, poisoning semantics, try\_\* variants); Atomics API.
- Tests: I/O smoke tests, channel semantics, timer accuracy, lock correctness and contention, perf micro-benchmarks.

Checklist

- [ ] Async I/O primitives; channels; timers; stats API.
- [ ] Mutex/RwLock/Atomics implemented and integrated with async runtime.
- [ ] Cross-platform coverage; feature flags for platform specifics.
- [ ] Tests across supported hosts.

Acceptance Criteria

- [ ] Runtime APIs match spec examples; stable behavior across platforms; basic throughput/latency acceptable; sync primitives behave correctly under contention.

---

## 0.15 (Module/Visibility Polish + Docs)

Goals

- Finalize module resolution UX; write docs.

Scope

- Resolver: edge cases for `pub(in path)`, shadowing rules; diagnostics polish.
- Docs: module/visibility guide; examples.
- CLI: symbol listing / token dump helpers.

Checklist

- [ ] Edge-case coverage + docs.

Acceptance Criteria

- [ ] Clear navigation and fix suggestions for users.

Deprecations

- [ ] Enable remaining soft deprecations as errors or lints-by-default.

---

## 0.16 (Performance & Incremental Improvements)

Goals

- Pass-level caching and parallelization; reduce rebuild times.

Scope

- Pass manager: cache keys; persist artifacts; re-export graph invalidation tuned.
- Parallelism: parse/HIR/typeck where dependency graph allows.
- Metrics: verbose cache hits/misses.

Checklist

- [ ] Cache AST/HIR/Resolved/Typed/Mono/IR; parallel compile independent subgraphs.

Acceptance Criteria

- [ ] Measurable rebuild time improvements; correct invalidation on symbol changes.

Deprecations

- [ ] Error on legacy tokens (`string`, `let`, `&&/||`) unless `--allow-legacy-syntax`.

---

## 0.16.5 (Toolchain — ve.toml, Targets, Cross)

Goals

- Introduce minimal toolchain manifest and targets surfaced via CLI consistent with spec.

Scope

- Manifest: `ve.toml` with package metadata, features, profiles (MVP).
- Targets: map target triples to LLVM codegen + linker flags; list targets; select target via CLI.
- Cross: initial cross-compilation plumbing leveraging LLVM/lld where available.
- Tests: manifest parsing; target selection; basic cross builds.

Checklist

- [ ] `ve.toml` parser and model; defaults; error messages.
- [ ] `ve targets list`; `--target` integration end-to-end to linker.
- [ ] Document setup for cross compilers where necessary.

Acceptance Criteria

- [ ] Developers can declare projects, select targets, and build for them (MVP).

---

## 0.17–0.19 (Spec Parity Hardening — Umbrella)

Goals

- Close remaining spec gaps and harden diagnostics/tests.

Scope

- Pattern matching: performance and guard corner cases.
- Unions/intersections: corner semantics; coherence refinements.
- Try expressions; decision tree optimizations; error message
  quality.
- Benchmarks: parser/typeck/codegen/runtime coverage.

Checklist

- [ ] Conformance tests green; expanded benchmarks in CI.

Acceptance Criteria

- [ ] No known spec deviations in implemented surfaces; docs complete.

---

## 0.17.5 (Const Generics & Lifetimes/Variance MVP)

Goals

- Introduce const generics for common patterns and a minimal lifetime/variance model for borrowed references.

Scope

- Const generics: arrays `[T; N]`, simple compile-time size checks, constraints (`[(); N]: Sized`-style).
- Lifetimes: basic checks that borrowed `&T` do not outlive sources; variance annotations where necessary; diagnostics wording.
- Out-of-scope: full lifetime algebra, higher-ranked trait bounds beyond simple cases (defer to post-1.0 if needed).

Checklist

- [ ] Parser/typeck support for const generics and minimal lifetime checks.
- [ ] Diagnostics for lifetime mismatches; tests exercising invariance/covariance basics.

Acceptance Criteria

- [ ] Safe, minimal lifetime enforcement and const generics available for common containers without overreach.

---

## 0.18 (Memory Model — ARC + Generational References)

Goals

- Implement ARC retain/release insertion and compile-time Generational References analysis; weak references semantics.

Scope

- Typeck: generational analysis (reject storing younger-gen refs into older-gen owners); diagnostics with fix-its (use `weak`).
- Codegen (LLVM): retain/release insertion points; destructor scheduling; optional non-atomic single-thread mode; weak upgrade/downgrade handling.
- Runtime hooks: counters and optional leak checker; debug builds can log ARC traffic (behind a flag).
- Tests: cycle prevention; weak upgrade returns none after free; deterministic destructor timing.

Checklist

- [ ] Generational analysis + diagnostics.
- [ ] Retain/release codegen in LLVM; drop order correctness.
- [ ] Weak semantics; upgrade/downgrade APIs/typeck integration.

Acceptance Criteria

- [ ] Programs that would form reference cycles are flagged at compile time; weak breaks cycles; ARC determinism validated in tests.

---

## 0.18.5 (Stdlib Expansion II)

Goals

- Broaden stdlib in line with the spec: io, fs, net, time, collections, json/toml; align async variants with runtime. Expand error/context/panic utilities.

Scope

- `std::io`, `std::fs`, `std::net`, `std::time`, `std::collections`, `std::json` (MVP), optional `std::toml`.
- Error facilities: standardized `Error` trait surfaces, context chaining utilities (Context/with_context), panic formatting helpers.
- Ensure async variants compose with 0.14.5 runtime APIs (read/write/accept/connect/sleep/timeout/interval).
- Stability: establish error types; `Result` usage; `must_use` on important return values.

Checklist

- [ ] API surfaces defined; docs with examples; unit/integration tests.
- [ ] Async compatibility verified; basic perf validated.
- [ ] Error/context/panic utilities implemented and documented.

Acceptance Criteria

- [ ] Stdlib usable for common tasks from spec examples with predictable behavior across platforms; improved ergonomics for error handling/panic formatting.

---

## 0.19 (Experimental — Advanced Types & Targets)

Goals

- Explore advanced type system features and additional targets under feature flags; optional binding generation and docgen prototypes.

Scope

- Advanced types (behind features): GATs, HKTs, refinement/dependent-type-like checks (limited), trait-object sums (dyn A + B) if pursued.
- WASM target MVP: build/link/run minimal programs (no full async I/O); document host bindings expectations.
- Binding generation: basic cbindgen surface; optional PyO3/napi examples; not required for 1.0.
- Docgen (optional): extract Markdown doc comments to simple HTML/Markdown output (MVP).

Checklist

- [ ] Feature flags for advanced types; tests behind flags; document instability.
- [ ] WASM target CI build; minimal runtime expectations documented.
- [ ] Example binding generation and docgen runs.

Acceptance Criteria

- [ ] Experimental tracks do not affect 1.0 stability; provide clear guidance and opt-in.

---

## 1.0 (Language v2.0 Stable)

Goals

- Feature-complete, documented, and stable toolchain with LLVM backend as default.

Scope

- Parser aligns with grammar; precedence verified.
- Resolver enforces visibility; module UX smooth.
- Typeck covers unions/intersections, traits (static; dyn if included), async typing, unsafe gating.
- Pipeline: HIR/Normalize/Mono/IR stable; LLVM backend primary; C backend optional/deprecated.
- Runtime MVP for structured concurrency passes tests; async I/O/channels/timers available.
- Stdlib prelude/core and expanded libs usable; CLI stable; fmt/lint usable.
- Incremental builds performant; CI stable; docs complete; migration guides (legacy → v2.0, C→LLVM).

Release Readiness Checklist

- [ ] LLVM backend is default; C backend optionally hidden behind a feature/deprecation notice.
- [ ] Verbose builds write AST/HIR/Resolved/Typed/Mono/IR + LLVM IR dumps.
- [ ] CI matrix green (Linux/Windows).

---

## Cross-Cutting Quality Gates

- Diagnostics
  - [ ] Error codes, notes, fix-its (division rule, visibility, trait obligations, optional `?`).
  - [ ] Spans preserved across lowering; tests verify rendering.

- Artifacts & Debugging
  - [ ] Verbose builds write AST/HIR/Resolved/Typed/Mono/IR/LLVM IR dumps.
  - [ ] CI uploads dumps for failing tests.

- Performance
  - [ ] Baseline benchmarks for parse/typeck/codegen/runtime.
  - [ ] Track regressions; set budgets per milestone.

- Compatibility & Deprecations
  - [ ] Clear warnings → lints → errors path for `let`, `string`, `&&/||`.
  - [ ] Provide formatter auto-fixes before enforcing errors.
  - [ ] Announce C backend deprecation timeline after LLVM backend stabilizes.

---

## Risk Management

- LLVM toolchain availability and Inkwell versioning
  - Mitigation: pin LLVM major supported; CI installs matching dev toolchain; document env setup.

- Precedence overhaul regressions
  - Mitigation: golden parser tests; operator suite.

- Moving monomorphization out of AST
  - Mitigation: dual-path behind a feature flag during transition; compare outputs in CI.

- Resolver complexity and incremental invalidation
  - Mitigation: strong unit tests; export graph fingerprints; deterministic behavior.

- Comptime engine sandboxing and resource limits
  - Mitigation: strict quotas; time/step guards; failable diagnostics with actionable hints.

- ARC/generational analysis false positives/negatives
  - Mitigation: staged rollout behind lint; extensive tests; clear weak-reference guidance.

- Async runtime cross-platform I/O differences
  - Mitigation: unified abstraction; per-platform feature flags; CI on multiple OSes.

---

## Milestone-to-Feature Traceability

- Grammar (0.3, 0.4, 0.6, 0.9, 0.10A)
- Modules/Visibility (0.5, 0.15)
- Traits (0.7 static, 0.7.5 dyn)
- HIR/Mono/IR (0.8)
- Async (0.9 syntax, 0.14 runtime, 0.14.5 I/O/channels/timers + sync primitives)
- Unsafe/FFI (0.10)
- Comptime (0.10A)
- Error handling polish (0.11)
- Panics & Hooks (0.11.5)
- Attributes (0.12)
- Test/Bench (0.12.5)
- Stdlib I (0.13) + Stdlib II (0.18.5)
- Performance/Incremental (0.16)
- Toolchain (0.16.5)
- Memory model ARC + Generational refs (0.18)
- Hardening (0.17–0.19 umbrella)
- Experimental advanced types/targets (0.19)
- Stable (1.0)

---

## Dependencies & Ordering (Summary)

1. Pre-0.3 (LLVM backend) → 0.3–0.4 (syntax + precedence)
2. 0.5 (resolver) → 0.6 (types breadth) → 0.7 (traits static) → 0.7.5 (dyn trait objects)
3. 0.8 (HIR/Mono refactor) → 0.9 (async syntax) → 0.10 (unsafe/FFI) → 0.10A (comptime)
4. 0.11 (error polish) → 0.11.5 (panics & hooks) → 0.12 (attributes) → 0.12.5 (test/bench)
5. 0.13 (stdlib I) → 0.14 (runtime MVP) → 0.14.5 (async I/O/channels/timers + sync)
6. 0.15 (resolver/docs) → 0.16 (perf/incremental) → 0.16.5 (toolchain/targets)
7. 0.17–0.19 (hardening), with 0.17.5 (const generics & lifetimes), 0.18 (ARC + Generational refs) and 0.18.5 (Stdlib II), and 0.19 (experimental) explicitly called out
8. 1.0 (stable)

---

## Success Criteria for 1.0

- Feature completeness vs. spec with explicit deferrals.
- Parser covers 95%+ of the spec including:
  - Advanced patterns (rest, @-bindings, references, ranges)
  - Closures and first-class functions
  - Async blocks and comprehensive async support
  - Unsafe and comptime blocks
  - Destructuring in variable declarations
  - Labeled loops and enhanced control flow
- Stable CLI and compilation model; fast incremental builds.
- Clear diagnostics with actionable fixes.
- Solid tests across parser/typeck/codegen/runtime; examples match spec.
- Documented migration for early adopters (legacy → v2.0, C backend → LLVM backend).
