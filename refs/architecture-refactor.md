# Architecture Refactor Plan (Maintainability and Scaling)

This document defines the target architecture to evolve Veil’s compiler and CLI into a scalable, maintainable, and extensible system that aligns with the v2.0 language specification. It focuses on principled layering, clean pass boundaries, and incremental, testable changes that preserve developer productivity.

Complementary docs:

- refs/migration-plan.md — Subsystem checklist and gap analysis
- refs/roadmap.md — Versioned milestones (pre-0.3 to 1.0)

---

## 1. Goals and Non-Goals

Goals

- Stabilize the front-end around a predictable pipeline: AST → HIR → (Resolve) → Typeck → HIR-Lowered → Mono → IR → Codegen.
- Decouple analysis from emission (move monomorphization and lowering out of the backend).
- Introduce a robust Resolver pass that enforces visibility and re-exports.
- Improve incremental compilation by caching pass-level artifacts with strong keys.
- Prepare for advanced features (traits, async/await/spawn, union/intersection types, `dyn Trait`, optional `?`, pipeline `|>`, postfix `++/--`, etc.).
- Keep the C backend as the initial target with clear backend interface(s).

Non-Goals (Here)

- Full async runtime design (tracked in roadmap).
- Exhaustive stdlib rollout (tracked in roadmap).

---

## 2. Current Pain Points

- Monomorphization is tightly coupled to the C backend, making feature work risky.
- Parser handles some desugaring and policy that belongs in lowering passes.
- No symbol resolver enforcing visibility and re-exports across the module graph.
- Precedence, operators, and syntax surface are expanding, increasing parser complexity.
- Type checker has to do multiple responsibilities (name resolution, structural typing, inference) without clear boundaries or caches.
- Incremental build caches at coarse granularity (module-level), limiting reuse.

---

## 3. Target Architecture (High-Level)

Pipeline (single module file flow; project graph composes these):

1. Parse
   - Input: source text
   - Output: AST (concrete syntax tree with spans)

2. HIR Lowering (Initial)
   - Input: AST
   - Output: HIR (typed-friendly, desugared structure, but still without symbol resolution or mono)

3. Resolve (Name Resolution + Visibility)
   - Input: HIR
   - Output: Resolved-HIR (names bound to symbol IDs, visibility enforced, re-exports applied)

4. Type Checking
   - Input: Resolved-HIR
   - Output: Typed-HIR (types assigned, constraints solved, traits obligations collected)

5. HIR Lowering (Normalization)
   - Input: Typed-HIR
   - Output: Norm-HIR (sugars removed: `|>`, `++/--`, postfix `?`, try blocks, etc.)

6. Monomorphization (Mono)
   - Input: Norm-HIR with generics
   - Output: Mono-HIR (generics instantiated; trait bounds satisfied; vtable requirements collected for dyn)

7. IR Generation
   - Input: Mono-HIR
   - Output: IR (backend-friendly representation: structured control-flow, canonical ops)

8. Codegen (C Backend initially)
   - Input: IR
   - Output: C code + compiled binary (plus runtime shims when needed)

Cross-cutting: Diagnostics, pass-level caches, fingerprints/hashes for incremental builds, and pass orchestration by a simple Pass Manager.

---

## 4. Proposed Repository & Crate Layout (Maintainable by Construction)

- Cargo workspace (modular crates; prevents 3000+ LoC files by design)
  - crates/lexer/ (minimal stub initially; tokens/trivia/diagnostics helpers)
    - lib.rs (public API, <400 LoC), tokens.rs, literals.rs, trivia.rs, diagnostics.rs

  - crates/parser/
    - grammar/veil.pest (Pest grammar; default frontend)
    - pest_adapter.rs (parse tree → AST with spans)
    - decl/, expr/, stmt/, types/, precedence.rs, mod.rs (each file <600 LoC; split further by node-kind as needed)

  - crates/hir/
    - lib.rs, nodes.rs (HIR structures), lower.rs (AST→HIR), visitors.rs

  - crates/resolve/
    - lib.rs, scope.rs, symbols.rs, visibility.rs, prelude.rs

  - crates/typeck/
    - lib.rs, expr.rs, stmt.rs, trait.rs, unify.rs, optional.rs, constraints.rs

  - crates/lower/
    - lib.rs, normalize.rs (HIR→Norm-HIR: desugar pipeline, ++/--, ?), pipeline.rs, option.rs

  - crates/mono/
    - lib.rs, collect.rs, instantiate.rs, vtable_req.rs

  - crates/ir/
    - lib.rs, types.rs, cfg.rs (control flow graph), builder.rs, verify.rs

  - crates/codegen-c/
    - lib.rs, emit.rs, names.rs, runtime_shims.rs

  - crates/compiler/
    - lib.rs, cache.rs, incremental.rs, graph.rs (module graph), pass_manager.rs

  - crates/diagnostics/
    - lib.rs, codes.rs, helpers.rs (error builders, notes, fix-its), render.rs

  - crates/runtime/
    - lib.rs, c_shims.rs (emit-once helpers), feature_flags.rs

  - crates/attr/
    - lib.rs (attribute parsing hooks, validation)

  - crates/helpers/
    - lib.rs (shared small utilities; keep tiny and stable)

  - crates/cli/
    - main.rs (vec), args.rs, commands/, outputs/

  - apps/vec/
    - Binary entry (may re-export crates/cli)

- Strict crate dependency graph (acyclic)
  - lexer → parser → hir → resolve → typeck → lower → mono → ir → codegen-c
  - diagnostics is used by all; helpers is leaf; compiler orchestrates passes and owns caches
  - No crate depends on codegen-c except compiler (or integration harness)

- File size budgets & structure
  - Hard caps: 500–800 LoC per file; split by domain (e.g., expr_binops.rs, expr_calls.rs)
  - One responsibility per file; prefer folders (decl/, expr/) to avoid growth
  - Code owners per crate; documented extension points (see §6)

- Extension playbook
  - New pass = new crate or module implementing the Pass interface (see §6); register in crates/compiler/pass_manager.rs
  - New syntax = update grammar/veil.pest + a focused adapter module; add parser golden tests; zero changes to unrelated crates

---

## 5. Data Model and IDs

Common invariants across passes:

- Span: always preserved for diagnostics.
- SymbolId: opaque integer for resolved entities (functions, types, structs, enums, traits, fields).
- TypeId (optional): canonical type identity used by type checker and IR; can be a hash-interned representation of Type.
- ModuleId: per file/compilation unit.

Inter-pass artifacts:

- AST: concrete syntax tree (rich spans).
- HIR: name-like nodes unresolved (has textual names) → post-Resolve carries SymbolIds.
- Typed-HIR: types assigned, constraints checked; carries TypeIds and evidence for trait obligations.
- Mono-HIR: generic instances materialized (mangled names or instance IDs).
- IR: normalized SSA/structured form for backend.

Interning (recommended):

- Strings (identifiers), types (canonical forms), and symbols are interned for memory/perf.
- Use simple interner pools keyed by hashes.

---

## 6. Pass Contracts and Interfaces

Interface principles (extensibility and boundaries)

- Each pass exposes a narrow interface: run(input: X, ctx: &Context) -> Result<Y>
- Inputs/outputs are stable data models (AST/HIR/Resolved-HIR/Typed-HIR/Norm-HIR/Mono-HIR/IR) with versions to permit evolution
- Passes are pure over inputs; all side effects (caches, logs) flow through Context
- No pass reaches across layers (e.g., Typeck must not call Codegen); all cross-talk via data artifacts
- Implementation is sharded across small modules (e.g., expr*\*.rs, stmt*\*.rs) to avoid large files; enforce caps via CI

### 6.1 Parser (AST)

- Input: source text
- Output: AST
- Responsibilities:
  - Only concrete syntax. Minimal sugar.
  - Accurate spans; attach attributes to nodes.
  - Primary frontend: Pest PEG grammar (.pest) produces parse trees; a thin adapter builds AST nodes with spans. This will be replaced by the self-hosted parser before 1.0.
  - Accept only `/` as module separator.
- Out of scope:
  - Visibility enforcement, name binding, generic instantiation.

### 6.2 HIR Lowering (AST→HIR)

- Input: AST
- Output: HIR
- Responsibilities:
  - Normalize literals, simple expressions into uniform HIR nodes.
  - Keep features intact but re-shape nodes to be type-friendly.
  - Preserve attributes and visibility flags (as parsed).
- Out of scope:
  - Final desugaring of operators/sugars (happens in later lowering).

### 6.3 Resolve

- Input: HIR
- Output: Resolved-HIR
- Responsibilities:
  - Build scopes; assign SymbolIds to names (locals, fields, modules, imports).
  - Handle re-exports, aliasing, and auto-prelude insertion (except in prelude).
  - Enforce visibility (`pub`, `pub(crate)`, `pub(super)`, `pub(in path)`).
  - Resolve module paths with `/` separator only.
- Out of scope:
  - Type inference/trait solving; only names and scopes.

### 6.4 Type Checking

- Input: Resolved-HIR
- Output: Typed-HIR
- Responsibilities:
  - Assign types; enforce `/` float vs `//` integer; optionals `T?`; postfix `?` semantics (as typing rule).
  - Union (`T | U`) / Intersection (`T & U`) typing lattice; match exhaustiveness for unions.
  - Trait definitions; impl checking; associated types/consts; simple `dyn Trait` object types; `where` clause enforcement.
  - Async typing rules: `async fn` returns Future-like type; `await` only in async context; `spawn` scoping (semantic checks to be extended later).
  - Unsafe gating for raw pointer deref and union reads.
- Out of scope:
  - Code generation/monomorphization; only typing and constraints generation.

### 6.5 HIR Normalization (Desugar)

- Input: Typed-HIR
- Output: Norm-HIR (still typed)
- Responsibilities:
  - Desugar pipeline `|>` to nested calls.
  - Desugar `++/--` to `+= 1`/`-= 1`.
  - Desugar postfix `?` to explicit early-return patterns.
  - Lower try blocks into appropriate control constructs.
  - Keep spans for all transformed nodes (best-effort merging).
- Out of scope:
  - Generics instantiation.

### 6.6 Monomorphization (Mono)

- Input: Norm-HIR
- Output: Mono-HIR
- Responsibilities:
  - Collect generic instances across call sites, including trait bounds.
  - Materialize mono functions/structs/enums; attach instance IDs/names.
  - Record vtable requirements for `dyn Trait` (phase 2) and static dispatch (phase 1).
- Out of scope:
  - Backend or C emission.

### 6.7 IR Generation

- Input: Mono-HIR
- Output: IR
- Responsibilities:
  - Produce structured control flow (basic blocks or a structured tree).
  - Canonicalize operations (casts, arithmetic, logical vs bitwise).
  - Prepare explicit call conventions (free functions, methods).
  - Encode early-return patterns from postfix `?` explicitly.
- Out of scope:
  - Target-specific code emission.

### 6.8 Codegen (C Backend)

- Input: IR
- Output: C code (and final binary)
- Responsibilities:
  - Emit runtime helpers once (arena, arrays, optional helpers).
  - Preserve name conventions: `ve_fn_<name>`, `ve_method_<type>_<name>`, etc.
  - Emit vtables (phase 2) and static dispatch (phase 1).
  - Implement visibility-driven symbol export rules.
  - Compile using clang (same toolchain path resolution as today).
- Out of scope:
  - Name resolution, typing, mono.

---

## 7. Pass Manager and Incremental Build

Pass Manager

- Enforces a strict DAG of pass dependencies at the crate level (no cycles); validated in CI
- Orchestrates execution and parallelizes where dependencies permit (module- and crate-level)
- Pass-level caching keyed by strong hashes; hot reload of artifacts for debugging
- Clear observability: timing per pass, cache hit/miss, memory, and invalidation reasons
- Provides verbose flags to dump artifacts:
  - AST → build/parsed_ast.txt
  - HIR/Resolved-HIR/Typed-HIR/Norm-HIR/Mono-HIR/IR → build/dumps/\*.txt
- Extensibility: pluggable pass registry in crates/compiler/pass_manager.rs; adding a pass requires no changes to existing passes

Caching (per module)

- Keys:
  - Parser cache: file content hash (sha256) + grammar (.pest) hash + toolchain version.
  - Resolve cache: HIR hash + import graph fingerprint.
  - Typeck cache: Resolved-HIR hash + type-env fingerprint (stdlib version).
  - Lowering cache: Typed-HIR hash.
  - Mono cache: Norm-HIR hash.
  - IR cache: Mono-HIR hash.
  - Codegen cache: IR hash + backend version + clang flags.
- Graph invalidation:
  - Use the dependency graph with re-export edges.
  - Propagate invalidation up the graph when exported symbol changes.

---

## 8. Diagnostics

- Centralize diagnostic creation in `src/diagnostics/`.
- Provide error codes and consistent formatting.
- Offer fix-its when possible (e.g., use `//` instead of `/` for integers).
- Attach context notes (visibility reasons, trait obligation source, etc.).
- Keep spans accurate through lowering by tracking original sources.

---

## 9. Runtime and Attributes

Runtime (C shims)

- Guarded emission (emit-once) per compile.
- Toggled by features (e.g., emit async scheduler stubs behind a config).
- Lives under `src/runtime/` to avoid backend entanglement.

Attributes

- Parse and validate: `derive`, `repr`, `inline`, `deprecated`, `must_use`, and custom metadata.
- Enforced in typeck (must_use) and codegen (`repr`, `inline` hints).
- Resides in `src/attr/`; integrates with parser to attach metadata.

---

## 10. Performance Considerations

- Interning (types, strings, symbols) to reduce memory/time.
- Reuse Vec buffers across passes when safe.
- Avoid cloning large trees; prefer immutable references and node IDs.
- Parallelize module-level passes when dependencies permit:
  - Parse/HIR for independent modules.
  - Typeck/Mono/IR/codegen on subgraphs where resolved.
- Parser (Pest) tuning: keep grammar unambiguous; use atomic/silent rules to limit backtracking; precompile grammar in release; measure parse times on large files.
- Retain single merged C output for now; revisit multi-object linking later.

---

## 11. Concurrency and Structured Concurrency (Future)

- Async feature rollout is staged:
  - Phase 1: Syntax/type support with minimal runtime to pass tests.
  - Phase 2: Work-stealing executor, I/O integration (platform-specific).
- Keep runtime boundary clean: a single header/body emission point; optional feature flags for Linux/Windows specifics.

---

## 12. Testing Strategy

Test categories:

- Parser golden tests (AST/HIR dumps).
- Precedence/operator suites.
- Name resolution/visibility tests across multiple modules.
- Type checker suites for:
  - `/` vs `//` rules; optionals and postfix `?`.
  - Union/intersection typing; match exhaustiveness.
  - Traits/impl/associated types (phase 1).
- Lowering tests for `|>`, `++/--`, `?`, try blocks (normalized forms).
- Monomorphization tests: instance discovery and duplication elimination.
- IR tests: CFG shape, instruction legality checks.
- Codegen E2E: compile & run; compare stdout/exit codes.
- Incremental: cache hit/miss scenarios; re-export invalidation.

Automation & CI Quality Gates:

- Verbose mode saves intermediate artifacts for triage
- CI artifacts attach dumps for failing cases
- Enforce file-size caps (500–800 LoC per file) and folder sharding (decl/, expr/, stmt/), fail build on violations
- Enforce crate dependency DAG (no forbidden edges; e.g., no pass crate depends on codegen-c)
- Formatting and lints: rustfmt/clippy as errors; deny warnings in CI
- Documentation & examples: require module-level docs and golden tests for new syntax/features
- Performance guardrails: parser/typeck/codegen micro-benchmark budgets; alert on regressions
- Build time and cache hit targets: report pass timings and cache stats; fail when thresholds regress beyond budget

---

## 13. Backends (Pluggability)

- Define a Backend trait (conceptual) for IR consumption:
  - fn emit(&self, ir: &ProgramIR, config: &CodegenConfig) -> Result<PathBuf>
- C backend is the default implementation.
- Future backends (WASM, LLVM IR) can reuse IR and most passes.

---

## 14. Configuration and Feature Flags

- Build config passed from CLI to codegen:
  - Target triple, optimization, verbose, extra clang args.
- Feature flags:
  - async-runtime-stub (on by default when syntax enabled)
  - vtable (phase 2), dyn-traits
  - repr-packed, repr-c enforcement

---

## 15. Backwards Compatibility and Deprecations

- Temporarily support legacy tokens/ops with warnings:
  - `let` → suggest `const`/`var`
  - `string` → suggest `str`
  - `&&/||` → suggest logical `&/|`
- Provide formatter auto-fixes in a later release.
- Document deprecations in MIGRATIONS.md, enforced post 0.5.

---

## 16. Implementation Steps (High-Level)

1. Introduce HIR crate and AST→HIR lowering (no behavior change).
2. Add Resolver pass (no enforcement initially), then enforce visibility.
3. Precedence overhaul and parser updates (operators, module paths); port grammar to Pest (.pest) with a thin AST builder adapter; adjust HIR/Typeck accordingly.
4. Normalize lowering (pipeline, ++/--, `?`, try) to shrink typeck/codegen burden.
5. Move monomorphization out of backend; establish Mono-HIR; update C backend to consume IR.
6. Establish IR and switch backend to IR consumption; maintain parity with existing tests.
7. Add trait support (phase 1 static dispatch), union/intersection rules; extend resolver/typeck.
8. Introduce async syntax/type support and minimal runtime stubs; tests only.
9. Incremental cache per pass; pass manager orchestration; resolver graph persisted.

Each step should:

- Land with incremental tests,
- Preserve CLI behavior,
- Dump artifacts in verbose mode for regression visibility.

---

## 17. Risks and Mitigations

- Risk: Moving mono out of backend breaks codegen.
  - Mitigation: Feature-flag the new pipeline; run both paths in CI for a period; compare outputs.
- Risk: Precedence overhaul causes subtle regressions.
  - Mitigation: Golden tests; expand operator conformance suites; document rules.
- Risk: Resolver complexity (re-exports) hurts incremental invalidation.
  - Mitigation: Build minimal re-export graph with strong tests; cache fingerprints per export set.
- Risk: Diagnostic drift (spans lost across lowering).
  - Mitigation: Carry original span sets; test error rendering with before/after snippets.
- Risk: PEG/Pest grammar ambiguity or performance regressions.
  - Mitigation: Keep grammar LL-friendly; add atomic rules to constrain backtracking; benchmark large sources; plan migration to self-hosted parser before 1.0.

---

## 18. Acceptance Criteria

- Clean pass boundaries and new directory structure implemented.
- Backend consumes IR (not AST); monomorphization pass stands alone.
- Resolver enforces visibility and re-exports with tests.
- Parser supports spec operators and module paths; precedence matches spec.
- Pest-based grammar (`src/parser/grammar/veil.pest`) and AST builder adapter produce AST parity with golden tests; used as the default frontend until self-hosting before 1.0.
- Type checker handles `/` vs `//`, optionals + `?`, unions/intersections (phase 1), basic trait checks (phase 1).
- Incremental builds cache pass artifacts; verbose dumps are accessible.
- All legacy tests pass; new conformance tests green; no performance regressions beyond tolerance (documented).

---

## 19. Appendix: HIR/IR Sketches (Conceptual)

HIR (selected)

- Module { items: [Item], attrs, span }
- Item:
  - Fn { name, vis, generics, params: [(Pat, TypeHint)], ret: TypeHint, body: Block, attrs }
  - Struct { name, vis, fields: [(name, vis, TypeHint)], generics, attrs }
  - Enum { name, vis, variants, generics, attrs }
  - Trait { name, vis, items, generics, where, attrs }
  - Impl { target, trait?: SymbolId, methods, generics, where, attrs }
  - TypeAlias { name, vis, type }
- Expr:
  - Call, Field, Index, Match, If, Loop, While, For, Block, Cast, Unary, Binary, Closure, AsyncBlock, Try, UnsafeBlock, Spawn, Template, New
- Types:
  - Prim, Ptr, Ref, Weak, Array, SizedArray, Optional, Union, Intersection, DynTrait, Generic, GenericInstance

IR (selected)

- ProgramIR { functions: [FnIR], globals: [GlobalIR], types: [TypeIR], tables: [VTableIR] }
- FnIR { name, sig, blocks: [Block], locals: [Local], attrs, link_visibility }
- Block { id, instrs: [Instr], term: Terminator }
- Instr: Assign, Call, Load, Store, BinOp, UnOp, Cast, Phi (if SSA), NewStruct, NewArray, etc.
- Terminator: Return, Branch { cond, then, else }, Switch, Loop, Unreachable

This plan emphasizes separation of concerns, predictable pass composition, better diagnostics, and scalable performance while preserving the current C backend and incremental build UX. It will enable us to deliver the v2.0 language features with reduced risk and increased maintainability.
