# Baselines: AST and C Emission

This document explains how to capture and update compiler baselines for:
- Parsed AST dumps
- Generated C source
- Timings and checksums

Baselines are used to:
- Guard refactors with golden snapshots
- Track performance regressions
- Aid debugging by collecting consistent artifacts on failures

The capture process is designed to avoid compiling/running the C backend unless explicitly desired.

---

## What gets captured

Per source file:
- ast.txt — Parsed AST (pretty-printed)
- emitted.c — Generated C (from the C backend frontend)
- summary.json — Metadata (timings, sizes, checksums, exit code, notes)
- capture.log — Raw CLI output for triage

Batch runs also produce:
- build/baselines/<timestamp>/manifest.json — Aggregated results across all files
- build/baselines/<timestamp>/files.txt — Flat list of input files included

Directory structure (example):

```
build/
  baselines/
    20250101T120000Z/
      manifest.json
      files.txt
      files/
        tests__hello_world.veil/
          ast.txt
          emitted.c
          summary.json
          capture.log
        lib__std__src__prelude.veil/
          ast.txt
          emitted.c
          summary.json
          capture.log
```

Notes:
- Paths are sanitized (e.g., `/` becomes `__`) to keep per-file output contained.
- `summary.json` includes absolute paths, hashes (sha256 when available), sizes, and a short note on how artifacts were produced.

---

## Prerequisites

- A working `ve` binary (either installed in PATH or built locally).
- Unix-like environment (Linux/macOS) with:
  - bash
  - python3 (for portable path handling)
  - `sha256sum` or `shasum` (optional; used for checksums)

---

## Capturing baselines for a single file

Use the helper script:

```
./scripts/capture_baselines.sh <input.veil> [--out <dir>] [--ve <path_to_ve>] [--keep-shim]
```

Examples:
- Minimal (uses `ve` from PATH, writes under `build/baselines/<timestamp>/...`):
  ```
  ./scripts/capture_baselines.sh tests/hello_world.veil
  ```

- Custom output directory and explicit `ve` path:
  ```
  ./scripts/capture_baselines.sh tests/hello_world.veil --out build/baselines/smoke --ve ./target/release/ve
  ```

What it does:
- Invokes `ve -v` to ensure the AST dump is produced at build/parsed_ast.txt.
- Generates C at build/temp.c.
- Captures both into the provided output directory.
- Avoids compiling C by using `--no-cc` when supported; otherwise, it creates a temporary no-op `clang` shim to keep the run lightweight.

Tip:
- Run with `-v` (verbose) by default to ensure the AST dump is generated. The script takes care of this.

---

## Capturing baselines for all tests and stdlib

Use the batch script:

```
./scripts/capture_all_baselines.sh [--ve <path_to_ve>] [--out <dir>] [--keep-shim]
                                   [--only-tests] [--only-stdlib]
                                   [--tests-glob "<glob>"] [--stdlib-glob "<glob>"]
                                   [--dry-run]
```

Examples:
- Full capture (tests + stdlib) with auto-discovery:
  ```
  ./scripts/capture_all_baselines.sh
  ```

- Only tests:
  ```
  ./scripts/capture_all_baselines.sh --only-tests
  ```

- Only stdlib with a custom ve binary and output directory:
  ```
  ./scripts/capture_all_baselines.sh --only-stdlib --ve ./target/release/ve --out build/baselines/preflight
  ```

- Dry run (show which files would be captured):
  ```
  ./scripts/capture_all_baselines.sh --dry-run
  ```

Defaults:
- Tests glob: `tests/**/*.veil` plus `tests/neg/**/*.veil`
- Stdlib glob: `lib/std/src/*.veil`

Outputs:
- `manifest.json` aggregates all `summary.json` files from subdirectories.
- `files.txt` lists all inputs included in the batch.

---

## `summary.json` schema (example)

```json
{
  "input": "/abs/path/to/tests/hello_world.veil",
  "timestamp": "20250101T120000Z",
  "ve_path": "/abs/path/to/ve",
  "elapsed_ms": 123,
  "exit_code": 0,
  "artifacts": {
    "ast": {
      "path": "/abs/path/to/build/baselines/20250101T120000Z/files/tests__hello_world.veil/ast.txt",
      "exists": true,
      "size_bytes": 4523,
      "sha256": "e3b0c44298..."
    },
    "c": {
      "path": "/abs/path/to/build/baselines/20250101T120000Z/files/tests__hello_world.veil/emitted.c",
      "exists": true,
      "size_bytes": 18237,
      "sha256": "4d967f9a0a..."
    },
    "log": {
      "path": "/abs/path/to/build/baselines/20250101T120000Z/files/tests__hello_world.veil/capture.log",
      "exists": true
    }
  },
  "notes": [
    "AST is captured by running ve with verbose mode, which emits build/parsed_ast.txt.",
    "C output is captured from build/temp.c prior to C compilation.",
    "Used --no-cc to avoid compiling C."
  ]
}
```

`manifest.json` is a JSON array of these objects (one per input file).

---

## Updating baselines

- Capture a new batch into a fresh timestamped directory:
  ```
  ./scripts/capture_all_baselines.sh --out build/baselines/NEW_RUN
  ```
- Use `manifest.json` to navigate and compare with previous runs.
- Recommended: keep old runs (they are cheap and useful for diffs). If space is an issue, prune older runs manually.

Optional (team practice):
- Maintain a `refs/baselines-index.json` or a symlink `build/baselines/latest` pointing to the most recent approved baseline run.

---

## CI integration (guidelines)

- As part of a PR:
  - Run `capture_all_baselines.sh` (or a subset) and upload artifacts (AST/C/logs/manifest) for inspection on failures.
  - Compare AST/C outputs against approved baselines (golden tests) to flag unexpected drift.
  - Track elapsed times (`elapsed_ms`) for regression detection; alert on substantial deviations.

- On failures:
  - Attach `capture.log`, `ast.txt`, and `emitted.c` for the offending cases.
  - Record the command lines used for reproducibility.

---

## Common pitfalls and troubleshooting

- No AST dump:
  - Ensure `-v` (verbose) is used; the scripts enforce this.
  - Check `capture.log` for early errors.

- Missing `ve` in PATH:
  - Pass `--ve ./target/release/ve` (or your path) to the scripts.

- C compilation failure:
  - Baseline capture does not need C compilation. The scripts use `--no-cc` when supported, or a temporary `clang` shim.

- Negative tests (`tests/neg`):
  - These often fail type checking intentionally. AST and emitted C may or may not be present depending on how far the pipeline runs. The scripts continue and record what exists.

- Platform specifics:
  - Output executable names differ by OS during normal runs (`program.exe` on Windows, `program` elsewhere). Baseline capture targets the AST and C emission steps, which are platform-agnostic.

- Paths and permissions:
  - If running in restricted environments, ensure write access to `build/` or override with `--out`.

---

## Advanced tips

- Narrow scope for quick iterations:
  ```
  ./scripts/capture_all_baselines.sh --only-tests --tests-glob "tests/hello_world.veil"
  ```

- Keep the temporary clang shim for post-mortem:
  ```
  ./scripts/capture_baselines.sh tests/hello_world.veil --keep-shim
  ```

- Compare across runs (example):
  ```
  diff -u build/baselines/RUN_A/files/tests__hello_world.veil/ast.txt \
          build/baselines/RUN_B/files/tests__hello_world.veil/ast.txt
  ```

---

## FAQ

Q: Do I need to build the `ve` binary first?
- Yes. Either install it in PATH or pass `--ve` with the path to your local build.

Q: Can I baseline only a subset of files?
- Yes. Use `--only-tests`, `--only-stdlib`, or pass a custom `--tests-glob`/`--stdlib-glob`.

Q: Will this modify my source tree?
- Baselines are written under `build/baselines/...` by default. No source files are changed.

Q: Can I run this on CI?
- Yes. Ensure the environment can execute bash and has permissions to write to the workspace. Upload the `build/baselines/<timestamp>` directory as an artifact.

---

If you have questions or want to extend baseline coverage (e.g., HIR/IR dumps), add a section here and wire new artifacts into the capture scripts and manifests.
